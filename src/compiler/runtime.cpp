/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR
    IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
    EVENT SHALL JOE HERMASZEWSKI OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
    INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
    THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are
    those of the authors and should not be interpreted as representing official
    policies, either expressed or implied, of Joe Hermaszewski.
*/

#include "runtime.hpp"

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/IRBuilder.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>
#include <llvm/Support/TargetSelect.h>

#include <engine/types.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

/// TODO find this automatically
const std::map<Type, Runtime::TypeInformation> Runtime::s_TypeInformationMap =
{
#if defined(ARCH_X86_64)
    {Type::VOID,   {ReturnType::IGNORE, ParamType::IGNORE}},
    {Type::BOOL,   {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::STRING, {ReturnType::DEFAULT, ParamType::EXPAND}}
#elif defined(ARCH_I686)
    {Type::VOID,   {ReturnType::IGNORE, ParamType::IGNORE}},
    {Type::BOOL,   {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::STRING, {ReturnType::INTEGER, ParamType::POINTER}}
#endif
};

const std::map<RuntimeFunction, Runtime::FunctionInfo> Runtime::s_FunctionInfos=
{
    {RuntimeFunction::STRING_EQUAL,    {"String_Equal",
                                        Type::BOOL,   {Type::STRING,
                                                       Type::STRING}}},
    {RuntimeFunction::STRING_NOTEQUAL, {"String_NotEqual",
                                        Type::BOOL,   {Type::STRING,
                                                       Type::STRING}}},
    {RuntimeFunction::STRING_CONCAT,   {"String_Concat",
                                        Type::STRING, {Type::STRING,
                                                       Type::STRING}}},
    {RuntimeFunction::STRING_COPY,     {"String_Copy",
                                        Type::STRING, {Type::STRING}}},
    {RuntimeFunction::STRING_DESTROY,  {"String_Destroy",
                                        Type::VOID,   {Type::STRING}}},
};

Runtime::Runtime()
    :m_LLVMContext( llvm::getGlobalContext() )
{
    llvm::InitializeNativeTarget();

    llvm::OwningPtr<llvm::MemoryBuffer> buffer;
    llvm::MemoryBuffer::getFile( "runtime.bc", buffer );
    assert( buffer && "Couldn't load runtime library" );
    m_RuntimeModule = llvm::ParseBitcodeFile ( buffer.get(),
                                               m_LLVMContext );
    assert( m_RuntimeModule && "Couldn't parse runtime library" );

    for( const auto& function_info : s_FunctionInfos )
    {
        m_Functions[function_info.first] =
                     m_RuntimeModule->getFunction( function_info.second.name );
        assert( m_Functions[function_info.first] &&
                "Couldn't find function in runtime" );
    }

#if defined( ARCH_X86_64 )
    m_StringType = llvm::cast<llvm::StructType>(
                m_Functions[RuntimeFunction::STRING_CONCAT]->getReturnType() );

    assert( m_StringType->isLayoutIdentical( llvm::cast<llvm::StructType>(
                m_Functions[RuntimeFunction::STRING_CONCAT]->getReturnType() ) ) );
#elif defined( ARCH_I686 )
    m_StringType = m_RuntimeModule->getTypeByName( "struct.jl_string" );
#else
    llvm::Type* size_type = GetLLVMType( Type::U32 );
    llvm::Type* char_ptr_type = llvm::Type::getInt8PtrTy( m_LLVMContext );
    m_StringType = llvm::StructType::create( std::vector<llvm::Type*>
                                               {size_type, char_ptr_type} );
#endif
    assert( m_StringType && "Can't find String type" );
    /// TODO make assertions about string type;
}

Runtime::~Runtime()
{
}

llvm::LLVMContext& Runtime::GetLLVMContext()
{
    return m_LLVMContext;
}

llvm::Module* Runtime::GetModule()
{
    return m_RuntimeModule;
}

llvm::Value* Runtime::CreateRuntimeCall( RuntimeFunction function,
                                         std::vector<llvm::Value*> params,
                                         llvm::IRBuilder<>& builder ) const
{
    const FunctionInfo& info = s_FunctionInfos.at(function);
    assert( params.size() == info.paramTypes.size() &&
            "Calling a function with the wrong number of params" );
    std::vector<ParamValue> param_values;
    param_values.reserve( params.size() );
    for( unsigned i = 0; i < params.size(); ++i )
        param_values.push_back( {params[i], info.paramTypes[i]} );

    return CreateCall( m_Functions.at(function),
                       info.returnType,
                       param_values,
                       builder );
}

//
// Misc
//

llvm::Type* Runtime::GetLLVMType( const CompleteType& type ) const
{
    llvm::Type* t = GetLLVMType( type.GetBaseType() );

    for( auto extent = type.GetArrayExtents().rbegin();
         extent != type.GetArrayExtents().rend();
         ++extent )
        t = llvm::ArrayType::get( t, *extent );

    return t;
}

llvm::Type* Runtime::GetLLVMType( Type type ) const
{
    llvm::Type* t;
    if( type == Type::DOUBLE )
        t = llvm::Type::getDoubleTy( m_LLVMContext );
    else if( type == Type::FLOAT )
        t = llvm::Type::getFloatTy( m_LLVMContext );
    else if( type == Type::BOOL )
        t = llvm::Type::getInt1Ty( m_LLVMContext );
    else if( IsIntegral( type ) )
        t = llvm::Type::getIntNTy( m_LLVMContext, SizeOf(type)*8 );
    else if( type == Type::STRING )
        t = m_StringType;
    else if( IsVectorType( type ) )
        t = llvm::VectorType::get( GetLLVMType( GetElementType( type ) ),
                                   GetVectorSize( type ) );
    else
    {
        assert( false && "Trying to get the llvm::Type of an unhandled Type" );
        return nullptr;
    }

    return t;
}

llvm::Value* Runtime::CreateCall( llvm::Function* function,
                                  Type return_type,
                                  const std::vector<ParamValue>& param_types,
                                  llvm::IRBuilder<>& builder ) const
{
    std::vector<llvm::Value*> params;

    for( auto param_type : param_types )
    {
        ParamType pass_type = s_TypeInformationMap.at(param_type.type).passType;
        switch( pass_type )
        {
        case ParamType::DEFAULT:
        {
            params.push_back( param_type.value );
            break;
        }
        case ParamType::EXPAND:
        {
            assert( llvm::isa<llvm::StructType>( param_type.value->getType() )&&
                    "Can't expand a non-struct type" );
            for( unsigned i = 0;
                 i < llvm::cast<llvm::StructType>(
                                 param_type.value->getType())->getNumElements();
                 ++i )
            {
                params.push_back( builder.CreateExtractValue(
                                                   param_type.value,
                                                   std::vector<unsigned>{i} ) );
            }
            break;
        }
        case ParamType::POINTER:
        {
            // Store it and send pointer
            llvm::Value* ptr = builder.CreateAlloca( m_StringType );
            builder.CreateStore( param_type.value, ptr );
            params.push_back( ptr );
            break;
        }
        case ParamType::IGNORE:
        {
            break;
        }
        }
    }

    llvm::Value* call = builder.CreateCall( function, params );

    switch( s_TypeInformationMap.at(return_type).returnType )
    {
    case ReturnType::DEFAULT:
    case ReturnType::IGNORE:
        return call;
    case ReturnType::POINTER:
        assert( false && "Complete me" );
        return nullptr;
    case ReturnType::INTEGER:
        // The function returns an integer big enough to hold the struct
        // Store the int
        llvm::Type* int_type = function->getReturnType();
        /// TODO types other than string
        llvm::Type* ptr_type = llvm::PointerType::get( m_StringType,
                                                       0 );
        llvm::Value* int_ptr = builder.CreateAlloca( int_type );
        builder.CreateStore( call, int_ptr );
        llvm::Value* string_ptr = builder.CreateBitCast( int_ptr,
                                                         ptr_type );
        return builder.CreateLoad( string_ptr );
    }

    assert( false && "How did you get here?" );
    return nullptr;
}


} // namespace Compiler
} // namespace JoeLang

