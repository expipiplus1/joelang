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
#include <engine/internal/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

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


    m_StringEqualFunction = m_RuntimeModule->getFunction( "String_Equal" );
    assert( m_StringEqualFunction && "Can't find String_Equal in runtime" );
    m_StringNotEqualFunction = m_RuntimeModule->getFunction( "String_NotEqual");
    assert( m_StringNotEqualFunction &&
           "Can't find String_NotEqual in runtime" );
    m_StringConcatFunction = m_RuntimeModule->getFunction( "String_Concat" );
    assert( m_StringConcatFunction && "Can't find String_Concat in runtime" );

#if defined( ARCH_X86_64 )
    m_StringType = llvm::cast<llvm::StructType>(
                                    m_StringConcatFunction->getReturnType() );

    assert( m_StringType->isLayoutIdentical( llvm::cast<llvm::StructType>(
                                m_StringConcatFunction->getReturnType() ) ) );
#elif defined( ARCH_I686 )
    m_StringType = m_RuntimeModule->getTypeByName( "struct.jl_string" );
#else
    llvm::Type* size_type = GetLLVMType( Type::U32 );
    llvm::Type* char_ptr_type = llvm::Type::getInt8PtrTy( m_LLVMContext );
    m_StringType = llvm::StructType::create( std::vector<llvm::Type*>
                                               {size_type, char_ptr_type} );
#endif
    assert( m_StringType &&
            "Can't find String type" );
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

//
// String functions
//
llvm::Value* Runtime::CreateStringEqualCall( llvm::Value* lhs,
                                             llvm::Value* rhs,
                                             llvm::IRBuilder<>& builder ) const
{
#if defined(ARCH_I686)
    return CreateCall( m_StringEqualFunction,
                       ReturnType::DEFAULT,
                       { {lhs, ParamType::POINTER},
                         {rhs, ParamType::POINTER} },
                       builder );
#elif defined(ARCH_X86_64)
    return CreateCall( m_StringEqualFunction,
                       ReturnType::DEFAULT,
                       { {lhs, ParamType::EXPAND},
                         {rhs, ParamType::EXPAND} },
                       builder );
#endif
}

llvm::Value* Runtime::CreateStringNotEqualCall(
                                            llvm::Value* lhs,
                                            llvm::Value* rhs,
                                            llvm::IRBuilder<>& builder ) const
{
#if defined(ARCH_I686)
    return CreateCall( m_StringNotEqualFunction,
                       ReturnType::DEFAULT,
                       { {lhs, ParamType::POINTER},
                         {rhs, ParamType::POINTER} },
                       builder );
#elif defined(ARCH_X86_64)
    return CreateCall( m_StringNotEqualFunction,
                       ReturnType::DEFAULT,
                       { {lhs, ParamType::EXPAND},
                         {rhs, ParamType::EXPAND} },
                       builder );
#endif
}

llvm::Value* Runtime::CreateStringConcatCall( llvm::Value* lhs,
                                              llvm::Value* rhs,
                                              llvm::IRBuilder<>& builder ) const
{
    /// TODO find this manually
#if defined(ARCH_I686)
    return CreateCall( m_StringConcatFunction,
                       ReturnType::INTEGER,
                       { {lhs, ParamType::POINTER},
                         {rhs, ParamType::POINTER} },
                       builder );
#elif defined(ARCH_X86_64)
    return CreateCall( m_StringConcatFunction,
                       ReturnType::DEFAULT,
                       { {lhs, ParamType::EXPAND},
                         {rhs, ParamType::EXPAND} },
                       builder );
#endif
}

//
// Misc
//

llvm::Type* Runtime::GetLLVMType(
                              Type base_type,
                              const std::vector<unsigned>& array_extents ) const
{
    llvm::Type* t;
    if( base_type == Type::DOUBLE )
        t = llvm::Type::getDoubleTy( m_LLVMContext );
    else if( base_type == Type::FLOAT )
        t = llvm::Type::getFloatTy( m_LLVMContext );
    else if( base_type == Type::BOOL )
        t = llvm::Type::getInt1Ty( m_LLVMContext );
    else if( IsIntegral( base_type ) )
        t = llvm::Type::getIntNTy( m_LLVMContext, SizeOf(base_type)*8 );
    else if( base_type == Type::STRING )
        t = m_StringType;
    else
    {
        assert( false && "Trying to get the llvm::Type of an unhandled Type" );
        return nullptr;
    }

    for( auto extent = array_extents.rbegin();
         extent != array_extents.rend();
         ++extent )
        t = llvm::ArrayType::get( t, *extent );

    return t;
}

llvm::Value* Runtime::CreateCall( llvm::Function* function,
                                  ReturnType return_type,
                                  const std::vector<ParamValue>& param_types,
                                  llvm::IRBuilder<>& builder ) const
{
    std::vector<llvm::Value*> params;

    for( auto param_type : param_types )
    {
        switch( param_type.param_type )
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
        }
    }

    llvm::Value* call = builder.CreateCall( function, params );

    switch( return_type )
    {
    case ReturnType::DEFAULT:
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

