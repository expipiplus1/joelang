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

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/PassManager.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>


#include <joelang/context.hpp>
#include <joelang/config.h>
#include <joelang/types.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/function.hpp>
#include <compiler/semantic.hpp>
#include <compiler/type_properties.hpp>



#ifndef JOELANG_RUNTIME_FILENAME
    #error Missing runtime filename
#endif

namespace JoeLang
{
namespace Compiler
{

/// TODO find this automatically
const std::map<Type, Runtime::TypeInformation> Runtime::s_TypeInformationMap =
{
#if defined(ARCH_X86_64)
    {Type::VOID,   {ReturnType::IGNORE,  ParamType::IGNORE}},
    {Type::BOOL,   {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::STRING, {ReturnType::DEFAULT, ParamType::EXPAND}},
    {Type::FLOAT,  {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::FLOAT3, {ReturnType::STRUCT,  ParamType::EXPAND}},
#elif defined(ARCH_I386)
    {Type::VOID,   {ReturnType::IGNORE, ParamType::IGNORE}},
    {Type::BOOL,   {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::STRING, {ReturnType::INTEGER, ParamType::POINTER}},
#endif
};

const std::map<RuntimeFunction, Runtime::FunctionInfo> Runtime::s_FunctionInfos=
{
    {RuntimeFunction::STRING_EQUAL,     {"String_Equal", "",
                                         Type::BOOL,   {Type::STRING,
                                                        Type::STRING}}},
    {RuntimeFunction::STRING_NOTEQUAL,  {"String_NotEqual", "",
                                         Type::BOOL,   {Type::STRING,
                                                        Type::STRING}}},
    {RuntimeFunction::STRING_CONCAT,    {"String_Concat", "",
                                         Type::STRING, {Type::STRING,
                                                        Type::STRING}}},
    {RuntimeFunction::STRING_COPY,      {"String_Copy", "",
                                         Type::STRING, {Type::STRING}}},
    {RuntimeFunction::STRING_DESTROY,   {"String_Destroy", "",
                                         Type::VOID,   {Type::STRING}}},

    {RuntimeFunction::FLOAT3_DOT,       {"dot_float3", "dot",
                                         Type::FLOAT,  {Type::FLOAT3,
                                                        Type::FLOAT3}}},
    {RuntimeFunction::FLOAT3_NORMALIZE, {"normalize_float3", "normalize",
                                         Type::FLOAT3, {Type::FLOAT3}}},
};

Runtime::Runtime( const JoeLang::Context& joelang_context )
    :m_JoeLangContext( joelang_context )
{
    llvm::InitializeNativeTarget();

    llvm::OwningPtr<llvm::MemoryBuffer> buffer;

    llvm::MemoryBuffer::getFile( JOELANG_RUNTIME_FILENAME, buffer );
    if( !buffer )
    {
        m_JoeLangContext.Error( "Couldn't load runtime module" );
        return;
    }
    m_Module = llvm::ParseBitcodeFile ( buffer.get(), m_LLVMContext );
    if( !m_Module )
    {
        m_JoeLangContext.Error( "Couldn't parse runtime module" );
        return;
    }

    if( !FindRuntimeFunctions() )
    {
        m_JoeLangContext.Error( "Error finding functions in runtime" );
        return;
    }

    if( !FindRuntimeTypes() )
    {
        m_JoeLangContext.Error( "Error determining runtime types" );
        return;
    }

    if( !FindLLVMTypes() )
    {
        m_JoeLangContext.Error( "Error determining internal types" );
        return;
    }

    if( !GenerateFunctionWrappers() )
    {
        m_JoeLangContext.Error( "Error generating runtime function wrappers" );
        return;
    }

    std::string error_string;
    m_ExecutionEngine.reset( llvm::EngineBuilder(m_Module)
                             .setErrorStr(&error_string)
                             .setOptLevel(llvm::CodeGenOpt::Default)
                             .create() );
    if( !m_ExecutionEngine )
        m_JoeLangContext.Error( "Error creating LLVM ExecutionEngine: " +
                                error_string );

    InitializeOptimizers();
}

Runtime::~Runtime()
{
    m_Module->dump();
    llvm::llvm_shutdown();
}

const JoeLang::Context& Runtime::GetJoeLangContext() const
{
    return m_JoeLangContext;
}

llvm::LLVMContext& Runtime::GetLLVMContext()
{
    return m_LLVMContext;
}

llvm::Module& Runtime::GetModule()
{
    return *m_Module;
}

CodeGenerator Runtime::CreateCodeGenerator()
{
    return CodeGenerator( *this );
}

std::vector<Function_sp> Runtime::GetRuntimeFunctions() const
{
    std::vector<Function_sp> ret;
    for( const auto& i : s_FunctionInfos )
        if( !i.second.runtimeName.empty() )
            ret.push_back( m_FunctionWrappers.at( i.first ) );
    return ret;
}

llvm::ExecutionEngine& Runtime::GetExecutionEngine()
{
    return *m_ExecutionEngine;
}

llvm::Value* Runtime::CreateRuntimeCall( RuntimeFunction function,
                                         std::vector<llvm::Value*> params,
                                         llvm::IRBuilder<>& builder )
{
    const FunctionInfo& info = s_FunctionInfos.at(function);
    assert( params.size() == info.paramTypes.size() &&
            "Calling a function with the wrong number of params" );
    std::vector<ParamValue> param_values;
    param_values.reserve( params.size() );

    for( unsigned i = 0; i < params.size(); ++i )
    {
        assert( params[i]->getType() == GetLLVMType( info.paramTypes[i] ) &&
                "Trying to call a runtime function with wrong argument types" );
        param_values.push_back( {params[i], info.paramTypes[i]} );
    }

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
    auto i = m_InternalTypeMap.find( type );
    assert( i != m_InternalTypeMap.end() &&
            "Trying to get the llvm::Type of an unhandled Type" );
    return i->second;
}

llvm::Type* Runtime::GetRuntimeLLVMType( Type type ) const
{
    auto i = m_RuntimeTypeMap.find( type );
    assert( i != m_RuntimeTypeMap.end() &&
            "Trying to get an unknown runtime llvm type" );
    return i->second;
}

llvm::Value* Runtime::CreateCall( llvm::Function* function,
                                  Type return_type,
                                  const std::vector<ParamValue>& param_types,
                                  llvm::IRBuilder<>& builder )
{
    std::vector<llvm::Value*> params;

    for( auto param_type : param_types )
    {
        ParamType pass_type = s_TypeInformationMap.at(param_type.type).passType;

        //
        // If this isn't the same as the internal type then we have to cast it
        // TODO, is there anywhere where a bitcast isn't sufficient
        //
        llvm::Value* param_value = CreateRuntimeTypeCast( param_type,
                                                          builder );

        assert( param_value && "Error casting value" );

        switch( pass_type )
        {
        case ParamType::DEFAULT:
        {
            params.push_back( param_type.value );
            break;
        }
        case ParamType::EXPAND:
        {
            assert( llvm::isa<llvm::StructType>( param_value->getType() )&&
                    "Can't expand a non-struct type" );
            for( unsigned i = 0;
                 i < llvm::cast<llvm::StructType>(
                                      param_value->getType())->getNumElements();
                 ++i )
            {
                params.push_back( builder.CreateExtractValue(
                                                   param_value,
                                                   std::vector<unsigned>{i} ) );
            }
            break;
        }
        case ParamType::POINTER:
        {
            // Store it and send pointer
            assert( param_type.type != Type::STRING && "complete me" );
            llvm::Value* ptr = builder.CreateAlloca(
                                           GetRuntimeLLVMType( Type::STRING ) );
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
    case ReturnType::STRUCT:
        return CreateInternalTypeCast( {call, return_type}, builder );
    case ReturnType::INTEGER:
        // The function returns an integer big enough to hold the struct
        // Store the int
        llvm::Type* int_type = function->getReturnType();
        /// TODO types other than string
        assert( return_type == Type::STRING &&
                "todo, things other than strings" );
        llvm::Type* ptr_type = llvm::PointerType::get(
                                            GetRuntimeLLVMType( Type::STRING ),
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

llvm::Value* Runtime::CreateRuntimeTypeCast( ParamValue value,
                                             llvm::IRBuilder<>& builder )
{
    llvm::Type* from_type = value.value->getType();
    llvm::Type* to_type = GetRuntimeLLVMType( value.type );
    llvm::Value* ret = nullptr;
    llvm::Value* from_value = value.value;

    //
    // If the type is alreay correct
    //
    if( from_type == to_type )
        return from_value;

    //
    // The case where we're casting from a vector to a struct with the same
    // total number of components
    //
    if( llvm::isa<llvm::VectorType>(from_type) &&
        llvm::isa<llvm::StructType>(to_type) )
    {
        llvm::Type* base_type = from_type->getScalarType();
        llvm::StructType* to_struct_type =
                                       llvm::cast<llvm::StructType>( to_type );
        ret = llvm::UndefValue::get( to_struct_type );


        unsigned current_index = 0;

        for( unsigned i = 0; i < to_struct_type->getNumElements(); ++i )
        {
            llvm::Type* element_type = to_struct_type->getElementType( i );
            llvm::Value* element = nullptr;

            //
            // make sure that the to_type only contains fields of this scalar type
            //
            assert( element_type->getScalarType() == base_type &&
                    "Struct isn't of uniform type" );

            //
            // If *e is a vector then we need to swizzle out the values from
            // from_value
            //
            if( llvm::isa<llvm::VectorType>(element_type) )
            {
                unsigned size = element_type->getVectorNumElements();
                std::vector<unsigned> indices( size );
                for( unsigned j = 0; j < size; ++j )
                    indices[j] = current_index + j;

                element = builder.CreateShuffleVector(
                                from_value,
                                llvm::UndefValue::get( from_value->getType() ),
                                llvm::ConstantDataVector::get( GetLLVMContext(),
                                                               indices ) );

                current_index += size;
            }
            else
            {
                assert( element_type->isPrimitiveType() &&
                        "unhandled, non primitive types here" );

                element = builder.CreateExtractElement(
                                 from_value,
                                 llvm::ConstantInt::get( GetLLVMType(Type::INT),
                                                         current_index ) );

                current_index += 1;
            }

            ret = builder.CreateInsertValue( ret, element, {i} );
        }
    }

    return ret;
}

llvm::Value* Runtime::CreateInternalTypeCast( ParamValue value,
                                              llvm::IRBuilder<>& builder )
{
    llvm::Type* from_type = value.value->getType();
    llvm::Type* to_type = GetLLVMType( value.type );
    llvm::Value* ret = nullptr;
    llvm::Value* from_value = value.value;

    //
    // If the type is alreay correct
    //
    if( from_type == to_type )
        return from_value;

    //
    // The case where we're casting from a vector to a struct with the same
    // total number of components
    //
    if( llvm::isa<llvm::VectorType>(to_type) &&
        llvm::isa<llvm::StructType>(from_type) )
    {
        llvm::Type* base_type = to_type->getScalarType();
        llvm::StructType* from_struct_type =
                                      llvm::cast<llvm::StructType>( from_type );
        llvm::VectorType* to_vector_type =
                                      llvm::cast<llvm::VectorType>( to_type );
        ret = llvm::UndefValue::get( to_vector_type );

        unsigned to_vector_size = to_vector_type->getVectorNumElements();


        unsigned current_index = 0;

        for( unsigned i = 0; i < from_struct_type->getNumElements(); ++i )
        {
            llvm::Type* element_type = from_struct_type->getElementType( i );
            llvm::Value* element = builder.CreateExtractValue( from_value,
                                                               {i} );

            //
            // make sure that the to_type only contains fields of this scalar type
            //
            assert( element_type->getScalarType() == base_type &&
                    "Struct isn't of uniform type" );

            //
            // If element_type is a vector then we need to swizzle out the
            // values from from_value
            //
            if( llvm::isa<llvm::VectorType>(element_type) )
            {
                //
                // expand this vector into one of the right size
                //
                unsigned element_size = element_type->getVectorNumElements();

                std::vector<unsigned> indices( to_vector_size );
                for( unsigned j = 0; j < indices.size(); ++j )
                    indices[j] = j < element_size ? j : element_size;

                //
                // Make it the same size as to_vector
                //
                llvm::Value* element_vector = builder.CreateShuffleVector(
                                element,
                                llvm::UndefValue::get( element_type ),
                                llvm::ConstantDataVector::get( GetLLVMContext(),
                                                               indices ) );

                //
                // Set the indices for inserting element_vector in the right
                // place
                //
                for( unsigned j = 0; j < indices.size(); ++j )
                    indices[j] = j < current_index ?
                                         j : j - current_index + indices.size();

                ret = builder.CreateShuffleVector(
                                ret,
                                element_vector,
                                llvm::ConstantDataVector::get( GetLLVMContext(),
                                                               indices ) );

                current_index += element_size;
            }
            else
            {
                assert( element_type->isPrimitiveType() &&
                        "unhandled, non primitive types here" );


                ret = builder.CreateInsertElement(
                                 ret,
                                 element,
                                 llvm::ConstantInt::get( GetLLVMType(Type::INT),
                                                         current_index ) );
                current_index += 1;
            }
        }
    }

    return ret;
}

bool Runtime::FindRuntimeFunctions()
{
    for( const auto& function_info : s_FunctionInfos )
    {
        m_Functions[function_info.first] =
                     m_Module->getFunction( function_info.second.bitcodeName );
        if( !m_Functions[function_info.first] )
            return false;
    }
    return true;
}

bool Runtime::FindLLVMTypes()
{
    std::function<llvm::Type*(Type)> GetType;
    GetType = [&]( Type type ) -> llvm::Type*
    {
        if( IsVectorType( type ) )
            return llvm::VectorType::get( GetType( GetScalarType( type ) ),
                                          GetNumElementsInType( type ) );
        else if( IsMatrixType( type ) )
            return llvm::ArrayType::get( GetType( GetMatrixElementType(type) ),
                                         GetNumColumnsInType( type ) );
        else if( type == Type::DOUBLE )
            return llvm::Type::getDoubleTy( m_LLVMContext );
        else if( type == Type::FLOAT )
            return llvm::Type::getFloatTy( m_LLVMContext );
        else if( type == Type::BOOL )
            return llvm::Type::getInt1Ty( m_LLVMContext );
        else if( IsIntegral( type ) )
            return llvm::Type::getIntNTy( m_LLVMContext, SizeOf(type)*8 );
        else if( type == Type::STRING )
            return GetRuntimeLLVMType( Type::STRING );
        else if( type == Type::VOID )
            return llvm::Type::getVoidTy( m_LLVMContext );
        else
            assert( false &&
                    "Trying to get the llvm::Type of an unhandled Type" );
        return nullptr;
    };

#define GET_TYPE(type) \
    m_InternalTypeMap[type] = GetType( type ); \
    if( !m_InternalTypeMap[type] ) \
        return false;

#define GET_TYPE_N(type) \
    GET_TYPE( type) \
    GET_TYPE( type##2 ) \
    GET_TYPE( type##3 ) \
    GET_TYPE( type##4 ) \
    GET_TYPE( type##2x2 ) \
    GET_TYPE( type##2x3 ) \
    GET_TYPE( type##2x4 ) \
    GET_TYPE( type##3x2 ) \
    GET_TYPE( type##3x3 ) \
    GET_TYPE( type##3x4 ) \
    GET_TYPE( type##4x2 ) \
    GET_TYPE( type##4x3 ) \
    GET_TYPE( type##4x4 )

    GET_TYPE_N( Type::BOOL )
    GET_TYPE_N( Type::CHAR )
    GET_TYPE_N( Type::SHORT )
    GET_TYPE_N( Type::INT )
    GET_TYPE_N( Type::ULONG )
    GET_TYPE_N( Type::UCHAR )
    GET_TYPE_N( Type::USHORT )
    GET_TYPE_N( Type::UINT )
    GET_TYPE_N( Type::ULONG )
    GET_TYPE_N( Type::FLOAT )
    GET_TYPE_N( Type::DOUBLE )
    GET_TYPE( Type::STRING )

    return true;

#undef GET_TYPE_N
#undef GET_TYPE
}

bool Runtime::FindRuntimeTypes()
{
#if defined( ARCH_X86_64 )
    m_RuntimeTypeMap[Type::STRING] =
            m_Functions[RuntimeFunction::STRING_CONCAT]->getReturnType();

    assert( llvm::cast<llvm::StructType>(m_RuntimeTypeMap[Type::STRING])->
            isLayoutIdentical(
                llvm::cast<llvm::StructType>(
             m_Functions[RuntimeFunction::STRING_CONCAT]->getReturnType() ) ) );


    m_RuntimeTypeMap[Type::FLOAT3] =
            m_Functions[RuntimeFunction::FLOAT3_NORMALIZE]->getReturnType();

#elif defined( ARCH_I386 )
    m_StringType = m_RuntimeModule->getTypeByName( "struct.jl_string" );
    assert( false && "complete me" );
#else
    llvm::Type* size_type = GetLLVMType( Type::U32 );
    llvm::Type* char_ptr_type = llvm::Type::getInt8PtrTy( m_LLVMContext );
    m_StringType = llvm::StructType::create( std::vector<llvm::Type*>
                                               {size_type, char_ptr_type} );
    assert( false && "complete me" );
#endif

    for( const auto& t : m_RuntimeTypeMap )
        if( !t.second )
            return false;

    return true;
}

bool Runtime::GenerateFunctionWrappers()
{
    for( const auto& i : s_FunctionInfos )
    {
        //
        // Generate a function wrapper for every function which has a name
        //
        if( !i.second.runtimeName.empty() )
        {
            m_FunctionWrappers[i.first] = GenerateFunctionWrapper( i.first );
            if( ! m_FunctionWrappers[i.first] )
                return false;
        }
    }
    return true;
}

Function_sp Runtime::GenerateFunctionWrapper( RuntimeFunction runtime_function )
{
    FunctionInfo info = s_FunctionInfos.at( runtime_function );

    std::vector<CompleteType> param_types( info.paramTypes.size() );
    std::transform( info.paramTypes.begin(),
                    info.paramTypes.end(),
                    param_types.begin(),
                    []( Type t ){ return CompleteType(t); } );

    llvm::Type* llvm_return_type = GetLLVMType( info.returnType );
    std::vector<llvm::Type*> llvm_parameter_types( param_types.size() );
    std::transform( info.paramTypes.begin(),
                    info.paramTypes.end(),
                    llvm_parameter_types.begin(),
                    [&]( Type t ){ return GetLLVMType(t); } );

#if !defined(NDEBUG)
    assert( llvm_return_type && "Couldn't get llvm return type" );
    for( auto& t : llvm_parameter_types )
        assert( t && "Couldn't get llvm parameter type" );
#endif

    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                                         llvm_return_type,
                                                         llvm_parameter_types,
                                                         false );
    assert( prototype && "Error generating wrapper function prototype" );

    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                //llvm::Function::PrivateLinkage,
                                                llvm::Function::ExternalLinkage,
                                                info.bitcodeName + "_wrapper",
                                                m_Module );

    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* wrapper_body = llvm::BasicBlock::Create(
                                                            GetLLVMContext(),
                                                            "entry",
                                                            function );

    llvm::IRBuilder<> builder( GetLLVMContext() );
    builder.SetInsertPoint( wrapper_body );

    //
    // Now we have to generate the wrapping code
    //
    std::vector<llvm::Value*> llvm_params;
    llvm_params.reserve( function->arg_size() );
    for( auto a = function->arg_begin(); a != function->arg_end(); ++a )
        llvm_params.push_back( a );

    llvm::Value* ret = CreateRuntimeCall( runtime_function,
                                          llvm_params,
                                          builder );

    builder.CreateRet( ret );

    return std::make_shared<Function> ( info.runtimeName,
                                        CompleteType( info.returnType ),
                                        std::move( param_types ),
                                        function );
}

void Runtime::InitializeOptimizers()
{
    m_LLVMFunctionPassManager.reset(
                                    new llvm::FunctionPassManager( m_Module ) );
    m_LLVMModulePassManager.reset( new llvm::PassManager() );

    llvm::PassManagerBuilder pass_manager_builder;
    pass_manager_builder.OptLevel = 3;
    pass_manager_builder.Inliner = llvm::createFunctionInliningPass();
    pass_manager_builder.LoopVectorize = true;
    pass_manager_builder.BBVectorize = true;
    pass_manager_builder.SLPVectorize = true;
    pass_manager_builder.populateFunctionPassManager(
                                                   *m_LLVMFunctionPassManager );
    pass_manager_builder.populateModulePassManager(
                                                   *m_LLVMModulePassManager );

    m_LLVMFunctionPassManager->doInitialization();
}

void Runtime::OptimizeFunction( llvm::Function& function )
{
    m_LLVMFunctionPassManager->run( function );
}

void Runtime::OptimizeModule()
{
    m_LLVMModulePassManager->run( *m_Module );
}

} // namespace Compiler
} // namespace JoeLang

