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

#include <iostream>

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
    {Type::VOID,     {ReturnType::IGNORE,  ParamType::IGNORE}},
    {Type::BOOL,     {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::STRING,   {ReturnType::DEFAULT, ParamType::EXPAND}},
    {Type::FLOAT,    {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::FLOAT2,   {ReturnType::DEFAULT, ParamType::DEFAULT}},
    {Type::FLOAT3,   {ReturnType::STRUCT,  ParamType::EXPAND}},
    {Type::FLOAT4,   {ReturnType::STRUCT,  ParamType::EXPAND}},
    {Type::FLOAT4x4, {ReturnType::POINTER, ParamType::POINTER}},
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

    {RuntimeFunction::FLOAT_DOT,        {"dot_float",  "dot",
                                         Type::FLOAT,  {Type::FLOAT,
                                                        Type::FLOAT}}},
    {RuntimeFunction::FLOAT2_DOT,       {"dot_float2", "dot",
                                         Type::FLOAT,  {Type::FLOAT2,
                                                        Type::FLOAT2}}},
    {RuntimeFunction::FLOAT3_DOT,       {"dot_float3", "dot",
                                         Type::FLOAT,  {Type::FLOAT3,
                                                        Type::FLOAT3}}},
    {RuntimeFunction::FLOAT4_DOT,       {"dot_float4", "dot",
                                         Type::FLOAT,  {Type::FLOAT4,
                                                        Type::FLOAT4}}},

    {RuntimeFunction::FLOAT_NORMALIZE,  {"normalize_float", "normalize",
                                         Type::FLOAT, {Type::FLOAT}}},
    {RuntimeFunction::FLOAT2_NORMALIZE, {"normalize_float2", "normalize",
                                         Type::FLOAT2, {Type::FLOAT2}}},
    {RuntimeFunction::FLOAT3_NORMALIZE, {"normalize_float3", "normalize",
                                         Type::FLOAT3, {Type::FLOAT3}}},
    {RuntimeFunction::FLOAT4_NORMALIZE, {"normalize_float4", "normalize",
                                         Type::FLOAT4, {Type::FLOAT4}}},

    {RuntimeFunction::FLOAT4x4_FLOAT4_MUL,
                                        {"mul_float4x4_float4", "mul",
                                         Type::FLOAT4, {Type::FLOAT4x4,
                                                        Type::FLOAT4}}},

    {RuntimeFunction::FLOAT4x4_FLOAT4x4_MUL,
                                        {"mul_float4x4_float4x4", "mul",
                                         Type::FLOAT4x4, {Type::FLOAT4x4,
                                                        Type::FLOAT4x4}}},
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

    if( !FindInternalTypes() )
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
    std::cout << GetTypeString(type) << std::endl;
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

    llvm::Value* ret_ptr = nullptr;

    if( s_TypeInformationMap.at(return_type).returnType == ReturnType::POINTER )
    {
        //
        // If we get the return value by pointer allocate it here and pass it in
        //
        ret_ptr = builder.CreateAlloca( GetRuntimeLLVMType( return_type ) );
        params.push_back( ret_ptr );
    }

    for( auto param_type : param_types )
    {
        ParamType pass_type = s_TypeInformationMap.at(param_type.type).passType;

        //
        // If this isn't the same as the internal type then we have to cast it
        // TODO, is there anywhere where a bitcast isn't sufficient
        //
        llvm::Value* param_value = CreateDeepCopy(
                                          param_type.value,
                                          GetRuntimeLLVMType( param_type.type ),
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
            llvm::Value* ptr = builder.CreateAlloca(
                                        GetRuntimeLLVMType( param_type.type ) );
            builder.CreateStore( param_value, ptr );
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
        //
        // load the pointer and cast it
        //
        call = builder.CreateLoad( ret_ptr );
    case ReturnType::STRUCT:
        return CreateDeepCopy( call, GetLLVMType( return_type), builder );
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

llvm::Value* Runtime::CreateDeepCopy( llvm::Value* value,
                                      llvm::Type* to_type,
                                      llvm::IRBuilder<>& builder )
{
    //
    // If the type is alreay correct
    //
    if( value->getType() == to_type )
        return value;

    //
    // We perform a depth first traversal of the from_type to get the values
    //
    std::stack<std::pair<llvm::Value*, unsigned>> elements;
    elements.push( {value, 0u} );

    std::function<llvm::Value*()> GetNextElement;
    GetNextElement =
        [&elements, &builder, &GetNextElement, this]
        () -> llvm::Value*
    {
        llvm::Type* t = elements.top().first->getType();

        unsigned end = t->isPrimitiveType() ? 1 :
                       t->isVectorTy() ? t->getVectorNumElements() :
                       t->isArrayTy() ? t->getArrayNumElements() :
                       t->isStructTy() ? t->getStructNumElements() : 0;

        assert( end != 0 && "How did we get an end of 0?" );

        if( elements.top().second == end )
        {
            //
            // If we've processed everything here, pop the stack
            //
            elements.pop();
            if( elements.empty() )
                return nullptr;
            ++elements.top().second;
            return GetNextElement();
        }

        if( t->isPrimitiveType() )
        {
            llvm::Value* ret = elements.top().first;
            ++elements.top().second;
            return ret;
        }

        if( t->isVectorTy() )
        {
            llvm::Value* ret = builder.CreateExtractElement(
                          elements.top().first,
                          llvm::ConstantInt::get( GetLLVMType( Type::INT),
                                                  elements.top().second ) );
            ++elements.top().second;
            return ret;
        }

        assert( t->isStructTy() || t->isArrayTy() );

        std::pair<llvm::Value*, unsigned> p;
        p.first = builder.CreateExtractValue( elements.top().first,
                                              {elements.top().second} );
        p.second = 0;
        elements.push( p );
        return GetNextElement();
    };

    using it = std::vector<llvm::Value*>::const_iterator;
    std::function<llvm::Value*(llvm::Type*, it&, it)> FillType;
    FillType =
        [&builder, &FillType, this]
        ( llvm::Type* t, it& begin, it end ) -> llvm::Value*
    {
        if( t->isPrimitiveType() )
        {
            //
            // return one element and increment begin
            //
            assert( begin != end && "No elements to take from" );
            return *begin++;
        }

        if( t->isVectorTy() )
        {
            //
            // We need to construct a vector from all of these values
            //
            llvm::Value* ret = llvm::UndefValue::get( t );

            //
            // insert all the elements we need
            //
            for( unsigned i = 0; i < t->getVectorNumElements(); ++i )
            {
                assert( begin != end && "No elements to take from" );
                ret = builder.CreateInsertElement(
                                ret,
                                *begin++,
                                llvm::ConstantInt::get( GetLLVMType( Type::INT),
                                                        i ) );
            }

            return ret;
        }

        assert( t->isStructTy() || t->isArrayTy() );

        unsigned size = t->isArrayTy() ? t->getArrayNumElements() :
                        t->isStructTy() ? t->getStructNumElements() : 0;

        llvm::Value* ret = llvm::UndefValue::get( t );

        assert( size != 0 && "Struct or array of size zero" );

        for( unsigned i = 0; i < size; ++i )
        {
            llvm::Type* sub_type =
                                t->isArrayTy() ? t->getArrayElementType() :
                                t->isStructTy() ? t->getStructElementType( i ) :
                                nullptr;
            assert( sub_type && "couldn't get sub_type" );

            llvm::Value* sub_value = FillType( sub_type, begin, end );

            ret = builder.CreateInsertValue( ret, sub_value, {i} );
        }

        return ret;
    };

    std::vector<llvm::Value*> flat_elements;
    while( llvm::Value* v = GetNextElement() )
        flat_elements.push_back(v);

    llvm::Value* ret = nullptr;

    it begin = flat_elements.begin();

    ret = FillType( to_type, begin, flat_elements.end() );

    assert( begin == flat_elements.end() && "Too few elements taken" );

    return ret;
}

llvm::Value* Runtime::CreatePointerCastCopy( llvm::Value* value,
                                             llvm::Type* to_type,
                                             llvm::IRBuilder<>& builder )
{
    //
    // If the type is alreay correct
    //
    if( value->getType() == to_type )
        return value;

    //
    // store it, cast the pointer and load
    //
    llvm::Value* ptr = builder.CreateAlloca( value->getType() );
    builder.CreateStore( value, ptr );
    llvm::Value* ret_ptr = builder.CreateBitCast( ptr,
                                                  to_type->getPointerTo() );
    return builder.CreateLoad( ret_ptr );
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

bool Runtime::FindInternalTypes()
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
    GET_TYPE( type ) \
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
    GET_TYPE_N( Type::LONG )
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

    m_RuntimeTypeMap[Type::FLOAT] =
            m_Functions[RuntimeFunction::FLOAT_NORMALIZE]->getReturnType();
    m_RuntimeTypeMap[Type::FLOAT2] =
            m_Functions[RuntimeFunction::FLOAT2_NORMALIZE]->getReturnType();
    m_RuntimeTypeMap[Type::FLOAT3] =
            m_Functions[RuntimeFunction::FLOAT3_NORMALIZE]->getReturnType();
    m_RuntimeTypeMap[Type::FLOAT4] =
            m_Functions[RuntimeFunction::FLOAT4_NORMALIZE]->getReturnType();

    //
    // Get the hidden return value pointer and find the type behind that
    //
    m_RuntimeTypeMap[Type::FLOAT4x4] =
            m_Functions[RuntimeFunction::FLOAT4x4_FLOAT4_MUL]->
                    getFunctionType()->getParamType(0)->getPointerElementType();

    assert( llvm::cast<llvm::StructType>(m_RuntimeTypeMap[Type::STRING])->
            isLayoutIdentical(
                llvm::cast<llvm::StructType>(
             m_Functions[RuntimeFunction::STRING_CONCAT]->getReturnType() ) ) );


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

