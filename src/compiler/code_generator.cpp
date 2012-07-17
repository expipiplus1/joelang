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

#include "code_generator.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <llvm/BasicBlock.h>
#include <llvm/Function.h>
#include <llvm/GlobalVariable.h>
#include <llvm/IRBuilder.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>

#include <engine/context.hpp>
#include <engine/state.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <engine/internal/type_properties.hpp>
#include <compiler/casting.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Compiler
{

/// TODO have a singleton for storing these
llvm::Module*       CodeGenerator::s_RuntimeModule = nullptr;
llvm::StructType*   CodeGenerator::s_StringType = nullptr;
llvm::Function*     CodeGenerator::s_StringEqualFunction = nullptr;
llvm::Function*     CodeGenerator::s_StringNotEqualFunction = nullptr;
llvm::Function*     CodeGenerator::s_StringConcatFunction = nullptr;

CodeGenerator::CodeGenerator( const Context& context )
    :m_Context( context )
    ,m_LLVMContext( llvm::getGlobalContext() )
    ,m_LLVMModule( new llvm::Module( "", m_LLVMContext ) )
    ,m_LLVMBuilder( m_LLVMContext )
    ,m_LLVMExecutionEngine( llvm::ExecutionEngine::createJIT( m_LLVMModule ) )
{
    assert( m_LLVMExecutionEngine && "Couldn't create a jit" );

    if( !s_RuntimeModule )
    {
        llvm::OwningPtr<llvm::MemoryBuffer> buffer;
        llvm::MemoryBuffer::getFile( "runtime.bc", buffer );
        assert( buffer && "Couldn't load runtime library" );
        s_RuntimeModule = llvm::ParseBitcodeFile ( buffer.get(),
                                                   m_LLVMContext );
        assert( s_RuntimeModule && "Couldn't parse runtime library" );


        s_StringEqualFunction = s_RuntimeModule->getFunction( "String_Equal" );
        assert( s_StringEqualFunction &&
               "Can't find String_Equal in runtime" );
        s_StringNotEqualFunction = s_RuntimeModule->getFunction(
                                                           "String_NotEqual" );
        assert( s_StringNotEqualFunction &&
               "Can't find String_NotEqual in runtime" );
        s_StringConcatFunction = s_RuntimeModule->getFunction(
                                                           "String_Concat" );
        assert( s_StringConcatFunction &&
               "Can't find String_Concat in runtime" );

        s_StringType = llvm::dyn_cast<llvm::StructType>(
                                    s_StringConcatFunction->getReturnType() );
        assert( s_StringType &&
                "Can't find String type" );
        /// TODO make assertions about string type;
    }
}

CodeGenerator::~CodeGenerator()
{
}

void CodeGenerator::GenerateCode(
                 const TranslationUnit& ast,
                 std::vector<Technique>& techniques,
                 std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine )
{
    for( const auto& declaration : ast.GetDeclarations() )
    {
        if( isa<TechniqueDeclaration>(declaration) )
        {
            TechniqueDeclaration& t =
                       static_cast<TechniqueDeclaration&>( *declaration.get() );
            techniques.push_back( t.GenerateTechnique( *this ) );
        }
        else if( isa<VariableDeclarationList>(declaration) )
        {
            VariableDeclarationList& v =
                    static_cast<VariableDeclarationList&>( *declaration.get() );
            v.CodeGen( *this );
        }
    }
    llvm_execution_engine = std::move( m_LLVMExecutionEngine );

    // output the module for fun
    m_LLVMModule->dump();
}

std::unique_ptr<StateAssignmentBase> CodeGenerator::GenerateStateAssignment(
        const StateBase& state,
        const Expression& expression )
{
    llvm::Type* t = GetLLVMType( state.GetType(), m_LLVMContext );
    assert( t && "trying to get the type of an unhandled JoeLang::Type" );
    assert( expression.GetReturnType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );

    //
    // create a function prototype which takes no arguments!
    //
    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                        t,
                                        std::vector<llvm::Type*>(),
                                        false );
    assert( prototype && "Error generating empty function prototype" );

    //
    // create an anonymous function
    //
    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                llvm::Function::InternalLinkage,
                                                state.GetName().c_str(),
                                                m_LLVMModule );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create( m_LLVMContext,
                                                       "",
                                                       function );
    m_LLVMBuilder.SetInsertPoint( body );

    //
    // Generate the code from the expression
    //
    llvm::Value* v = expression.CodeGen( *this );
    assert( v && "Invalid expression llvm::Value*" );

    //
    // set it as the return value for the function
    //
    m_LLVMBuilder.CreateRet( v );
    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function in stateassignment not valid" );

    //
    // Get the function pointer
    //
    void* function_ptr = m_LLVMExecutionEngine->getPointerToFunction(function);

    //
    // Cast to the appropriate type
    //
    StateAssignmentBase* sa;
    switch( state.GetType() )
    {
        case Type::BOOL:
            sa = new StateAssignment<jl_bool>
             ( static_cast<const State<bool>&>(state),
               reinterpret_cast<bool(*)()>(function_ptr) );
            break;
        case Type::FLOAT:
            sa = new StateAssignment<jl_float>
             ( static_cast<const State<jl_float>&>(state),
               reinterpret_cast<jl_float(*)()>(function_ptr) );
            break;
        case Type::DOUBLE:
            sa = new StateAssignment<jl_double>
             ( static_cast<const State<jl_double>&>(state),
               reinterpret_cast<jl_double(*)()>(function_ptr) );
            break;
        case Type::I8:
            sa = new StateAssignment<jl_i8>
             ( static_cast<const State<jl_i8>&>(state),
               reinterpret_cast<jl_i8(*)()>(function_ptr) );
            break;
        case Type::I16:
            sa = new StateAssignment<jl_i16>
             ( static_cast<const State<jl_i16>&>(state),
               reinterpret_cast<jl_i16(*)()>(function_ptr) );
            break;
        case Type::I32:
            sa = new StateAssignment<jl_i32>
             ( static_cast<const State<jl_i32>&>(state),
               reinterpret_cast<jl_i32(*)()>(function_ptr) );
            break;
        case Type::I64:
            sa = new StateAssignment<jl_i64>
             ( static_cast<const State<jl_i64>&>(state),
               reinterpret_cast<jl_i64(*)()>(function_ptr) );
            break;
        case Type::U8:
            sa = new StateAssignment<jl_u8>
             ( static_cast<const State<jl_u8>&>(state),
               reinterpret_cast<jl_u8(*)()>(function_ptr) );
            break;
        case Type::U16:
            sa = new StateAssignment<jl_u16>
             ( static_cast<const State<jl_u16>&>(state),
               reinterpret_cast<jl_u16(*)()>(function_ptr) );
            break;
        case Type::U32:
            sa = new StateAssignment<jl_u32>
             ( static_cast<const State<jl_u32>&>(state),
               reinterpret_cast<jl_u32(*)()>(function_ptr) );
            break;
        case Type::U64:
            sa = new StateAssignment<jl_u64>
             ( static_cast<const State<jl_u64>&>(state),
               reinterpret_cast<jl_u64(*)()>(function_ptr) );
            break;
        default:
            sa = nullptr;
    }
    return std::unique_ptr<StateAssignmentBase>( sa );
}

GenericValue CodeGenerator::EvaluateExpression( const Expression& expression )
{
    /// TODO is this really necessary?
    assert( expression.IsConst() &&
            "Trying to evaluate a non-const expression" );

    llvm::Type* t = GetLLVMType( expression, m_LLVMContext );
    assert( t && "trying to get the type of an unhandled JoeLang::Type" );

    auto insert_point = m_LLVMBuilder.saveAndClearIP();

    //
    // create a function prototype which takes no arguments!
    //
    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                        t,
                                        std::vector<llvm::Type*>(),
                                        false );
    assert( prototype && "Error generating empty function prototype" );

    //
    // create an anonymous function
    //
    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                llvm::Function::InternalLinkage,
                                                "TemporaryEvaluation",
                                                m_LLVMModule );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create( m_LLVMContext,
                                                       "",
                                                       function );
    m_LLVMBuilder.SetInsertPoint( body );

    //
    // Generate the code from the expression
    //
    llvm::Value* v = expression.CodeGen( *this );
    assert( v && "Invalid expression llvm::Value*" );

    //
    // Set it as the return value for the function
    //
    m_LLVMBuilder.CreateRet( v );
    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function not valid" );

    //
    // Evaluate the function
    //
    llvm::GenericValue g = m_LLVMExecutionEngine->runFunction( function,
                                                               {} );

    //
    // Extract the result
    //
    GenericValue ret;

    if( IsIntegral( expression.GetReturnType() ) )
    {
        jl_u64 result = g.IntVal.getLimitedValue();
        switch( expression.GetReturnType() )
        {
        case Type::BOOL:
            ret = GenericValue( static_cast<jl_bool>(result) );
            break;
        case Type::I8:
            ret = GenericValue( static_cast<jl_i8>(result) );
            break;
        case Type::I16:
            ret = GenericValue( static_cast<jl_i16>(result) );
            break;
        case Type::I32:
            ret = GenericValue( static_cast<jl_i32>(result) );
            break;
        case Type::I64:
            ret = GenericValue( static_cast<jl_i64>(result) );
            break;
        case Type::U8:
            ret = GenericValue( static_cast<jl_u8>(result) );
            break;
        case Type::U16:
            ret = GenericValue( static_cast<jl_u16>(result) );
            break;
        case Type::U32:
            ret = GenericValue( static_cast<jl_u32>(result) );
            break;
        case Type::U64:
            ret = GenericValue( static_cast<jl_u64>(result) );
            break;
        default:
            assert( false &&
                    "Trying to get the integer result of a non-integer "
                    "expression" );
        }
    }
    else if( IsFloatingPoint( expression.GetReturnType() ) )
    {
        switch( expression.GetReturnType() )
        {
        case Type::FLOAT:
            ret = GenericValue( g.FloatVal );
            break;
        case Type::DOUBLE:
            ret = GenericValue( g.DoubleVal );
            break;
        default:
            assert( false &&
                    "Trying to get the floating result of a non-floating "
                    "expression" );
        }
    }
    else
    {
        assert( false && "Complete me" );
    }

    m_LLVMBuilder.restoreIP( insert_point );
    return ret;
}

//
// Cast Operators
//

llvm::Value* CodeGenerator::CreateCast( const Expression& e, Type type )
{
    Type         e_type = e.GetReturnType();
    llvm::Value* e_code = e.CodeGen( *this );

    if( !e_code )
        return nullptr;

    //
    // For a cast to bool, compare to zero
    //
    if( type == Type::BOOL )
    {
        if( IsFloatingPoint( e_type ) )
            return m_LLVMBuilder.CreateFCmpOEQ(
                        e_code,
                        llvm::ConstantFP::getNullValue( e_code->getType() ) );
        return m_LLVMBuilder.CreateIsNotNull( e_code );
    }

    if( IsFloatingPoint( type ) )
    {
        if( IsFloatingPoint( e_type ) )
            return m_LLVMBuilder.CreateFPCast(
                                           e_code,
                                           GetLLVMType( type, m_LLVMContext ) );
        if( IsSigned( e_type ) )
            return m_LLVMBuilder.CreateSIToFP(
                                           e_code,
                                           GetLLVMType( type, m_LLVMContext ) );
        return m_LLVMBuilder.CreateUIToFP( e_code,
                                           GetLLVMType( type, m_LLVMContext ) );
    }

    assert( IsIntegral( type ) && "Type should be integral" );
    if( IsIntegral( e_type ) )
    {
        return m_LLVMBuilder.CreateIntCast( e_code,
                                            GetLLVMType( type, m_LLVMContext ),
                                            IsSigned( e_type ) );
    }

    assert( IsFloatingPoint( e_type ) && "e_type should be floating point" );
    if( IsSigned( type ) )
        return m_LLVMBuilder.CreateFPToSI( e_code,
                                           GetLLVMType( type, m_LLVMContext ) );
    else
        return m_LLVMBuilder.CreateFPToUI( e_code,
                                           GetLLVMType( type, m_LLVMContext ) );
}

//
// Unary Operators
//


llvm::Value* CodeGenerator::CreateNeg( const Expression& e )
{
    return m_LLVMBuilder.CreateNeg( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNot( const Expression& e )
{
    return m_LLVMBuilder.CreateNot( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLNot( const Expression& e )
{
    return m_LLVMBuilder.CreateIsNotNull( e.CodeGen( *this ) );
}

//
// Binary Operators
// Todo typing for all of these
//

llvm::Value* CodeGenerator::CreateLOr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l,
                                        const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ),
                                   r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpEQ( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNeq( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpNE( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFAdd( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateAdd( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateSub( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFSub( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateSub( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFMul( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateMul( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFDiv( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        if( IsSigned( l.GetReturnType() ) )
            return m_LLVMBuilder.CreateSDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
        else
            return m_LLVMBuilder.CreateUDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsSigned( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateSRem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateURem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
}

//
// Ternary Operators
//

llvm::Value* CodeGenerator::CreateSelect( const Expression& condition,
                                          const Expression& true_expression,
                                          const Expression& false_expression )
{
    return m_LLVMBuilder.CreateSelect( condition.CodeGen( *this ),
                                       true_expression.CodeGen( *this ),
                                       false_expression.CodeGen( *this ) );

}

//
// Other things
//

llvm::Value* CodeGenerator::CreateArrayIndex( const Expression& array,
                                              const Expression& index)
{
    return m_LLVMBuilder.CreateGEP( array.CodeGen( *this ),
                                    index.CodeGen( *this ) );
}

//
// Constants
//
llvm::Constant* CodeGenerator::CreateInteger( unsigned long long value,
                                              unsigned size,
                                              bool is_signed )
{
    return llvm::ConstantInt::get( llvm::Type::getIntNTy( m_LLVMContext, size ),
                                   value,
                                   is_signed );
}

llvm::Constant* CodeGenerator::CreateFloating( double value, bool is_double )
{
    return llvm::ConstantFP::get( is_double
                                    ? llvm::Type::getDoubleTy(m_LLVMContext)
                                    : llvm::Type::getFloatTy(m_LLVMContext),
                                  value );
}

llvm::Constant* CodeGenerator::CreateString( const std::string& value )
{
    //
    // Set up the globalvariable holding the characters in an array
    //
    llvm::ArrayType* array_type = llvm::ArrayType::get(
                                         GetLLVMType( Type::U8, m_LLVMContext ),
                                         value.size() );
    std::vector<llvm::Constant*> characters;
    for( char c : value )
        characters.push_back( CreateInteger( c, 8, false ) );
    llvm::Constant* data_constant = llvm::ConstantArray::get(
                                                        array_type,
                                                        characters );
    llvm::GlobalVariable* data_array = new llvm::GlobalVariable(
                                           *m_LLVMModule,
                                           array_type,
                                           true,
                                           llvm::GlobalVariable::PrivateLinkage,
                                           data_constant,
                                           "string_data" );

    llvm::Constant* size_constant = CreateInteger( value.size(), 64, false );

    llvm::Constant* data_pointer = llvm::ConstantExpr::getGetElementPtr(
                                            data_array,
                                            std::vector<llvm::Constant*>
                                              {CreateInteger( 0, 32, false ),
                                               CreateInteger( 0, 32, false )} );

    llvm::Constant* string = llvm::ConstantStruct::get(
                                                   s_StringType,
                                                   std::vector<llvm::Constant*>
                                                     {size_constant,
                                                      data_pointer} );
    return string;
}

//
// Variables
//

llvm::GlobalVariable* CodeGenerator::CreateGlobalVariable(
                                Type type,
                                std::vector<unsigned> array_extents,
                                bool is_const,
                                const GenericValue& initializer,
                                const std::string& name )
{
    assert( ( type == initializer.GetType() ||
              initializer.GetType() == Type::UNKNOWN_TYPE ) &&
            "Initializer type mismatch" );
    llvm::Type* t = type == Type::STRING ? s_StringType :
                                           GetLLVMType( type, m_LLVMContext );
    llvm::Constant* init = nullptr;
    if( initializer.GetType() != Type::UNKNOWN_TYPE )
        init = initializer.CodeGen( *this );
    return new llvm::GlobalVariable( *m_LLVMModule,
                                     t,
                                     is_const,
                                     llvm::GlobalVariable::PrivateLinkage,
                                     init,
                                     name );
}

llvm::Value* CodeGenerator::CreateVariableRead( const Variable& variable )
{
    return m_LLVMBuilder.CreateLoad( variable.GetLLVMPointer() );
}

void CodeGenerator::CreateVariableAssignment( const Variable& variable,
                                              const Expression& e )
{
    assert( !variable.IsConst() &&
            "Trying to codegen an assignment to a const variable" );
    assert( variable.GetType() == e.GetReturnType() &&
            "Trying to assign a variable with a different type" );
    llvm::Value* assigned_value = e.CodeGen( *this );
    m_LLVMBuilder.CreateStore( assigned_value, variable.GetLLVMPointer() );
}

//
// Getters
//

llvm::LLVMContext& CodeGenerator::GetLLVMContext() const
{
    return m_LLVMContext;
}

} // namespace Compiler
} // namespace JoeLang
