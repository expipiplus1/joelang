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
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/IRBuilder.h>

#include <engine/context.hpp>
#include <engine/state.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <engine/internal/type_properties.hpp>
#include <compiler/casting.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Compiler
{

CodeGenerator::CodeGenerator( const Context& context )
    :m_Context( context )
    ,m_LLVMContext( llvm::getGlobalContext() )
    ,m_LLVMModule( new llvm::Module( "", m_LLVMContext ) )
    ,m_LLVMBuilder( m_LLVMContext )
    ,m_LLVMExecutionEngine( llvm::ExecutionEngine::createJIT( m_LLVMModule ) )
{
    assert( m_LLVMExecutionEngine && "Couldn't create a jit" );
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
llvm::Value* CodeGenerator::CreateInteger( unsigned long long value,
                                           unsigned size,
                                           bool is_signed )
{
    return llvm::ConstantInt::get( llvm::Type::getIntNTy( m_LLVMContext, size ),
                                   value,
                                   is_signed );
}

llvm::Value* CodeGenerator::CreateFloating( double value,
                                            bool is_double )
{
    return llvm::ConstantFP::get( is_double
                                    ? llvm::Type::getDoubleTy(m_LLVMContext)
                                    : llvm::Type::getFloatTy(m_LLVMContext),
                                  value );
}

//
// Variables
//

llvm::GlobalVariable* CodeGenerator::CreateGlobalVariable(
                                Type type,
                                std::vector<Expression_sp> array_extents,
                                bool is_const,
                                const std::unique_ptr<Expression>& initializer )
{
    llvm::Type* t = GetLLVMType( type, m_LLVMContext );
    llvm::Constant* init = nullptr;
    if( initializer )
        init = llvm::dyn_cast<llvm::Constant>(initializer->CodeGen( *this ));
    return new llvm::GlobalVariable( *m_LLVMModule,
                                     t,
                                     is_const,
                                     llvm::GlobalVariable::CommonLinkage,
                                     init,
                                     "" );
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
