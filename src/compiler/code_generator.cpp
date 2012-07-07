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
    :m_context( context )
    ,m_llvmContext( llvm::getGlobalContext() )
    ,m_llvmModule( new llvm::Module( "", m_llvmContext ) )
    ,m_llvmBuilder( m_llvmContext )
    ,m_llvmExecutionEngine( llvm::ExecutionEngine::createJIT( m_llvmModule ) )
{
    assert( m_llvmExecutionEngine && "Couldn't create a jit" );
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
    llvm_execution_engine = std::move( m_llvmExecutionEngine );

    // output the module for fun
    m_llvmModule->dump();
}

std::unique_ptr<StateAssignmentBase> CodeGenerator::GenerateStateAssignment(
        const StateBase& state,
        const Expression& expression )
{
    llvm::Type* t = GetLLVMType( state.GetType(), m_llvmContext );
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
                                                m_llvmModule );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create( m_llvmContext,
                                                       "",
                                                       function );
    m_llvmBuilder.SetInsertPoint( body );

    //
    // Generate the code from the expression
    //
    llvm::Value* v = expression.CodeGen( *this );
    assert( v && "Invalid expression llvm::Value*" );

    //
    // set it as the return value for the function
    //
    m_llvmBuilder.CreateRet( v );
    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function in stateassignment not valid" );

    //
    // Get the function pointer
    //
    void* function_ptr = m_llvmExecutionEngine->getPointerToFunction(function);

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
            return m_llvmBuilder.CreateFCmpOEQ(
                        e_code,
                        llvm::ConstantFP::getNullValue( e_code->getType() ) );
        return m_llvmBuilder.CreateIsNotNull( e_code );
    }

    if( IsFloatingPoint( type ) )
    {
        if( IsFloatingPoint( e_type ) )
            return m_llvmBuilder.CreateFPCast(
                                           e_code,
                                           GetLLVMType( type, m_llvmContext ) );
        if( IsSigned( e_type ) )
            return m_llvmBuilder.CreateSIToFP(
                                           e_code,
                                           GetLLVMType( type, m_llvmContext ) );
        return m_llvmBuilder.CreateUIToFP( e_code,
                                           GetLLVMType( type, m_llvmContext ) );
    }

    assert( IsIntegral( type ) && "Type should be integral" );
    if( IsIntegral( e_type ) )
    {
        return m_llvmBuilder.CreateIntCast( e_code,
                                            GetLLVMType( type, m_llvmContext ),
                                            IsSigned( e_type ) );
    }

    assert( IsFloatingPoint( e_type ) && "e_type should be floating point" );
    if( IsSigned( type ) )
        return m_llvmBuilder.CreateFPToSI( e_code,
                                           GetLLVMType( type, m_llvmContext ) );
    else
        return m_llvmBuilder.CreateFPToUI( e_code,
                                           GetLLVMType( type, m_llvmContext ) );
}

//
// Unary Operators
//


llvm::Value* CodeGenerator::CreateNeg( const Expression& e )
{
    return m_llvmBuilder.CreateNeg( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNot( const Expression& e )
{
    return m_llvmBuilder.CreateNot( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLNot( const Expression& e )
{
    return m_llvmBuilder.CreateIsNotNull( e.CodeGen( *this ) );
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
    return m_llvmBuilder.CreateOr( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l,
                                        const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateAnd( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateOr( l.CodeGen( *this ),
                                   r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpEQ( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNeq( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpNE( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_llvmBuilder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_llvmBuilder.CreateFAdd( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_llvmBuilder.CreateAdd( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateSub( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_llvmBuilder.CreateFSub( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_llvmBuilder.CreateSub( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_llvmBuilder.CreateFMul( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_llvmBuilder.CreateMul( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_llvmBuilder.CreateFDiv( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        if( IsSigned( l.GetReturnType() ) )
            return m_llvmBuilder.CreateSDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
        else
            return m_llvmBuilder.CreateUDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsSigned( l.GetReturnType() ) )
        return m_llvmBuilder.CreateSRem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_llvmBuilder.CreateURem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
}

//
// Ternary Operators
//

llvm::Value* CodeGenerator::CreateSelect( const Expression& condition,
                                          const Expression& true_expression,
                                          const Expression& false_expression )
{
    return m_llvmBuilder.CreateSelect( condition.CodeGen( *this ),
                                       true_expression.CodeGen( *this ),
                                       false_expression.CodeGen( *this ) );

}

//
// Constants
//
llvm::Value* CodeGenerator::CreateInteger( unsigned long long value,
                                           unsigned size,
                                           bool is_signed )
{
    return llvm::ConstantInt::get( llvm::Type::getIntNTy( m_llvmContext, size ),
                                   value,
                                   is_signed );
}

llvm::Value* CodeGenerator::CreateFloating( double value,
                                            bool is_double )
{
    return llvm::ConstantFP::get( is_double
                                    ? llvm::Type::getDoubleTy(m_llvmContext)
                                    : llvm::Type::getFloatTy(m_llvmContext),
                                  value );
}

//
// Variables
//

llvm::GlobalVariable* CodeGenerator::CreateGlobalVariable(
                                Type type,
                                bool is_const,
                                const std::unique_ptr<Expression>& initializer )
{
    llvm::Type* t = GetLLVMType( type, m_llvmContext );
    llvm::Constant* init = nullptr;
    if( initializer )
        init = llvm::dyn_cast<llvm::Constant>(initializer->CodeGen( *this ));
    return new llvm::GlobalVariable( *m_llvmModule,
                                     t,
                                     is_const,
                                     llvm::GlobalVariable::CommonLinkage,
                                     init,
                                     "" );
}

llvm::Value* CodeGenerator::CreateVariableRead( const Variable& variable )
{
    return m_llvmBuilder.CreateLoad( variable.GetLLVMPointer() );
}

void CodeGenerator::CreateVariableAssignment( const Variable& variable,
                                              const Expression& e )
{
    assert( !variable.IsConst() &&
            "Trying to codegen an assignment to a const variable" );
    assert( variable.GetType() == e.GetReturnType() &&
            "Trying to assign a variable with a different type" );
    llvm::Value* assigned_value = e.CodeGen( *this );
    m_llvmBuilder.CreateStore( assigned_value, variable.GetLLVMPointer() );
}

//
// Getters
//

llvm::LLVMContext& CodeGenerator::GetLLVMContext() const
{
    return m_llvmContext;
}

} // namespace Compiler
} // namespace JoeLang
