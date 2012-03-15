/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
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
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Support/IRBuilder.h>

#include <engine/context.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <parser/tokens/expression.hpp>
#include <parser/tokens/declaration.hpp>
#include <parser/tokens/definition.hpp>
#include <parser/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Parser
{

CodeGenerator::CodeGenerator( const Context& context, std::vector<Technique>& techniques )
    :m_context( context )
    ,m_techniques( techniques )
    ,m_llvmContext( llvm::getGlobalContext() )
    ,m_llvmModule( new llvm::Module( "", m_llvmContext ) )
    ,m_llvmBuilder( m_llvmContext )
    ,m_llvmExecutionEngine( llvm::ExecutionEngine::createJIT( m_llvmModule ) )
{
    assert( m_llvmExecutionEngine );
}

CodeGenerator::~CodeGenerator()
{
}

bool CodeGenerator::GenerateCode( const std::unique_ptr<TranslationUnit>& ast,
                                  std::vector<Technique>& techniques,
                                  std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine )
{
    for( const auto& declaration : ast->GetDeclarations() )
        declaration->Accept( *this );

    //techniques = std::move(m_techniques);
    llvm_execution_engine = std::move( m_llvmExecutionEngine );

    return m_good;
}

void CodeGenerator::Error( const std::string& message )
{
    m_good = false;
    std::cout << "Code Generation Error: " << message << std::endl;
}

void CodeGenerator::Visit( DeclarationBase& d )
{
}

void CodeGenerator::Visit( TechniqueDeclaration& t )
{
    //TODO Techniques shouldn't be declared without a definition
    const std::shared_ptr<TechniqueDefinition>& definition = t.GetDefinition();
    assert( definition && "TODO get rid of this" );
    m_techniques.push_back( definition->GetTechnique( *this ) );
}

StateAssignment CodeGenerator::GenerateStateAssignment(
        const State& state,
        const Expression& expression )
{
    std::vector<llvm::Type*> no_arguments;

    //TODO correct return type
    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                        llvm::Type::getInt64Ty( m_llvmContext ),
                                        no_arguments,
                                        false );
    assert( prototype && "Error generating empty function prototype" );

    llvm::Function* function = llvm::Function::Create( prototype,
                                                       llvm::Function::ExternalLinkage,
                                                       "",
                                                       m_llvmModule );
    assert( prototype && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create( m_llvmContext,
                                                       "",
                                                       function );
    m_llvmBuilder.SetInsertPoint( body );

    llvm::Value* v = expression.CodeGen( *this );
    assert( v && "Invalid expression llvm::Value*" );

    m_llvmBuilder.CreateRet( v );

    function->dump();

    //TODO handle this a bit better than aborting
    llvm::verifyFunction( *function, llvm::AbortProcessAction );

    void* function_ptr = m_llvmExecutionEngine->getPointerToFunction( function );

    return StateAssignment( state, (long long(*)())function_ptr );
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

//TODO type check
llvm::Value* CodeGenerator::CreateLNot( const Expression& e )
{
    return m_llvmBuilder.CreateIsNotNull( e.CodeGen( *this ) );
}

//
// Binary Operators
// Todo typing for all of these
//

llvm::Value* CodeGenerator::CreateLOr( const Expression& l, const Expression& r )
{
    // TODO cast to bool
    return nullptr;
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l, const Expression& r )
{
    // TODO cast to bool
    return nullptr;
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateOr( l.CodeGen( *this ),
                                   r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpEQ( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNeq( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpNE( l.CodeGen( *this ),
                                       r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLT( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateAdd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateSub( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateSub( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateMul( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateSDiv( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l, const Expression& r )
{
    return m_llvmBuilder.CreateSRem( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

//
// Getters
//

llvm::LLVMContext& CodeGenerator::GetLLVMContext() const
{
    return m_llvmContext;
}

} // namespace Parser
} // namespace JoeLang
