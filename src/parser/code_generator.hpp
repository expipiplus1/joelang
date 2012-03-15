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

#pragma once

#include <string>
#include <memory>
#include <vector>

#include <llvm/Support/IRBuilder.h>

namespace llvm
{
    class ExecutionEngine;
    class LLVMContext;
    class Module;
}

namespace JoeLang
{

class Context;
class State;
class StateAssignment;
class Technique;

namespace Parser
{

class DeclarationBase;
class Expression;
class TechniqueDeclaration;
class TranslationUnit;

class CodeGenerator
{
public:
    // http://gcc.gnu.org/bugzilla/show_bug.cgi?id=52591
    CodeGenerator( const Context& context, std::vector<Technique>& techniques );
    ~CodeGenerator();

    bool GenerateCode( const std::unique_ptr<TranslationUnit>& ast,
                       std::vector<Technique>& techniques,
                       std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine );

    void Error( const std::string& message );

    void Visit( DeclarationBase& p );
    void Visit( TechniqueDeclaration& t );

    StateAssignment GenerateStateAssignment( const State& state,
                                             const std::unique_ptr<Expression>& expression ) ;

    //Unary Operators
    llvm::Value* CreateNeg(  llvm::Value* v );
    llvm::Value* CreateNot(  llvm::Value* v );
    llvm::Value* CreateLogicalNot( llvm::Value* v );

    //Binary Operators
    llvm::Value* CreateLOr( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateLAnd( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateOr( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateXor( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateAnd( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateEq( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateNeq( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateLT( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateGT( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateLTE( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateGTE( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateShl( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateShr( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateAdd( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateSub( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateMul( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateDiv( llvm::Value* l, llvm::Value* r );
    llvm::Value* CreateMod( llvm::Value* l, llvm::Value* r );

    llvm::LLVMContext& GetLLVMContext() const;

private:
    const Context& m_context;

    bool m_good = true;

    std::vector<Technique>& m_techniques;

    llvm::LLVMContext&              m_llvmContext;
    llvm::Module*                   m_llvmModule;
    llvm::IRBuilder<>               m_llvmBuilder;
    std::unique_ptr<llvm::ExecutionEngine> m_llvmExecutionEngine;
};

} // namespace Parser
} // namespace JoeLang
