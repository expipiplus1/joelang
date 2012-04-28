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

#pragma once

#include <string>
#include <memory>
#include <vector>

#include <llvm/Support/IRBuilder.h>

namespace llvm
{
    class ExecutionEngine;
    class GlobalVariable;
    class LLVMContext;
    class Module;
}

namespace JoeLang
{

class Context;
class StateBase;
class StateAssignmentBase;
class Technique;

enum class Type;

namespace Compiler
{

class DeclarationBase;
class Expression;
class TechniqueDeclaration;
class TranslationUnit;

class CodeGenerator
{
public:
    CodeGenerator( const Context& context );
    ~CodeGenerator();

    void GenerateCode(
                const TranslationUnit& ast,
                std::vector<Technique>& techniques,
                std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine );

    std::unique_ptr<StateAssignmentBase> GenerateStateAssignment(
                                                 const StateBase& state,
                                                 const Expression& expression );

    // Cast Operators
    llvm::Value* CreateCast( const Expression& e, Type type );

    // Unary Operators
    llvm::Value* CreateNeg(  const Expression& e );
    llvm::Value* CreateNot(  const Expression& e );
    llvm::Value* CreateLNot( const Expression& e );

    // Binary Operators
    llvm::Value* CreateLOr(  const Expression& l, const Expression& r );
    llvm::Value* CreateLAnd( const Expression& l, const Expression& r );
    llvm::Value* CreateOr(   const Expression& l, const Expression& r );
    llvm::Value* CreateXor(  const Expression& l, const Expression& r );
    llvm::Value* CreateAnd(  const Expression& l, const Expression& r );
    llvm::Value* CreateEq(   const Expression& l, const Expression& r );
    llvm::Value* CreateNeq(  const Expression& l, const Expression& r );
    llvm::Value* CreateLT(   const Expression& l, const Expression& r );
    llvm::Value* CreateGT(   const Expression& l, const Expression& r );
    llvm::Value* CreateLTE(  const Expression& l, const Expression& r );
    llvm::Value* CreateGTE(  const Expression& l, const Expression& r );
    llvm::Value* CreateShl(  const Expression& l, const Expression& r );
    llvm::Value* CreateShr(  const Expression& l, const Expression& r );
    llvm::Value* CreateAdd(  const Expression& l, const Expression& r );
    llvm::Value* CreateSub(  const Expression& l, const Expression& r );
    llvm::Value* CreateMul(  const Expression& l, const Expression& r );
    llvm::Value* CreateDiv(  const Expression& l, const Expression& r );
    llvm::Value* CreateMod(  const Expression& l, const Expression& r );

    // Ternary Operators
    llvm::Value* CreateSelect( const Expression& condition,
                               const Expression& true_expression,
                               const Expression& false_expression );

    /**
      * Create the llvm::Value representing an integer
      * \param value
      *   The value with which to create the integer
      * \param size
      *   The number of bits to the integer
      * \returns the llvm::Value representing the integer
      */
    llvm::Value* CreateInteger( unsigned long long value,
                                unsigned size,
                                bool is_signed );

    /**
      * Create the llvm::Value representing a floating point value
      * \param value
      *   The value with which to create the float
      * \param is_double
      *   Whether this is a double precision float
      * \returns the llvm::Value representing the float
      */
    llvm::Value* CreateFloating( double value,
                                 bool is_double );

    // Variables
    /**
      * Allocate a llvm::GlobalVariable for the current module
      * \param type
      *   The type of the global variable
      * \param is_const
      *   Whether this is a const variable
      * \param initializer
      *   An optional value to initialize the variable with
      * \returns the llvm::GlobalVariable allocated
      */
    llvm::GlobalVariable* CreateGlobalVariable(
                     Type type,
                     bool is_const,
                     const std::unique_ptr<Expression>& initializer = nullptr );

    // Getters
    llvm::LLVMContext& GetLLVMContext() const;

private:
    const Context& m_context;

    llvm::LLVMContext&              m_llvmContext;
    llvm::Module*                   m_llvmModule;
    llvm::IRBuilder<>               m_llvmBuilder;
    std::unique_ptr<llvm::ExecutionEngine> m_llvmExecutionEngine;
};

} // namespace Compiler
} // namespace JoeLang
