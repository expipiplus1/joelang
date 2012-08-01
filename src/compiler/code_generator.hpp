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

#include <stack>
#include <string>
#include <memory>
#include <vector>

#include <llvm/IRBuilder.h>

namespace llvm
{
    class ExecutionEngine;
    struct GenericValue;
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
typedef std::unique_ptr<Expression> Expression_up;
typedef std::shared_ptr<Expression> Expression_sp;
class GenericValue;
class Runtime;
class TechniqueDeclaration;
class TranslationUnit;
class Variable;

/// todo make this reusable
class CodeGenerator
{
public:
    CodeGenerator( Runtime& runtime );
    ~CodeGenerator();

    void GenerateCode(
                const TranslationUnit& ast,
                std::vector<Technique>& techniques,
                std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine );

    std::unique_ptr<StateAssignmentBase> GenerateStateAssignment(
                                                 const StateBase& state,
                                                 const Expression& expression,
                                                 const std::string& name = "" );

    GenericValue EvaluateExpression( const Expression& expression );

    // Type construction
    /** This asserts that all the arguments are not null **/
    llvm::Value* CreateVectorConstructor(
                                  Type type,
                                  const std::vector<Expression_up>& arguments );

    llvm::Value* CreateScalarConstructor( Type type,
                                          const Expression& argument );


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

    // Other things
    llvm::Value* CreateArrayIndex( const Expression& array,
                                   const Expression& index );

    llvm::Value* CreateArrayIndexPointerTo( const Expression& array,
                                            const Expression& index );

    /**
      * Create the llvm::Value representing an integer
      * \param value
      *   The value with which to create the integer
      * \param type
      *   The type of integer
      * \returns the llvm::Value representing the integer
      *
      * This will assert if type isn't an integer type
      */
    llvm::Constant* CreateInteger( unsigned long long value, Type type );

    /**
      * Create the llvm::Value representing a floating point value
      * \param value
      *   The value with which to create the float
      * \param type
      *   The type of floating point number
      * \returns the llvm::Value representing the float
      *
      * This will assert if type isn't a floating point type
      */
    llvm::Constant* CreateFloating( double value, Type type );

    /**
      * Create the llvm::Value representing a floating point vector
      * \param value
      *   The vector of values with which to create the vector
      * \param type
      *   The type of vector
      * \returns the llvm::Value representing the float vector
      *
      * This will assert if type isn't a floating point vector type
      */
    llvm::Constant* CreateFloatingVector( const std::vector<double>& value,
                                          Type type );

    /**
      * Create the llvm::Value representing a string struct
      * \param value
      *   The value with which to create the string
      * \returns the llvm::Value representing the string
      */
    llvm::Constant* CreateString( const std::string& value );

    /**
      * Create the llvm::Value representing an array of values
      * \param value
      *   The value with which to create the array
      * \returns the llvm::Value representing the array
      *
      * This will assert that all the values are of matching type
      */
    llvm::Constant* CreateArray( const std::vector<GenericValue>& value );

    // Variables
    /**
      * Allocate a llvm::GlobalVariable for the current module
      * \param type
      *   The type of the global variable
      * \param array_extents
      *   The array dimensions (if any)
      * \param is_const
      *   Whether this is a const variable
      * \param initializer
      *   An optional value to initialize the variable with
      * \returns the llvm::GlobalVariable allocated
      */
    llvm::GlobalVariable* CreateGlobalVariable(
                     Type type,
                     std::vector<unsigned> array_extents,
                     bool is_const,
                     const GenericValue& initializer,
                     const std::string& name );

    llvm::Value* CreateVariableRead( const Variable& variable );

    void CreateVariableAssignment( const Expression& variable,
                                   const Expression& e );

private:
    /**
      * Destroys the temporay strings created in evaluating the expression
      */
    void CreateDestroyTemporaryCalls();

    /**
      * Create an anonymous llvm function taking no arguments
      */
    llvm::Function* CreateFunctionPtrFromExpression(
                                                const Expression& expression,
                                                std::string name = "" );

    std::stack<llvm::Value*> m_Temporaries;

    Runtime&                 m_Runtime;

    llvm::Module*            m_LLVMModule;
    llvm::IRBuilder<>        m_LLVMBuilder;
    std::unique_ptr<llvm::ExecutionEngine> m_LLVMExecutionEngine;
};

} // namespace Compiler
} // namespace JoeLang
