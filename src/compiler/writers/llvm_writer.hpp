/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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

#include <functional>
#include <map>
#include <memory>
#include <stack>
#include <vector>

#include <llvm/IR/IRBuilder.h>

namespace llvm
{
class Constant;
class ConstantFolder;
class ExecutionEngine;
class Function;
class GlobalVariable;
template <bool, typename, typename>
class IRBuilder;
template <bool>
class IRBuilderDefaultInserter;
class Module;
class Value;
}

namespace JoeLang
{
class StateAssignmentBase;
using StateAssignmentBase_up = std::unique_ptr<StateAssignmentBase>;
class ParameterBase;
using ParameterBase_up = std::unique_ptr<ParameterBase>;
enum class Type;

namespace Compiler
{

class CastNode;
class ConstantNodeBase;
class ExpressionNode;
class Function;
class GenericValue;
class Node;
class PointerExpressionNode;
class Runtime;
class StateAssignmentNode;
class Swizzle;
class Variable;
class VariableNode;

class LLVMWriter
{
public:
    LLVMWriter( Runtime& runtime );
    ~LLVMWriter();

    GenericValue EvaluateExpression( const ExpressionNode& expression );

    StateAssignmentBase_up GenerateStateAssignment(
        const StateAssignmentNode& state_assignment_node );

    void GenerateFunction( const Function& function );

    void AddGlobalVariable( const Variable& variable );

    ParameterBase_up GenerateParameter( const Variable& uniform );

private:

    void* WrapExpressionCommon( const ExpressionNode& expression, llvm::Function*& llvm_function );
    template <typename T>
    std::function<T()> WrapExpression( const ExpressionNode& expression,
                                       llvm::Function*& llvm_function );

    llvm::Function* GenerateFunctionDeclaration( const Function& function );

    void GenerateNodeFunctionDependencies( const Node& node );

    //
    // The set of all the functions we've generated
    //
    std::map<const Function*, llvm::Function*> m_GeneratedFunctions;

    //
    // All the global variables
    //
    std::map<const Variable*, llvm::GlobalVariable*> m_GlobalVariables;


    using IRBuilder =
        llvm::IRBuilder<true, llvm::ConstantFolder, llvm::IRBuilderDefaultInserter<true>>;

    Runtime& m_Runtime;
    llvm::Module& m_Module;
    llvm::ExecutionEngine& m_ExecutionEngine;

    //
    // This holds the string values generated for the current expression and should be cleared after
    // every expression
    //
    std::stack<llvm::Value*> m_StringTemporaries;

    //
    // This should be cleared at the end of each function
    //
    std::map<const Variable*, llvm::Value*> m_LocalVariables;

    IRBuilder m_Builder;

    //
    // This should be called after generating every expression
    //
    void GenerateExpressionCleanup();

    //
    // This should be called after every function
    //
    void GenerateFunctionCleanup();

    //
    // Statement Generation
    //
    void GenerateStatement( const Node& statement );

    void GenerateSequence( const Node& sequence );

    void GenerateConditional( const ExpressionNode& condition,
                              const Node& true_statement,
                              const Node* else_statement );

    void GenerateVoidReturn();

    void GenerateReturn( const ExpressionNode& returned );

    //
    // Value generation
    //

    llvm::Value* GenerateValue( const ExpressionNode& expression );

    llvm::Value* GenerateAddress( const PointerExpressionNode& expression );

    //
    // Addresses
    //

    llvm::Value* GenerateStore( const PointerExpressionNode& address,
                                const ExpressionNode& assigned );

    llvm::Value* GenerateSwizzleStore( const PointerExpressionNode& address,
                                       const ExpressionNode& assigned,
                                       const Swizzle& swizzle );

    llvm::Value* GenerateVariableAddress( const VariableNode& variable_node );

    llvm::Value* GenerateArrayIndex( const PointerExpressionNode& address,
                                     const ExpressionNode& index );

    llvm::Value* GeneratePreIncrement( const PointerExpressionNode& address );

    llvm::Value* GeneratePreDecrement( const PointerExpressionNode& address );

    //
    // Values
    //

    llvm::Value* GenerateCast( const CastNode& expression );

    llvm::Constant* GenerateConstant( const ConstantNodeBase& expression );

    llvm::Constant* GenerateZero( Type type );

    llvm::Value* GenerateCall( const ExpressionNode& expression );

    llvm::Value* GenerateExtractElement( const ExpressionNode& vector,
                                         const ExpressionNode& index );

    llvm::Value* GenerateInsertElement( const ExpressionNode& vector,
                                        const ExpressionNode& element,
                                        const ExpressionNode& index );

    llvm::Value* GenerateSwizzle( const ExpressionNode& expression, const Swizzle& swizzle );

    llvm::Value* GenerateSelect( const ExpressionNode& true_expression,
                                 const ExpressionNode& false_expression,
                                 const ExpressionNode& condition );

    llvm::Value* GenerateVectorConstructor( const ExpressionNode& constructor );

    llvm::Value* GenerateMatrixConstructor( const ExpressionNode& constructor );

    llvm::Value* GeneratePostIncrement( const PointerExpressionNode& address );

    llvm::Value* GeneratePostDecrement( const PointerExpressionNode& address );

    //
    // Binary Operators
    //
    llvm::Value* GenerateOr( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateAnd( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateExclusiveOr( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateCompareEqual( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateCompareNotEqual( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateCompareLessThan( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateCompareGreaterThan( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                                const ExpressionNode& rhs );

    llvm::Value* GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                   const ExpressionNode& rhs );

    llvm::Value* GenerateLeftShift( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateRightShift( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateAdd( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateSubtract( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateMultiply( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateDivide( const ExpressionNode& lhs, const ExpressionNode& rhs );

    llvm::Value* GenerateModulo( const ExpressionNode& lhs, const ExpressionNode& rhs );


    //
    // Unary operators
    //

    llvm::Value* GenerateNegate( const ExpressionNode& expression );

    llvm::Value* GenerateBitwiseNot( const ExpressionNode& expression );

    llvm::Value* GenerateLogicalNot( const ExpressionNode& expression );

    // Helpers
    llvm::Value* GenerateScalarOrVectorCast( llvm::Value* from_value,
                                             Type from_type,
                                             Type to_type );
    llvm::Value* GenerateMatrixCast( llvm::Value* from_value, Type from_type, Type to_type );

    llvm::Constant* GenerateConstantInt( Type type, unsigned long integer_value );

    llvm::Constant* GenerateConstantFloat( Type type, double float_value );

    llvm::Constant* GenerateConstantIntVector( Type type, const std::vector<unsigned long>& ints );

    llvm::Constant* GenerateConstantFloatVector( Type type, const std::vector<double>& floats );

    llvm::Constant* GenerateConstantIntMatrix( Type type, const std::vector<unsigned long>& ints );

    llvm::Constant* GenerateConstantFloatMatrix( Type type, const std::vector<double>& floats );

    llvm::Constant* GenerateGenericValue( const GenericValue& generic_value );
};

} // namespace Compiler
} // namespace JoeLang
