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
#include <vector>

namespace llvm
{
class ConstantFolder;
class ExecutionEngine;
class Function;
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
enum class Type;

namespace Compiler
{

class CastNode;
class ConstantNodeBase;
class ExpressionNode;
class Function;
class Node;
class Runtime;
class StateAssignmentNode;
class Swizzle;

class LLVMWriter
{
public:
    LLVMWriter( Runtime& runtime );

    StateAssignmentBase_up GenerateStateAssignment(
        const StateAssignmentNode& state_assignment_node );

    void GenerateFunction( const Function& function );

private:
    void* WrapExpressionCommon( const ExpressionNode& expression );
    template <typename T>
    std::function<T()> WrapExpression( const ExpressionNode& expression );

    llvm::Function* GenerateFunctionDeclaration( const Function& function );

    void GenerateNodeFunctionDependencies( const Node& node );

    //
    // The set of all the functions we've generated
    //
    std::map<const Function*, llvm::Function*> m_GeneratedFunctions;

    Runtime& m_Runtime;
    llvm::Module& m_Module;
    llvm::ExecutionEngine& m_ExecutionEngine;

    using IRBuilder =
        llvm::IRBuilder<true, llvm::ConstantFolder, llvm::IRBuilderDefaultInserter<true>>;

    //
    // Statement Generation
    //
    void GenerateStatement( const Node& statement, IRBuilder& builder ) const;

    void GenerateSequence( const Node& sequence, IRBuilder& builder ) const;

    void GenerateReturn( const ExpressionNode& returned, IRBuilder& builder ) const;
    void GenerateVoidReturn( IRBuilder& builder ) const;

    //
    // Value generation
    //

    llvm::Value* GenerateValue( const ExpressionNode& expression, IRBuilder& builder ) const;

    llvm::Value* GenerateCast( const CastNode& expression, IRBuilder& builder ) const;

    llvm::Value* GenerateConstant( const ConstantNodeBase& expression, IRBuilder& builder ) const;

    llvm::Value* GenerateCall( const ExpressionNode& expression, IRBuilder& builder ) const;

    llvm::Value* GenerateExtractElement( const ExpressionNode& expression,
                                         const ExpressionNode& index,
                                         IRBuilder& builder ) const;

    llvm::Value* GenerateSwizzle( const ExpressionNode& expression,
                                  const Swizzle& swizzle,
                                  IRBuilder& builder ) const;

    llvm::Value* GenerateVectorConstructor( const ExpressionNode& constructor,
                                            IRBuilder& builder ) const;

    llvm::Value* GenerateMatrixConstructor( const ExpressionNode& constructor,
                                            IRBuilder& builder ) const;

    //
    // Binary Operators
    //
    llvm::Value* GenerateOr( const ExpressionNode& lhs,
                             const ExpressionNode& rhs,
                             IRBuilder& builder ) const;

    llvm::Value* GenerateAnd( const ExpressionNode& lhs,
                              const ExpressionNode& rhs,
                              IRBuilder& builder ) const;

    llvm::Value* GenerateExclusiveOr( const ExpressionNode& lhs,
                                      const ExpressionNode& rhs,
                                      IRBuilder& builder ) const;

    llvm::Value* GenerateCompareEqual( const ExpressionNode& lhs,
                                       const ExpressionNode& rhs,
                                       IRBuilder& builder ) const;

    llvm::Value* GenerateCompareNotEqual( const ExpressionNode& lhs,
                                          const ExpressionNode& rhs,
                                          IRBuilder& builder ) const;

    llvm::Value* GenerateCompareLessThan( const ExpressionNode& lhs,
                                          const ExpressionNode& rhs,
                                          IRBuilder& builder ) const;

    llvm::Value* GenerateCompareGreaterThan( const ExpressionNode& lhs,
                                             const ExpressionNode& rhs,
                                             IRBuilder& builder ) const;

    llvm::Value* GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                                const ExpressionNode& rhs,
                                                IRBuilder& builder ) const;

    llvm::Value* GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                   const ExpressionNode& rhs,
                                                   IRBuilder& builder ) const;

    llvm::Value* GenerateLeftShift( const ExpressionNode& lhs,
                                    const ExpressionNode& rhs,
                                    IRBuilder& builder ) const;

    llvm::Value* GenerateRightShift( const ExpressionNode& lhs,
                                     const ExpressionNode& rhs,
                                     IRBuilder& builder ) const;

    llvm::Value* GenerateAdd( const ExpressionNode& lhs,
                              const ExpressionNode& rhs,
                              IRBuilder& builder ) const;

    llvm::Value* GenerateSubtract( const ExpressionNode& lhs,
                                   const ExpressionNode& rhs,
                                   IRBuilder& builder ) const;

    llvm::Value* GenerateMultiply( const ExpressionNode& lhs,
                                   const ExpressionNode& rhs,
                                   IRBuilder& builder ) const;

    llvm::Value* GenerateDivide( const ExpressionNode& lhs,
                                 const ExpressionNode& rhs,
                                 IRBuilder& builder ) const;

    llvm::Value* GenerateModulo( const ExpressionNode& lhs,
                                 const ExpressionNode& rhs,
                                 IRBuilder& builder ) const;

    // Helpers
    llvm::Value* GenerateScalarOrVectorCast( llvm::Value* from_value,
                                             Type from_type,
                                             Type to_type,
                                             IRBuilder& builder ) const;
    llvm::Value* GenerateMatrixCast( llvm::Value* from_value,
                                     Type from_type,
                                     Type to_type,
                                     IRBuilder& builder ) const;
};

} // namespace Compiler
} // namespace JoeLang
