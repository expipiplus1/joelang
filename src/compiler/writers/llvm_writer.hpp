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
#include <memory>
#include <vector>

namespace llvm
{
class ConstantFolder;
class ExecutionEngine;
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

class ExpressionNode;
class Runtime;
class StateAssignmentNode;

class LLVMWriter
{
public:
    LLVMWriter( Runtime& runtime );

    StateAssignmentBase_up GenerateStateAssignment(
        const StateAssignmentNode& state_assignment_node );

private:
    void* WrapExpressionCommon( const ExpressionNode& expression );
    template <typename T>
    std::function<T()> WrapExpression( const ExpressionNode& expression );

    Runtime& m_Runtime;
    llvm::Module& m_Module;
    llvm::ExecutionEngine& m_ExecutionEngine;

    //
    // Value generation
    //
    using IRBuilder =
        llvm::IRBuilder<true, llvm::ConstantFolder, llvm::IRBuilderDefaultInserter<true>>;

    llvm::Value* GenerateValue( const ExpressionNode& expression, IRBuilder& builder );
    llvm::Value* GenerateCast( const ExpressionNode& expression, IRBuilder& builder );
    llvm::Value* GenerateConstant( const ExpressionNode& expression, IRBuilder& builder );

    // Helpers
    llvm::Value* GenerateScalarOrVectorCast( llvm::Value* from_value,
                                             Type from_type,
                                             Type to_type,
                                             IRBuilder& builder );
    llvm::Value* GenerateMatrixCast( llvm::Value* from_value,
                                     Type from_type,
                                     Type to_type,
                                     IRBuilder& builder );
};

} // namespace Compiler
} // namespace JoeLang
