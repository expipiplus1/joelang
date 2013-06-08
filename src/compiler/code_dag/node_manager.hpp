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

namespace JoeLang
{

enum class ShaderDomain;
class StateBase;
enum class Type;

namespace Compiler
{

class CastNode;
class CompileStatementNode;
class CompleteType;
template <typename>
class ConstantNode;
class ExpressionNode;
using ExpressionNode_ref = std::reference_wrapper<const ExpressionNode>;
class Function;
class FunctionNode;
using Function_sp = std::shared_ptr<Function>;
class Node;
enum class NodeType;
using Node_ref = std::reference_wrapper<const Node>;
using Node_up = std::unique_ptr<Node>;
class PassNode;
using PassNode_ref = std::reference_wrapper<const PassNode>;
class PointerExpressionNode;
class StateAssignmentNode;
using StateAssignmentNode_ref = std::reference_wrapper<const StateAssignmentNode>;
class StatementNode;
using StatementNode_ref = std::reference_wrapper<const StatementNode>;
class Swizzle;
class SwizzleNode;
class SwizzleStoreNode;
class TechniqueNode;
class TemporaryAssignmentNode;
class TemporaryNode;
class TypeNode;
class Variable;
class VariableNode;
using Variable_sp = std::shared_ptr<Variable>;
class ZeroNode;

class NodeManager
{
public:
    NodeManager();
    ~NodeManager();

    //
    // Transformations on the tree
    //

    //
    // This will make temporaries out of all shared nodes underneath node
    // Node should be a statement node
    //
    const StatementNode& InsertTemporaries( const StatementNode& node );

    //
    // All the functions to do with creating nodes
    //

    const Node& MakeNode( NodeType node_type, std::vector<Node_ref> children = {} );

    const StatementNode& MakeStatementNode( NodeType node_type,
                                            std::vector<Node_ref> children = {} );

    const ExpressionNode& MakeExpressionNode( NodeType node_type,
                                              std::vector<Node_ref> children = {} );

    const PointerExpressionNode& MakePointerExpressionNode( NodeType node_type,
                                                            std::vector<Node_ref> children = {} );

    const SwizzleStoreNode& MakeSwizzleStoreNode( const PointerExpressionNode& assignee,
                                                  const ExpressionNode& assigned,
                                                  const Swizzle& swizzle );

    template <typename T>
    const ConstantNode<T>& MakeConstant( T constant_value );

    const ZeroNode& MakeZero( Type type );

    const VariableNode& MakeVariableNode( Variable_sp variable );

    const FunctionNode& MakeFunctionNode( const Function& function );

    const SwizzleNode& MakeSwizzleNode( const ExpressionNode& swizzled, const Swizzle& swizzle );

    const ExpressionNode& MakeCastNode( const ExpressionNode& expression, CompleteType type );

    const TechniqueNode& MakeTechniqueNode( std::string name, std::vector<PassNode_ref> passes );

    const PassNode& MakePassNode( std::string name, std::vector<Node_ref> state_assignments );

    const StateAssignmentNode& MakeStateAssignmentNode( const StateBase& state,
                                                        const ExpressionNode& assigned_expression );

    const CompileStatementNode& MakeCompileStatementNode(
        ShaderDomain domain,
        const FunctionNode& entry_function,
        const std::vector<ExpressionNode_ref>& parameters );

    const TemporaryAssignmentNode& MakeTemporaryAssignmentNode( unsigned temporary_number,
                                                                const ExpressionNode& assigned );

    const TemporaryNode& MakeTemporaryNode( unsigned temporary_number, const CompleteType& type );

    const PointerExpressionNode& MakeGLSLBuiltinNode( std::string builtin_name );

private:
    const ExpressionNode& InsertTemporariesIntoExpression(
        const ExpressionNode& expression,
        std::vector<StatementNode_ref>& temporary_assignments );

    // TODO make this a little better
    std::vector<Node_up> m_Nodes;

    unsigned m_NumTemporaries = 0;
};

} // namespace Compiler
} // namespace JoeLang

#include "node_manager-inl.hpp"
