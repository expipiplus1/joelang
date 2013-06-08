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
#include <set>
#include <vector>

namespace JoeLang
{
namespace Compiler
{

enum class NodeType
{
    Unimplemented,

    //
    // Structural
    //

    // Holds a list of passes
    Technique,
    // Holds a list of StateAssignments
    Pass,
    // Holds a State and an expression
    StateAssignment,
    // Like a Call,
    CompileStatement,

    //
    // Statements
    //

    Statement_start,
    // Holds a list of statements
    Sequence = Statement_start,
    // Holds an expression or is a void return
    Return,
    // Holds an ExpressionNode in [0] a statement in [1] and an optional else statement in [2]
    Conditional,
    // Holds an expression in [0]
    ExpressionStatement,
    // Holds an expression in [0], and a temporary number
    TemporaryAssignment,
    Statement_end = TemporaryAssignment,

    //
    // Misc
    //
    // Holds a const Function&
    FunctionIdentifier,

    //
    // Expressions
    //

    Expression_start,
    // Holds a Constant of its template type
    Constant = Expression_start,
    // Holds a type
    Zero,
    // Holds a Swizzle and an expression
    Swizzle,

    // Holds one Pointer Expression
    PostIncrement,
    PostDecrement,

    //
    // Binary operators
    // These all hold two expressions
    //
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseExclusiveOr,
    BitwiseAnd,
    CompareEqual,
    CompareNotEqual,
    CompareLessThan,
    CompareGreaterThan,
    CompareLessThanEquals,
    CompareGreaterThanEquals,
    LeftShift,
    RightShift,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    //
    // Unary operators
    // These all hold one expression
    //
    Negate,
    BitwiseNot,
    LogicalNot,

    // Holds two expressions to select from in [0] and [1] and a boolean expression in [2]
    Select,
    // Holds an expression in [0]
    Cast,

    // Holds a variable number of arguments and a functionn node last
    Call,

    // Holds a matrix expression in [0] and an index expression in [1]
    ExtractColumn,
    // Holds a matrix expression in [0], a vector expression in [1] and an index expression in [2]
    InsertColumn,
    // Holds a vector expression in [0] and an index expression in [1]
    ExtractElement,
    // Holds a vector expression in [0], a scalar expression in [1] and an index expression in [2]
    InsertElement,
    // Holds n scalar expressions
    VectorConstructor,
    // Holds n vector expressions
    MatrixConstructor,

    //
    // Just holds a temporary number
    //
    Temporary,


    //
    // PointerExpressions return a pointer, which must be accessed with Load or Store
    //
    PointerExpression_start,

    // Holds a Variable_sp
    VariableIdentifier = PointerExpression_start,
    // Holds a PointerExpression in [0] and an index expression in [1]
    ArrayIndex,

    // Holds a PointerExpression in [0] and an expression in [1], this returns the pointer to the
    // stored
    Store,

    // Holds a PointerExpression in [0] and an expression in [1], this returns the pointer to the
    // stored
    SwizzleStore,

    // Holds one Pointer Expression
    PreIncrement,
    PreDecrement,

    //
    // These are special nodes used after transformations in the glsl writer
    //
    GLSLBuiltinVariable,

    PointerExpression_end = GLSLBuiltinVariable,

    Expression_end = PointerExpression_end,
};

class Node;
using Node_ref = std::reference_wrapper<const Node>;

class Node
{
public:
    unsigned GetNumChildren() const;

    const std::vector<Node_ref>& GetChildren() const;

    const Node& GetChild( unsigned index ) const;

    std::set<const Node*> GetDescendantsWithNodeType( NodeType node_type ) const;

    NodeType GetNodeType() const;

    virtual
    ~Node();

protected:
    friend class NodeManager;
    Node( NodeType type, std::vector<Node_ref> children = {} );

private:

    const NodeType m_Type;
    const std::vector<Node_ref> m_Children;
};

} // namespace Compiler
} // namespace JoeLang
