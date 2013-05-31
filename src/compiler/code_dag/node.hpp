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
#include <vector>

namespace JoeLang
{
namespace Compiler
{

enum class NodeType
{
    Unimplemented,

    //
    // Miscellaneous
    //
    Type,

    //
    // Structural
    //

    Technique,
    Pass,
    StateAssignment,

    //
    // Statements
    //

    Sequence,
    Return,

    //
    // Expressions
    //

    Expression_start,
    Constant = Expression_start,
    Zero,
    VariableIdentifier,
    FunctionIdentifier,
    Swizzle,

    BinaryOperator_start,
    LogicalOr = BinaryOperator_start,
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
    BinaryOperator_end = Modulo,

    UnaryOperator_start,
    Negate = UnaryOperator_start,
    BitwiseNot,
    LogicalNot,
    PreIncrement,
    PreDecrement,
    UnaryOperator_end = PreDecrement,

    Select,
    Cast,

    ArrayIndex,

    Call,

    ExtractElement,
    InsertElement,
    VectorConstructor,
    MatrixConstructor,
    Expression_end = MatrixConstructor,
};

class Node;
using Node_ref = std::reference_wrapper<const Node>;

class Node
{
public:
    unsigned GetNumChildren() const;

    const std::vector<Node_ref>& GetChildren() const;

    const Node& GetChild( unsigned index ) const;

    NodeType GetNodeType() const;

    virtual
    ~Node();

protected:
    friend class NodeManager;
    Node( NodeType type, std::vector<Node_ref> children );

private:

    const NodeType m_Type;
    const std::vector<Node_ref> m_Children;
};

} // namespace Compiler
} // namespace JoeLang
