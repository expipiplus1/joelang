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

#include "expression_node.hpp"

#include <cassert>

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/swizzle_store_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/support/casting.hpp>

namespace JoeLang
{
namespace Compiler
{

ExpressionNode::ExpressionNode( NodeType type, std::vector<Node_ref> children )
    : Node( type, std::move( children ) )
{
    assert( type >= NodeType::Expression_start && type <= NodeType::Expression_end &&
            "ExpressionNode given a non-expression node type" );
}

CompleteType ExpressionNode::GetType() const
{
    switch( GetNodeType() )
    {
    case NodeType::Constant:
        return CompleteType( cast<ConstantNodeBase>( *this ).GetType() );
    case NodeType::Zero:
        return cast<ZeroNode>( *this ).GetType();
    case NodeType::VariableIdentifier:
        return cast<VariableNode>( *this ).GetType();
    case NodeType::Swizzle:
        return cast<SwizzleNode>( *this ).GetType();
    case NodeType::SwizzleStore:
        return cast<SwizzleStoreNode>( *this ).GetType();
    case NodeType::LogicalOr:
    case NodeType::LogicalAnd:
    case NodeType::CompareEqual:
    case NodeType::CompareNotEqual:
    case NodeType::CompareLessThan:
    case NodeType::CompareGreaterThan:
    case NodeType::CompareLessThanEquals:
    case NodeType::CompareGreaterThanEquals:
        // todo, vectors of bool
        return CompleteType( Type::BOOL );

    // All of these have at least one operand, which will have been cast to the right type
    case NodeType::BitwiseOr:
    case NodeType::BitwiseExclusiveOr:
    case NodeType::BitwiseAnd:
    case NodeType::LeftShift:
    case NodeType::RightShift:
    case NodeType::Add:
    case NodeType::Subtract:
    case NodeType::Multiply:
    case NodeType::Divide:
    case NodeType::Modulo:
    case NodeType::Negate:
    case NodeType::BitwiseNot:
    case NodeType::LogicalNot:
    case NodeType::PreIncrement:
    case NodeType::PreDecrement:
    case NodeType::Select:
    case NodeType::InsertElement:
    case NodeType::InsertColumn:
    case NodeType::Store:
        return cast<ExpressionNode>( GetChild( 0 ) ).GetType();
    case NodeType::Cast:
        return cast<CastNode>( *this ).GetType();
    case NodeType::ArrayIndex:
    {
        CompleteType array_type = cast<ExpressionNode>( GetChild( 0 ) ).GetType();
        ArrayExtents array_extents = array_type.GetArrayExtents();
        assert( !array_extents.empty() && "Trying to index into a non-array type" );
        array_extents.resize( array_extents.size() - 1 );
        return CompleteType( array_type.GetBaseType(), std::move( array_extents ) );
    }
    case NodeType::ExtractElement:
        return CompleteType( cast<ExpressionNode>( GetChild( 0 ) ).GetType().GetElementType() );
    case NodeType::ExtractColumn:
        return CompleteType( cast<ExpressionNode>( GetChild( 0 ) )
                                 .GetType().GetMatrixColumnType() );

    // The function identifier is the last node in a call expression
    case NodeType::Call:
        return cast<FunctionNode>( GetChild( GetNumChildren() - 1 ) ).GetReturnType();
    // The constructors hold the type in the last node
    case NodeType::VectorConstructor:
        return CompleteType( GetVectorType(
            cast<ExpressionNode>( GetChild( 0 ) ).GetType().GetType(), GetNumChildren() ) );
    case NodeType::MatrixConstructor:
        return CompleteType( GetMatrixType(
            cast<ExpressionNode>( GetChild( 0 ) ).GetType().GetType(), GetNumChildren() ) );

    default:
        assert( false && "Trying to get the type of an unhandled node" );
        return CompleteType();
    }
}

const ExpressionNode& ExpressionNode::GetOperand( unsigned index ) const
{
    switch( GetNodeType() )
    {
    case NodeType::Constant:
    case NodeType::Zero:
    case NodeType::VariableIdentifier:
        assert( false && "Trying to get an out of bounds operand" );

    // In all these cases, the only operand is the one in [0]
    case NodeType::Swizzle:
    case NodeType::Negate:
    case NodeType::BitwiseNot:
    case NodeType::LogicalNot:
    case NodeType::PreIncrement:
    case NodeType::PreDecrement:
    case NodeType::Cast:
        assert( index < 1 && "Trying to get an out of bounds operand" );
        return cast<ExpressionNode>( GetChild( index ) );

    // In these cases, there are two operands in [0] and [1]
    case NodeType::LogicalOr:
    case NodeType::LogicalAnd:
    case NodeType::CompareEqual:
    case NodeType::CompareNotEqual:
    case NodeType::CompareLessThan:
    case NodeType::CompareGreaterThan:
    case NodeType::CompareLessThanEquals:
    case NodeType::CompareGreaterThanEquals:
    case NodeType::BitwiseOr:
    case NodeType::BitwiseExclusiveOr:
    case NodeType::BitwiseAnd:
    case NodeType::LeftShift:
    case NodeType::RightShift:
    case NodeType::Add:
    case NodeType::Subtract:
    case NodeType::Multiply:
    case NodeType::Divide:
    case NodeType::Modulo:
    case NodeType::ArrayIndex:
    case NodeType::ExtractColumn:
    case NodeType::ExtractElement:
        assert( index < 2 && "Trying to get an out of bounds operand" );
        return cast<ExpressionNode>( GetChild( index ) );

    // In these cases, there are three operands in [0..2]`
    case NodeType::Select:
    case NodeType::InsertColumn:
    case NodeType::InsertElement:
        assert( index < 3 && "Trying to get an out of bounds operand" );
        return cast<ExpressionNode>( GetChild( index ) );

    // These all have a variable number of operands and a non-operand at the end
    case NodeType::Call:
        assert( index < GetNumChildren() - 1 && "Trying to get an out of bounds operand" );
        return cast<ExpressionNode>( GetChild( index ) );

    case NodeType::VectorConstructor:
    case NodeType::MatrixConstructor:
        assert( index < GetNumChildren() && "Trying to get an out of bounds operand" );
        return cast<ExpressionNode>( GetChild( index ) );

    default:
        assert( false && "Trying to get an operand of an unhandled expression type" );
    }
}

bool ExpressionNode::classof( const Node* n )
{
    return n->GetNodeType() >= NodeType::Expression_start &&
           n->GetNodeType() <= NodeType::Expression_end;
}

} // namespace Compiler
} // namespace JoeLang
