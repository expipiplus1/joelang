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

#include "node_manager.hpp"

#include <algorithm>
#include <map>
#include <stack>
#include <vector>

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/compile_statement_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/glsl_builtin_node.hpp>
#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/pass_node.hpp>
#include <compiler/code_dag/pointer_expression_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/code_dag/statement_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/swizzle_store_node.hpp>
#include <compiler/code_dag/technique_node.hpp>
#include <compiler/code_dag/temporary_assignment_node.hpp>
#include <compiler/code_dag/temporary_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/support/casting.hpp>

namespace JoeLang
{
namespace Compiler
{

NodeManager::NodeManager()
{
}

NodeManager::~NodeManager()
{
}

//
// Tree transformations
//
const StatementNode& NodeManager::InsertTemporaries( const StatementNode& node )
{
    switch( node.GetNodeType() )
    {
    case NodeType::Sequence:
    {
        std::vector<Node_ref> new_children;
        for( const Node& n : node.GetChildren() )
            new_children.push_back( InsertTemporaries( cast<StatementNode>( n ) ) );
        return MakeStatementNode( NodeType::Sequence, std::move( new_children ) );
    }
    case NodeType::Return:
    case NodeType::ExpressionStatement:
    case NodeType::Conditional:
    {
        if( node.GetNumChildren() == 0 )
            return node;

        std::vector<StatementNode_ref> temporary_assignments;
        const ExpressionNode& new_expression = InsertTemporariesIntoExpression(
            cast<ExpressionNode>( node.GetChild( 0 ) ), temporary_assignments );
        if( temporary_assignments.empty() )
            return node;

        if( node.GetNodeType() == NodeType::Conditional )
            if( node.GetNumChildren() == 2 )
                temporary_assignments.push_back( MakeStatementNode(
                    NodeType::Conditional,
                    { new_expression,
                      InsertTemporaries( cast<StatementNode>( node.GetChild( 0 ) ) ) } ) );
            else
                temporary_assignments.push_back( MakeStatementNode(
                    NodeType::Conditional,
                    { new_expression,
                      InsertTemporaries( cast<StatementNode>( node.GetChild( 1 ) ) ),
                      InsertTemporaries( cast<StatementNode>( node.GetChild( 2 ) ) ) } ) );
        else
            temporary_assignments.push_back(
                MakeStatementNode( node.GetNodeType(), { new_expression } ) );
        std::vector<Node_ref> nodes;
        nodes.reserve( temporary_assignments.size() );
        for( const auto i : temporary_assignments )
            nodes.push_back( i.get() );
        return MakeStatementNode( NodeType::Sequence, std::move( nodes ) );
    }
    default:
        assert( false && "Trying to insert temporaries into an unhandled statement node" );
    }
}

const ExpressionNode& NodeManager::InsertTemporariesIntoExpression(
    const ExpressionNode& expression,
    std::vector<StatementNode_ref>& temporary_assignments )
{
    std::set<const ExpressionNode*> seen_nodes;
    std::vector<const ExpressionNode*> duplicate_nodes;
    //
    // Walk over the expression tree and when we see a node more than once
    //
    std::stack<ExpressionNode_ref> explore_stack;
    explore_stack.push( expression );
    while( !explore_stack.empty() )
    {
        const ExpressionNode& e = explore_stack.top();
        explore_stack.pop();

        auto i = seen_nodes.find( &e );
        if( i != seen_nodes.end() )
        {
            // We've seen this node before
            auto j = std::find( duplicate_nodes.begin(), duplicate_nodes.end(), &e );
            if( j == duplicate_nodes.end() )
                duplicate_nodes.push_back( &e );
        }
        else
        {
            seen_nodes.insert( &e );
            for( const Node& n : e.GetChildren() )
            {
                const ExpressionNode* s = dyn_cast<ExpressionNode>( &n );
                if( !s )
                    // Don't bother with non-expressions
                    continue;
                explore_stack.push( *s );
            }
        }
    }

    //
    // If there were no duplicates, we can return the original expression
    //
    if( duplicate_nodes.empty() )
        return expression;

    //
    // There were duplicate expressions, so assign them to temporaries, and recreate the tree
    // with references to those temporaries
    //
    std::map<const ExpressionNode*, unsigned> temporary_map;
    for( auto i = duplicate_nodes.rbegin(); i != duplicate_nodes.rend(); ++i )
    {
        unsigned temporary_number = m_NumTemporaries++;
        temporary_map[*i] = temporary_number;
        temporary_assignments.push_back( MakeTemporaryAssignmentNode( temporary_number, **i ) );
    }

    std::function<const ExpressionNode&( const ExpressionNode& )> get_new_expression;
    get_new_expression = [&get_new_expression, &temporary_map, this]( const ExpressionNode & e )
        ->const ExpressionNode &
    {
        auto i = temporary_map.find( &e );
        if( i != temporary_map.end() )
            return MakeTemporaryNode( i->second, e.GetType() );

        //
        // Otherwise we have to copy this node
        //
        if( e.GetNumChildren() == 0 )
            return e;

        //
        // If it has children, filter those
        //
        std::vector<Node_ref> new_children;
        new_children.reserve( e.GetNumChildren() );
        for( const Node& child : e.GetChildren() )
        {
            if( !isa<ExpressionNode>( child ) )
                new_children.push_back( child );
            else
                new_children.push_back( get_new_expression( cast<ExpressionNode>( child ) ) );
        }

        switch( e.GetNodeType() )
        {
        case NodeType::Swizzle:
            assert( new_children.size() == 1 && "Wrong number of children in swizzle node" );
            return MakeSwizzleNode( cast<ExpressionNode>( new_children.front().get() ),
                                    cast<SwizzleNode>( e ).GetSwizzle() );
        case NodeType::SwizzleStore:
            assert( new_children.size() == 2 && "Wrong number of children in swizzle store node" );
            return MakeSwizzleStoreNode( cast<PointerExpressionNode>( new_children[0].get() ),
                                         cast<ExpressionNode>( new_children[1].get() ),
                                         cast<SwizzleStoreNode>( e ).GetSwizzle() );
        case NodeType::Cast:
            assert( new_children.size() == 1 && "Wrong number of children in cast node" );
            return MakeCastNode( cast<ExpressionNode>( new_children.front().get() ),
                                 cast<CastNode>( e ).GetType() );
        default:
            if( isa<PointerExpressionNode>( e ) )
                return MakePointerExpressionNode( e.GetNodeType(), std::move( new_children ) );
            else
                return MakeExpressionNode( e.GetNodeType(), std::move( new_children ) );
        }
    }
    ;
    return get_new_expression( expression );
}

//
// Node Creation
//

const Node& NodeManager::MakeNode( NodeType node_type, std::vector<Node_ref> children )
{
    m_Nodes.emplace_back( new Node( node_type, std::move( children ) ) );
    return *m_Nodes.back();
}

const StatementNode& NodeManager::MakeStatementNode( NodeType node_type,
                                                     std::vector<Node_ref> children )
{
    assert( node_type >= NodeType::Statement_start && node_type <= NodeType::Statement_end &&
            "Trying to make an statement node with a non statement node type" );

    m_Nodes.emplace_back( new StatementNode( node_type, std::move( children ) ) );
    return cast<StatementNode>( *m_Nodes.back() );
}

const ExpressionNode& NodeManager::MakeExpressionNode( NodeType node_type,
                                                       std::vector<Node_ref> children )
{
    assert( node_type >= NodeType::Expression_start && node_type <= NodeType::Expression_end &&
            "Trying to make an expression node with a non expression node type" );

    m_Nodes.emplace_back( new ExpressionNode( node_type, std::move( children ) ) );
    return cast<ExpressionNode>( *m_Nodes.back() );
}

const PointerExpressionNode& NodeManager::MakePointerExpressionNode(
    NodeType node_type,
    std::vector<Node_ref> children )
{
    assert( node_type >= NodeType::PointerExpression_start &&
            node_type <= NodeType::PointerExpression_end &&
            "Trying to make a pointer expression node with a non pointer expression node type" );

    m_Nodes.emplace_back( new PointerExpressionNode( node_type, std::move( children ) ) );

    return cast<PointerExpressionNode>( *m_Nodes.back() );
}

const SwizzleStoreNode& NodeManager::MakeSwizzleStoreNode( const PointerExpressionNode& assignee,
                                                           const ExpressionNode& assigned,
                                                           const Swizzle& swizzle )
{
    m_Nodes.emplace_back( new SwizzleStoreNode( assignee, assigned, swizzle ) );
    return cast<SwizzleStoreNode>( *m_Nodes.back() );
}

const ZeroNode& NodeManager::MakeZero( Type type )
{
    m_Nodes.emplace_back( new ZeroNode( type ) );
    return cast<ZeroNode>( *m_Nodes.back() );
}

const VariableNode& NodeManager::MakeVariableNode( Variable_sp variable )
{
    m_Nodes.emplace_back( new VariableNode( std::move( variable ) ) );
    return cast<VariableNode>( *m_Nodes.back() );
}

const FunctionNode& NodeManager::MakeFunctionNode( const Function& function )
{
    m_Nodes.emplace_back( new FunctionNode( function ) );
    return cast<FunctionNode>( *m_Nodes.back() );
}

const SwizzleNode& NodeManager::MakeSwizzleNode( const ExpressionNode& swizzled,
                                                 const Swizzle& swizzle )
{
    m_Nodes.emplace_back( new SwizzleNode( swizzled, swizzle ) );
    return cast<SwizzleNode>( *m_Nodes.back() );
}

const ExpressionNode& NodeManager::MakeCastNode( const ExpressionNode& expression,
                                                 CompleteType type )
{
    if( type == expression.GetType() )
        return expression;
    m_Nodes.emplace_back( new CastNode( expression, std::move( type ) ) );
    return cast<CastNode>( *m_Nodes.back() );
}

const TechniqueNode& NodeManager::MakeTechniqueNode( std::string name,
                                                     std::vector<PassNode_ref> passes )
{
    // Hopefully this will be a no-op

    std::vector<Node_ref> ns;
    for( const PassNode& n : passes )
        ns.emplace_back( n );

    m_Nodes.emplace_back( new TechniqueNode( name, std::move( ns ) ) );
    return cast<TechniqueNode>( *m_Nodes.back() );
}

const PassNode& NodeManager::MakePassNode( std::string name, std::vector<Node_ref> statements )
{
    m_Nodes.emplace_back( new PassNode( name, std::move( statements ) ) );
    return cast<PassNode>( *m_Nodes.back() );
}

const StateAssignmentNode& NodeManager::MakeStateAssignmentNode(
    const StateBase& state,
    const ExpressionNode& assigned_expression )
{
    m_Nodes.emplace_back( new StateAssignmentNode( state, assigned_expression ) );
    return cast<StateAssignmentNode>( *m_Nodes.back() );
}

const CompileStatementNode& NodeManager::MakeCompileStatementNode(
    ShaderDomain domain,
    const FunctionNode& entry_function,
    const std::vector<ExpressionNode_ref>& parameters )
{
    std::vector<Node_ref> children;
    children.reserve( parameters.size() + 1 );
    for( const ExpressionNode& p : parameters )
        children.push_back( p );
    children.push_back( entry_function );
    m_Nodes.emplace_back( new CompileStatementNode( domain, std::move( children ) ) );
    return cast<CompileStatementNode>( *m_Nodes.back() );
}

const TemporaryAssignmentNode& NodeManager::MakeTemporaryAssignmentNode(
    unsigned temporary_number,
    const ExpressionNode& assigned )
{
    m_Nodes.emplace_back( new TemporaryAssignmentNode( temporary_number, assigned ) );
    return cast<TemporaryAssignmentNode>( *m_Nodes.back() );
}

const TemporaryNode& NodeManager::MakeTemporaryNode( unsigned temporary_number,
                                                     const CompleteType& type )
{
    m_Nodes.emplace_back( new TemporaryNode( temporary_number, type ) );
    return cast<TemporaryNode>( *m_Nodes.back() );
}

const PointerExpressionNode& NodeManager::MakeGLSLBuiltinNode( std::string builtin_name )
{
    m_Nodes.emplace_back( new GLSLBuiltinNode( std::move( builtin_name ) ) );
    return cast<GLSLBuiltinNode>( *m_Nodes.back() );
}

} // namespace Compiler
} // namespace JoeLang
