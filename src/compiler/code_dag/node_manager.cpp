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

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/type_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/pass_node.hpp>
#include <compiler/code_dag/technique_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>

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

const Node& NodeManager::MakeNode( NodeType node_type, std::vector<Node_ref> children )
{
    m_Nodes.emplace_back( new Node( node_type, std::move( children ) ) );
    return *m_Nodes.back();
}

const TypeNode& NodeManager::MakeTypeNode( const CompleteType& type )
{
    m_Nodes.emplace_back( new TypeNode( type ) );
    return static_cast<const TypeNode&>( *m_Nodes.back() );
}

const ZeroNode& NodeManager::MakeZero( Type type )
{
    m_Nodes.emplace_back( new ZeroNode( type ) );
    return static_cast<const ZeroNode&>( *m_Nodes.back() );
}

const VariableNode& NodeManager::MakeVariableNode( Variable_sp variable )
{
    m_Nodes.emplace_back( new VariableNode( std::move( variable ) ) );
    return static_cast<const VariableNode&>( *m_Nodes.back() );
}

const FunctionNode& NodeManager::MakeFunctionNode( Function_sp function )
{
    m_Nodes.emplace_back( new FunctionNode( std::move( function ) ) );
    return static_cast<const FunctionNode&>( *m_Nodes.back() );
}

const SwizzleNode& NodeManager::MakeSwizzleNode( const Node& swizzled, const Swizzle& swizzle )
{
    m_Nodes.emplace_back( new SwizzleNode( swizzled, swizzle ) );
    return static_cast<const SwizzleNode&>( *m_Nodes.back() );
}

const TechniqueNode& NodeManager::MakeTechniqueNode( std::string name, std::vector<PassNode_ref> passes )
{
    // Hopefully this will be a no-op
    
    std::vector<Node_ref> ns;
    for( const PassNode& n : passes )
        ns.emplace_back( n );
    
    m_Nodes.emplace_back( new TechniqueNode( name, std::move( ns ) ) );
    return static_cast<const TechniqueNode&>( *m_Nodes.back() );
}

const PassNode& NodeManager::MakePassNode( std::string name, std::vector<StateAssignmentNode_ref> state_assignments )
{
    // Hopefully this will be a no-op
    
    std::vector<Node_ref> ns;
    for( const StateAssignmentNode& n : state_assignments )
        ns.emplace_back( n );
    
    m_Nodes.emplace_back( new PassNode( name, std::move( ns ) ) );
    return static_cast<const PassNode&>( *m_Nodes.back() );
}

const StateAssignmentNode& NodeManager::MakeStateAssignmentNode( const StateBase& state, const Node& assigned_expression )
{
    m_Nodes.emplace_back( new StateAssignmentNode( state, assigned_expression ) );
    return static_cast<const StateAssignmentNode&>( *m_Nodes.back() );
}

} // namespace Compiler
} // namespace JoeLang
