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

#include "dot_writer.hpp"

#include <cassert>
#include <algorithm>
#include <string>

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/compile_statement_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/pass_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/swizzle_store_node.hpp>
#include <compiler/code_dag/technique_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/casting.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{
namespace Compiler
{

std::string DotWriter::GenerateDotString()
{
    std::string header = "digraph code_dag {\n"
                         "graph [compound=true, clusterrank=\"local\"];\n";
    std::string footer = "}\n";

    std::string content;
    std::string edges;

    for( const TechniqueNode& technique_node : m_TechniqueClusters )
    {
        std::string subgraph_header = "subgraph cluster_" + GetUniqueIdentifier() + "{\n";
        std::string subgraph_footer = "\nstyle = \"rounded,solid\";\n}\n";
        edges += GetEdges( technique_node );
        content += subgraph_header + m_Labels + subgraph_footer;
        m_Labels.clear();
    }

    for( const auto& function_cluster : m_FunctionClusterMap )
    {
        std::string subgraph_header = "subgraph " + function_cluster.second + "{\n";
        std::string subgraph_footer = "style = \"rounded,dashed\";\n}\n";

        std::string cluster_label =
            function_cluster.second + "_TITLE [label = \"" +
            function_cluster.first->GetIdentifier() + "\", style=\"rounded\"];\n";

        const Node& function_node = function_cluster.first->GetCodeDag();
        edges += GetEdges( function_node );
        edges += function_cluster.second + "_TITLE -> " + GetIdentifier( function_node ) + ";\n";
        content += subgraph_header + cluster_label + m_Labels + subgraph_footer;
        m_Labels.clear();
    }

    std::string ret = header + content + "\n" + edges + footer;

    Clear();

    return ret;
}

void DotWriter::AddFunction( const Function& function )
{
    m_FunctionClusterMap[&function] = "cluster_" + GetUniqueIdentifier();
    
    GenerateInvisibleEdges( function.GetCodeDag() );
}

void DotWriter::AddTechnique( const TechniqueNode& technique_node )
{
    m_TechniqueClusters.push_back( technique_node );
    
    GenerateInvisibleEdges( technique_node );
}

void DotWriter::GenerateInvisibleEdges( const Node& node )
{    
    std::set<const Node*> function_dependency_nodes = node.GetDescendantsWithNodeType( NodeType::FunctionIdentifier );
    std::vector<const Function*> function_dependencies( function_dependency_nodes.size() );
    std::transform( function_dependency_nodes.begin(),
                    function_dependency_nodes.end(),
                    function_dependencies.begin(),
                    []( const Node* n ){ return &cast<FunctionNode>(n)->GetFunction(); });
    
    std::vector<Node_ref> base_leaf_nodes; 
    std::vector<std::pair<Node_ref, unsigned>> stack = {{ std::make_pair( std::cref(node), 0 ) }};
    unsigned max_depth = 0;
    while (!stack.empty())
    {
        const Node& n = stack.back().first;
        unsigned depth = stack.back().second;
        
        stack.pop_back();
        for( const Node& s : n.GetChildren() )
            stack.push_back( std::make_pair( std::cref(s), depth + 1) );
        
        if( n.GetNumChildren() == 0 )
            if( depth > max_depth )
            {
                base_leaf_nodes.push_back( n );
                max_depth = depth;
            }
    }
    //for( const Node& n : base_leaf_nodes )
    m_InvisibleEdgeMap[&(base_leaf_nodes.back().get())] = function_dependencies;
}

void DotWriter::Clear()
{
    m_Identifiers.clear();
}

bool DotWriter::HasSeen( const Node& node ) const
{
    return m_Identifiers.find( &node ) != m_Identifiers.end();
}

std::string DotWriter::GetEdges( const Node& node )
{

    std::string identifier = GetIdentifier( node );
    std::string edges;

    std::string description = GetNodeDescription( node );
    switch( node.GetNodeType() )
    {
    case NodeType::VariableIdentifier:
    case NodeType::FunctionIdentifier:
        m_Labels += identifier + " [shape=\"box\", label=\"" + description + "\"];\n";
        break;
    default:
        m_Labels +=
            identifier + " [shape=\"box\", style=\"rounded\", label=\"" + description + "\"];\n";
        break;
    }

    //
    // If this is a function identifier, draw an edge to the cluster for this funciton if we have it
    //
    if( node.GetNodeType() == NodeType::FunctionIdentifier )
    {
        const FunctionNode& f = static_cast<const FunctionNode&>( node );
        auto i = m_FunctionClusterMap.find( &f.GetFunction() );
        if( i != m_FunctionClusterMap.end() )
            edges += identifier + " -> " + i->second + "_TITLE" + 
                     "[constraint=\"false\",lhead=" + i->second + "];\n";
    }

    for( const Node& s : node.GetChildren() )
    {
        bool has_seen = HasSeen( s );
        std::string sub_identifier = GetIdentifier( s );
        edges += identifier + " -> " + sub_identifier + ";\n";
        if( !has_seen )
            edges += GetEdges( s );
    }
    
    auto i = m_InvisibleEdgeMap.find( &node );
    if( i != m_InvisibleEdgeMap.end() )
    {
        for( const Function* f : i->second )
        {
            auto j = m_FunctionClusterMap.find( f );
            if( j != m_FunctionClusterMap.end() )
                edges += identifier + " -> " + j->second + "_TITLE" + 
                         "[style=\"invisible\"];\n";
        }
    }
    
    return edges;
}

std::string DotWriter::GetIdentifier( const Node& node )
{
    auto i = m_Identifiers.find( &node );
    if( i != m_Identifiers.end() )
        return i->second;

    std::string identifier = GetUniqueIdentifier();
    m_Identifiers[&node] = identifier;
    return identifier;
}

std::string DotWriter::GetUniqueIdentifier()
{
    return "_" + std::to_string( m_NumUniqueIdentifiers++ );
}

std::string DotWriter::GetNodeDescription( const Node& node ) const
{
    switch( node.GetNodeType() )
    {
    case NodeType::Unimplemented:
        return "Unimplemented";
    case NodeType::Technique:
        return "Technique: " + static_cast<const TechniqueNode&>( node ).GetName();
    case NodeType::Pass:
        return "Pass: " + static_cast<const PassNode&>( node ).GetName();
    case NodeType::StateAssignment:
        return "StateAssignment";
    case NodeType::CompileStatement:
        return "Compile: " +
               std::string( cast<CompileStatementNode>( node ).GetDomain() == ShaderDomain::VERTEX
                                ? "Vertex"
                                : "Fragment" );
    case NodeType::Sequence:
        return "Sequence";
    case NodeType::Conditional:
        return "If";
    case NodeType::Return:
        return "Return";
    case NodeType::LogicalOr:
        return "LogicalOr";
    case NodeType::LogicalAnd:
        return "LogicalAnd";
    case NodeType::BitwiseOr:
        return "BitwiseOr";
    case NodeType::BitwiseExclusiveOr:
        return "BitwiseExclusiveOr";
    case NodeType::BitwiseAnd:
        return "BitwiseAnd";
    case NodeType::CompareEqual:
        return "CompareEqual";
    case NodeType::CompareNotEqual:
        return "CompareNotEqual";
    case NodeType::CompareLessThan:
        return "CompareLessThan";
    case NodeType::CompareGreaterThan:
        return "CompareGreaterThan";
    case NodeType::CompareLessThanEquals:
        return "CompareLessThanEquals";
    case NodeType::CompareGreaterThanEquals:
        return "CompareGreaterThanEquals";
    case NodeType::LeftShift:
        return "LeftShift";
    case NodeType::RightShift:
        return "RightShift";
    case NodeType::Add:
        return "Add";
    case NodeType::Subtract:
        return "Subtract";
    case NodeType::Multiply:
        return "Multiply";
    case NodeType::Divide:
        return "Divide";
    case NodeType::Modulo:
        return "Modulo";
    case NodeType::Negate:
        return "Negate";
    case NodeType::BitwiseNot:
        return "BitwiseNot";
    case NodeType::LogicalNot:
        return "LogicalNot";
    case NodeType::PreIncrement:
        return "PreIncrement";
    case NodeType::PreDecrement:
        return "PreDecrement";
    case NodeType::Select:
        return "Select";
    case NodeType::Store:
        return "Store";
    case NodeType::SwizzleStore:
        return "Swizzle Store: " +
               static_cast<const SwizzleStoreNode&>( node ).GetSwizzle().GetString();
    case NodeType::Cast:
        return "Cast: " + cast<CastNode>( node ).GetType().GetString();
    case NodeType::ArrayIndex:
        return "ArrayIndex";
    case NodeType::Constant:
        switch( cast<ExpressionNode>( node ).GetType().GetType() )
        {
        case Type::INT:
            return "Constant " +
                   std::to_string( static_cast<const ConstantNode<jl_int>&>( node ).GetConstant() );
        case Type::UINT:
            return "Constant " + std::to_string( static_cast<const ConstantNode<jl_uint>&>( node )
                                                     .GetConstant() );
        case Type::DOUBLE:
            return "Constant " + std::to_string( static_cast<const ConstantNode<jl_double>&>( node )
                                                     .GetConstant() );
        case Type::FLOAT:
            return "Constant " + std::to_string( static_cast<const ConstantNode<jl_float>&>( node )
                                                     .GetConstant() );
        default:
            return "Constant";
        }
    case NodeType::Zero:
        return "Zero";
    case NodeType::VariableIdentifier:
        return "Variable: " + static_cast<const VariableNode&>( node ).GetVariable().GetName();
    case NodeType::FunctionIdentifier:
        return "Function: " +
               static_cast<const FunctionNode&>( node ).GetFunction().GetIdentifier();
    case NodeType::Swizzle:
        return "Swizzle: " + static_cast<const SwizzleNode&>( node ).GetSwizzle().GetString();
    case NodeType::Call:
        return "Call";
    case NodeType::ExtractColumn:
        return "ExtractColumn";
    case NodeType::InsertColumn:
        return "InsertColumn";
    case NodeType::ExtractElement:
        return "ExtractElement";
    case NodeType::InsertElement:
        return "InsertElement";
    case NodeType::VectorConstructor:
        return "VectorConstructor: " + cast<ExpressionNode>( node ).GetType().GetString();
    case NodeType::MatrixConstructor:
        return "MatrixConstructor: " + cast<ExpressionNode>( node ).GetType().GetString();
    default:
        assert( false && "Trying to get the description of an unhandled node type" );
        std::abort();
    }
}

} // namespace Compiler
} // namespace JoeLang
