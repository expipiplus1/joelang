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

#include "shader_compilation_context.hpp"

#include <algorithm>
#include <functional>
#include <string>
#include <vector>
#include <set>

#include <compiler/code_dag/compile_statement_node.hpp>
#include <compiler/writers/glsl_writer.hpp>
#include <joelang/shader.hpp>
#include <joelang/context.hpp>

namespace JoeLang
{
namespace Compiler
{

ShaderCompilationContext::ShaderCompilationContext( const Context& joelang_context,
                                                    std::vector<CompileStatementNode_ref> compile_statements)
    :m_Context( joelang_context )
    ,m_CompileStatements( std::move( compile_statements ) )
{
    //
    // Todo, check the staging
    //

    //
    // Sort the compile statements to be in order of data flow though stages
    //
    std::sort( m_CompileStatements.begin(),
               m_CompileStatements.end(),
               []( const CompileStatementNode& c1, const CompileStatementNode& c2 )
               { return c1.GetDomain() < c2.GetDomain(); } );
}

const Context& ShaderCompilationContext::GetJoeLangContext() const
{
    return m_Context;
}

std::set<ShaderDomain> ShaderCompilationContext::GetActiveDomains() const
{
    std::set<ShaderDomain> ret;
    for( const CompileStatementNode& compile_statement : m_CompileStatements )
        ret.insert( compile_statement.GetDomain() );
    return ret;
}
    
std::string ShaderCompilationContext::GetInputPrefix( ShaderDomain domain ) const
{
    switch( domain )
    {
    case ShaderDomain::VERTEX:
        return "attribute_";
    case ShaderDomain::FRAGMENT:
        return "vertextofragment_";
    }
}

std::string ShaderCompilationContext::GetOutputPrefix( ShaderDomain domain ) const
{
    switch( domain )
    {
    case ShaderDomain::VERTEX:
        return "vertextofragment_";
    case ShaderDomain::FRAGMENT:
        return "fragmentout_";
    }
}

std::string ShaderCompilationContext::GetSource( ShaderDomain domain ) const
{
    for( const CompileStatementNode& compile_statement : m_CompileStatements )
        if( compile_statement.GetDomain() == domain )
            return GLSLWriter::GenerateGLSL( *this, compile_statement );
    assert( false && "Trying to get a string for a unspecified shader" );
}

void ShaderCompilationContext::Error( const std::string& message ) const
{
    // Todo, add some context to these error messages
    GetJoeLangContext().Error( message );
}

    /*
void ShaderCompilationContext::FindVariables( const CompileStatementNode& compile_statement ) const
{
    const Function& entry_function = compile_statement.GetEntryFunction();

    //
    // Find all the functions this stage uses
    //
    bool recursion;
    std::set<const Function*> function_dependencies =
        entry_function.GetFunctionDependencies( recursion );
    function_dependencies.insert( &entry_function );

    if( recursion )
    {
        Error( "Can't have recursion in glsl" );
        return;
    }

    //
    // Inputs and uniforms are immutable in glsl so if we write to them we need to create a global
    // copy and initialize that at the start of main.
    // TODO, don't do this for variables which are never written to
    //
    for( const Function* function : function_dependencies )
    {
        const Node& function_node = function->GetCodeDag();
        const std::set<const Node*> variable_nodes =
            function_node.GetDescendantsWithNodeType( NodeType::VariableIdentifier );
        for( const Node* variable_node : variable_nodes )
        {
            const Variable& v = cast<VariableNode>( *variable_node ).GetVariable();
            if( v.IsGlobal() )//|| ( function == &entry_function && v.IsParameter() ) )
            {
                // TODO, allow this, as long as it's only used as an input in one shader and an
                // output in another earlier in the pipeline
                assert( !(v.IsIn() && v.IsOut()) && "Global Variable is both in and out" );

                // TODO: Insert some checking code to make sure we don't have 'in uniforms' or
                // 'in out' variables in global scope

                const Semantic& semantic = v.GetSemantic();
                if( semantic.HasBuiltin( compile_statement.GetDomain(), v.IsIn() ) )
                    m_VariableNames.insert(
                                           std::make_pair( &v, semantic.GetBuiltin( compile_statement.GetDomain(), v.IsIn() ) ) );
                else if( v.IsIn() )
                    input.insert( &v );
                else if( v.IsOut() )
                    output.insert( &v );
                else if( v.IsUniform() )
                    uniform.insert( &v );
                else
                    global.insert( &v );
            }
        }
    }
}
     */

} // namespace Compiler
} // namespace JoeLang
