/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

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

#include "effect_factory.hpp"

#include <fstream>
#include <iostream>
#include <memory>
#include <vector>

#include <compiler/code_dag/compile_statement_node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/pass_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/code_dag/technique_node.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/dot_writer.hpp>
#include <compiler/writers/llvm_writer.hpp>
#include <joelang/config.h>
#include <joelang/context.hpp>
#include <joelang/effect.hpp>
#include <joelang/parameter.hpp>
#include <joelang/technique.hpp>

#include <joelang/program.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{

class ParameterBase;
using ParameterBase_up = std::unique_ptr<ParameterBase>;

namespace Compiler
{

EffectFactory::EffectFactory( const Context& context, Runtime& runtime )
    : m_Context( context ),
      m_Runtime( runtime ),
      m_LLVMWriter( m_Runtime )
{
}

Pass EffectFactory::GeneratePass( const PassNode& pass_node )
{
    Pass::StateAssignmentVector state_assignments;
    std::vector<Shader> shaders;

    for( const Node& node : pass_node.GetChildren() )
    {
        switch( node.GetNodeType() )
        {
        case NodeType::StateAssignment:
            state_assignments.push_back(
                m_LLVMWriter.GenerateStateAssignment( cast<StateAssignmentNode>( node ) ) );
            break;
        case NodeType::CompileStatement:
#ifdef JOELANG_WITH_OPENGL
            shaders.push_back( Shader( m_Context, cast<CompileStatementNode>( node ) ) );
#else
            m_Context.Error( "Trying to compile a shader without opengl" );
#endif
            break;
        default:
            assert( false && "Unhandled node type in Pass node" );
            std::abort();
        }
    }

#ifdef JOELANG_WITH_OPENGL
    //
    // Create a Program for the pass from all the shaders
    //
    Program program( std::move( shaders ) );
    program.Compile();
#else
    Program program;
#endif

    return Pass( pass_node.GetName(), std::move( state_assignments ), std::move( program ) );
}

Technique EffectFactory::GenerateTechnique( const TechniqueNode& technique_node )
{
    std::vector<Pass> passes;
    for( const Node& pass_node : technique_node.GetChildren() )
    {
        assert( pass_node.GetNodeType() == NodeType::Pass &&
                "Child of technique node wasn't a pass node" );
        passes.push_back( GeneratePass( static_cast<const PassNode&>( pass_node ) ) );
    }
    return Technique( technique_node.GetName(), std::move( passes ) );
}

std::vector<Technique> EffectFactory::GenerateTechniques(
    const std::vector<TechniqueNode_ref>& technique_nodes )
{
    std::vector<Technique> ret;
    for( const TechniqueNode& t : technique_nodes )
        ret.push_back( GenerateTechnique( t ) );
    return ret;
}

std::unique_ptr<Effect> EffectFactory::CreateEffectFromString( const std::string& source,
                                                               const std::string& name )
{
    Lexer token_stream( source, name );
    Parser parser( token_stream );
    SemaAnalyzer sema_analyzer( m_Context, m_Runtime, m_LLVMWriter );
    CodeGenerator code_generator( m_Runtime.CreateCodeGenerator() );
    NodeManager node_manager;

    parser.Parse();

    if( !parser.Good() )
    {
        for( const auto& s : parser.GetErrors() )
            m_Context.Error( s );
        return nullptr;
    }

    TranslationUnit& tu = *parser.GetTranslationUnit();

    //
    // This will resolve identifiers and insert implicit casts
    // It may also codegen variables used in initializers
    //
    sema_analyzer.Analyze( tu );

    if( !sema_analyzer.Good() )
    {
        for( const auto& s : sema_analyzer.GetErrors() )
            m_Context.Error( s );
        return nullptr;
    }
    for( const auto& s : sema_analyzer.GetWarnings() )
        m_Context.Error( s );

    for( const Function_sp& f : sema_analyzer.GetFunctions() )
    {
        f->GenerateCodeDag( node_manager );

        //
        // Todo, replce this with a better pass system
        //
        f->SetCodeDag( node_manager.InsertTemporaries( f->GetCodeDag() ) );
    }

#if 0
    DotWriter dot_writer;

    for( const Function_sp& f : sema_analyzer.GetFunctions() )
    {
        dot_writer.AddFunction( *f, &node_manager.InsertTemporaries( f->GetCodeDag() ) );
    }

    //for( const Variable_sp& v : sema_analyzer.GetGlobalVariables() )
    //dot_writer.AddVariable( v );

    for( const TechniqueNode& t : sema_analyzer.GetTechniqueNodes( node_manager ) )
        dot_writer.AddTechnique( t );

    std::cout << dot_writer.GenerateDotString();

    return nullptr;
#endif

    std::vector<Technique> techniques =
        GenerateTechniques( sema_analyzer.GetTechniqueNodes( node_manager ) );

    //std::vector<ParameterBase_up> parameters =
    //code_generator.GenerateParameters( sema_analyzer.GetUniformVariables() );

    //
    // Todo, investigate if this is optimizing all the old effects again
    //
    //m_Runtime.OptimizeModule();

    return std::unique_ptr<Effect>( new Effect( std::move( techniques ), {} ) );
}

} // namespace Compiler
} // namespace JoeLang
