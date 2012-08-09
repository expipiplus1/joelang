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

#include "definition.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <compiler/casting.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/token.hpp>
#include <compiler/tokens/pass_statements/pass_statement.hpp>
#include <compiler/tokens/pass_statements/state_assignment_statement.hpp>
#include <compiler/tokens/pass_statements/compile_statement.hpp>
#include <joelang/pass.hpp>
#include <joelang/program.hpp>
#include <joelang/shader.hpp>
#include <joelang/state_assignment.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PassDefinition
//------------------------------------------------------------------------------

PassDefinition::PassDefinition( PassStatementVector statements )
    :Token( TokenTy::PassDefinition )
{
    for( auto& s : statements )
    {
        //
        // Split the statements into state assignments and compile directives
        //
        if( isa<StateAssignmentStatement>( s ) )
            m_StateAssignments.emplace_back(
                        static_cast<StateAssignmentStatement*>( s.release() ) );
        else if( isa<CompileStatement>( s ) )
            m_CompileStatements.emplace_back(
                        static_cast<CompileStatement*>( s.release() ) );
        else
            assert( false && "Unhandled type of PassStatement" );
    }
#ifndef NDEBUG
    for( const auto& s : m_StateAssignments )
        assert( s && "null StateAssignmentStatement given to PassDefinition" );
    for( const auto& s : m_CompileStatements )
        assert( s && "null CompileStatement given to PassDefinition" );
#endif
}

PassDefinition::~PassDefinition()
{
}

void PassDefinition::PerformSema( SemaAnalyzer& sema )
{
    for( auto& s : m_StateAssignments )
        s->PerformSema( sema );
    for( auto& c : m_CompileStatements )
        c->PerformSema( sema );
}

const PassDefinition::StateAssignStmtVector&
                                    PassDefinition::GetStateAssignments() const
{
    return m_StateAssignments;
}

const PassDefinition::CompileStatementVector&
                                    PassDefinition::GetCompileStatements() const
{
    return m_CompileStatements;
}

bool PassDefinition::Parse( Parser& parser,
                            std::unique_ptr<PassDefinition>& token )
{
    // The definition starts with an open brace
    if( !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    // Parse some StateAssignmentStatements or CompileStatementss
    PassStatementVector statements;
    parser.ExpectSequenceOf<PassStatement>( statements );
    // The parser may have advanced the lexer too far
    CHECK_PARSER;

    // The definiton ends with a close brace
    if( !parser.ExpectTerminal( TerminalType::CLOSE_BRACE ) )
        return false;

    token.reset( new PassDefinition( std::move(statements) ) );
    return true;
}

//------------------------------------------------------------------------------
// PassDeclarationOrIdentifier
//------------------------------------------------------------------------------

PassDeclarationOrIdentifier::PassDeclarationOrIdentifier(
        std::string                      identifier,
        std::unique_ptr<PassDeclaration> declaration )
    :Token( TokenTy::PassDeclarationOrIdentifier )
    ,m_Identifier ( std::move(identifier)  )
    ,m_Declaration( std::move(declaration) )
{
    // Assert if both are full, or none are
    assert( m_Identifier.empty() == bool(m_Declaration) &&
            "PassDeclarationOrIdentifier must have one and only one value" );
}

PassDeclarationOrIdentifier::~PassDeclarationOrIdentifier()
{
}

Pass PassDeclarationOrIdentifier::GeneratePass( CodeGenerator& code_gen ) const
{
    assert( m_DefinitionRef && "Trying to generate a pass with no definition" );
    assert( *m_DefinitionRef && "Trying to generate an undefined pass" );

    const std::string& name = IsIdentifier() ? m_Identifier
                                             : m_Declaration->GetName();

    //
    // Create the state assignments from this pass
    //

    std::vector<std::unique_ptr<StateAssignmentBase> > state_assignments;
    for( const auto& s : (*m_DefinitionRef)->GetStateAssignments() )
        state_assignments.push_back( s->GenerateStateAssignment( code_gen,
                                                                 name ) );


    //
    // Create a shader for every compile statement in the pass
    //

    std::vector<Shader> shaders;
    for( const auto& c : (*m_DefinitionRef)->GetCompileStatements() )
        shaders.emplace_back( c->GetEntryFunction() );

    //
    // Create a Program for the pass from all the shaders
    //

    Program program( std::move(shaders) );

    //
    // Compile the program
    //
    // todo, how to report errors here?
    program.Compile();

    return Pass( name, std::move(state_assignments), std::move(program) );
}

void PassDeclarationOrIdentifier::PerformSema( SemaAnalyzer& sema )
{
    if( m_Declaration )
    {
        m_Declaration->PerformSema( sema );
        if( !m_Declaration->GetName().empty() )
            // If this declaration has a name it will have declared itself with
            // sema and we must pick up the shared pointer from there
            // TODO passes with the same name
            m_DefinitionRef = sema.GetPass( m_Declaration->GetName() );
        else
        {
            assert( m_Declaration->GetDefinition() &&
                    "Anonymous pass without a definition" );
            // This pass doesn't have a name so construct the shared_ptr here
            m_DefinitionRef = std::make_shared<std::unique_ptr<PassDefinition> >
                                            ( m_Declaration->TakeDefinition() );
        }
    }
    else
    {
        SemaAnalyzer::PassDefinitionRef d = sema.GetPass( m_Identifier );
        if( !d )
            sema.Error( "Use of undeclared pass: " + m_Identifier );
        else
            m_DefinitionRef = d;
    }
}

bool PassDeclarationOrIdentifier::IsIdentifier() const
{
    return !m_Identifier.empty();
}

const std::string& PassDeclarationOrIdentifier::GetIdentifier() const
{
    assert( IsIdentifier() &&
            "Can't get the identifier of a PassDeclarationOrIdentifier without "
            "an identifier" );
    return m_Identifier;
}

const PassDeclaration& PassDeclarationOrIdentifier::GetDeclaration() const
{
    assert( m_Declaration &&
            "Can't get the declaration of a PassDeclarationOrIdentifier "
            "without a declaration" );
    return *m_Declaration;
}

PassDeclaration& PassDeclarationOrIdentifier::GetDeclaration()
{
    assert( m_Declaration &&
            "Can't get the declaration of a PassDeclarationOrIdentifier "
            "without a declaration" );
    return *m_Declaration;
}

bool PassDeclarationOrIdentifier::Parse(
                           Parser& parser,
                           std::unique_ptr<PassDeclarationOrIdentifier>& token )
{
    std::string identifier;
    // Try to parse an identifier into identifier
    if( parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
    {
        // This must be terminated by a semicolon
        if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
            return false;

        // Construct a PassDeclarationOrIdentifier with an identifier
        token.reset( new PassDeclarationOrIdentifier( std::move(identifier),
                                                      nullptr ) );
        return true;
    }
    // The lexer may be out of step
    CHECK_PARSER;

    // This must be a declaration
    std::unique_ptr<PassDeclaration> declaration;
    if( !parser.Expect<PassDeclaration>( declaration ) )
        return false;

    // Construct a PassDeclarationOrIdentifier with a declaration
    token.reset( new PassDeclarationOrIdentifier( "",
                                                  std::move(declaration) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
