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

#include <compiler/sema_analyzer.hpp>
#include <compiler/parser.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/token.hpp>
#include <compiler/tokens/state_assignment_statement.hpp>
#include <engine/pass.hpp>
#include <engine/state_assignment.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PassDefinition
//------------------------------------------------------------------------------

PassDefinition::PassDefinition( StateAssignStmtVector state_assignments )
    :Token( TokenTy::PassDefinition )
    ,m_stateAssignments( std::move(state_assignments) )
{
#ifndef NDEBUG
    for( const auto& s : m_stateAssignments )
        assert( s && "null StateAssignmentStatement given to PassDefinition" );
#endif
}

PassDefinition::~PassDefinition()
{
}

void PassDefinition::PerformSema( SemaAnalyzer& sema )
{
    for( auto& s : m_stateAssignments )
        s->PerformSema( sema );
}

void PassDefinition::Print( int depth ) const
{
    for( const auto& s: m_stateAssignments )
        s->Print( depth );
}

const PassDefinition::StateAssignStmtVector&
                                    PassDefinition::GetStateAssignments() const
{
    return m_stateAssignments;
}

bool PassDefinition::Parse( Parser& parser,
                            std::unique_ptr<PassDefinition>& token )
{
    // The definition starts with an open brace
    if( !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    // Parse some StateAssignmentStatements
    StateAssignStmtVector state_assignments;
    parser.ExpectSequenceOf<StateAssignmentStatement>( state_assignments );
    // The parser may have advanced the lexer too far
    CHECK_PARSER;

    // The definiton ends with a close brace
    if( !parser.ExpectTerminal( TerminalType::CLOSE_BRACE ) )
        return false;

    token.reset( new PassDefinition( std::move(state_assignments) ) );
    return true;
}

//------------------------------------------------------------------------------
// PassDeclarationOrIdentifier
//------------------------------------------------------------------------------

PassDeclarationOrIdentifier::PassDeclarationOrIdentifier(
        std::string                      identifier,
        std::unique_ptr<PassDeclaration> declaration )
    :Token( TokenTy::PassDeclarationOrIdentifier )
    ,m_identifier ( std::move(identifier)  )
    ,m_declaration( std::move(declaration) )
{
    // Assert if both are full, or none are
    assert( m_identifier.empty() == bool(m_declaration) &&
            "PassDeclarationOrIdentifier must have one and only one value" );
}

PassDeclarationOrIdentifier::~PassDeclarationOrIdentifier()
{
}

Pass PassDeclarationOrIdentifier::GeneratePass( CodeGenerator& code_gen ) const
{
    assert( m_definitionRef && "Trying to generate a pass with no definition" );
    assert( *m_definitionRef && "Trying to generate an undefined pass" );

    std::vector<std::unique_ptr<StateAssignmentBase> > state_assignments;
    for( const auto& s : (*m_definitionRef)->GetStateAssignments() )
        state_assignments.push_back( s->GenerateStateAssignment( code_gen ) );
    return Pass( IsIdentifier() ? m_identifier : m_declaration->GetName(),
                 std::move(state_assignments) );
}

void PassDeclarationOrIdentifier::Print( int depth ) const
{
    if( m_definitionRef &&
        *m_definitionRef )
    {
        (*m_definitionRef)->Print( depth );
    }
    else if( IsIdentifier() )
    {
        for( int i = 0; i < depth * 4; ++i )
            std::cout << " ";
        std::cout << m_identifier;
    }
    else
    {
        m_declaration->Print( depth );
    }
}

void PassDeclarationOrIdentifier::PerformSema( SemaAnalyzer& sema )
{
    if( m_declaration )
    {
        m_declaration->PerformSema( sema );
        m_definitionRef = sema.GetPass( m_declaration->GetName() );
    }
    else
    {
        SemaAnalyzer::PassDefinitionRef d = sema.GetPass( m_identifier );
        if( !d )
            sema.Error( "Use of undeclared pass: " + m_identifier );
        else
            m_definitionRef = d;
    }
}

bool PassDeclarationOrIdentifier::IsIdentifier() const
{
    return !m_identifier.empty();
}

const std::string& PassDeclarationOrIdentifier::GetIdentifier() const
{
    assert( IsIdentifier() &&
            "Can't get the identifier of a PassDeclarationOrIdentifier without "
            "an identifier" );
    return m_identifier;
}

const PassDeclaration& PassDeclarationOrIdentifier::GetDeclaration() const
{
    assert( m_declaration &&
            "Can't get the declaration of a PassDeclarationOrIdentifier "
            "without a declaration" );
    return *m_declaration;
}

PassDeclaration& PassDeclarationOrIdentifier::GetDeclaration()
{
    assert( m_declaration &&
            "Can't get the declaration of a PassDeclarationOrIdentifier "
            "without a declaration" );
    return *m_declaration;
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
