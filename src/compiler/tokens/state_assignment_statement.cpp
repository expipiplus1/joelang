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

#include "state_assignment_statement.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// StateAssignmentStatement
//------------------------------------------------------------------------------
StateAssignmentStatement::StateAssignmentStatement(
                                    std::string identifier,
                                    std::unique_ptr< Expression > expression )
    :m_identifier( std::move(identifier) )
    ,m_expression( std::move(expression) )
{
    assert( !m_identifier.empty() &&
            "Trying to create a StateAssignmentStatement with an empty state "
            "name" );
    assert( m_expression &&
            "Trying to create a StateAssignmentStatement with a null "
            "expression" );
}

StateAssignmentStatement::~StateAssignmentStatement()
{
}

void StateAssignmentStatement::PerformSema( SemaAnalyzer& sema )
{
    sema.EnterScope();

    const StateBase* state = sema.GetState( m_identifier );
    if( !state )
        sema.Error( "Undeclared state: " + m_identifier );
    else
        sema.LoadStateEnumerants( *state );

    // TODO cast to state type here
    m_expression->PerformSema( sema );

    sema.LeaveScope();
}

void StateAssignmentStatement::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "State Assignment to " << m_identifier << "\n";
    m_expression->Print( depth + 1 );
}

bool StateAssignmentStatement::Parse(
                              Parser& parser,
                              std::unique_ptr<StateAssignmentStatement>& token )
{
    // Read the state identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    // The assignment operator
    if( !parser.ExpectTerminal( TerminalType::EQUALS ) )
        return false;

    // And the expression
    std::unique_ptr< Expression > expression;
    if( !parser.Expect< Expression >( expression ) )
        return false;

    // Closing semicolon
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new StateAssignmentStatement( std::move(identifier),
                                               std::move(expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
