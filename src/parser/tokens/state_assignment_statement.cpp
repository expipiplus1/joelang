/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "state_assignment_statement.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <engine/state_assignment.hpp>
#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>
#include <parser/tokens/expression.hpp>

namespace JoeLang
{
namespace Parser
{

StateAssignmentStatement::StateAssignmentStatement( std::string state_name, std::unique_ptr< Expression > expression )
    :m_stateName( std::move( state_name ) )
    ,m_expression( std::move( expression ) )
{
}

StateAssignmentStatement::~StateAssignmentStatement()
{
}

StateAssignment StateAssignmentStatement::GetStateAssignment() const
{
    //TODO
    return StateAssignment();
}

void StateAssignmentStatement::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "State Assignment to " << m_stateName << "\n";
    m_expression->Print( depth + 1 );
}

bool StateAssignmentStatement::Parse( Parser& parser, std::unique_ptr<StateAssignmentStatement>& token )
{
    std::string state_name;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, state_name ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::EQUALS ) )
        return false;

    std::unique_ptr< Expression > expression;
    if( !Expect< Expression >( parser, expression ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::SEMICOLON ) )
        return false;

    token.reset( new StateAssignmentStatement( std::move( state_name ),
                                               std::move( expression ) ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
