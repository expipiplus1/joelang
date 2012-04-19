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

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PassDefinition
//------------------------------------------------------------------------------

PassDefinition::PassDefinition( StateAssignStmtVector state_assignments )
    :m_stateAssignments( std::move(state_assignments) )
{
    for( const auto& s : m_stateAssignments )
        assert( s && "null StateAssignmentStatement given to PassDefinition" );
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

} // namespace Compiler
} // namespace JoeLang
