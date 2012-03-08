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

#include "definition.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <engine/pass.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>
#include <parser/tokens/declaration.hpp>
#include <parser/tokens/token.hpp>
#include <parser/tokens/state_assignment_statement.hpp>

namespace JoeLang
{
namespace Parser
{

//------------------------------------------------------------------------------
// PassDefinition
//------------------------------------------------------------------------------

PassDefinition::PassDefinition( std::vector< std::unique_ptr<StateAssignmentStatement> > state_assignments )
    :m_stateAssignments( std::move( state_assignments ) )
{
}

PassDefinition::~PassDefinition()
{
}

Pass PassDefinition::GetPass() const
{
    //TODO
    std::vector<StateAssignment> state_assignments;
    for( const auto& state_assignment : m_stateAssignments )
        state_assignments.push_back( state_assignment->GetStateAssignment() );
    return Pass( std::move( state_assignments ) );

}

void PassDefinition::Print( int depth ) const
{
    for( const auto& state_assignment : m_stateAssignments )
        state_assignment->Print( depth );
}

bool PassDefinition::Parse( Parser& parser, std::unique_ptr<PassDefinition>& token )
{
    if( !parser.ExpectTerminal( Lexer::OPEN_BRACE ) )
        return false;

    std::vector< std::unique_ptr<StateAssignmentStatement> > state_assignments;
    ExpectSequenceOf<StateAssignmentStatement>( parser, state_assignments );
    CHECK_PARSER;

    if( !parser.ExpectTerminal( Lexer::CLOSE_BRACE ) )
        return false;

    token.reset( new PassDefinition( std::move( state_assignments ) ) );
    return true;
}

//------------------------------------------------------------------------------
// TechniqueDefinition
//------------------------------------------------------------------------------

TechniqueDefinition::TechniqueDefinition( std::vector< std::unique_ptr<PassDeclaration> > passes )
    :m_passes( std::move( passes ) )
{
}

TechniqueDefinition::~TechniqueDefinition()
{
}

Technique TechniqueDefinition::GetTechnique() const
{
    //TODO
    std::vector<Pass> passes;
    for( const auto& pass : m_passes )
    {
        const std::shared_ptr<PassDefinition>& definition = pass->GetDefinition();
        // assert on null?
        if( definition )
            passes.push_back( definition->GetPass() );
    }
    return Technique( std::move( passes ) );
}

void TechniqueDefinition::Print( int depth ) const
{
    for( const auto& pass : m_passes )
        pass->Print( depth );
}


bool TechniqueDefinition::Parse( Parser& parser, std::unique_ptr<TechniqueDefinition>& token )
{
    if( !parser.ExpectTerminal( Lexer::OPEN_BRACE ) )
        return false;

    std::vector< std::unique_ptr<PassDeclaration> > passes;
    ExpectSequenceOf<PassDeclaration>( parser, passes );
    CHECK_PARSER;

    if( !parser.ExpectTerminal( Lexer::CLOSE_BRACE ) )
        return false;

    token.reset( new TechniqueDefinition( std::move( passes ) ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
