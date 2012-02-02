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

#include "declaration.hpp"

#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>
#include <parser/tokens/state_assignment.hpp>
#include <parser/tokens/token.hpp>

namespace JoeLang
{
namespace Parser
{

//------------------------------------------------------------------------------
// DeclarationBase
//------------------------------------------------------------------------------

DeclarationBase::~DeclarationBase()
{
}

bool DeclarationBase::Parse( Parser& parser, std::unique_ptr<DeclarationBase>& token )
{
    std::unique_ptr<Token> t;
    if( !ExpectAnyOf< TechniqueDefinition,
                      PassDefinition,
                      EmptyDeclaration>( parser, t ) )
        return false;

    Token* p = t.release();
    token.reset( dynamic_cast<DeclarationBase*>( p ) );
    if( !token )
    {
        delete p;
        return false;
    }
    return true;
}

//------------------------------------------------------------------------------
// EmptyDeclaration
//------------------------------------------------------------------------------


EmptyDeclaration::EmptyDeclaration()
{
}

EmptyDeclaration::~EmptyDeclaration()
{
}

void EmptyDeclaration::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Empty Declaration\n";
}

bool EmptyDeclaration::Parse( Parser& parser, std::unique_ptr<EmptyDeclaration>& token )
{
    if( !parser.ExpectTerminal( Lexer::SEMICOLON ) )
        return false;

    token.reset( new EmptyDeclaration );
    return true;
}

//------------------------------------------------------------------------------
// PassDeclaration
//------------------------------------------------------------------------------

PassDefinition::PassDefinition( std::string name, std::vector< std::unique_ptr<StateAssignment> > state_assignments )
    :m_name( std::move( name ) )
    ,m_stateAssignments( std::move( state_assignments ) )
{
}

PassDefinition::~PassDefinition()
{
}

void PassDefinition::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Pass: " << ( m_name.size() ? m_name : "Unnamed" ) << "\n";
    for( const auto& state_assignment : m_stateAssignments )
        state_assignment->Print( depth + 1 );
}

bool PassDefinition::Parse( Parser& parser, std::unique_ptr<PassDefinition>& token )
{
    if( !parser.ExpectTerminal( Lexer::PASS ) )
        return false;

    std::string name;
    parser.ExpectTerminal( Lexer::IDENTIFIER, name );

    if( !parser.ExpectTerminal( Lexer::OPEN_BRACE ) )
        return false;

    std::vector< std::unique_ptr<StateAssignment> > state_assignments;
    ExpectSequenceOf<StateAssignment>( parser, state_assignments );


    if( !parser.ExpectTerminal( Lexer::CLOSE_BRACE ) )
        return false;

    token.reset( new PassDefinition( name, std::move( state_assignments ) ) );
    return true;
}


TechniqueDefinition::TechniqueDefinition( std::string name, std::vector< std::unique_ptr<PassDefinition> > passes )
    :m_name( std::move( name ) )
    ,m_passes( std::move( passes ) )
{
}

TechniqueDefinition::~TechniqueDefinition()
{
}

void TechniqueDefinition::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Technique: " << ( m_name.size() ? m_name : "Unnamed" ) << "\n";

    for( const auto& pass : m_passes )
        pass->Print( depth + 1 );
}


bool TechniqueDefinition::Parse( Parser& parser, std::unique_ptr<TechniqueDefinition>& token )
{
    if( !parser.ExpectTerminal( Lexer::TECHNIQUE ) )
        return false;

    std::string name;
    parser.ExpectTerminal( Lexer::IDENTIFIER, name );

    if( !parser.ExpectTerminal( Lexer::OPEN_BRACE ) )
        return false;

    std::vector< std::unique_ptr<PassDefinition> > passes;
    ExpectSequenceOf<PassDefinition>( parser, passes );

    if( !parser.ExpectTerminal( Lexer::CLOSE_BRACE ) )
        return false;

    token.reset( new TechniqueDefinition( name, std::move( passes ) ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
