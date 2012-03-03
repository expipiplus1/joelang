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
#include <parser/tokens/definition.hpp>
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
    if( !ExpectAnyOf< TechniqueDeclaration,
                      PassDeclaration,
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

PassDeclaration::PassDeclaration( std::string name, std::shared_ptr<PassDefinition> definition )
    :m_name( std::move( name ) )
    ,m_definition( std::move( definition ) )
{
}

PassDeclaration::~PassDeclaration()
{
}

void PassDeclaration::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Pass: " << ( m_name.size() ? m_name : "Unnamed" ) << "\n";
    if( m_definition )
    {
        m_definition->Print( depth + 1);
    }
    else
    {
        for( int i = 0; i < depth * 4; ++i )
            std::cout << " ";
        std::cout << "No definition\n";
    }
}

bool PassDeclaration::Parse( Parser& parser, std::unique_ptr<PassDeclaration>& token )
{
    if( !parser.ExpectTerminal( Lexer::PASS ) )
        return false;

    std::string name;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, name ) )
        return false;

    if( parser.ExpectTerminal( Lexer::SEMICOLON ) )
    {
        //
        // This is a regular declaration
        // TODO: Look up the name in a table and create links and things
        //
        token.reset( new PassDeclaration( name, nullptr ) );
        return true;
    }
    CHECK_PARSER;

    //
    // This declaration is also has a definition
    //
    std::unique_ptr< PassDefinition > definition;

    if( !Expect<PassDefinition>( parser, definition ) )
        return false;

    token.reset( new PassDeclaration( std::move(name),
                                      std::move(definition) ) );
    return true;
}

//------------------------------------------------------------------------------
// TechniqueDeclaration
//------------------------------------------------------------------------------

TechniqueDeclaration::TechniqueDeclaration( std::string name, std::shared_ptr<TechniqueDefinition> definition )
    :m_name( std::move( name ) )
    ,m_definition( std::move( definition ) )
{
}

TechniqueDeclaration::~TechniqueDeclaration()
{
}

void TechniqueDeclaration::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Technique: " << ( m_name.size() ? m_name : "Unnamed" ) << "\n";
    if( m_definition )
    {
        m_definition->Print( depth + 1);
    }
    else
    {
        for( int i = 0; i < depth * 4 + 4; ++i )
            std::cout << " ";
        std::cout << "No definition\n";
    }
}


bool TechniqueDeclaration::Parse( Parser& parser, std::unique_ptr<TechniqueDeclaration>& token )
{
    if( !parser.ExpectTerminal( Lexer::TECHNIQUE ) )
        return false;

    std::string name;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, name ) )
        return false;

    if( parser.ExpectTerminal( Lexer::SEMICOLON ) )
    {
        //
        // This is a regular declaration
        // TODO: Look up the name in a table and create links and things
        //
        token.reset( new TechniqueDeclaration( name, nullptr ) );
        return true;
    }
    CHECK_PARSER;

    //
    // This declaration is also a definition
    //
    std::unique_ptr< TechniqueDefinition > definition;

    if( !Expect<TechniqueDefinition>( parser, definition ) )
        return false;

    token.reset( new TechniqueDeclaration( std::move(name),
                                           std::move(definition) ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
