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

#include "parser.hpp"

#include <iostream>
#include <string>

#include <parser/lexer.hpp>
#include <parser/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Parser
{

void Parser::Print() const
{
    if( m_translationUnit )
        m_translationUnit->Print();
}

bool Parser::Parse ( const std::string& string )
{
    m_lexer.reset( new Lexer::Lexer( string ) );

    if( TranslationUnit::Parse( *this, m_translationUnit ) )
        return true;

    std::cout << "Error parsing: expected one of: ";
    for( Lexer::TerminalType expected_terminal : m_expectedTerminals )
    {
        std::cout << "\"" << Lexer::GetTerminalString( expected_terminal ) << "\", ";
    }
    std::cout << "\n";
    return false;
}

bool Parser::ExpectTerminal( Lexer::TerminalType terminal_type )
{
    // TODO: Remove dummy
    std::string dummy;
    return ExpectTerminal( terminal_type, dummy );
}

bool Parser::ExpectTerminal( Lexer::TerminalType terminal_type, std::string& string )
{
    if( m_lexer->Expect( terminal_type, string ) )
    {
        m_expectedTerminals.clear();
        return true;
    }

    m_expectedTerminals.insert( terminal_type );
    return false;
}

std::size_t Parser::GetLexerPosition() const
{
    return m_lexer->GetPosition();
}

void Parser::Error()
{
    m_good = false;
}

bool Parser::Good() const
{
    return m_good;
}

} // namespace Parser
} // namespace JoeLang
