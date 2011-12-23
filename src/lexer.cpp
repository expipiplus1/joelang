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

#include "lexer.hpp"

#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace JoeLang
{
namespace Lexer
{

std::vector< std::unique_ptr< TokenMatcher > > Lexer::s_terminals;

Lexer::Lexer()
    :m_currentIndex( 0 )
{
    if( s_terminals.empty() )
    {
        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new WhitespaceTokenMatcher() ) );

        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new KeywordTokenMatcher( TokenType::PASS, "pass" ) ) );
        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new KeywordTokenMatcher( TokenType::TECHNIQUE, "technique" ) ) );

        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new IdentifierTokenMatcher() ) );

        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new LiteralTokenMatcher( TokenType::OPEN_BRACE, "{" ) ) );
        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new LiteralTokenMatcher( TokenType::CLOSE_BRACE, "}" ) ) );
        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new LiteralTokenMatcher( TokenType::SEMICOLON, ";" ) ) );
        s_terminals.push_back( std::unique_ptr< TokenMatcher >
            ( new LiteralTokenMatcher( TokenType::EQUALS, "=" ) ) );
    }
}

bool Lexer::Lex( const std::string& string )
{
    std::string::const_iterator curr_position = string.begin();
    std::string::const_iterator end_position = string.end();
    while( curr_position != end_position )
    {
        std::string match_string;
        bool match = false;
        for( const auto& token_type : s_terminals )
        {
            match_string = token_type->Match( curr_position, end_position );
            if( match_string.size() != 0 )
            {
                if( token_type->IsSignificant() )
                    m_tokenStream.push_back( std::make_pair( token_type->GetTokenType(), match_string ) );
                curr_position += match_string.size();
                match = true;
                break;
            }
        }
        if( !match )
        {
            m_tokenStream.clear();
            return false;
        }
    }
    m_tokenStream.push_back( std::make_pair( TokenType::END_OF_FILE, std::string() ) );
    m_currentIndex = 0;
    return true;
}

bool Lexer::TryConsume( TokenType token_type, std::pair< TokenType, std::string >& terminal )
{
    if( m_tokenStream[m_currentIndex].first == token_type )
    {
        terminal = m_tokenStream[m_currentIndex];
        ConsumeNext();
        return true;
    }
    return false;
}

bool Lexer::TryConsume( TokenType token_type )
{
    if( m_tokenStream[m_currentIndex].first == token_type )
    {
        ConsumeNext();
        return true;
    }
    return false;
}

void Lexer::ConsumeNext()
{
    //
    // Never advance over the end
    //
    if( m_currentIndex < m_tokenStream.size() - 1 )
        ++m_currentIndex;
}

} // namespace Lexer
} // namespace JoeLang
