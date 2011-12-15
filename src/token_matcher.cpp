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

#include "token_matcher.hpp"

#include <algorithm>
#include <regex>
#include <string>

namespace JoeLang
{
namespace Lexer
{

TokenMatcher::TokenMatcher( TokenType token_type, bool is_significant )
    :m_tokenType( token_type )
    ,m_isSignificant( is_significant )
{
}

bool TokenMatcher::IsSignificant() const
{
    return m_isSignificant;
}

TokenType TokenMatcher::GetTokenType() const
{
    return m_tokenType;
}

LiteralTokenMatcher::LiteralTokenMatcher(TokenType token_type, std::string literal, bool is_significant)
    :LiteralTokenMatcher( token_type, literal, literal, is_significant )
{
}

LiteralTokenMatcher::LiteralTokenMatcher(TokenType token_type, std::string literal, std::string name, bool is_significant)
    :TokenMatcher( token_type, is_significant )
    ,m_literal( literal )
    ,m_name( name )
{
}

std::string LiteralTokenMatcher::Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const
{
    std::size_t string_size = string_end - string_begin;
    if( string_size >= m_literal.size() &&
        std::equal( m_literal.begin(), m_literal.end(), string_begin ) )
        return m_literal;
    return std::string();
}

KeywordTokenMatcher::KeywordTokenMatcher( TokenType token_type, std::string literal, bool is_significant )
    :KeywordTokenMatcher( token_type, literal, literal, is_significant )
{
}

KeywordTokenMatcher::KeywordTokenMatcher(TokenType token_type, std::string literal, std::string name, bool is_significant )
    :TokenMatcher( token_type, is_significant )
    ,m_literal( literal )
    ,m_name( name )
{
}

std::string KeywordTokenMatcher::Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const
{
    std::size_t string_size = string_end - string_begin;
    if( string_size >= m_literal.size() &&
        std::equal( m_literal.begin(), m_literal.end(), string_begin ) )
    {
        if( string_size > m_literal.size() )
        {
            char next_char = *( string_begin + m_literal.size() );
            if( ( next_char >= 'a' && next_char <= 'z' ) ||
                ( next_char >= 'A' && next_char <= 'Z' ) ||
                ( next_char >= '0' && next_char <= '9' ) ||
                ( next_char == '_' ) )
            {
                return std::string();
            }
        }
        return m_literal;
    }
    return std::string();
}

RegexTokenMatcher::RegexTokenMatcher( TokenType token_type, std::regex regex, std::string name, bool is_significant )
    :TokenMatcher( token_type, is_significant )
    ,m_regex( regex )
    ,m_name( name )
{
}

RegexTokenMatcher::RegexTokenMatcher(TokenType token_type, std::string regex_string, std::string name, bool is_significant)
    :RegexTokenMatcher( token_type, std::regex( regex_string ), name, is_significant )
{
}

std::string RegexTokenMatcher::Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const
{
    std::smatch match;
    if( std::regex_search( string_begin, string_end, match, m_regex, std::regex_constants::match_continuous ) )
    {
        return match[0];
    }
    return std::string();;
}


} // namespace Lexer
} // namespace JoeLang
