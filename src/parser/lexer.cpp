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

#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <parser/terminal_types.hpp>

namespace JoeLang
{
namespace Lexer
{


//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

Lexer::Lexer( std::string string )
    :m_string( std::move( string ) )
{
    m_position = m_string.begin();
    ConsumeIgnoredTerminals();
}

bool Lexer::Expect( TerminalType terminal_type, std::string& string )
{
    if( terminal_type == TerminalType::END_OF_INPUT &&
        m_position == m_string.end() )
        return true;

    std::size_t chars_read;
    std::map< TerminalType, LiteralTerminal >::const_iterator literal_iterator;
    std::map< TerminalType, FunctionalTerminal >::const_iterator functional_iterator;

    // If this is a punctuation token
    literal_iterator = g_punctuationTerminals.find( terminal_type );
    if( literal_iterator != g_punctuationTerminals.end() )
    {
        const LiteralTerminal& terminal = literal_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // Check that this terminal isn't also matched by something longer
        for( const auto& contending_terminal : g_punctuationTerminals )
            if( contending_terminal.second.matched_string.size() > terminal.matched_string.size() &&
                contending_terminal.second.Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        ReadChars( chars_read );
        ConsumeIgnoredTerminals();
        return true;
    }

    // If this is a literal token
    functional_iterator = g_literalTerminals.find( terminal_type );
    if( functional_iterator != g_literalTerminals.end() )
    {
        const FunctionalTerminal& terminal = functional_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // if it's an integer literal, we need to check that it's not just
        // parsing the first part of a float

        if( terminal_type == INTEGER_LITERAL )
            if( g_literalTerminals.at(FLOATING_LITERAL).Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        ReadChars( chars_read );
        ConsumeIgnoredTerminals();
        return true;
    }

    // If this is a keyword
    literal_iterator = g_keywordTerminals.find( terminal_type );
    if( literal_iterator != g_keywordTerminals.end() )
    {
        const LiteralTerminal& terminal = literal_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // Check that this terminal isn't also matched by something longer
        for( const auto& contending_terminal : g_keywordTerminals )
            if( contending_terminal.second.matched_string.size() > terminal.matched_string.size() &&
                contending_terminal.second.Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        ReadChars( chars_read );
        ConsumeIgnoredTerminals();
        return true;
    }

    // If it's an identifier
    if( terminal_type == IDENTIFIER )
    {
        if( !IsNonDigit( *m_position ) )
            return false;

        std::string::const_iterator word_end = m_position + 1;
        while( IsDigitOrNonDigit( *word_end ) )
            ++word_end;

        for( const auto& terminal_reader : g_keywordTerminals )
            if( std::size_t( word_end - m_position ) == terminal_reader.second.matched_string.size() &&
                std::equal( m_position, word_end, terminal_reader.second.matched_string.begin() ) )
                return false;

        string = std::string( m_position, word_end );
        ReadChars( word_end - m_position );
        ConsumeIgnoredTerminals();
        return true;
    }

    // If it's anything else
    return false;
}

std::size_t Lexer::GetPosition() const
{
    return m_position - m_string.begin();
}

std::size_t Lexer::GetColumnNumber() const
{
    return m_columnNumber;
}

std::size_t Lexer::GetLineNumber() const
{
    return m_lineNumber;
}

void Lexer::ConsumeIgnoredTerminals()
{
    int chars_read;
    do
    {
        for( const auto& terminal_reader : g_ignoredTerminals )
        {
            chars_read = terminal_reader.second.Read( m_position, m_string.end() );
            if( chars_read )
                break;
        }
        ReadChars( chars_read );
    } while( chars_read );
}

void Lexer::ReadChars( std::size_t num_chars )
{
    std::string::const_iterator end = m_position + num_chars;
    while( m_position < end )
    {
        if( *m_position == '\n' )
        {
            ++m_lineNumber;
            m_columnNumber = 0;
        }
        ++m_columnNumber;
        ++m_position;
    }
}

} // namespace Lexer
} // namespace JoeLang
