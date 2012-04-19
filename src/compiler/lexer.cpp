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

#include "lexer.hpp"

#include <cassert>
#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <utility>

#include <compiler/terminal_types.hpp>

namespace JoeLang
{
namespace Compiler
{


//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

Lexer::Lexer( std::string string )
    :m_string( std::move( string ) )
    ,m_position( m_string.begin() )
{
    ConsumeIgnoredTerminals();
}

TerminalType Lexer::ReadPunctuationTerminal( TerminalType terminal,
                                             std::string& string ) const
{
    // Find this terminal type in the punctuation terminal map
    std::map<TerminalType, LiteralTerminal>::const_iterator literal_iterator =
        g_punctuationTerminals.find( terminal );

    // If it wasn't in the map, return UNKNOWN_CHARACTER
    if( literal_iterator == g_punctuationTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    // Try and read this terminal
    const LiteralTerminal& terminal_reader = literal_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );

    // If it didn't match, return UNKNOWN_CHARACTER
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // Check that this terminal isn't also matched by something longer
    for( const auto& c: g_punctuationTerminals )
        if( c.second.matched_string.size() >
                                        terminal_reader.matched_string.size() &&
            c.second.Read( m_position, m_string.end() ) )
            return c.first;

    // Fill up string
    string = std::string( m_position, m_position + chars_read );
    return terminal;
}

TerminalType Lexer::ReadLiteralTerminal( TerminalType terminal,
                                         std::string& string ) const
{
    // Find this terminal type in the literal terminal map
    std::map<TerminalType, FunctionalTerminal>::const_iterator
                        functional_iterator = g_literalTerminals.find(terminal);

    // If it wasn't in the map, return UNKNOWN_CHARACTER
    if( functional_iterator == g_literalTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    // Try and match this terminal
    const FunctionalTerminal& terminal_reader = functional_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );

    // if it didn't match return
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // fill up string
    string = std::string( m_position, m_position + chars_read );
    return terminal;
}

TerminalType Lexer::ReadKeywordTerminal( TerminalType terminal,
                                         std::string& string ) const
{
    // Find this terminal in the punctuation terminal map
    std::map<TerminalType, LiteralTerminal>::const_iterator literal_iterator =
        g_keywordTerminals.find(terminal);

    // If it wasn't in the map return
    if( literal_iterator == g_keywordTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    // Try and read this terminal
    const LiteralTerminal& terminal_reader = literal_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );

    // If it didn't match return
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // Check that this terminal isn't also matched by something longer
    for( const auto& c: g_keywordTerminals )
        if( c.second.matched_string.size() >
                                        terminal_reader.matched_string.size() &&
            c.second.Read( m_position, m_string.end() ) )
            return c.first;

    // fill up the string
    string = std::string( m_position, m_position + chars_read );
    return terminal;
}

bool Lexer::Expect( TerminalType terminal_type, std::string& string )
{
    // If we've reached the end of the string
    if( terminal_type == TerminalType::END_OF_INPUT &&
        m_position == m_string.end() )
        return true;

    TerminalType t;

    // See if it's a punctuation terminal
    t = ReadPunctuationTerminal( terminal_type, string );

    // If it wasn't matched by the punctuation try and match it as a literal
    if( t == TerminalType::UNKNOWN_CHARACTER )
        t = ReadLiteralTerminal( terminal_type, string );

    // If it wasn't matched try and match a keyword
    if( t == TerminalType::UNKNOWN_CHARACTER )
        t = ReadKeywordTerminal( terminal_type, string );

    // If we've found a map, advance the position and clean up any whitespace
    if( t == terminal_type )
    {
        ReadChars( string.size() );
        ConsumeIgnoredTerminals();
        return true;
    }
    return false;
}

TerminalType Lexer::PeekNextTerminal( std::string& string ) const
{
    // Try and match any punctuation terminals
    for( const auto& t_reader : g_punctuationTerminals )
    {
        TerminalType t = ReadPunctuationTerminal( t_reader.first, string );
        if( t != TerminalType::UNKNOWN_CHARACTER )
            return t;
    }

    // Try and match any literal terminals
    for( const auto& t_reader : g_literalTerminals )
    {
        TerminalType t = ReadLiteralTerminal( t_reader.first, string );
        if( t != TerminalType::UNKNOWN_CHARACTER )
            return t;
    }

    // Try and match any keyword terminals
    for( const auto& t_reader : g_keywordTerminals )
    {
        TerminalType t = ReadPunctuationTerminal( t_reader.first, string );
        if( t != TerminalType::UNKNOWN_CHARACTER )
            return t;
    }

    // If it's anything else
    return TerminalType::UNKNOWN_CHARACTER;
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
    int chars_read = 0;
    // While we keep having eaten something, try and parse the next terminal
    do
    {
        for( const auto& terminal_reader : g_ignoredTerminals )
        {
            chars_read = terminal_reader.second.Read( m_position,
                                                      m_string.end() );
            if( chars_read )
                break;
        }
        ReadChars( chars_read );
    } while( chars_read );
}

void Lexer::ReadChars( std::size_t num_chars )
{
    std::string::const_iterator end = m_position + num_chars;
    assert( end <= m_string.end() &&
            "Trying to advance the lexer past the end of the file" );
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

} // namespace Compiler
} // namespace JoeLang
