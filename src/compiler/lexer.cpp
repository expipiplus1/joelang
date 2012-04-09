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

TerminalType Lexer::ReadPunctuationTerminal( TerminalType terminal, std::string& string )
{
    std::map< TerminalType, LiteralTerminal >::const_iterator literal_iterator =
        g_punctuationTerminals.find( terminal );

    if( literal_iterator == g_punctuationTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    const LiteralTerminal& terminal_reader = literal_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // Check that this terminal isn't also matched by something longer
    for( const auto& c: g_punctuationTerminals )
        if( c.second.matched_string.size() > terminal_reader.matched_string.size() &&
            c.second.Read( m_position, m_string.end() ) )
            return c.first;

    string = std::string( m_position, m_position + chars_read );
    ReadChars( chars_read );
    ConsumeIgnoredTerminals();
    return terminal;
}

TerminalType Lexer::ReadLiteralTerminal( TerminalType terminal, std::string& string )
{
    std::map< TerminalType, FunctionalTerminal >::const_iterator functional_iterator =
        g_literalTerminals.find( terminal);
    if( functional_iterator == g_literalTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    const FunctionalTerminal& terminal_reader = functional_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // if it's an integer literal, we need to check that it's not just
    // parsing the first part of a float
    //if( terminal_type == TerminalType::INTEGER_LITERAL )
     //   if( g_literalTerminals.at(TerminalType::FLOATING_LITERAL).Read( m_position, m_string.end() ) )
      //      return false;

    string = std::string( m_position, m_position + chars_read );
    ReadChars( chars_read );
    ConsumeIgnoredTerminals();
    return terminal;
}

TerminalType Lexer::ReadKeywordTerminal( TerminalType terminal, std::string& string )
{
    std::map< TerminalType, LiteralTerminal >::const_iterator literal_iterator =
        g_keywordTerminals.find( terminal );

    if( literal_iterator == g_keywordTerminals.end() )
        return TerminalType::UNKNOWN_CHARACTER;

    const LiteralTerminal& terminal_reader = literal_iterator->second;
    std::size_t chars_read = terminal_reader.Read( m_position, m_string.end() );
    if( !chars_read )
        return TerminalType::UNKNOWN_CHARACTER;

    // Check that this terminal isn't also matched by something longer
    for( const auto& c: g_keywordTerminals )
        if( c.second.matched_string.size() > terminal_reader.matched_string.size() &&
            c.second.Read( m_position, m_string.end() ) )
            return c.first;

    string = std::string( m_position, m_position + chars_read );
    ReadChars( chars_read );
    ConsumeIgnoredTerminals();
    return terminal;
}

bool Lexer::Expect( TerminalType terminal_type, std::string& string )
{
    if( terminal_type == TerminalType::END_OF_INPUT &&
        m_position == m_string.end() )
        return true;

    TerminalType t;

    t = ReadPunctuationTerminal( terminal_type, string );
    if( t != TerminalType::UNKNOWN_CHARACTER )
        return t == terminal_type;

    t = ReadLiteralTerminal( terminal_type, string );
    if( t != TerminalType::UNKNOWN_CHARACTER )
        return t == terminal_type;

    t = ReadKeywordTerminal( terminal_type, string );
    if( t != TerminalType::UNKNOWN_CHARACTER )
        return t == terminal_type;

    // If it's anything else
    return false;
}

TerminalType Lexer::GetNextTerminal( std::string& string )
{
    for( const auto& t_reader : g_punctuationTerminals )
    {
        TerminalType t = ReadPunctuationTerminal( t_reader.first, string );
        if( t != TerminalType::UNKNOWN_CHARACTER )
            return t;
    }

    for( const auto& t_reader : g_literalTerminals )
    {
        TerminalType t = ReadLiteralTerminal( t_reader.first, string );
        if( t != TerminalType::UNKNOWN_CHARACTER )
            return t;
    }

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

} // namespace Compiler
} // namespace JoeLang
