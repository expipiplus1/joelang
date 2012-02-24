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

namespace JoeLang
{
namespace Lexer
{

//------------------------------------------------------------------------------
// Tables of terminals
// TODO: When Clang supports initializer lists put these in terminal_types.hpp
//------------------------------------------------------------------------------

//
// Ignored Sequence
//
static const FunctionalTerminal g_ignoredTerminals[] =
{
    { ReadWhitespace,   WHITESPACE,    "whitespace"    },
    { ReadLineComment,  LINE_COMMENT,  "line comment"  },
    { ReadBlockComment, BLOCK_COMMENT, "block comment" }
};

//
// Punctuation
//
static const LiteralTerminal g_punctuationTerminals[] =
{
    { "{",  OPEN_BRACE,     "" },
    { "}",  CLOSE_BRACE,    "" },
    { "(",  OPEN_ROUND,     "" },
    { ")",  CLOSE_ROUND,    "" },
    { "<",  OPEN_ANGLED,    "" },
    { ">",  CLOSE_ANGLED,   "" },
    { "[",  OPEN_SQUARE,    "" },
    { "]",  CLOSE_SQUARE,   "" },

    { "==", EQUALITY,       "" },
    { "!=", NOT_EQUALITY,   "" },
    { "<=", LESS_THAN_EQUALS,    "" },
    { ">=", GREATER_THAN_EQUALS, "" },
    { "<",  LESS_THAN,      "" },
    { ">",  GREATER_THAN,   "" },
    // TODO:

    { "=",  EQUALS,          "" },
    { "+=", PLUS_EQUALS,     "" },
    { "-=", MINUS_EQUALS,    "" },
    { "*=", MULTIPLY_EQUALS, "" },
    { "/=", DIVIDE_EQUALS,   "" },
    { "%=", MODULO_EQUALS,   "" },
    { "&=", AND_EQUALS,      "" },
    { "|=", INCLUSIVE_OR_EQUALS, "" },
    { "^=", EXCLUSIVE_OR_EQUALS, "" },
    { "<<=", LEFT_SHIFT_EQUALS,  "" },
    { ">>=", RIGHT_SHIFT_EQUALS, "" },

    { "&&", LOGICAL_AND,    "" },
    { "||", LOGICAL_OR,     "" },
    { "!",  LOGICAL_NOT,    "" },

    { "++", INCREMENT,      "" },
    { "--", DECREMENT,      "" },
    { "+",  PLUS,           "" },
    { "-",  MINUS,          "" },
    { "*",  MULTIPLY,       "" },
    { "/",  DIVIDE,         "" },
    { "%",  MODULO,         "" },
    { "&",  AND,            "" },
    { "|",  INCLUSIVE_OR,   "" },
    { "^",  EXCLUSIVE_OR,   "" },
    { "~",  BITWISE_NOT,    "" },
    { "<<", LEFT_SHIFT,     "" },
    { ">>", RIGHT_SHIFT,    "" },

    { ";",  SEMICOLON,      "" },
    { ",",  COMMA,          "" },
    { ".",  PERIOD,         "" },
    { ":",  COLON,          "" },
    { "?",  QUERY,          "" }
};

//
// Literals
//
static const FunctionalTerminal g_literalTerminals[] =
{
    { ReadIntegerLiteral,   INTEGER_LITERAL,    "integer literal"   },
    { ReadFloatingLiteral,  FLOATING_LITERAL,   "floating literal"  },
    //{ ReadCharacterLiteral, CHARACTER_LITERAL,  "character literal" },
    //{ ReadStringLiteral,    STRING_LITERAL,     "string literal"    }
};

//
// Keywords
//
static const LiteralTerminal g_keywordTerminals[] =
{
    { "technique",  TECHNIQUE,  "" },
    { "pass",       PASS,       "" },

    { "int",        TYPE_INT,   "" },

    { "true",       TRUE,   "" },
    { "false",      FALSE,   "" }
};

//
// Identifier
//
static const FunctionalTerminal g_identifierTerminals[] =
{
    //{ ReadIdentifier,   IDENTIFIER, "identifier" }
};

const std::string& GetTerminalString( TerminalType terminal_type )
{
    for( const auto& i : g_ignoredTerminals )
        if( i.terminal_type == terminal_type )
            return i.readable_string;

    for( const auto& i : g_punctuationTerminals )
         if( i.terminal_type == terminal_type )
         {
            if( i.readable_string.empty() )
                return i.matched_string;
            else
                return i.readable_string;
         }

    for( const auto& i : g_literalTerminals )
        if( i.terminal_type == terminal_type )
            return i.readable_string;

    for( const auto& i : g_keywordTerminals )
         if( i.terminal_type == terminal_type )
         {
            if( i.readable_string.empty() )
                return i.matched_string;
            else
                return i.readable_string;
         }

    const static std::string s = "Invalid Terminal";
    const static std::string i = "identifier";
    const static std::string e = "EOF";

    if( terminal_type == IDENTIFIER )
        return i;
    else if ( terminal_type == END_OF_INPUT )
        return e;

    return s;
}

//------------------------------------------------------------------------------
// Terminal
//------------------------------------------------------------------------------

TerminalPosition::TerminalPosition( TerminalType terminal_type,
                    std::string::const_iterator begin,
                    std::string::const_iterator end )
    :terminal_type(terminal_type)
    ,begin(begin)
    ,end(end)
{}

//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

std::map< TerminalType, const LiteralTerminal* > Lexer::s_punctuationTerminalMap;
std::map< TerminalType, const FunctionalTerminal* > Lexer::s_literalTerminalMap;
std::map< TerminalType, const LiteralTerminal* > Lexer::s_keywordTerminalMap;

Lexer::Lexer( std::string string )
    :m_string( std::move( string ) )
{
    if( s_punctuationTerminalMap.empty() )
        for( const LiteralTerminal& terminal : g_punctuationTerminals )
            s_punctuationTerminalMap.insert( std::make_pair( terminal.terminal_type, &terminal ) );

    if( s_literalTerminalMap.empty() )
        for( const FunctionalTerminal& terminal : g_literalTerminals )
            s_literalTerminalMap.insert( std::make_pair( terminal.terminal_type, &terminal ) );

    if( s_keywordTerminalMap.empty() )
        for( const LiteralTerminal& terminal : g_keywordTerminals )
            s_keywordTerminalMap.insert( std::make_pair( terminal.terminal_type, &terminal ) );

    m_position = m_string.begin();
    ConsumeIgnoredTerminals();
}

bool Lexer::Expect( TerminalType terminal_type, std::string& string )
{
    if( terminal_type == END_OF_INPUT &&
        m_position == m_string.end() )
        return true;

    std::size_t chars_read;
    std::map< TerminalType, const LiteralTerminal* >::const_iterator literal_iterator;
    std::map< TerminalType, const FunctionalTerminal* >::const_iterator functional_iterator;

    // If this is a punctuation token
    literal_iterator = s_punctuationTerminalMap.find( terminal_type );
    if( literal_iterator != s_punctuationTerminalMap.end() )
    {
        const LiteralTerminal& terminal = *literal_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // Check that this terminal isn't also matched by something longer
        for( const LiteralTerminal& contending_terminal : g_punctuationTerminals )
            if( contending_terminal.matched_string.size() > terminal.matched_string.size() &&
                contending_terminal.Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        m_position += chars_read;
        ConsumeIgnoredTerminals();
        return true;
    }

    // If this is a literal token
    functional_iterator = s_literalTerminalMap.find( terminal_type );
    if( functional_iterator != s_literalTerminalMap.end() )
    {
        const FunctionalTerminal& terminal = *functional_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // if it's an integer literal, we need to check that it's not just
        // parsing the first part of a float

        if( terminal_type == INTEGER_LITERAL )
            if( s_literalTerminalMap[FLOATING_LITERAL]->Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        m_position += chars_read;
        ConsumeIgnoredTerminals();
        return true;
    }

    // If this is a keyword
    literal_iterator = s_keywordTerminalMap.find( terminal_type );
    if( literal_iterator != s_keywordTerminalMap.end() )
    {
        const LiteralTerminal& terminal = *literal_iterator->second;
        chars_read = terminal.Read( m_position, m_string.end() );
        if( !chars_read )
            return false;

        // Check that this terminal isn't also matched by something longer
        for( const LiteralTerminal& contending_terminal : g_keywordTerminals )
            if( contending_terminal.matched_string.size() > terminal.matched_string.size() &&
                contending_terminal.Read( m_position, m_string.end() ) )
                return false;

        string = std::string( m_position, m_position + chars_read );
        m_position += chars_read;
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

        for( const LiteralTerminal& terminal_reader : g_keywordTerminals )
            if( std::size_t( word_end - m_position ) == terminal_reader.matched_string.size() &&
                std::equal( m_position, word_end, terminal_reader.matched_string.begin() ) )
                return false;

        string = std::string( m_position, word_end );
        m_position = word_end;
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

void Lexer::ConsumeIgnoredTerminals()
{
    int chars_read;
    do
    {
        for( const FunctionalTerminal& terminal_reader : g_ignoredTerminals )
        {
            chars_read = terminal_reader.Read( m_position, m_string.end() );
            if( chars_read )
                break;
        }
        m_position += chars_read;
    } while( chars_read );
}

} // namespace Lexer
} // namespace JoeLang
