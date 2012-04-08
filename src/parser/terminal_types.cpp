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

#include "terminal_types.hpp"

#include <algorithm>
#include <cassert>
#include <functional>
#include <map>
#include <string>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Tables of terminals
//------------------------------------------------------------------------------

//
// Ignored Sequence
//
const std::map<TerminalType, FunctionalTerminal> g_ignoredTerminals =
{
    { TerminalType::WHITESPACE,    { ReadWhitespace,   "whitespace"    } },
    { TerminalType::LINE_COMMENT,  { ReadLineComment,  "line comment"  } },
    { TerminalType::BLOCK_COMMENT, { ReadBlockComment, "block comment" } }
};

//
// Punctuation
//
const std::map<TerminalType, LiteralTerminal> g_punctuationTerminals =
{
    { TerminalType::OPEN_BRACE,           { "{"  } },
    { TerminalType::CLOSE_BRACE,          { "}"  } },
    { TerminalType::OPEN_ROUND,           { "("  } },
    { TerminalType::CLOSE_ROUND,          { ")"  } },
    { TerminalType::OPEN_ANGLED,          { "<"  } },
    { TerminalType::CLOSE_ANGLED,         { ">"  } },
    { TerminalType::OPEN_SQUARE,          { "["  } },
    { TerminalType::CLOSE_SQUARE,         { "]"  } },

    { TerminalType::EQUALITY,             { "==" } },
    { TerminalType::NOT_EQUALITY,         { "!=" } },
    { TerminalType::LESS_THAN_EQUALS,     { "<=" } },
    { TerminalType::GREATER_THAN_EQUALS,  { ">=" } },
    { TerminalType::LESS_THAN,            { "<"  } },
    { TerminalType::GREATER_THAN,         { ">"  } },

    { TerminalType::EQUALS,               { "="  } },
    { TerminalType::PLUS_EQUALS,          { "+=" } },
    { TerminalType::MINUS_EQUALS,         { "-=" } },
    { TerminalType::MULTIPLY_EQUALS,      { "*=" } },
    { TerminalType::DIVIDE_EQUALS,        { "/=" } },
    { TerminalType::MODULO_EQUALS,        { "%=" } },
    { TerminalType::AND_EQUALS,           { "&=" } },
    { TerminalType::INCLUSIVE_OR_EQUALS,  { "|=" } },
    { TerminalType::EXCLUSIVE_OR_EQUALS,  { "^=" } },
    { TerminalType::LEFT_SHIFT_EQUALS,    { "<<="} },
    { TerminalType::RIGHT_SHIFT_EQUALS,   { ">>="} },

    { TerminalType::LOGICAL_AND,          { "&&" } },
    { TerminalType::LOGICAL_OR,           { "||" } },
    { TerminalType::LOGICAL_NOT,          { "!"  } },

    { TerminalType::INCREMENT,            { "++" } },
    { TerminalType::DECREMENT,            { "--" } },
    { TerminalType::PLUS,                 { "+"  } },
    { TerminalType::MINUS,                { "-"  } },
    { TerminalType::MULTIPLY,             { "*"  } },
    { TerminalType::DIVIDE,               { "/"  } },
    { TerminalType::MODULO,               { "%"  } },
    { TerminalType::AND,                  { "&"  } },
    { TerminalType::INCLUSIVE_OR,         { "|"  } },
    { TerminalType::EXCLUSIVE_OR,         { "^"  } },
    { TerminalType::BITWISE_NOT,          { "~"  } },
    { TerminalType::LEFT_SHIFT,           { "<<" } },
    { TerminalType::RIGHT_SHIFT,          { ">>" } },

    { TerminalType::SEMICOLON,            { ";"  } },
    { TerminalType::COMMA,                { ","  } },
    { TerminalType::PERIOD,               { "."  } },
    { TerminalType::COLON,                { ":"  } },
    { TerminalType::QUERY,                { "?"  } }
};

//
// Literals
//
const std::map<TerminalType, FunctionalTerminal> g_literalTerminals =
{
    { TerminalType::INTEGER_LITERAL,   { ReadIntegerLiteral,   "integer literal"   } },
    { TerminalType::FLOATING_LITERAL,  { ReadFloatingLiteral,  "floating literal"  } },
    { TerminalType::CHARACTER_LITERAL, { ReadCharacterLiteral, "character literal" } },
    { TerminalType::STRING_LITERAL,    { ReadStringLiteral,    "string literal"    } },
    { TerminalType::IDENTIFIER,        { ReadIdentifier,       "identifier"        } },
};

//
// Keywords
//
const std::map<TerminalType, LiteralTerminal> g_keywordTerminals =
{
    { TerminalType::TECHNIQUE,    { "technique",  "" } },
    { TerminalType::PASS,         { "pass",       "" } },

    { TerminalType::TYPE_INT,     { "int",        "" } },

    { TerminalType::TRUE,         { "true",       "" } },
    { TerminalType::FALSE,        { "false",      "" } }
};

//TODO use the map properly
const std::string& GetTerminalString( TerminalType terminal_type )
{
    for( const auto& i : g_ignoredTerminals )
        if( i.first == terminal_type )
            return i.second.readable_string;

    for( const auto& i : g_punctuationTerminals )
         if( i.first == terminal_type )
         {
            if( i.second.readable_string.empty() )
                return i.second.matched_string;
            else
                return i.second.readable_string;
         }

    for( const auto& i : g_literalTerminals )
        if( i.first == terminal_type )
            return i.second.readable_string;

    for( const auto& i : g_keywordTerminals )
         if( i.first == terminal_type )
         {
            if( i.second.readable_string.empty() )
                return i.second.matched_string;
            else
                return i.second.readable_string;
         }

    const static std::string s = "Invalid Terminal";
    const static std::string i = "identifier";
    const static std::string e = "EOF";

    if( terminal_type == TerminalType::IDENTIFIER )
        return i;
    else if ( terminal_type == TerminalType::END_OF_INPUT )
        return e;

    return s;
}

//------------------------------------------------------------------------------
// LiteralTerminal
//------------------------------------------------------------------------------

std::size_t LiteralTerminal::Read( std::string::const_iterator begin,
                                   std::string::const_iterator end ) const
{
    assert( end >= begin && "begin is past end" );
    if( std::size_t(end - begin) < matched_string.size() ||
        !std::equal( matched_string.begin(), matched_string.end(), begin ) )
        return 0;

    return matched_string.size();
}

//------------------------------------------------------------------------------
// FunctionalTerminal
//------------------------------------------------------------------------------

std::size_t FunctionalTerminal::Read( std::string::const_iterator begin,
                                      std::string::const_iterator end ) const
{
    assert( end >= begin && "begin is past end" );
    return function( begin, end );
}

//------------------------------------------------------------------------------
// Reading Functions
//------------------------------------------------------------------------------

std::size_t ReadWhitespace( std::string::const_iterator begin,
                            std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    while( p < end )
    {
        char c = *p;
        if( c != ' '  &&
            c != '\n' &&
            c != '\r' &&
            c != '\t' &&
            c != '\v' )
            break;
        ++p;
    }

    return p - begin;
}

std::size_t ReadLineComment( std::string::const_iterator begin,
                             std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( end - begin < 2 )
        return 0;

    if( *p++ == '/' &&
        *p++ == '/' )
    {
        while( p < end )
        {
            char c = *p;
            if( c == '\n' )
                break;
            ++p;
        }
        return p - begin;
    }

    return 0;
}

std::size_t ReadBlockComment( std::string::const_iterator begin,
                              std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( end - begin < 4 )
        return 0;

    if( *p++ == '/' &&
        *p++ == '*' )
    {
        ++p;
        while( p < end )
        {
            char c = *p;
            if( c == '/' &&
                *(p-1) == '*' )
                return p - begin + 1;

            ++p;
        }
        return 0;
    }

    return 0;
}

std::size_t ReadIdentifier( std::string::const_iterator begin,
                            std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( !IsNonDigit( *p) )
        return 0;

    ++p;
    while( IsDigitOrNonDigit( *p ) )
        ++p;

    for( const auto& terminal_reader : g_keywordTerminals )
        if( std::size_t( p - begin ) == terminal_reader.second.matched_string.size() &&
            std::equal( begin, p, terminal_reader.second.matched_string.begin() ) )
            return 0;

    return p - begin;
}

std::size_t ReadDigitSequence( std::string::const_iterator begin,
                               std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( ( c >= '0' ) && ( c <= '9' ) && ( p < end ) )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

std::size_t ReadHexDigitSequence( std::string::const_iterator begin,
                                  std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( IsHexDigit( c ) && p < end )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

std::size_t ReadOctalDigitSequence( std::string::const_iterator begin,
                                    std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( ( c >= '0' ) && ( c <= '7' ) && p < end )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

std::size_t ReadIntegerLiteral( std::string::const_iterator begin,
                                std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( *p == '0' )
    {
        ++p;
        if( p >= end )
            return 1;

        if( *p == 'x' ||
            *p == 'X' )
        {
            ++p;
            int n = ReadHexDigitSequence( p, end );
            if( !n )
                return 0;
            p += n;
            if( *p == 'e' ||
                *p == 'E' )
                return 0;
            return p - begin;
        }

        p += ReadOctalDigitSequence( p, end );
    }
    else
    {
        p += ReadDigitSequence( begin, end );
    }
    if( *p == '.' ||
        *p == 'e' ||
        *p == 'E' )
        return 0;
    return p - begin;
}

std::size_t ReadExponent( std::string::const_iterator begin,
                          std::string::const_iterator end )
{
    if( end - begin < 2 )
        return 0;

    std::string::const_iterator p = begin;
    char c = *p;
    if( ( c != 'e' ) && ( c != 'E' ) )
        return 0;

    ++p;
    c = *p;

    if( ( c == '+' ) || ( c == '-' ) )
        ++p;

    int s = ReadDigitSequence( p, end );
    if( s == 0 )
        return 0;

    p += s;
    return p - begin;
}

std::size_t ReadFloatingLiteral( std::string::const_iterator begin,
                                 std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    int s = ReadDigitSequence( p, end );
    if( !s )
    {
        if( *p != '.' )
            return 0;

        ++p;
        s = ReadDigitSequence( p, end );
        if( !s )
            return 0;

        p += s;

        p += ReadExponent( p, end );
    }
    else
    {
        p += s;
        if( p >= end )
            return 0;

        if( *p != '.' )
        {
            s = ReadExponent( p, end );
            if( !s )
                return 0;
        }
        else
        {
            ++p;
            p += ReadDigitSequence( p, end );
            p += ReadExponent( p, end );
        }
    }

    if( p >= end )
        return p - begin;

    char c = *p;
    if( c == 'f' || c == 'F' )
        ++p;

    return p - begin;
}

std::size_t ReadCharOrEscapedChar( std::string::const_iterator begin,
                                   std::string::const_iterator end )
{
    if( end - begin < 2 )
        return 0;

    std::string::const_iterator p = begin;
    if( *p == '\\' )
        ++p;

    ++p;
    return p - begin;
}

std::size_t ReadCharacterLiteral( std::string::const_iterator begin,
                                  std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( *p != '\'' )
        return 0;

    p += ReadCharOrEscapedChar( p, end );

    if( *p != '\'' )
        return 0;

    return p - begin;
}

std::size_t ReadStringLiteral( std::string::const_iterator begin,
                               std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( *p != '\"' )
        return 0;

    ++p;

    while( *p != '\"' &&
           p < end )
        p += ReadCharOrEscapedChar( p, end );

    if( p == end )
        return 0;

    ++p;

    return p - begin;
}

bool IsHexDigit( char c )
{
    return ( c >= '0' && c <= '9' ) ||
           ( c >= 'a' && c <= 'f' ) ||
           ( c >= 'A' && c <= 'F' );
}

bool IsNonDigit( char c )
{
    return ( c >= 'a' && c <= 'z' ) ||
           ( c >= 'A' && c <= 'Z' ) ||
           ( c == '_' );
}

bool IsDigitOrNonDigit( char c )
{
    return IsNonDigit( c ) ||
           ( c >= '0' && c <= '9' );
}

} // namespace Compiler
} // namespace JoeLang
