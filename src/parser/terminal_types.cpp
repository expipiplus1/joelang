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
#include <functional>
#include <string>
#include <vector>

namespace JoeLang
{
namespace Lexer
{

//------------------------------------------------------------------------------
// Tables of terminals
//------------------------------------------------------------------------------

//
// Ignored Sequence
//
const std::vector<FunctionalTerminal> g_ignoredTerminals =
{
    { ReadWhitespace,   WHITESPACE,    "whitespace"    },
    { ReadLineComment,  LINE_COMMENT,  "line comment"  },
    { ReadBlockComment, BLOCK_COMMENT, "block comment" }
};

//
// Punctuation
//
const std::vector<LiteralTerminal> g_punctuationTerminals =
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
const std::vector<FunctionalTerminal> g_literalTerminals =
{
    { ReadIntegerLiteral,   INTEGER_LITERAL,    "integer literal"   },
    { ReadFloatingLiteral,  FLOATING_LITERAL,   "floating literal"  },
    //{ ReadCharacterLiteral, CHARACTER_LITERAL,  "character literal" },
    //{ ReadStringLiteral,    STRING_LITERAL,     "string literal"    }
};

//
// Keywords
//
const std::vector<LiteralTerminal> g_keywordTerminals =
{
    { "technique",  TECHNIQUE,  "" },
    { "pass",       PASS,       "" },

    { "int",        TYPE_INT,   "" },

    { "true",       TRUE,   "" },
    { "false",      FALSE,   "" }
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
// LiteralTerminal
//------------------------------------------------------------------------------

int LiteralTerminal::Read( std::string::const_iterator begin,
                           std::string::const_iterator end ) const
{
    if( std::size_t(end - begin) < matched_string.size() ||
        !std::equal( matched_string.begin(), matched_string.end(), begin ) )
        return 0;

    return matched_string.size();
}

//------------------------------------------------------------------------------
// FunctionalTerminal
//------------------------------------------------------------------------------

int FunctionalTerminal::Read( std::string::const_iterator begin,
                              std::string::const_iterator end ) const
{
    return function( begin, end );
}

//------------------------------------------------------------------------------
// Reading Functions
//------------------------------------------------------------------------------

int ReadWhitespace( std::string::const_iterator begin,
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

int ReadLineComment( std::string::const_iterator begin,
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

int ReadBlockComment(   std::string::const_iterator begin,
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

int ReadDigitSequence(      std::string::const_iterator begin,
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

int ReadHexDigitSequence(      std::string::const_iterator begin,
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

int ReadOctalDigitSequence(      std::string::const_iterator begin,
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

int ReadIntegerLiteral(     std::string::const_iterator begin,
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
            if( n )
                return n + 2;
        }

        return 1 + ReadOctalDigitSequence( p, end );
    }
    else
    {
        return ReadDigitSequence( begin, end );
    }
}

int ReadExponent(           std::string::const_iterator begin,
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

int ReadFloatingLiteral(    std::string::const_iterator begin,
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

    //TODO enable single precision floating point
    //if( p >= end )
    //    return p - begin;

    //char c = *p;
    //if( c == 'f' || c == 'F' )
    //    ++p;

    return p - begin;
}

/*
int ReadCharacterLiteral(   std::string::const_iterator begin,
                            std::string::const_iterator end );

int ReadStringLiteral(      std::string::const_iterator begin,
                            std::string::const_iterator end );

int ReadIdentifier( std::string::const_iterator begin,
                    std::string::const_iterator end );

*/

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

} // namespace Lexer
} // namespace JoeLang
