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
    { TerminalType::INTEGER_LITERAL,   { ReadIntegerLiteral,
                                         "integer literal"   } },
    { TerminalType::FLOATING_LITERAL,  { ReadFloatingLiteral,
                                         "floating literal"  } },
    { TerminalType::CHARACTER_LITERAL, { ReadCharacterLiteral,
                                         "character literal" } },
    { TerminalType::STRING_LITERAL,    { ReadStringLiteral,
                                         "string literal"    } },
    { TerminalType::IDENTIFIER,        { ReadIdentifier,
                                         "identifier"        } },
};

//
// Keywords
//
const std::map<TerminalType, LiteralTerminal> g_keywordTerminals =
{
    { TerminalType::TECHNIQUE,     { "technique",     "" } },
    { TerminalType::PASS,          { "pass",          "" } },
    { TerminalType::COMPILE,       { "compile",       "" } },
    { TerminalType::PIXEL_SHADER,  { "pixel_shader",  "" } },
    { TerminalType::VERTEX_SHADER, { "vertex_shader", "" } },
    { TerminalType::RETURN,        { "return",        "" } },

    //
    // Storage class specifiers
    //
    { TerminalType::STATIC,        { "static",    "" } },
    { TerminalType::EXTERN,        { "extern",    "" } },
    { TerminalType::UNIFORM,       { "uniform",   "" } },
    { TerminalType::VARYING,       { "varying",   "" } },
    { TerminalType::IN,            { "in",        "" } },
    { TerminalType::OUT,           { "out",       "" } },
    { TerminalType::INOUT,         { "inout",     "" } },

    //
    // Type qualifiers
    //
    { TerminalType::CONST,         { "const",     "" } },
    { TerminalType::VOLATILE,      { "volatile",  "" } },
    { TerminalType::INLINE,        { "inline",    "" } },

    //
    // Types
    //
    { TerminalType::TYPE_VOID,     { "void",      "" } },
    { TerminalType::TYPE_BOOL,     { "bool",      "" } },
    { TerminalType::TYPE_CHAR,     { "char",      "" } },
    { TerminalType::TYPE_SHORT,    { "short",     "" } },
    { TerminalType::TYPE_INT,      { "int",       "" } },
    { TerminalType::TYPE_LONG,     { "long",      "" } },
    { TerminalType::TYPE_FLOAT,    { "float",     "" } },
    { TerminalType::TYPE_FLOAT4,   { "float4",    "" } },
    { TerminalType::TYPE_DOUBLE,   { "double",    "" } },
    { TerminalType::TYPE_SIGNED,   { "signed",    "" } },
    { TerminalType::TYPE_UNSIGNED, { "unsigned",  "" } },
    { TerminalType::TYPE_STRING,   { "string",    "" } },


    //
    // Constants
    //
    { TerminalType::TRUE,          { "true",      "" } },
    { TerminalType::FALSE,         { "false",     "" } }
};

const std::string& GetTerminalString( TerminalType terminal_type )
{
    const static std::map<TerminalType, std::string> s_TerminalStringMap
    {  
        { TerminalType::END_OF_INPUT,         "EOF"                   },
        { TerminalType::WHITESPACE,           "whitespace"            },
        { TerminalType::COMMENT,              "comment"               },
        { TerminalType::UNKNOWN_CHARACTER,    "unknown character"     },
        { TerminalType::OPEN_BRACE,           "{"                     },
        { TerminalType::CLOSE_BRACE,          "}"                     },
        { TerminalType::OPEN_ROUND,           "("                     },
        { TerminalType::CLOSE_ROUND,          ")"                     },
        { TerminalType::OPEN_ANGLED,          "<"                     },
        { TerminalType::CLOSE_ANGLED,         ">"                     },
        { TerminalType::OPEN_SQUARE,          "["                     },
        { TerminalType::CLOSE_SQUARE,         "]"                     },
        { TerminalType::EQUALS,               "="                     },
        { TerminalType::PLUS_EQUALS,          "+="                    },
        { TerminalType::MINUS_EQUALS,         "-="                    },
        { TerminalType::MULTIPLY_EQUALS,      "*="                    },
        { TerminalType::DIVIDE_EQUALS,        "/="                    },
        { TerminalType::MODULO_EQUALS,        "&="                    },
        { TerminalType::LEFT_SHIFT_EQUALS,    "<<="                   },
        { TerminalType::RIGHT_SHIFT_EQUALS,   ">>="                   },
        { TerminalType::AND_EQUALS,           "&="                    },
        { TerminalType::INCLUSIVE_OR_EQUALS,  "|="                    },
        { TerminalType::EXCLUSIVE_OR_EQUALS,  "^="                    },
        { TerminalType::EQUALITY,             "=="                    },
        { TerminalType::NOT_EQUALITY,         "!="                    },
        { TerminalType::LESS_THAN_EQUALS,     "<="                    },
        { TerminalType::GREATER_THAN_EQUALS,  ">="                    },
        { TerminalType::LESS_THAN,            "<"                     },
        { TerminalType::GREATER_THAN,         ">"                     },
        { TerminalType::LOGICAL_AND,          "&&"                    },
        { TerminalType::LOGICAL_OR,           "||"                    },
        { TerminalType::LOGICAL_NOT,          "!"                     },
        { TerminalType::INCREMENT,            "++"                    },
        { TerminalType::DECREMENT,            "--"                    },
        { TerminalType::PLUS,                 "+"                     },
        { TerminalType::MINUS,                "-"                     },
        { TerminalType::MULTIPLY,             "*"                     },
        { TerminalType::DIVIDE,               "/"                     },
        { TerminalType::MODULO,               "%"                     },
        { TerminalType::AND,                  "&"                     },
        { TerminalType::INCLUSIVE_OR,         "|"                     },
        { TerminalType::EXCLUSIVE_OR,         "^"                     },
        { TerminalType::BITWISE_NOT,          "~"                     },
        { TerminalType::LEFT_SHIFT,           "<<"                    },
        { TerminalType::RIGHT_SHIFT,          ">>"                    },
        { TerminalType::SEMICOLON,            ";"                     },
        { TerminalType::COMMA,                ","                     },
        { TerminalType::PERIOD,               "."                     },
        { TerminalType::COLON,                ":"                     },
        { TerminalType::QUERY,                "?"                     },
        { TerminalType::ARROW,                "->"                    },
        { TerminalType::FLOATING_LITERAL,     "floating literal"      },
        { TerminalType::INTEGER_LITERAL,      "integer literal"       },
        { TerminalType::CHARACTER_LITERAL,    "character literal"     },
        { TerminalType::STRING_LITERAL,       "string literal"        },
        { TerminalType::TECHNIQUE,            "technique"             },
        { TerminalType::PASS,                 "pass"                  },
        { TerminalType::COMPILE,              "compile"               },
        { TerminalType::PIXEL_SHADER,         "pixel_shader"          },
        { TerminalType::VERTEX_SHADER,        "vertex_shader"         },
        { TerminalType::RETURN,               "return"                },
        { TerminalType::STATIC,               "static"                },
        { TerminalType::EXTERN,               "extern"                },
        { TerminalType::UNIFORM,              "uniform"               },
        { TerminalType::VARYING,              "varying"               },
        { TerminalType::IN,                   "in"                    },
        { TerminalType::OUT,                  "out"                   },
        { TerminalType::INOUT,                "inout"                 },
        { TerminalType::CONST,                "const"                 },
        { TerminalType::VOLATILE,             "volatile"              },
        { TerminalType::INLINE,               "inline"                },
        { TerminalType::TYPE_VOID,            "void"                  },
        { TerminalType::TYPE_BOOL,            "bool"                  },
        { TerminalType::TYPE_CHAR,            "char"                  },
        { TerminalType::TYPE_SHORT,           "short"                 },
        { TerminalType::TYPE_INT,             "int"                   },
        { TerminalType::TYPE_LONG,            "long"                  },
        { TerminalType::TYPE_FLOAT,           "float"                 },
        { TerminalType::TYPE_FLOAT4,          "float4"                },
        { TerminalType::TYPE_DOUBLE,          "double"                },
        { TerminalType::TYPE_SIGNED,          "signed"                },
        { TerminalType::TYPE_UNSIGNED,        "unsigned"              },
        { TerminalType::TYPE_STRING,          "string"                },
        { TerminalType::TRUE,                 "true"                  },
        { TerminalType::FALSE,                "false"                 },
        { TerminalType::IDENTIFIER,           "identifier"            }
    };

    assert( s_TerminalStringMap.find( terminal_type ) != 
            s_TerminalStringMap.end() &&
            "Trying to get the string for an unknown terminal" );
    return s_TerminalStringMap.at( terminal_type );
}

//------------------------------------------------------------------------------
// LiteralTerminal
//------------------------------------------------------------------------------

std::size_t LiteralTerminal::Read( std::string::const_iterator begin,
                                   std::string::const_iterator end ) const
{
    assert( end >= begin && "begin is past end" );
    // Check that there is room for this literal
    // And that it matches
    // And that it matches the whole word (if there's room)
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
        if( std::size_t( p - begin ) ==
                                terminal_reader.second.matched_string.size() &&
            std::equal( begin,
                        p,
                        terminal_reader.second.matched_string.begin() ) )
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

    // If we've not seen any digits we can return
    if( p-begin == 0 )
        return 0;

    // Check that we haven't just read the first part of a float
    if( *p == '.' ||
        *p == 'e' ||
        *p == 'E' )
        return 0;

    // Read the integer suffix
    // TODO return any chars here for better diagnostics
    if( *p == 'u' )
        ++p;

    if( *p == 'i' )
        ++p;
    else if( *p == 'l' )
        ++p;
    else if( *p == 's' )
        ++p;
    else if( *p == 't' )
        ++p;

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
    if( c == 'f' )
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

bool IsValidIdentifier( const std::string& identifier )
{
    return identifier.size() != 0 &&
           ReadIdentifier( identifier.begin(), identifier.end() ) ==
                                                          identifier.size() &&
           std::none_of( g_keywordTerminals.begin(), g_keywordTerminals.end(),
                         [&identifier]
                         (const std::pair<TerminalType, LiteralTerminal>& t )
                         {return t.second.matched_string == identifier;} );
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
