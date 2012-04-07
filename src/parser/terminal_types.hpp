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

#pragma once

#include <functional>
#include <map>
#include <string>

namespace JoeLang
{
namespace Lexer
{

enum TerminalType : int
{
    //
    // Special Terminals
    //

    // End of input
    END_OF_INPUT,

    // Ignored sequence
    WHITESPACE,
    LINE_COMMENT,
    BLOCK_COMMENT,

    // Funny characters
    UNHANDLED_CHARACTER,

    //
    // Punctuation
    //

    // Brackets
    OPEN_BRACE,
    CLOSE_BRACE,
    OPEN_ROUND,
    CLOSE_ROUND,
    OPEN_ANGLED,
    CLOSE_ANGLED,
    OPEN_SQUARE,
    CLOSE_SQUARE,

    // Assignment
    EQUALS,
    PLUS_EQUALS,
    MINUS_EQUALS,
    MULTIPLY_EQUALS,
    DIVIDE_EQUALS,
    MODULO_EQUALS,
    LEFT_SHIFT_EQUALS,
    RIGHT_SHIFT_EQUALS,
    AND_EQUALS,
    INCLUSIVE_OR_EQUALS,
    EXCLUSIVE_OR_EQUALS,

    // Comparison
    EQUALITY,
    NOT_EQUALITY,
    LESS_THAN_EQUALS,
    GREATER_THAN_EQUALS,
    LESS_THAN,
    GREATER_THAN,

    // Logic
    LOGICAL_AND,
    LOGICAL_OR,
    LOGICAL_NOT,

    // Arithmetic
    INCREMENT,
    DECREMENT,
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
    MODULO,
    AND,
    INCLUSIVE_OR,
    EXCLUSIVE_OR,
    BITWISE_NOT,
    LEFT_SHIFT,
    RIGHT_SHIFT,

    // Misc
    SEMICOLON,
    COMMA,
    PERIOD,
    COLON,
    QUERY,

    //
    // Literals
    //
    INTEGER_LITERAL,
    FLOATING_LITERAL,
    BOOLEAN_LITERAL,
    CHARACTER_LITERAL,
    STRING_LITERAL,

    //
    // Keywords
    //
    TECHNIQUE,
    PASS,

    // Types
    TYPE_INT,

    // Constants
    TRUE,
    FALSE,

    //
    // Identifier
    //
    IDENTIFIER
};

const std::string& GetTerminalString( TerminalType terminal_type );

//------------------------------------------------------------------------------
// LiteralTerminal
//------------------------------------------------------------------------------

struct LiteralTerminal
{
    int Read( const std::string::const_iterator begin,
              const std::string::const_iterator end ) const;

    std::string matched_string;
    std::string readable_string;
};

//------------------------------------------------------------------------------
// FunctionalTerminal
//------------------------------------------------------------------------------

struct FunctionalTerminal
{
    int Read( std::string::const_iterator begin,
              std::string::const_iterator end ) const;

    std::function< int( std::string::const_iterator,
                        std::string::const_iterator ) > function;
    std::string readable_string;
};

//------------------------------------------------------------------------------
// The terminals
//------------------------------------------------------------------------------

extern const std::map<TerminalType, FunctionalTerminal> g_ignoredTerminals;
extern const std::map<TerminalType, LiteralTerminal>    g_punctuationTerminals;
extern const std::map<TerminalType, FunctionalTerminal> g_literalTerminals;
extern const std::map<TerminalType, LiteralTerminal>    g_keywordTerminals;

//------------------------------------------------------------------------------
// Reading Functions
//------------------------------------------------------------------------------

int ReadWhitespace(  std::string::const_iterator begin,
                     std::string::const_iterator end );
int ReadLineComment( std::string::const_iterator begin,
                     std::string::const_iterator end );
int ReadBlockComment( std::string::const_iterator begin,
                      std::string::const_iterator end );

int ReadIntegerLiteral(  std::string::const_iterator begin,
                         std::string::const_iterator end );
int ReadFloatingLiteral( std::string::const_iterator begin,
                         std::string::const_iterator end );
int ReadBooleanLiteral(  std::string::const_iterator begin,
                         std::string::const_iterator end );
int ReadCharacterLiteral( std::string::const_iterator begin,
                          std::string::const_iterator end );
int ReadStringLiteral(   std::string::const_iterator begin,
                         std::string::const_iterator end );

//matches [0-9a-fA-F]
bool IsHexDigit( char c );
//matches [a-zA-Z_]
bool IsNonDigit( char c );
//matches [a-zA-Z0-9_]
bool IsDigitOrNonDigit( char c );

} // namespace Lexer
} // namespace JoeLang
