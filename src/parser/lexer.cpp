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

    { "==", EQUALITY,       "" },
    { "!=", NOT_EQUAL,      "" },
    { "<=", LESS_THAN_EQUALS,    "" },
    { ">=", GREATER_THAN_EQUALS, "" },
    { ">",  LESS_THAN,      "" },
    { ">",  GREATER_THAN,   "" },
    // TODO:

    { "=",  EQUALS,          "" },
    { "+=", PLUS_EQUALS,     "" },
    { "-=", MINUS_EQUALS,    "" },
    { "*=", MULTIPLY_EQUALS, "" },
    { "/=", DIVIDE_EQUALS,   "" },
    { "%=", MODULO_EQUALS,   "" },
    { "&=", AND_EQUALS,      "" },
    { "|=", OR_EQUALS,       "" },
    { "^=", XOR_EQUALS,      "" },
    { "<<=", LEFT_SHIFT_EQUALS,  "" },
    { ">>=", RIGHT_SHIFT_EQUALS, "" },

    { "&&", LOGICAL_AND,    "" },
    { "||", LOGICAL_OR,     "" },

    { "++", INCREMENT,      "" },
    { "--", DECREMENT,      "" },
    { "+",  PLUS,           "" },
    { "-",  MINUS,          "" },
    { "*",  MULTIPLY,       "" },
    { "/",  DIVIDE,         "" },
    { "%",  MODULO,         "" },
    { "&",  AND,            "" },
    { "|",  OR,             "" },
    { "^",  XOR,            "" },
    { "<<", LEFT_SHIFT,     "" },
    { ">>", RIGHT_SHIFT,    "" },

    { ";",  SEMICOLON,      "" }
};

//
// Literals
//
static const FunctionalTerminal g_literalTerminals[] =
{
    //{ ReadIntegerLiteral,   INTEGER_LITERAL,    "integer literal"   },
    //{ ReadFloatingLiteral,  FLOATING_LITERAL,   "floating literal"  },
    //{ ReadBooleanLiteral,   BOOLEAN_LITERAL,    "boolean literal"   },
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

    { "int",        TYPE_INT,   "" }
};

//
// Identifier
//
static const FunctionalTerminal g_identifierTerminals[] =
{
    //{ ReadIdentifier,   IDENTIFIER, "identifier" }
};

//------------------------------------------------------------------------------
// Terminal
//------------------------------------------------------------------------------

Terminal::Terminal( TerminalType terminal_type,
                    std::string::const_iterator begin,
                    std::string::const_iterator end )
    :terminal_type(terminal_type)
    ,begin(begin)
    ,end(end)
{}

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
    if( terminal_type == END_OF_INPUT &&
        m_position == m_string.end() )
        return true;

    std::size_t chars_read;

    //
    // Read punctuation and literals
    //

    for( const LiteralTerminal& terminal_reader : g_punctuationTerminals )
    {
        chars_read = terminal_reader.Read( m_position, m_string.end() );
        if( chars_read )
        {
            if( terminal_type == terminal_reader.terminal_type )
            {
                string = std::string( m_position, m_position + chars_read );
                m_position += chars_read;
                ConsumeIgnoredTerminals();
                return true;
            }
            return false;
        }
        if( terminal_type == terminal_reader.terminal_type )
            return false;
    }

    for( const FunctionalTerminal& terminal_reader : g_literalTerminals )
    {
        chars_read = terminal_reader.Read( m_position, m_string.end() );
        if( chars_read )
        {
            if( terminal_type == terminal_reader.terminal_type )
            {
                string = std::string( m_position, m_position + chars_read );
                m_position += chars_read;
                ConsumeIgnoredTerminals();
                return true;
            }
            return false;
        }
        if( terminal_type == terminal_reader.terminal_type )
            return false;
    }

    //
    // The remaining terminals all work on words, find the end of the next word
    //

    if( !IsNonDigit( *m_position ) )
        return false;

    std::string::const_iterator word_end = m_position + 1;
    while( IsDigitOrNonDigit( *word_end ) )
        ++word_end;

    std::size_t word_size = word_end - m_position;

    for( const LiteralTerminal& terminal_reader : g_keywordTerminals )
    {
        if( word_size == terminal_reader.matched_string.size() &&
            std::equal( m_position, word_end, terminal_reader.matched_string.begin() ) )
        {
            if( terminal_type == terminal_reader.terminal_type )
            {
                string = std::string( m_position, m_position + word_size );
                m_position += word_size;
                ConsumeIgnoredTerminals();
                return true;
            }
            return false;
        }
    }

    if( terminal_type == IDENTIFIER )
    {
        string = std::string( m_position, m_position + word_size );
        m_position += word_size;
        ConsumeIgnoredTerminals();
        return true;
    }
    return false;
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
