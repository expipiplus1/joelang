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

#pragma once

#include <string>

namespace JoeLang
{
namespace Compiler
{

enum class TerminalType;

/**
  * \class Lexer
  * \brief A class to hold onto a source string and produce tokens of it
  */
class Lexer
{
public:
    /**
      * \param string
      *   The string for the whole proprocessed soruce file
      */
                 Lexer           ( std::string string );
                 ~Lexer          () = default;

    /**
      * This function advances the stream by one token if terminal_type is the
      * next token in the stream, otherwise the stream doesn't move
      * \param terminal_type
      *   The TerminalType to look for
      * \returns whether terminal_type was the next terminal in the stream
      */
    bool         Expect          ( TerminalType terminal_type );
    /**
      * This function advances the stream by one token if terminal_type is the
      * next token in the stream, otherwise the stream doesn't move
      * \param terminal_type
      *   The TerminalType to look for
      * \param string
      *   If terminal_type was found, fill string with the parsed characters
      * \returns whether terminal_type was the next terminal in the stream
      */
    bool         Expect          ( TerminalType terminal_type,
                                   std::string& string );

    /**
      * \param string
      *   The characters belonging to the next terminal
      * \returns
      *   The TerminalType of the next token in the stream
      */
    TerminalType PeekNextTerminal ( std::string& string ) const;

    /**
      * \param string
      *   The characters belonging to the next identifier if there is one
      * \returns
      *   true if the next terminal is an identifier
      */
    bool PeekIdentifier( std::string& string ) const;

    /** \returns The position in characters from the start of the string **/
    std::size_t  GetPosition     () const;
    /** \returns The line numer in the currenst string **/
    std::size_t  GetLineNumber   () const;
    /** \returns The number of characters since the last newline + 1 **/
    std::size_t  GetColumnNumber () const;

private:
    /** Consumes terminals until a non-whitespace and non-comment one **/
    void         ConsumeIgnoredTerminals ();

    /**
      * Tries to read a punctuation terminal
      * \param terminal
      *   The TerminalType to look for
      * \param string
      *   The string to fill with the parsed characters on a sucessful parse
      * \returns UNKNOWN_CHARACTER if it didn't parse a token
      *   Otherwise the TerminalType parsed
      */
    TerminalType ReadPunctuationTerminal ( TerminalType terminal,
                                           std::string& string ) const;
    /**
      * Tries to read a literal terminal
      * \param terminal
      *   The TerminalType to look for
      * \param string
      *   The string to fill with the parsed characters on a sucessful parse
      * \returns UNKNOWN_CHARACTER if it didn't parse a token
      *   Otherwise the TerminalType parsed
      */
    TerminalType ReadLiteralTerminal     ( TerminalType terminal,
                                           std::string& string ) const;
    /**
      * Tries to read a keyword terminal
      * \param terminal
      *   The TerminalType to look for
      * \param string
      *   The string to fill with the parsed characters on a sucessful parse
      * \returns UNKNOWN_CHARACTER if it didn't parse a token
      *   Otherwise the TerminalType parsed
      */
    TerminalType ReadKeywordTerminal     ( TerminalType terminal,
                                           std::string& string ) const;
    /** Advances the position in the stream by num_chars.
      * It also keeps track of the line and column numbers
      * \param num_chars
      *   The number of characters by which to advance the position
      */
    void         ReadChars               ( std::size_t num_chars );


    const std::string           m_string;

    std::string::const_iterator m_position;
    std::size_t                 m_lineNumber = 1;
    std::size_t                 m_columnNumber = 1;
};

} // namespace Compiler
} // namespace JoeLang
