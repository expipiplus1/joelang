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

#include <memory>
#include <mutex>
#include <string>

namespace JoeLang
{
namespace Compiler
{

class UCPPLexerState;

enum class TerminalType;

/**
  * \class Lexer
  * \brief A class to hold onto a source string and produce tokens of it
  */
class Lexer
{
public:
    /**
      * \param filename
      *   The file to lex
      */
                 Lexer           ( const std::string& source,
                                   const std::string& filename );
                 
                ~Lexer           ();

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
      *   A string with which to return the characters belonging to the terminal
      * \returns
      *   The TerminalType of the next token in the stream
      */
    TerminalType PeekNextTerminal ( std::string* string = nullptr ) const;

    /**
      * \param string
      *   The characters belonging to the next identifier if there is one
      * \returns
      *   true if the next terminal is an identifier
      */
    bool PeekIdentifier( std::string& string ) const;

    /** \returns The position in the file **/
    std::size_t  GetPosition     () const;
    /** \returns The line numer in the currenst string **/
    std::size_t  GetLineNumber   () const;
    /** \returns The number of characters since the last newline + 1 **/
    std::size_t  GetColumnNumber () const;

private:
    /**
      * This will advance the lexer to the next token, unless we've reached the
      * end of the file
      */
    void AdvanceLexer();
    
    //
    // The string for the current terminal
    //
    std::string m_TerminalString; 
    
    //
    // The type of the current terminal
    //
    TerminalType m_TerminalType;

    //
    // The number of tokens lexed
    //
    std::size_t m_NumTokensLexed = 0;
    
    std::unique_ptr<UCPPLexerState>         m_LexerState;
    std::unique_lock<std::mutex>            m_UCPPLock;
};

} // namespace Compiler
} // namespace JoeLang
