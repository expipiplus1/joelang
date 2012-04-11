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
#include <set>
#include <string>
#include <vector>

namespace JoeLang
{
namespace Compiler
{

enum class TerminalType;

class Lexer;
class Token;
class TranslationUnit;

class Parser
{
public:
    Parser();
    ~Parser();

    /** Prints the CST **/
    void                                    Print() const;

    /** Tries to parse the string
      * \param string
      *   The string to parse
      * \returns
      *   true if the parsing was successful,
      *   false if the parsing failed
      */
    bool                                    Parse( const std::string& string );

    /** \returns the parsed CST or null if we haven't parsed a TU **/
    const std::unique_ptr<TranslationUnit>& GetTranslationUnit() const;

    /** Call to indicate that an error has occured during parsing **/
    void Error  ();
    /** Call to indicate that an error has occured during parsing
      * \param error_message
      *   The error message
      */
    void Error  ( std::string error_message );
    /** \returns true if no error has occured **/
    bool Good   () const;


    /**
      * Function to try to match a terminal
      * \returns true if we have consumed a terminal
      * \param terminal_type
      *   The terminal type to try and consume
      */
    bool ExpectTerminal     ( TerminalType terminal_type );
    /** Function to try to match a terminal and return the matched string
      * \returns true if we have consumed a terminal
      * \param terminal_type
      *   The terminal type to try and consume
      * \param string
      *   The string to fill with the matched characters
      */
    bool ExpectTerminal     ( TerminalType terminal_type,
                              std::string& string );

    /**
      * Function to try and match a token
      * \tparam T
      *   The type of token to try and match
      * \returns true if we have consumed the desired token
      */
    template< typename T >
    bool Expect             ();
    /**
      * Function to try and match a token and return the matched token
      * \tparam T
      *   The type of token to try and match
      * \tparam U
      *   The type of pointer to return, determined automatically
      * \param token
      *   The token to fill with the matched token
      * \returns true if we have consumed the desired token
      */
    template< typename T, typename U >
    bool Expect             ( std::unique_ptr<U>& token );

    /**
      * Function to try and match one or more of the desired token
      * \tparam T
      *   The type of token to try and match
      * \tparam U
      *   The type of pointers to return, determined automatically
      * \param token_sequence
      *   A vector of token pointers to fill with the matched tokens
      * \returns true if we have consumed one or more tokens
      */
    template<typename T, typename U>
    bool ExpectSequenceOf ( std::vector< std::unique_ptr<U> >& token_sequence );

    /**
      * Function to try and match any one of the desired tokens
      * \tparam T
      *   The type of tokens to try and match
      * \param token
      *   The matched token or null
      * \returns true if we have consumed a token
      */
    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf        ( std::unique_ptr<Token>& token );
    template<typename T>
    bool ExpectAnyOf        ( std::unique_ptr<Token>& token );
    /**
      * Function to try and match any one of the desired tokens
      * \tparam T
      *   The type of tokens to try and match
      * \returns true if we have consumed a token
      */
    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf        ();
    template<typename T>
    bool ExpectAnyOf        ();

private:
    std::unique_ptr<TranslationUnit>    m_translationUnit;

    std::unique_ptr<Lexer>              m_lexer;

    bool                                m_good = true;
    std::string                         m_errorMessage;
    std::set<TerminalType>              m_expectedTerminals;
};

} // namespace Compiler
} // namespace JoeLang

#include "parser-inl.hpp"
