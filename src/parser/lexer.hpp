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

#include <string>

namespace JoeLang
{
namespace Compiler
{

enum class TerminalType;

//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

class Lexer
{
public:
                 Lexer           () = delete;
                 Lexer           ( std::string string );
                 ~Lexer          () = default;

    //
    // Expect return true and moves the position forward if the next terminal
    // matches terminal_type. It optionally returns the matched string
    //
    bool         Expect          ( TerminalType terminal_type );
    bool         Expect          ( TerminalType terminal_type,
                                  std::string& string );

    std::size_t  GetPosition     () const;
    std::size_t  GetLineNumber   () const;
    std::size_t  GetColumnNumber () const;

private:
    void         ConsumeIgnoredTerminals ();
    TerminalType ReadPunctuationTerminal ( TerminalType terminal,
                                           std::string& string );
    TerminalType ReadLiteralTerminal     ( TerminalType terminal,
                                           std::string& string );
    TerminalType ReadKeywordTerminal     ( TerminalType terminal,
                                           std::string& string );
    void         ReadChars               ( std::size_t num_chars );

    const std::string           m_string;
    std::string::const_iterator m_position;
    std::size_t                 m_lineNumber = 1;
    std::size_t                 m_columnNumber = 1;
};

} // namespace Compiler
} // namespace JoeLang
