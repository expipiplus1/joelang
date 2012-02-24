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

#include <map>
#include <stack>
#include <string>
#include <vector>

#include <parser/terminal_types.hpp>

namespace JoeLang
{
namespace Lexer
{

const std::string& GetTerminalString( TerminalType terminal_type );

//------------------------------------------------------------------------------
// Terminal
//------------------------------------------------------------------------------

struct TerminalPosition
{
    TerminalPosition( TerminalType terminal_type,
              std::string::const_iterator begin,
              std::string::const_iterator end );

    TerminalType terminal_type;
    std::string::const_iterator begin;
    std::string::const_iterator end;
};

//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

class Lexer
{
public:
    Lexer() = delete;
    Lexer( std::string string );
    ~Lexer() = default;

    //
    // Expect return true and moves the position forward if the next terminal
    // matches terminal_type. It optionally returns the matched string
    //
    bool Expect( TerminalType terminal_type );
    bool Expect( TerminalType terminal_type, std::string& string );

    std::size_t GetPosition() const;
    std::size_t GetLineNumber() const;
    std::size_t GetColumnNumber() const;

    //
    // One can optionally store the position in the stream for looking ahead
    // more than one token
    //
    void PushState();
    void PopState();
    void RestoreState();

private:
    void ConsumeIgnoredTerminals();
    void ReadChars( std::size_t num_chars );

    static std::map< TerminalType, const LiteralTerminal* > s_punctuationTerminalMap;
    static std::map< TerminalType, const FunctionalTerminal* > s_literalTerminalMap;
    static std::map< TerminalType, const LiteralTerminal* > s_keywordTerminalMap;

    std::size_t m_numTokensRead;
    std::vector<TerminalPosition> m_readTerminals;

    const std::string m_string;
    std::string::const_iterator m_position;
    std::size_t m_lineNumber = 1;
    std::size_t m_columnNumber = 1;
    std::stack<std::string::iterator> m_savedPositions;
};

} // namespace Lexer
} // namespace JoeLang
