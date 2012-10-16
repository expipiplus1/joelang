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

#include "lexer.hpp"

#include <cstdio>

#include <cassert>
#include <algorithm>
#include <functional>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <utility>

#include <compiler/terminal_types.hpp>
#include <compiler/ucpp.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Lexer
//------------------------------------------------------------------------------

Lexer::Lexer( const std::string& filename )
    :m_UCPPLock( g_UCPPMutex, std::defer_lock )
{
    //
    // Make sure that ucpp is initialized
    //
    InitializeUCPP();
    
    //
    // Grab the lock on ucpp
    //
    m_UCPPLock.lock();
    
    //
    // Open the file
    //
    m_File.reset( std::fopen( filename.c_str(), "r" ) );
    
    //
    // Set up the lexerstate
    //
    m_LexerState.reset( new UCPPLexerState( filename, m_File.get() ) );
    
    //
    // Lex the first token
    //
    AdvanceLexer();
}

Lexer::~Lexer()
{
    //
    // Free the lexerstate and close the file
    //
    m_LexerState.reset();
    m_File.reset();
    
    //
    // Release the lock on ucpp
    //
    m_UCPPLock.unlock();
}

bool Lexer::Expect( TerminalType terminal_type )
{
    if( m_TerminalType == terminal_type )
    {
        AdvanceLexer();
        return true;
    }
    return false;
}

bool Lexer::Expect( TerminalType terminal_type, std::string& string )
{
    if( m_TerminalType == terminal_type )
    {
        AdvanceLexer();
        string = m_TerminalString;
        return true;
    }
    return false;
}

TerminalType Lexer::PeekNextTerminal( std::string& string ) const
{
    string = m_TerminalString;
    return m_TerminalType;
}

bool Lexer::PeekIdentifier( std::string& string ) const
{
    if( m_TerminalType == TerminalType::IDENTIFIER )
    {
        string = m_TerminalString;
        return true;
    }
    return false;
}

std::size_t Lexer::GetPosition() const
{
    return ftell( m_File.get() );
}

std::size_t Lexer::GetLineNumber() const
{
    return m_LexerState->GetLineNumber();
}

void Lexer::AdvanceLexer()
{
    m_TerminalType = m_LexerState->Lex( m_TerminalString );
}

} // namespace Compiler
} // namespace JoeLang
