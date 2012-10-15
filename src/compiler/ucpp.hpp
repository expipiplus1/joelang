/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include <cstdio>

#include <memory>
#include <mutex>
#include <string>

#include <ucpp/cpp.h>

struct lexer_state;

namespace JoeLang
{
namespace Compiler
{

enum class TerminalType;

void InitializeUCPP();

/**
  * \class UCPPContext
  * \brief A class to manage the lifetime of the ucpp allocated memory
  */
class UCPPContext
{
public:
    ~UCPPContext();
    
private:
    friend
    void InitializeUCPP();

    /**
      * The constructor is private and can only be called from InitializeUCPP()
      */
    UCPPContext();
};

/**
  * \class UCPPLexerState
  * \brief A class to manage the lifetime of a ucpp lexer state and buffers
  */
class UCPPLexerState
{
public:
    UCPPLexerState( const std::string& filename, FILE* file );
    ~UCPPLexerState();
    
    TerminalType Lex( std::string& string );
    
    unsigned GetLineNumber();
private:
    static
    const long s_LexerFlags;
    
    lexer_state m_LexerState;
};

/**
  * This holds onto the ucpp buffers and releases them at program termination
  */
extern std::unique_ptr<UCPPContext> g_UCPPContext;

/**
  * This mutex is used to control access to ucpp
  */
extern std::mutex g_UCPPMutex;

} // namespace Compiler
} // namespace JoeLang
