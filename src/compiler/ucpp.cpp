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

#include "ucpp.hpp"

#include <cstdio>

#include <cassert>
#include <memory>
#include <mutex>
#include <string>

#include <ucpp/cpp.h>

namespace JoeLang
{
namespace Compiler
{

const long UCPPContext::s_LexerFlags = WARN_STANDARD |
                                       WARN_ANNOYING |
                                       WARN_TRIGRAPHS |
                                       WARN_TRIGRAPHS_MORE |
                                       WARN_PRAGMA |
                                       CPLUSPLUS_COMMENTS |
                                       //LINE_NUM |
                                       HANDLE_ASSERTIONS |
                                       //HANDLE_PRAGMA |
                                       MACRO_VAARG |
                                       //UTF8_SOURCE |
                                       LEXER |
                                       HANDLE_TRIGRAPHS;

void InitializeUCPP()
{
    static std::once_flag once_flag;
    std::call_once( once_flag, [](){g_UCPPContext.reset(new UCPPContext);} );
}

UCPPContext::UCPPContext()
{
    // Lock the ucpp resources
    std::lock_guard<std::mutex> lock(s_Mutex);

    // Initialize static data
    init_cpp();

    // Define special macros such as __FILE__
    no_special_macros = 0;

    // Don't define __STDC_VERSION__
    c99_compliant = 0;

    // Don't define __STDC_HOSTED__
    c99_hosted = -1;

    // Initialize the macro table including assertions
    init_tables(true);

    // Set no include paths
    init_include_path(nullptr);

    // Emit all dependencies
    emit_dependencies = false;
    // emit #define macros
    emit_defines = false;
    // emit assertions
    emit_assertions = false;
    // don't emit things to a file
    emit_output = nullptr;
    // no transient characters
    transient_characters = nullptr;
}

UCPPContext::~UCPPContext()
{
    /*
    char filename[] = "testfile";
    set_init_filename(filename, false);

    int error;
    while(!(error = lex(&ls)))
    {
        std::cout << "File: " << current_filename <<
                     " Line: " << ls.line <<
                     " type: " << ls.ctok->type <<
                     " tok: " << ls.ctok->name << "\n";
    }
    if( error != CPPERR_EOF )
    {
        std::cout << "Error lexing: " << error << std::endl;
    }
    std::fclose(f);
    */

    // Lock the ucpp resources
    std::lock_guard<std::mutex> lock(s_Mutex);
    assert( !m_File && "Something went terribly wrong" );
    wipeout();
}

void UCPPContext::BeginLexing( std::string& filename )
{
    //
    // grab the ucpp resource lock
    // This is released in EndLexing
    s_Mutex.lock();

    m_LexerState.reset( new lexer_state );

    //
    // This filename can't be fopened (or at least, we'll do it ourselves)
    //
    set_init_filename(filename.c_str(), false);

    //
    // Load the file
    //
    m_File.reset( std::fopen( filename.c_str(), "r" ) );
    m_LexerState->input = m_File.get();

    //
    // Initialize the lexer members
    //
    init_lexer_state( m_LexerState.get() );

    //
    // Set the lexer to be a lexer
    //
    init_lexer_mode( m_LexerState.get() );

    m_LexerState->flags = s_LexerFlags;
}

void UCPPContext::EndLexing()
{
    m_LexerState->input = nullptr;
    free_lexer_state( m_LexerState.get() );
    m_LexerState.reset();
    s_Mutex.unlock();
}

} // namespace Compiler
} // namespace JoeLang
