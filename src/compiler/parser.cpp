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

#include "parser.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <string>
#include <sstream>

#include <compiler/lexer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Compiler
{

Parser::Parser( Lexer& token_stream )
    :m_TokenStream( token_stream )
{
}

bool Parser::Parse()
{
    // Try and parse a translation unit
    if( TranslationUnit::Parse( *this, m_TranslationUnit ) )
    {
        assert( Good() && "Parse was a success but we are not good" );
        return true;
    }

    //
    // Report error
    // TODO move this to a better place
    if( m_Errors.empty() )
    {
        std::stringstream ss;
        ss << "Error parsing at line " << m_TokenStream.GetLineNumber()
           << "\n";
        ss << "Expected one of: ";
        for( TerminalType expected_terminal : m_ExpectedTerminals )
        {
            ss << "\'"
                      << Compiler::GetTerminalString( expected_terminal )
                      << "\', ";
        }
        std::string next_terminal;
        m_TokenStream.PeekNextTerminal( &next_terminal );
        ss << "Got: '" << next_terminal << "'";
        m_Errors.push_back( ss.str() );
    }
    return false;
}

TerminalType Parser::PeekTerminal()
{
    return m_TokenStream.PeekNextTerminal();
}

bool Parser::PeekIdentifier( std::string& identifier )
{
    return m_TokenStream.PeekIdentifier( identifier );
}

bool Parser::ExpectTerminal( TerminalType terminal_type )
{
    std::string dummy;
    return ExpectTerminal( terminal_type, dummy );
}

bool Parser::ExpectTerminal( TerminalType terminal_type, std::string& string )
{
    if( m_TokenStream.Expect( terminal_type, string ) )
    {
        m_ExpectedTerminals.clear();
        return true;
    }

    m_ExpectedTerminals.insert( terminal_type );
    return false;
}

const std::unique_ptr<TranslationUnit>& Parser::GetTranslationUnit() const
{
    return m_TranslationUnit;
}

void Parser::Error()
{
    m_Good = false;
}

void Parser::Error( std::string error_message )
{
    m_Errors.emplace_back( std::move( error_message ) );
    Error();
}

const std::vector<std::string>& Parser::GetErrors() const
{
    return m_Errors;
}

bool Parser::Good() const
{
    return m_Good;
}

} // namespace Compiler
} // namespace JoeLang
