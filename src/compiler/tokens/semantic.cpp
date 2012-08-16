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

#include "semantic.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Semantic
//------------------------------------------------------------------------------

Semantic::Semantic( std::string string, Expression_up index_expression )
    :Token( TokenTy::Semantic )
    ,m_String( std::move(string) )
    ,m_IndexExpression( std::move(index_expression) )
{
    assert( !m_String.empty() && "Semantic given an empty string" );
}

Semantic::~Semantic()
{
}

void Semantic::PerformSema( SemaAnalyzer& sema )
{
}

bool Semantic::Parse( Parser& parser, Semantic_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::COLON ) )
        return false;

    std::string string;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, string ) )
    {
        parser.Error( "Expected semantic" );
        return false;
    }

    //
    // try and parse the optional index
    //
    Expression_up index_expression;
    if( parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
    {
        if( !parser.Expect<Expression>( index_expression ) )
        {
            parser.Error( "Expected semantic index expression" );
            return false;
        }
        if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        {
            parser.Error( "Expected closing square bracket on semantic index" );
            return false;
        }
    }

    token.reset( new Semantic( std::move(string),
                               std::move(index_expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
