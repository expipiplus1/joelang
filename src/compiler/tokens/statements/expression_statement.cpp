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

#include "expression_statement.hpp"

#include <cassert>
#include <memory>

#include <compiler/complete_type.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/statements/statement.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// ExpressionStatement
//------------------------------------------------------------------------------

ExpressionStatement::ExpressionStatement( Expression_up expression )
    :Statement( TokenTy::ExpressionStatement )
    ,m_Expression( std::move(expression) )
{
    assert( m_Expression && "ExpressionStatement given a null expression" );
}

ExpressionStatement::~ExpressionStatement()
{
}

bool ExpressionStatement::AlwaysReturns() const
{
    return false;
}

void ExpressionStatement::PerformSema( SemaAnalyzer& sema,
                                       const CompleteType& return_type )
{
    m_Expression->ResolveIdentifiers( sema );
    m_Expression->PerformSema( sema );
}

void ExpressionStatement::CodeGen( CodeGenerator& code_gen )
{
    assert( false && "Complete me" );
}

bool ExpressionStatement::Parse( Parser& parser, ExpressionStatement_up& token )
{
    Expression_up expression;
    if( !parser.Expect<Expression>( expression ) )
        return false;

    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
    {
        parser.Error( "Expected ';' after expression" );
        return false;
    }

    token.reset( new ExpressionStatement( std::move(expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
