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

#include "return_statement.hpp"

#include <cassert>
#include <memory>

#include <compiler/code_generator.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/statements/statement.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// ReturnStatement
//------------------------------------------------------------------------------

ReturnStatement::ReturnStatement( Expression_up expression )
    :Statement( TokenTy::ReturnStatement )
    ,m_Expression( std::move(expression) )
{
}

ReturnStatement::~ReturnStatement()
{
}

bool ReturnStatement::AlwaysReturns() const
{
    return true;
}

std::set<Function_sp> ReturnStatement::GetCallees() const
{
    if( m_Expression )
        return m_Expression->GetCallees();
    else
        return {};
}

std::set<Variable_sp> ReturnStatement::GetVariables() const
{
    if( m_Expression )
        return m_Expression->GetVariables();
    else
        return {};
}

void ReturnStatement::PerformSema( SemaAnalyzer& sema,
                                   const CompleteType& return_type )
{
    // If we need a return value make sure we have one
    if( !m_Expression )
    {
        if( return_type.GetBaseType() != Type::VOID )
            sema.Error( "No return expression in function returning non-void" );
        return;
    }

    m_Expression->ResolveIdentifiers( sema );

    // Cast the return type to what we want if this fails, sema will know
    m_Expression = CastExpression::Create( return_type,
                                           std::move(m_Expression) );
    m_Expression->PerformSema( sema );
}

void ReturnStatement::CodeGen( CodeGenerator& code_gen )
{
    code_gen.CreateReturnStatement( m_Expression );
}

void ReturnStatement::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << "return";
    if( m_Expression )
        shader_writer << " " << *m_Expression;
    shader_writer << ";";
}

bool ReturnStatement::Parse( Parser& parser, ReturnStatement_up& token )
{
    // Try and parse the return keyword
    if( !parser.ExpectTerminal( TerminalType::RETURN ) )
        return false;

    // Parse the optional expression
    Expression_up expression;
    parser.Expect<Expression>( expression );

    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
    {
        parser.Error( "Expected ';' after return expression" );
        return false;
    }

    token.reset( new ReturnStatement( std::move(expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
