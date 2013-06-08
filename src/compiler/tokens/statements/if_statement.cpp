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

#include "if_statement.hpp"

#include <cassert>
#include <memory>

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/statement_node.hpp>
#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/statements/statement.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// IfStatement
//------------------------------------------------------------------------------

IfStatement::IfStatement( Expression_up condition,
                          Statement_up true_statement,
                          Statement_up else_statement )
    : Statement( TokenTy::IfStatement ),
      m_Condition( std::move( condition ) ),
      m_TrueStatement( std::move( true_statement ) ),
      m_ElseStatement( std::move( else_statement ) )

{
    assert( m_Condition && "If statement given null condition" );
    assert( m_TrueStatement && "If statement given null true_statement" );
}

IfStatement::~IfStatement()
{
}

const StatementNode& IfStatement::GenerateCodeDag( NodeManager& node_manager ) const
{
    std::vector<Node_ref> nodes = { { m_Condition->GenerateCodeDag( node_manager ),
                                      m_TrueStatement->GenerateCodeDag( node_manager ) } };

    if( m_ElseStatement )
        nodes.push_back( m_ElseStatement->GenerateCodeDag( node_manager ) );

    return node_manager.MakeStatementNode( NodeType::Conditional, std::move( nodes ) );
}

bool IfStatement::AlwaysReturns() const
{
    if( !m_ElseStatement )
        return false;

    return m_TrueStatement->AlwaysReturns() && m_ElseStatement->AlwaysReturns();
}

std::set<Function_sp> IfStatement::GetCallees() const
{
    assert( false && "Remove me" );
}

std::set<Variable_sp> IfStatement::GetVariables() const
{
    assert( false && "Remove me" );
}

std::set<Variable_sp> IfStatement::GetWrittenToVariables() const
{
    assert( false && "Remove me" );
}

void IfStatement::PerformSema( SemaAnalyzer& sema, const CompleteType& return_type )
{
    m_Condition->PerformSema( sema );
    CastExpression_up c = CastExpression::Create( Type::BOOL, std::move( m_Condition ), false );
    c->PerformSemaNoRecurse( sema );
    m_Condition = std::move( c );

    // Create the scope for the true branch
    SemaAnalyzer::ScopeHolder scope( sema );
    scope.Enter();

    m_TrueStatement->PerformSema( sema, return_type );

    scope.Leave();

    if( m_ElseStatement )
    {
        SemaAnalyzer::ScopeHolder scope( sema );
        scope.Enter();

        m_ElseStatement->PerformSema( sema, return_type );

        scope.Leave();
    }
}

void IfStatement::CodeGen( CodeGenerator& code_gen )
{
    assert( false && "remove me" );
}

void IfStatement::Write( ShaderWriter& shader_writer ) const
{
    assert( false && "remove me" );
}

bool IfStatement::Parse( Parser& parser, IfStatement_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::IF ) )
        return false;

    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
    {
        parser.Error( "Expected '(' after 'if'" );
        return false;
    }

    Expression_up condition;
    if( !parser.Expect<Expression>( condition ) )
    {
        parser.Error( "Expected expression" );
        return false;
    }

    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
    {
        parser.Error( "Expected ')' after expression" );
        return false;
    }

    Statement_up true_statement;
    if( !parser.Expect<Statement>( true_statement ) )
    {
        parser.Error( "Expected statement" );
        return false;
    }

    Statement_up else_statement;
    if( parser.ExpectTerminal( TerminalType::ELSE ) )
    {
        if( !parser.Expect<Statement>( else_statement ) )
        {
            parser.Error( "Expected statement" );
            return false;
        }
    }

    token.reset( new IfStatement(
        std::move( condition ), std::move( true_statement ), std::move( else_statement ) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
