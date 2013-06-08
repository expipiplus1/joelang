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

#include "compound_statement.hpp"

#include <algorithm>
#include <cassert>
#include <memory>
#include <set>
#include <vector>

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/statements/return_statement.hpp>
#include <compiler/tokens/statements/statement.hpp>
#include <compiler/writers/shader_writer.hpp>

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/statement_node.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// CompoundStatement
//------------------------------------------------------------------------------

CompoundStatement::CompoundStatement( std::vector<Statement_up> statements )
    :Statement( TokenTy::CompoundStatement )
    ,m_Statements( std::move(statements) )
{
}

CompoundStatement::~CompoundStatement()
{
}

const StatementNode& CompoundStatement::GenerateCodeDag( NodeManager& node_manager ) const
{
    std::vector<Node_ref> nodes; 
    for( const Statement_up& s : m_Statements )
        nodes.emplace_back( s->GenerateCodeDag( node_manager ) );
    return node_manager.MakeStatementNode( NodeType::Sequence, std::move( nodes ) );
}

bool CompoundStatement::AlwaysReturns() const
{
    //
    // This assumes that we've performed sema on this object and have dropped
    // Statements after the return statement
    //
    if( m_Statements.empty() )
        return false;
    return m_Statements.back()->AlwaysReturns();
}

std::set<Function_sp> CompoundStatement::GetCallees() const
{
    std::set<Function_sp> ret;
    for( const auto& s : m_Statements )
    {
        auto f = s->GetCallees();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

std::set<Variable_sp> CompoundStatement::GetVariables() const
{
    std::set<Variable_sp> ret;
    for( const auto& s : m_Statements )
    {
        auto f = s->GetVariables();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}


std::set<Variable_sp> CompoundStatement::GetWrittenToVariables() const
{
    std::set<Variable_sp> ret;
    for( const auto& s : m_Statements )
    {
        auto f = s->GetWrittenToVariables();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

void CompoundStatement::PerformSemaAsFunction( SemaAnalyzer& sema,
                                               const CompleteType& return_type )
{
    PerformSemaCommon( sema, return_type, true );
}

void CompoundStatement::PerformSema( SemaAnalyzer& sema,
                                     const CompleteType& return_type )
{
    // Create the scope for this statement
    SemaAnalyzer::ScopeHolder scope( sema );
    scope.Enter();

    PerformSemaCommon( sema, return_type, false );

    scope.Leave();
}

void CompoundStatement::PerformSemaCommon( SemaAnalyzer& sema,
                                           const CompleteType& return_type,
                                           bool must_return )
{
    for( auto& statement : m_Statements )
        statement->PerformSema( sema, return_type );

    // Remove all statements after the first returning one
    for( unsigned i = 0; i < m_Statements.size(); ++i )
        if( m_Statements[i]->AlwaysReturns() &&
            i != m_Statements.size() - 1 )
        {
            sema.Warning( "Statements will never be executed" );
            m_Statements.erase( m_Statements.begin()+i+1, m_Statements.end() );
            break;
        }

    if( must_return )
    {
        if( return_type.IsVoid() &&
            !AlwaysReturns() )
            m_Statements.emplace_back( new ReturnStatement( nullptr ) );
        if( !AlwaysReturns() )
            sema.Error( "control reaches the end of a non-void function" );
    }
}

void CompoundStatement::CodeGen( CodeGenerator& code_gen )
{
    for( auto& s : m_Statements )
        s->CodeGen( code_gen );
}

void CompoundStatement::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << "{";
    shader_writer.PushIndentation();

    for( const auto& statement : m_Statements )
    {
        shader_writer.NewLine();
        shader_writer << *statement;
    }

    shader_writer.PopIndentation();
    shader_writer.NewLine();
    shader_writer << "}";
}

bool CompoundStatement::Parse( Parser& parser, CompoundStatement_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    std::vector<Statement_up> statements;
    parser.ExpectSequenceOf<Statement>( statements );
    CHECK_PARSER;

    if( !parser.ExpectTerminal( TerminalType::CLOSE_BRACE ) )
        return false;
    
    statements.erase( std::remove_if(statements.begin(), statements.end(), 
                                  [](const Statement_up& s)
    { return s->GetSubClassID() == TokenTy::EmptyStatement;}), statements.end() );

    token.reset( new CompoundStatement( std::move(statements) ) );
    return true;
}

} // namespace Compiler 
} // namespace JoeLang
