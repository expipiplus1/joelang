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

#include "conditional_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/tokens/expressions/assignment_expression.hpp>
#include <compiler/tokens/expressions/binary_operator_expression.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// ConditionalExpression
//------------------------------------------------------------------------------

ConditionalExpression::ConditionalExpression(
                                  Expression_up condition,
                                  Expression_up true_expression,
                                  Expression_up false_expression )
    :Expression( TokenTy::ConditionalExpression )
    ,m_Condition( std::move(condition) )
    ,m_TrueExpression( std::move(true_expression) )
    ,m_FalseExpression( std::move(false_expression) )
{
    assert( m_Condition &&
            "ConditionalExpression given a null condition" );
    assert( m_TrueExpression &&
            "ConditionalExpression given a true_expression" );
    assert( m_FalseExpression &&
            "ConditionalExpression given a null false_expression" );
}

ConditionalExpression::~ConditionalExpression()
{
}

bool ConditionalExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO check that the true and false expressions are of equal array size
    bool good = true;

    m_Condition = CastExpression::Create( Type::BOOL,
                                          std::move(m_Condition),
                                          false );

    good &= m_Condition->PerformSema( sema );
    good &= m_TrueExpression->PerformSema( sema );
    good &= m_FalseExpression->PerformSema( sema );

    const CompleteType& t = GetType();
    if( t.IsUnknown() )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here, so report it
        if( !m_TrueExpression->GetType().IsUnknown() &&
            !m_FalseExpression->GetType().IsUnknown() )
            sema.Error( "Incompatable operand types in conditional expression "+
                        m_TrueExpression->GetType().GetString() +
                        " and " +
                        m_FalseExpression->GetType().GetString() );
    }
    else
    {
        CastExpression_up c;
        c = CastExpression::Create( t, std::move(m_TrueExpression), false );
        good &= c->PerformSemaNoRecurse( sema );
        m_TrueExpression = std::move(c);

        c = CastExpression::Create( t, std::move(m_FalseExpression), false );
        good &= c->PerformSemaNoRecurse( sema );
        m_FalseExpression = std::move(c);
    }

    return good;
}

const ExpressionNode& ConditionalExpression::GenerateCodeDag( NodeManager& node_manager ) const
{
    const ExpressionNode& true_node = m_TrueExpression->GenerateCodeDag( node_manager );
    const ExpressionNode& false_node = m_FalseExpression->GenerateCodeDag( node_manager );
    const ExpressionNode& condition = m_Condition->GenerateCodeDag( node_manager );
    
    return node_manager.MakeExpressionNode( NodeType::Select, {true_node, false_node, condition} );
}
    
llvm::Value* ConditionalExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateSelect( *m_Condition,
                                  *m_TrueExpression,
                                  *m_FalseExpression );
}

void ConditionalExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << "(" << *m_Condition << " ? " << *m_TrueExpression <<
                                            " : " << *m_FalseExpression << ")";
}

CompleteType ConditionalExpression::GetType() const
{
    return GetCommonType( m_TrueExpression->GetType(),
                          m_FalseExpression->GetType() );
}

std::set<Function_sp> ConditionalExpression::GetCallees() const
{
    auto ret = m_Condition->GetCallees();
    auto f   = m_TrueExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> ConditionalExpression::GetVariables() const
{
    auto ret = m_Condition->GetVariables();
    auto f   = m_TrueExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> ConditionalExpression::GetWrittenToVariables(
                                                        bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a conditional expression" );
    auto ret = m_Condition->GetWrittenToVariables( false );
    auto f   = m_TrueExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    return ret;
}

bool ConditionalExpression::IsConst() const
{
    /// TODO only check the taken select
    return m_Condition->IsConst() &&
           m_TrueExpression->IsConst() &&
           m_FalseExpression->IsConst();

}

bool ConditionalExpression::Parse( Parser& parser,
                                   Expression_up& token )
{
    // Try and parse the condition
    Expression_up condition;
    if( !parser.Expect< LogicalOrExpression >( condition ) )
        return false;

    // If the next terminal isn't QUERY then just return the LogicalOrExpression
    if( !parser.ExpectTerminal( TerminalType::QUERY ) )
    {
        token = std::move( condition );
        return true;
    }

    // We've seen the QUERY do must parse the rest of the ternary expression
    Expression_up true_expression;
    if( !parser.Expect<Expression>( true_expression ) )
        return false;

    if( !parser.ExpectTerminal( TerminalType::COLON ) )
        return false;

    Expression_up false_expression;
    if( !parser.Expect<AssignmentExpression>( false_expression ) )
        return false;

    token.reset( new ConditionalExpression( std::move(condition),
                                            std::move(true_expression),
                                            std::move(false_expression) ) );
    return true;
}

bool ConditionalExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::ConditionalExpression;
}

bool ConditionalExpression::classof( const ConditionalExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
