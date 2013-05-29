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

#include "assignment_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/conditional_expression.hpp>
#include <compiler/tokens/expressions/postfix_expression.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Assignment Expression
//------------------------------------------------------------------------------

AssignmentExpression::AssignmentExpression(
                        Expression_up assignee,
                        AssignmentOperator assignment_operator,
                        Expression_up assigned_expression )
    :Expression( TokenTy::AssignmentExpression )
    ,m_Assignee          ( std::move(assignee) )
    ,m_AssignmentOperator( assignment_operator)
    ,m_AssignedExpression( std::move(assigned_expression) )
{
    assert( m_Assignee &&
            "AssignmentExpression given an invalid assignee" );
    assert( m_AssignedExpression &&
            "AssignmentExpression given an invalid assigned expression" );
}

AssignmentExpression::~AssignmentExpression()
{
}

bool AssignmentExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    // Extract the identifier from the assignee
    good &= m_Assignee->PerformSema( sema );

    if( !m_Assignee->IsLValue() )
    {
        good = false;
        sema.Error( "Trying to assign to a RValue" );
    }

    if( m_Assignee->IsConst() )
    {
        good = false;
        sema.Error( "Trying to assign to a const expression" );
    }

    // Assuming that we can assign to the same as ReturnType;
    m_AssignedExpression = CastExpression::Create(
                                              m_Assignee->GetType(),
                                              std::move(m_AssignedExpression),
                                              false );

    good &= m_AssignedExpression->PerformSema( sema );

    return good;
}

llvm::Value* AssignmentExpression::CodeGen( CodeGenerator& code_gen ) const
{
    //
    // Get the swizzle data if there is any
    //
    Swizzle swizzle;
    if( PostfixExpression* p = dyn_cast<PostfixExpression>(m_Assignee.get()) )
        if( MemberAccessOperator* m =
                             dyn_cast<MemberAccessOperator>(&p->GetOperator()) )
            if( m->IsSwizzle() )
                swizzle = m->GetSwizzle();

    return code_gen.CreateAssignment( *m_Assignee,
                                      *m_AssignedExpression,
                                      swizzle,
                                      m_AssignmentOperator );
}

llvm::Value* AssignmentExpression::CodeGenPointerTo(
                                                  CodeGenerator& code_gen) const
{
    return m_Assignee->CodeGenPointerTo( code_gen );
}

void AssignmentExpression::Write( ShaderWriter& shader_writer ) const
{
    // Todo this isn't right, need to handle += and friends properly
    shader_writer << *m_Assignee << " = " << *m_AssignedExpression;
}

CompleteType AssignmentExpression::GetType() const
{
    return m_Assignee->GetType();
}

std::set<Function_sp> AssignmentExpression::GetCallees() const
{
    //assert( false && "Do assignment properly" );
    auto ret = m_Assignee->GetCallees();
    auto f   = m_AssignedExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> AssignmentExpression::GetVariables() const
{
    //assert( false && "Do assignment properly" );
    auto ret = m_Assignee->GetVariables();
    auto f   = m_AssignedExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> AssignmentExpression::GetWrittenToVariables(
                                                        bool is_assigned ) const
{
    auto ret = m_Assignee->GetWrittenToVariables( true );
    auto f   = m_AssignedExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    return ret;
}

bool AssignmentExpression::IsLValue() const
{
    return true;
}

bool AssignmentExpression::IsConst() const
{
    return false;
}

bool AssignmentExpression::Parse( Parser& parser,
                                  Expression_up& token )
{
    Expression_up lhs_expression;
    if( !parser.Expect< ConditionalExpression >( lhs_expression ) )
        return false;

    std::unique_ptr<AssignmentOperatorToken> assignment_operator;
    if( !parser.Expect<AssignmentOperatorToken>( assignment_operator ) )
    {
        CHECK_PARSER;

        token = std::move( lhs_expression );
        return true;
    }

    Expression_up assignment_expression;
    if( !parser.Expect< AssignmentExpression >( assignment_expression ) )
        return false;

    token.reset( new AssignmentExpression( std::move(lhs_expression),
                                           assignment_operator->GetOp(),
                                           std::move(assignment_expression) ) );
    return true;
}

bool AssignmentExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::AssignmentExpression;
}

bool AssignmentExpression::classof( const AssignmentExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
