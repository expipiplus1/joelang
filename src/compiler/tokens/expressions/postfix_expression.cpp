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

#include "postfix_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <compiler/tokens/expressions/type_constructor_expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PostfixExpression
//------------------------------------------------------------------------------

PostfixExpression::PostfixExpression(
                             Expression_up expression,
                             std::unique_ptr<PostfixOperator> postfix_operator )
    :Expression( TokenTy::PostfixExpression )
    ,m_Expression( std::move(expression) )
    ,m_PostfixOperator( std::move(postfix_operator) )
{
    assert( m_Expression && "PostfixExpression given a null expression" );
    assert( m_PostfixOperator && "PostfixExpression given a null operator" );
}

PostfixExpression::~PostfixExpression()
{
}

PostfixOperator& PostfixExpression::GetOperator()
{
    return *m_PostfixOperator;
}

Expression_up PostfixExpression::TakeExpression()
{
    return std::move(m_Expression);
}

bool PostfixExpression::PerformSema( SemaAnalyzer& sema )
{
    //
    // The operators are free to change our expression, for example folding
    // swizzles
    //
    return m_PostfixOperator->PerformSema( sema, m_Expression );
}

llvm::Value* PostfixExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGen( code_gen, *m_Expression );
}

llvm::Value* PostfixExpression::CodeGenPointerTo(
                                                CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGenPointerTo( code_gen, *m_Expression );
}

void PostfixExpression::Write( ShaderWriter& shader_writer ) const
{
    m_PostfixOperator->Write( shader_writer, *m_Expression );
}

CompleteType PostfixExpression::GetType() const
{
    return m_PostfixOperator->GetType( *m_Expression );
}

std::set<Function_sp> PostfixExpression::GetCallees() const
{
    return m_PostfixOperator->GetCallees( *m_Expression );
}

std::set<Variable_sp> PostfixExpression::GetVariables() const
{
    return m_PostfixOperator->GetVariables( *m_Expression );
}

std::set<Variable_sp> PostfixExpression::GetWrittenToVariables(
                                                     bool is_assigned ) const
{
    return m_PostfixOperator->GetWrittenToVariables( *m_Expression,
                                                     is_assigned );
}

bool PostfixExpression::IsConst() const
{
    return m_PostfixOperator->IsConst( *m_Expression );
}

bool PostfixExpression::IsLValue() const
{
    return m_PostfixOperator->IsLValue( *m_Expression );
}

bool PostfixExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try and parse the primary expression
    Expression_up type_constructor_expression;
    if( !parser.Expect<TypeConstructorExpression>(
                                                 type_constructor_expression ) )
        return false;

    // try and parse all the postfix operators
    std::vector< std::unique_ptr<PostfixOperator> > operators;
    parser.ExpectSequenceOf<PostfixOperator>( operators );
    CHECK_PARSER;

    // set ret to just the primary expression
    Expression_up ret = std::move( type_constructor_expression );

    // If we have any postfix operators create a new postfix expression with the
    // last one and the operator
    for( auto& postfix_operator : operators )
        ret.reset( new PostfixExpression( std::move(ret),
                                          std::move(postfix_operator) ) );
    token = std::move(ret);
    return true;
}

bool PostfixExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::PostfixExpression;
}

bool PostfixExpression::classof( const PostfixExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
