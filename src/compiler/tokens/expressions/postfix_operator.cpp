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

#include "postfix_operator.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
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
// PostfixOperator
//------------------------------------------------------------------------------

PostfixOperator::PostfixOperator( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

PostfixOperator::~PostfixOperator()
{
}

bool PostfixOperator::Parse( Parser& parser,
                             std::unique_ptr<PostfixOperator>& token )
{
    // Try and parse any of the operators
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<SubscriptOperator,
                            ArgumentListOperator,
                            MemberAccessOperator,
                            IncrementOrDecrementOperator>( t ) )
        return false;

    // Cast the result to a PostfixOperator
    assert( isa<PostfixOperator>(t) );
    token.reset( static_cast<PostfixOperator*>( t.release() ) );
    return true;
}

llvm::Value* PostfixOperator::CodeGenPointerTo(
                                            CodeGenerator& code_gen,
                                            const Expression_up& expression )
{
    assert( false && "complete me" );
    return nullptr;
}


bool PostfixOperator::IsLValue( const Expression& expression ) const
{
    return false;
}



bool PostfixOperator::classof( const Token* e )
{
    return e->GetSubClassID() >= TokenTy::PostfixOperator_Start &&
           e->GetSubClassID() <= TokenTy::PostfixOperator_End;
}

bool PostfixOperator::classof( const PostfixOperator* e )
{
    return true;
}

//------------------------------------------------------------------------------
// SubscriptOperator
//------------------------------------------------------------------------------

SubscriptOperator::SubscriptOperator( Expression_up index_expression )
    :PostfixOperator( TokenTy::SubscriptOperator )
    ,m_IndexExpression( std::move(index_expression) )
{
    assert( m_IndexExpression &&
            "SubscriptOperator given a null index expression" );
}

SubscriptOperator::~SubscriptOperator()
{
}

bool SubscriptOperator::PerformSema( SemaAnalyzer& sema,
                                     const Expression_up& expression )
{
    bool good = m_IndexExpression->ResolveIdentifiers( sema );
    if( good )
    {
        m_IndexExpression = CastExpression::Create( Type::I64,
                                                    std::move(m_IndexExpression) );
        good &= m_IndexExpression->PerformSema( sema );
    }
    if( expression->GetReturnType() != Type::ARRAY )
    {
        sema.Error( "Trying to index into a non-array" );
        return false;
    }
    const ArrayExtents extents = expression->GetArrayExtents();
    assert( extents.size() != 0 && "Indexing into a non array" );
    if( m_IndexExpression->IsConst() )
    {
        unsigned index = sema.EvaluateExpression( *m_IndexExpression ).GetI64();
        if( index >= extents[0] )
            sema.Error( "Indexing beyond array bounds" );
    }
    m_ArrayExtents.assign( ++extents.begin(), extents.end() );
    return good;
}

llvm::Value* SubscriptOperator::CodeGen( CodeGenerator& code_gen,
                                         const Expression_up& expression )
{
    assert( expression && "SubscriptOperator given an null expression" );
    return code_gen.CreateArrayIndex( *expression, *m_IndexExpression );
}

llvm::Value* SubscriptOperator::CodeGenPointerTo(
                                            CodeGenerator& code_gen,
                                            const Expression_up& expression )
{
    assert( expression && "SubscriptOperator given an null expression" );
    return code_gen.CreateArrayIndexPointerTo( *expression,
                                               *m_IndexExpression );
}

Type SubscriptOperator::GetReturnType( const Expression_up& expression ) const
{
    assert( expression && "SubscriptOperator given an null expression" );
    const ArrayExtents& array_extents = expression->GetArrayExtents();
    if( array_extents.size() > 1 )
        return Type::ARRAY;
    return expression->GetUnderlyingType();
}

Type SubscriptOperator::GetUnderlyingType(
                                        const Expression_up& expression ) const
{
    assert( expression && "SubscriptOperator given an null expression" );
    return expression->GetUnderlyingType();
}

const ArrayExtents& SubscriptOperator::GetArrayExtents(
                                        const Expression_up& expression ) const
{
    return m_ArrayExtents;
}

bool SubscriptOperator::IsConst( const Expression& expression ) const
{
    return expression.IsConst() &&
           m_IndexExpression->IsConst();
}

bool SubscriptOperator::IsLValue( const Expression& expression ) const
{
    return true;
}

void SubscriptOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "SubscriptOperator\n";
    m_IndexExpression->Print( depth + 1 );
}

bool SubscriptOperator::Parse( Parser& parser,
                               std::unique_ptr<SubscriptOperator>& token )
{
    // open bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
        return false;

    // parse the index expression
    Expression_up index_expression;
    if( !parser.Expect<Expression>( index_expression ) )
        return false;

    // close bracket
    if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        return false;

    token.reset( new SubscriptOperator( std::move(index_expression) ) );
    return true;
}

//------------------------------------------------------------------------------
// ArgumentListOperator
//------------------------------------------------------------------------------

ArgumentListOperator::ArgumentListOperator(
        ArgumentExpressionVector argument_expressions )
    :PostfixOperator( TokenTy::ArgumentListOperator )
    ,m_ArgumentExpressions( std::move(argument_expressions) )
{
#ifndef NDEBUG
    for( const auto& e : m_ArgumentExpressions )
        assert( e && "ArgumentListOperator given a null argument expression" );
#endif
}

ArgumentListOperator::~ArgumentListOperator()
{
}

bool ArgumentListOperator::PerformSema(
                                SemaAnalyzer& sema,
                                const Expression_up& expression )
{
    assert( false && "Complete me" );
    return false;
}

llvm::Value* ArgumentListOperator::CodeGen( CodeGenerator& code_gen,
                                            const Expression_up& expression )
{
    assert( false && "complete me" );
    return nullptr;
}

Type ArgumentListOperator::GetReturnType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

Type ArgumentListOperator::GetUnderlyingType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

const ArrayExtents& ArgumentListOperator::GetArrayExtents(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    const static ArrayExtents empty;
    return empty;
}

bool ArgumentListOperator::IsConst( const Expression& expression ) const
{
    return false;
}

void ArgumentListOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "ArgumentListOperator\n";
    for( const auto& i : m_ArgumentExpressions )
        i->Print( depth + 1 );
}

bool ArgumentListOperator::Parse( Parser& parser,
                                  std::unique_ptr<ArgumentListOperator>& token )
{
    // parse (
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;

    // The vector to hold the expressions
    ArgumentExpressionVector argument_expressions;

    // The pointer to hold each argument as we parse
    Expression_up argument;

    // Try and parse the first argument
    if( parser.Expect<AssignmentExpression>( argument ) )
    {
        // We parsed the first argument push it onto the vector
        argument_expressions.push_back( std::move( argument ) );

        // Each subsequent argument expects a comma
        while( parser.ExpectTerminal( TerminalType::COMMA ) )
        {
            // If we've seen a comma we must have an expression to push
            if( !parser.Expect<AssignmentExpression>( argument ) )
                return false;
            argument_expressions.push_back( std::move( argument ) );
        }
    }

    // The lexer may be out of step
    CHECK_PARSER;

    // parse closing )
    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
        return false;

    token.reset( new ArgumentListOperator( std::move(argument_expressions) ) );
    return true;
}

//------------------------------------------------------------------------------
// MemberAccessOperator
//------------------------------------------------------------------------------

MemberAccessOperator::MemberAccessOperator( std::string identifier )
    :PostfixOperator( TokenTy::MemberAccessOperator )
    ,m_Identifier( std::move( identifier ) )
{
}

MemberAccessOperator::~MemberAccessOperator()
{
}

bool MemberAccessOperator::PerformSema(
                                SemaAnalyzer& sema,
                                const Expression_up& expression )
{
    assert( false && "Complete me" );
    return false;
}

llvm::Value* MemberAccessOperator::CodeGen( CodeGenerator& code_gen,
                                            const Expression_up& expression )
{
    assert( false && "complete me" );
    return nullptr;
}

Type MemberAccessOperator::GetReturnType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

Type MemberAccessOperator::GetUnderlyingType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

const ArrayExtents& MemberAccessOperator::GetArrayExtents(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    const static ArrayExtents empty;
    return empty;
}

bool MemberAccessOperator::IsConst( const Expression& expression ) const
{
    return expression.IsConst();
}

void MemberAccessOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << ".\n";
    for( int i = 0; i < depth * 4 + 4; ++i )
        std::cout << " ";
    std::cout << m_Identifier << std::endl;
}

bool MemberAccessOperator::Parse( Parser& parser,
                                  std::unique_ptr<MemberAccessOperator>& token )
{
    // Parse the member access operator
    if( !parser.ExpectTerminal( TerminalType::PERIOD ) )
        return false;

    // Store the member identifier in identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    token.reset( new MemberAccessOperator( std::move(identifier) ) );
    return true;
}

//------------------------------------------------------------------------------
// IncrementOrDecrementOperator
//------------------------------------------------------------------------------

IncrementOrDecrementOperator::IncrementOrDecrementOperator( Op op )
    :PostfixOperator( TokenTy::IncrementOrDecrementOperator )
    ,m_Operator( op )
{
}

IncrementOrDecrementOperator::~IncrementOrDecrementOperator()
{
}

bool IncrementOrDecrementOperator::PerformSema(
                                SemaAnalyzer& sema,
                                const Expression_up& expression )
{
    assert( false && "Complete me" );
    return false;
}

llvm::Value* IncrementOrDecrementOperator::CodeGen(
                                               CodeGenerator& code_gen,
                                               const Expression_up& expression )
{
    assert( false && "complete me" );
    return nullptr;
}

Type IncrementOrDecrementOperator::GetReturnType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

Type IncrementOrDecrementOperator::GetUnderlyingType(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    return Type::UNKNOWN;
}

const ArrayExtents& IncrementOrDecrementOperator::GetArrayExtents(
                                        const Expression_up& expression ) const
{
    assert( false && "Complete me" );
    const static ArrayExtents empty;
    return empty;
}

bool IncrementOrDecrementOperator::IsConst( const Expression& expression ) const
{
    return false;
}

void IncrementOrDecrementOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << (m_Operator == Op::INCREMENT ? "++" : "--") << std::endl;
}

bool IncrementOrDecrementOperator::Parse(
                          Parser& parser,
                          std::unique_ptr<IncrementOrDecrementOperator>& token )
{
    // Try to parse ++
    if( parser.ExpectTerminal( TerminalType::INCREMENT ) )
    {
        token.reset( new IncrementOrDecrementOperator( Op::INCREMENT ) );
        return true;
    }

    // Try to parse --
    if( parser.ExpectTerminal( TerminalType::DECREMENT ) )
    {
        token.reset( new IncrementOrDecrementOperator( Op::DECREMENT ) );
        return true;
    }

    return false;
}

} // namespace Compiler
} // namespace JoeLang
