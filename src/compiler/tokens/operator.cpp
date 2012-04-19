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

#include "operator.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <compiler/parser.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// AssignmentOperator
//------------------------------------------------------------------------------

AssignmentOperator::AssignmentOperator( Op op )
    :m_operator(op)
{
}

AssignmentOperator::~AssignmentOperator()
{
}

void AssignmentOperator::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "assignment_op" << std::endl;
}

bool AssignmentOperator::Parse( Parser& parser,
                                std::unique_ptr<AssignmentOperator>& token )
{
    // A vector of terminals for assignment operators and their Op enumerants
    static const std::vector< std::pair<TerminalType, Op> >
           s_assignment_operator_terminals =
    {
        { TerminalType::EQUALS,             Op::EQUALS },
        { TerminalType::PLUS_EQUALS,        Op::PLUS_EQUALS },
        { TerminalType::MINUS_EQUALS,       Op::MINUS_EQUALS },
        { TerminalType::MULTIPLY_EQUALS,    Op::MULTIPLY_EQUALS },
        { TerminalType::DIVIDE_EQUALS,      Op::DIVIDE_EQUALS },
        { TerminalType::MODULO_EQUALS,      Op::MODULO_EQUALS },
        { TerminalType::LEFT_SHIFT_EQUALS,  Op::SHL_EQUALS },
        { TerminalType::RIGHT_SHIFT_EQUALS, Op::SHR_EQUALS },
        { TerminalType::AND_EQUALS,         Op::AND_EQUALS },
        { TerminalType::INCLUSIVE_OR_EQUALS, Op::OR_EQUALS },
        { TerminalType::EXCLUSIVE_OR_EQUALS, Op::XOR_EQUALS }
    };

    // Try and match any of these operators
    for( const auto& p : s_assignment_operator_terminals )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new AssignmentOperator( p.second ) );
            return true;
        }
    return false;
}

//------------------------------------------------------------------------------
// PostfixOperator
//------------------------------------------------------------------------------

PostfixOperator::PostfixOperator()
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
    token.reset( static_cast<PostfixOperator*>( t.release() ) );
    return true;
}

//------------------------------------------------------------------------------
// SubscriptOperator
//------------------------------------------------------------------------------

SubscriptOperator::SubscriptOperator( std::unique_ptr<Expression> expression )
    :m_expression( std::move(expression) )
{
    assert( m_expression && "SubscriptOperator given a null index expression" );
}

SubscriptOperator::~SubscriptOperator()
{
}

void SubscriptOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "SubscriptOperator\n";
    m_expression->Print( depth + 1 );
}

bool SubscriptOperator::Parse( Parser& parser,
                               std::unique_ptr<SubscriptOperator>& token )
{
    // open bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
        return false;

    // parse the index expression
    std::unique_ptr<Expression> expression;
    if( !parser.Expect<Expression>( expression ) )
        return false;

    // close bracket
    if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        return false;

    token.reset( new SubscriptOperator( std::move(expression) ) );
    return true;
}

//------------------------------------------------------------------------------
// ArgumentListOperator
//------------------------------------------------------------------------------

ArgumentListOperator::ArgumentListOperator(
        ArgumentExpressionVector argument_expressions )
    :m_argumentExpressions( std::move(argument_expressions) )
{
    for( const auto& e : m_argumentExpressions )
        assert( e && "ArgumentListOperator given a null argument expression" );
}

ArgumentListOperator::~ArgumentListOperator()
{
}

void ArgumentListOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "ArgumentListOperator\n";
    for( const auto& i : m_argumentExpressions )
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
    std::unique_ptr<Expression> argument;

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
    :m_identifier( std::move( identifier ) )
{
}

MemberAccessOperator::~MemberAccessOperator()
{
}

void MemberAccessOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << ".\n";
    for( int i = 0; i < depth * 4 + 4; ++i )
        std::cout << " ";
    std::cout << m_identifier << std::endl;
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
    :m_operator( op )
{
}

IncrementOrDecrementOperator::~IncrementOrDecrementOperator()
{
}

void IncrementOrDecrementOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << (m_operator == Op::INCREMENT ? "++" : "--") << std::endl;
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
