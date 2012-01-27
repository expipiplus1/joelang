/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "expression.hpp"

#include <iostream>
#include <memory>
#include <utility>

#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>

namespace JoeLang
{
namespace Parser
{

//------------------------------------------------------------------------------
// Expression
//------------------------------------------------------------------------------

Expression::Expression()
{
}

Expression::~Expression()
{
}

bool Expression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return AssignmentExpression::Parse( parser, token );
}

//------------------------------------------------------------------------------
// Assignment Expression
//------------------------------------------------------------------------------

AssignmentExpression::AssignmentExpression( std::unique_ptr<Expression> unary_expression,
                                            std::unique_ptr<AssignmentOperator> assignment_operator,
                                            std::unique_ptr<Expression> assignment_expression )
    :m_unaryExpression( std::move( unary_expression ) )
    ,m_assignmentOperator( std::move( assignment_operator ) )
    ,m_assignmentExpression( std::move( assignment_expression ) )
{
}

AssignmentExpression::~AssignmentExpression()
{
}

void AssignmentExpression::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Assignment Expression\n";

    m_unaryExpression->Print( depth + 1 );
    m_assignmentOperator->Print( depth + 1 );
    m_assignmentExpression->Print( depth + 1 );
}

bool AssignmentExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( Expect< ConditionalExpression >( parser, token ) )
        return true;

    std::unique_ptr<Expression> unary_expression;
    if( !Expect< UnaryExpression >( parser, unary_expression ) )
        return false;

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !Expect< AssignmentOperator >( parser, assignment_operator ) )
        return false;

    std::unique_ptr<Expression> assignment_expression;
    if( !Expect< AssignmentExpression >( parser, assignment_expression ) )
        return false;

    token.reset( new AssignmentExpression( std::move( unary_expression ),
                                           std::move( assignment_operator ),
                                           std::move( assignment_expression ) ) );
    return true;
}

AssignmentOperator::AssignmentOperator( Lexer::TerminalType terminal_type )
    :m_terminalType( terminal_type )
{
}

AssignmentOperator::~AssignmentOperator()
{
}

void AssignmentOperator::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Assignment Operator\n";
}

bool AssignmentOperator::Parse( Parser& parser, std::unique_ptr<AssignmentOperator>& token )
{
    if( !parser.ExpectTerminal( Lexer::EQUALS ) )
        return false;

    token.reset( new AssignmentOperator( Lexer::EQUALS ) );
    return true;
}

//------------------------------------------------------------------------------
// Conditional Expression
//------------------------------------------------------------------------------

ConditionalExpression::ConditionalExpression()
{
}

ConditionalExpression::~ConditionalExpression()
{
}

void ConditionalExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Conditional Expression\n";
}

bool ConditionalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER ) )
        return false;

    token.reset( new ConditionalExpression() );
    return true;
}

//------------------------------------------------------------------------------
// Unary Expression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression()
{
}

UnaryExpression::~UnaryExpression()
{
}

void UnaryExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Unary Expression\n";
}

bool UnaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER ) )
        return false;

    token.reset( new UnaryExpression() );
    return true;
}


} // namespace Parser
} // namespace JoeLang
