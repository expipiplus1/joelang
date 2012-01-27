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
#include <string>
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

//TODO
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

ConditionalExpression::ConditionalExpression( std::unique_ptr<Expression> condition,
                                              std::unique_ptr<Expression> true_expression,
                                              std::unique_ptr<Expression> false_expression )
    :m_condition( std::move( condition ) )
    ,m_trueExpression( std::move( true_expression ) )
    ,m_falseExpression( std::move( false_expression ) )
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

    m_condition->Print( depth + 1 );
    m_trueExpression->Print( depth + 1 );
    m_falseExpression->Print( depth + 1 );
}

bool ConditionalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<Expression> condition;
    if( !Expect< LogicalOrExpression >( parser, condition ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::QUERY ) )
    {
        token = std::move( condition );
        return true;
    }

    std::unique_ptr<Expression> true_expression;
    if( !Expect<Expression>( parser, true_expression ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::COLON ) )
        return false;

    std::unique_ptr<Expression> false_expression;
    if( !Expect<ConditionalExpression>( parser, false_expression ) )
        return false;

    token.reset( new ConditionalExpression( std::move( condition ),
                                            std::move( true_expression ),
                                            std::move( false_expression ) ) );
    return true;
}

//------------------------------------------------------------------------------
// BinaryOperatorExpression
//------------------------------------------------------------------------------

BinaryOperatorExpression::BinaryOperatorExpression( Lexer::TerminalType operator_terminal,
                                                    std::unique_ptr<Expression> left_side,
                                                    std::unique_ptr<Expression> right_side )
    :m_operatorTerminal( operator_terminal )
    ,m_leftSide( std::move( left_side ) )
    ,m_rightSide( std::move( right_side ) )
{
}

BinaryOperatorExpression::~BinaryOperatorExpression()
{
}

void BinaryOperatorExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Binary Operator Expression\n";
    m_leftSide->Print( depth + 1 );
    m_rightSide->Print( depth + 1 );
}
template< typename ExpressionType, typename SubExpressionType >
bool BinaryOperatorExpression::ParseLeftAssociative( Parser& parser, std::unique_ptr<Expression>& token,
                                  const std::vector<Lexer::TerminalType>& operator_terminals )
{
    std::unique_ptr<Expression> left;
    if( !Expect<SubExpressionType>( parser, left ) )
        return false;

    std::vector< std::pair< Lexer::TerminalType,
                            std::unique_ptr<Expression> > > rest;

    while( true )
    {
        bool cont = false;
        Lexer::TerminalType operator_terminal;
        for( Lexer::TerminalType o : operator_terminals )
        {
            if( parser.ExpectTerminal( o ) )
            {
                operator_terminal = o;
                cont = true;
                break;
            }
        }

        if( !cont )
            break;

        std::unique_ptr<Expression> next;
        if( !Expect<SubExpressionType>( parser, next ) )
            return false;

        rest.push_back( std::make_pair( operator_terminal,
                                        std::move( next ) ) );
    }

    for( auto& expression : rest )
        left.reset( new ExpressionType( expression.first,
                                        std::move( left ),
                                        std::move( expression.second ) ) );

    token = std::move( left );
    return true;
}

//template< typename ExpressionType, typename SubExpressionType >
//static bool ParseRightAssociative( Parser& parser, std::unique_ptr<Expression>& token,
                                   //const std::vector<Lexer::TerminalType>& operator_terminals );

//------------------------------------------------------------------------------
// Logical Or Expression
//------------------------------------------------------------------------------

LogicalOrExpression::LogicalOrExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

LogicalOrExpression::~LogicalOrExpression()
{
}

bool LogicalOrExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::vector<Lexer::TerminalType> operators;
    operators.push_back( Lexer::LOGICAL_OR );

    return ParseLeftAssociative<LogicalOrExpression, LogicalAndExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// LogicalAndExpression
//------------------------------------------------------------------------------

LogicalAndExpression::LogicalAndExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

LogicalAndExpression::~LogicalAndExpression()
{
}

bool LogicalAndExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::vector<Lexer::TerminalType> operators;
    operators.push_back( Lexer::LOGICAL_AND );

    //TODO
    return ParseLeftAssociative<LogicalAndExpression, PrimaryExpression>( parser, token, operators );
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

//TODO
bool UnaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return Expect<PrimaryExpression>( parser, token );
   /*
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER ) )
        return false;

    token.reset( new UnaryExpression() );
    return true;
    */
}

//------------------------------------------------------------------------------
// Primary Expression
//------------------------------------------------------------------------------

PrimaryExpression::PrimaryExpression( std::string identifier )
    :m_identifier( std::move( identifier ) )
{
}

PrimaryExpression::~PrimaryExpression()
{
}

void PrimaryExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_identifier << "\n";
}

//TODO
bool PrimaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::string identifier;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, identifier ) )
        return false;

    token.reset( new PrimaryExpression( identifier ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
