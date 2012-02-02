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
    std::unique_ptr<Expression> lhs_expression;
    if( !Expect< ConditionalExpression >( parser, lhs_expression ) )
        return false;

    if( typeid( *lhs_expression ) != typeid( PrimaryExpression ) &&
        typeid( *lhs_expression ) != typeid( UnaryExpression ) &&
        typeid( *lhs_expression ) != typeid( PostfixExpression ) )
    {
        token = std::move( lhs_expression );
        return true;
    }

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !Expect< AssignmentOperator >( parser, assignment_operator ) )
    {
        token = std::move( lhs_expression );
        return true;
    }

    std::unique_ptr<Expression> assignment_expression;
    if( !Expect< AssignmentExpression >( parser, assignment_expression ) )
        return false;

    token.reset( new AssignmentExpression( std::move( lhs_expression ),
                                           std::move( assignment_operator ),
                                           std::move( assignment_expression ) ) );
    return true;
}

//------------------------------------------------------------------------------
// AssignmentOperator
//------------------------------------------------------------------------------

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
    std::cout << GetTerminalString( m_terminalType ) << std::endl;
}

bool AssignmentOperator::Parse( Parser& parser, std::unique_ptr<AssignmentOperator>& token )
{
    // sigh, initializer lists
    static Lexer::TerminalType s_assignment_operator_terminals[] =
    {
        Lexer::EQUALS,
        Lexer::PLUS_EQUALS,
        Lexer::MINUS_EQUALS,
        Lexer::MULTIPLY_EQUALS,
        Lexer::DIVIDE_EQUALS,
        Lexer::MODULO_EQUALS,
        Lexer::LEFT_SHIFT_EQUALS,
        Lexer::RIGHT_SHIFT_EQUALS,
        Lexer::AND_EQUALS,
        Lexer::INCLUSIVE_OR_EQUALS,
        Lexer::EXCLUSIVE_OR_EQUALS
    };

    for( Lexer::TerminalType assignment_operator_terminal : s_assignment_operator_terminals )
    {
        if( parser.ExpectTerminal( assignment_operator_terminal ) )
        {
            token.reset( new AssignmentOperator( assignment_operator_terminal ) );
            return true;
        }
    }
    return false;
}

//------------------------------------------------------------------------------
// ConditionalExpression
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
    if( !Expect<AssignmentExpression>( parser, false_expression ) )
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
    // Why doesn't clang support initializer lists yet!
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
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
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
        operators.push_back( Lexer::LOGICAL_AND );

    return ParseLeftAssociative<LogicalAndExpression, InclusiveOrExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// InclusiveOrExpression
//------------------------------------------------------------------------------

InclusiveOrExpression::InclusiveOrExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

InclusiveOrExpression::~InclusiveOrExpression()
{
}

bool InclusiveOrExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
        operators.push_back( Lexer::INCLUSIVE_OR );

    return ParseLeftAssociative<InclusiveOrExpression, ExclusiveOrExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// ExclusiveOrExpression
//------------------------------------------------------------------------------

ExclusiveOrExpression::ExclusiveOrExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

ExclusiveOrExpression::~ExclusiveOrExpression()
{
}

bool ExclusiveOrExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
        operators.push_back( Lexer::EXCLUSIVE_OR );

    return ParseLeftAssociative<ExclusiveOrExpression, AndExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// AndExpression
//------------------------------------------------------------------------------

AndExpression::AndExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

AndExpression::~AndExpression()
{
}

bool AndExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
        operators.push_back( Lexer::AND );

    return ParseLeftAssociative<AndExpression, EqualityExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// EqualityExpression
//------------------------------------------------------------------------------

EqualityExpression::EqualityExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

EqualityExpression::~EqualityExpression()
{
}

bool EqualityExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
    {
        operators.push_back( Lexer::EQUALITY );
        operators.push_back( Lexer::NOT_EQUALITY );
    }

    return ParseLeftAssociative<EqualityExpression, RelationalExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// RelationalExpression
//------------------------------------------------------------------------------

RelationalExpression::RelationalExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

RelationalExpression::~RelationalExpression()
{
}

bool RelationalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
    {
        operators.push_back( Lexer::LESS_THAN );
        operators.push_back( Lexer::GREATER_THAN );
        operators.push_back( Lexer::LESS_THAN_EQUALS );
        operators.push_back( Lexer::GREATER_THAN_EQUALS );
    }

    return ParseLeftAssociative<RelationalExpression, ShiftExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// ShiftExpression
//------------------------------------------------------------------------------

ShiftExpression::ShiftExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

ShiftExpression::~ShiftExpression()
{
}

bool ShiftExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
    {
        operators.push_back( Lexer::LEFT_SHIFT );
        operators.push_back( Lexer::RIGHT_SHIFT );
    }

    return ParseLeftAssociative<ShiftExpression, AdditiveExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// AdditiveExpression
//------------------------------------------------------------------------------

AdditiveExpression::AdditiveExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

AdditiveExpression::~AdditiveExpression()
{
}

bool AdditiveExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
    {
        operators.push_back( Lexer::PLUS );
        operators.push_back( Lexer::MINUS );
    }

    return ParseLeftAssociative<AdditiveExpression, MultiplicativeExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// MultiplicativeExpression
//------------------------------------------------------------------------------

MultiplicativeExpression::MultiplicativeExpression( Lexer::TerminalType operator_terminal,
                                          std::unique_ptr<Expression> left_side,
                                          std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

MultiplicativeExpression::~MultiplicativeExpression()
{
}

bool MultiplicativeExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    static std::vector<Lexer::TerminalType> operators;
    if( operators.size() == 0 )
    {
        operators.push_back( Lexer::MULTIPLY );
        operators.push_back( Lexer::DIVIDE );
        operators.push_back( Lexer::MODULO );
    }

    //TODO
    return ParseLeftAssociative<MultiplicativeExpression, CastExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// CastExpression
//------------------------------------------------------------------------------

CastExpression::CastExpression()
{
}

CastExpression::~CastExpression()
{
}

void CastExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Cast Expression\n";
}

//TODO
bool CastExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return Expect<UnaryExpression>( parser, token );
}

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression( std::unique_ptr<UnaryOperator> unary_operator,
                                  std::unique_ptr<Expression> unary_expression )
    :m_unaryOperator( std::move( unary_operator ) )
    ,m_unaryExpression( std::move( unary_expression ) )
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
    std::unique_ptr<UnaryOperator> unary_operator;
    if( Expect<UnaryOperator>( parser, unary_operator ) )
    {
        std::unique_ptr<Expression> unary_expression;
        if( !Expect<UnaryExpression>( parser, unary_expression ) )
            return false;
        token.reset( new UnaryExpression( std::move( unary_operator ),
                                          std::move( unary_expression ) ) );
        return true;
    }

    return Expect<PostfixExpression>( parser, token );
}

//------------------------------------------------------------------------------
// UnaryOperator
//------------------------------------------------------------------------------

UnaryOperator::UnaryOperator( Lexer::TerminalType terminal_type )
    :m_terminalType( terminal_type )
{
}

UnaryOperator::~UnaryOperator()
{
}

void UnaryOperator::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << GetTerminalString( m_terminalType ) << std::endl;
}

bool UnaryOperator::Parse( Parser& parser, std::unique_ptr<UnaryOperator>& token )
{
    static Lexer::TerminalType s_unary_operator_terminals[] =
    {
        Lexer::INCREMENT,
        Lexer::DECREMENT,
        Lexer::PLUS,
        Lexer::MINUS,
        Lexer::BITWISE_NOT,
        Lexer::LOGICAL_NOT
    };

    for( Lexer::TerminalType unary_operator_terminal : s_unary_operator_terminals )
    {
        if( parser.ExpectTerminal( unary_operator_terminal ) )
        {
            token.reset( new UnaryOperator( unary_operator_terminal ) );
            return true;
        }
    }
    return false;
}

//------------------------------------------------------------------------------
// Postfix Expression
//------------------------------------------------------------------------------

PostfixExpression::PostfixExpression()
{
}

PostfixExpression::~PostfixExpression()
{
}

void PostfixExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Postfix Expression\n";
}

//TODO
bool PostfixExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return Expect<PrimaryExpression>( parser, token );
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
