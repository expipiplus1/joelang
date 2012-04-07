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
#include <sstream>
#include <string>
#include <utility>

#include <llvm/DerivedTypes.h>
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Support/IRBuilder.h>

#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>
#include <parser/code_generator.hpp>
#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>
#include <parser/tokens/token.hpp>

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

llvm::Value* Expression::CodeGen( CodeGenerator& code_generator ) const
{
    return nullptr;
}

Type Expression::GetReturnType() const
{
    return Type::UNKNOWN_TYPE;
}

bool Expression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return Expect<AssignmentExpression>( parser, token );
}

//------------------------------------------------------------------------------
// Assignment Expression
//------------------------------------------------------------------------------

AssignmentExpression::AssignmentExpression( std::unique_ptr<Expression> unary_expression,
                                            std::unique_ptr<AssignmentOperator> assignment_operator,
                                            std::unique_ptr<Expression> assignment_expression )
    :m_assignee( std::move( unary_expression ) )
    ,m_assignmentOperator( std::move( assignment_operator ) )
    ,m_assignment( std::move( assignment_expression ) )
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

    m_assignee->Print( depth + 1 );
    m_assignmentOperator->Print( depth + 1 );
    m_assignment->Print( depth + 1 );
}

Type AssignmentExpression::GetReturnType() const
{
    return m_assignee->GetReturnType();
}

bool AssignmentExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<Expression> lhs_expression;
    if( !Expect< ConditionalExpression >( parser, lhs_expression ) )
        return false;

    if( typeid( *lhs_expression ) != typeid( IdentifierExpression ) &&
        typeid( *lhs_expression ) != typeid( PrimaryExpression ) &&
        typeid( *lhs_expression ) != typeid( UnaryExpression ) &&
        typeid( *lhs_expression ) != typeid( PostfixExpression ) )
    {
        token = std::move( lhs_expression );
        return true;
    }

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !Expect< AssignmentOperator >( parser, assignment_operator ) )
    {
        CHECK_PARSER;

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

llvm::Value* ConditionalExpression::CodeGen( CodeGenerator& code_generator ) const
{
    return code_generator.CreateSelect( *m_condition,
                                        *m_trueExpression,
                                        *m_falseExpression );
}

Type ConditionalExpression::GetReturnType() const
{
    return GetCommonType( m_trueExpression->GetReturnType(),
                          m_falseExpression->GetReturnType() );
}

bool ConditionalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<Expression> condition;
    if( !Expect< LogicalOrExpression >( parser, condition ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::QUERY ) )
    {
        CHECK_PARSER;

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
    std::cout << GetTerminalString( m_operatorTerminal ) << std::endl;
    m_leftSide->Print( depth + 1 );
    m_rightSide->Print( depth + 1 );
}

llvm::Value* BinaryOperatorExpression::CodeGen( CodeGenerator& code_generator ) const
{
    //CHECK_CODE_GENERATOR;
    switch( m_operatorTerminal )
    {
        case Lexer::LOGICAL_OR:
            return code_generator.CreateLOr( *m_leftSide,
                                             *m_rightSide );
        case Lexer::LOGICAL_AND:
            return code_generator.CreateLAnd( *m_leftSide,
                                              *m_rightSide );
        case Lexer::INCLUSIVE_OR:
            return code_generator.CreateOr( *m_leftSide,
                                            *m_rightSide );
        case Lexer::EXCLUSIVE_OR:
            return code_generator.CreateXor( *m_leftSide,
                                             *m_rightSide );
        case Lexer::AND:
            return code_generator.CreateAnd( *m_leftSide,
                                             *m_rightSide );
        case Lexer::EQUALITY:
            return code_generator.CreateEq( *m_leftSide,
                                            *m_rightSide );
        case Lexer::NOT_EQUALITY:
            return code_generator.CreateNeq( *m_leftSide,
                                             *m_rightSide );
        case Lexer::LESS_THAN:
            return code_generator.CreateLT( *m_leftSide,
                                            *m_rightSide );
        case Lexer::GREATER_THAN:
            return code_generator.CreateGT( *m_leftSide,
                                            *m_rightSide );
        case Lexer::LESS_THAN_EQUALS:
            return code_generator.CreateLTE( *m_leftSide,
                                             *m_rightSide );
        case Lexer::GREATER_THAN_EQUALS:
            return code_generator.CreateGTE( *m_leftSide,
                                             *m_rightSide );
        case Lexer::LEFT_SHIFT:
            return code_generator.CreateShl( *m_leftSide,
                                             *m_rightSide );
        case Lexer::RIGHT_SHIFT:
            return code_generator.CreateShr( *m_leftSide,
                                             *m_rightSide );
        case Lexer::PLUS:
            return code_generator.CreateAdd( *m_leftSide,
                                             *m_rightSide );
        case Lexer::MINUS:
            return code_generator.CreateSub( *m_leftSide,
                                             *m_rightSide );
        case Lexer::MULTIPLY:
            return code_generator.CreateMul( *m_leftSide,
                                             *m_rightSide );
        case Lexer::DIVIDE:
            return code_generator.CreateDiv( *m_leftSide,
                                             *m_rightSide );
        case Lexer::MODULO:
            return code_generator.CreateMod( *m_leftSide,
                                             *m_rightSide );
        default:
            return nullptr;
    }
}

Type BinaryOperatorExpression::GetReturnType() const
{
    return GetCommonType( m_leftSide->GetReturnType(),
                          m_rightSide->GetReturnType() );
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
//                                   const std::vector<Lexer::TerminalType>& operator_terminals );

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

Type LogicalOrExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalOrExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    // Why doesn't clang support initializer lists!
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::LOGICAL_OR
    };

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

Type LogicalAndExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalAndExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::LOGICAL_AND
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::INCLUSIVE_OR
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::EXCLUSIVE_OR
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::AND
    };

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

Type EqualityExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool EqualityExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::EQUALITY,
        Lexer::NOT_EQUALITY
    };

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

Type RelationalExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool RelationalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::LESS_THAN,
        Lexer::GREATER_THAN,
        Lexer::LESS_THAN_EQUALS,
        Lexer::GREATER_THAN_EQUALS
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::LEFT_SHIFT,
        Lexer::RIGHT_SHIFT
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::PLUS,
        Lexer::MINUS
    };

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
    const static std::vector<Lexer::TerminalType> operators =
    {
        Lexer::MULTIPLY,
        Lexer::DIVIDE,
        Lexer::MODULO
    };

    return ParseLeftAssociative<MultiplicativeExpression, CastExpression>( parser, token, operators );
}

//------------------------------------------------------------------------------
// CastExpression
//------------------------------------------------------------------------------

CastExpression::CastExpression( Type cast_type )
    :m_castType( cast_type )
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
Type CastExpression::GetReturnType() const
{
    return m_castType;
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
                                  std::unique_ptr<Expression> expression )
    :m_unaryOperator( std::move( unary_operator ) )
    ,m_expression( std::move( expression ) )
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
    m_unaryOperator->Print( depth + 1 );
    m_expression->Print( depth + 1 );
}

Type UnaryExpression::GetReturnType() const
{
    if( m_unaryOperator->GetTerminalType() == Lexer::LOGICAL_NOT )
        return Type::BOOL;
    return m_expression->GetReturnType();
}

llvm::Value* UnaryExpression::CodeGen( CodeGenerator& code_generator ) const
{
    //TODO
    switch( m_unaryOperator->GetTerminalType() )
    {
        case Lexer::PLUS:
            return m_expression->CodeGen( code_generator );
        case Lexer::MINUS:
            return code_generator.CreateNeg( *m_expression );
        case Lexer::BITWISE_NOT:
            return code_generator.CreateNot( *m_expression );
        case Lexer::LOGICAL_NOT:
            return code_generator.CreateLNot( *m_expression );
        default:
            return nullptr;
    }
}

bool UnaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<UnaryOperator> unary_operator;
    if( Expect<UnaryOperator>( parser, unary_operator ) )
    {
        std::unique_ptr<Expression> expression;
        if( unary_operator->GetTerminalType() == Lexer::INCREMENT ||
            unary_operator->GetTerminalType() == Lexer::DECREMENT )
        {
            if( !Expect<UnaryExpression>( parser, expression ) )
                return false;
        }
        else
        {
            if( !Expect<CastExpression>( parser, expression ) )
                return false;
        }

        token.reset( new UnaryExpression( std::move( unary_operator ),
                                          std::move( expression ) ) );
        return true;
    }

    CHECK_PARSER;

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

Lexer::TerminalType UnaryOperator::GetTerminalType() const
{
    return m_terminalType;
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
// PostfixExpression
//------------------------------------------------------------------------------

PostfixExpression::PostfixExpression( std::unique_ptr<Expression> expression,
                                      std::unique_ptr<PostfixOperator> postfix_operator )
    :m_expression( std::move( expression ) )
    ,m_postfixOperator( std::move( postfix_operator ) )
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
    m_expression->Print( depth + 1 );
    m_postfixOperator->Print( depth + 1 );
}

Type PostfixExpression::GetReturnType() const
{
    //TODO
    assert(false && "complete me");
    return Type::UNKNOWN_TYPE;
}

bool PostfixExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<Expression> primary_expression;
    if( !Expect<PrimaryExpression>( parser, primary_expression ) )
        return false;

    std::vector< std::unique_ptr<PostfixOperator> > operators;
    if( !ExpectSequenceOf<PostfixOperator>( parser, operators ) )
    {
        CHECK_PARSER;

        token = std::move( primary_expression );
        return true;
    }

    std::unique_ptr<Expression> postfix_expression = std::move( primary_expression );
    for( auto& postfix_operator : operators )
    {
        postfix_expression.reset( new PostfixExpression( std::move( postfix_expression ),
                                                         std::move( postfix_operator ) ) );
    }

    token = std::move( postfix_expression );
    return true;
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

bool PostfixOperator::Parse( Parser& parser, std::unique_ptr<PostfixOperator>& token )
{
    std::unique_ptr<Token> t;
    if( !ExpectAnyOf<SubscriptOperator,
                     ArgumentListOperator,
                     MemberAccessOperator,
                     IncrementalOperator>( parser, t ) )
        return false;

    Token* p = t.release();
    token.reset( dynamic_cast<PostfixOperator*>( p ) );
    if( !token )
    {
        delete p;
        return false;
    }
    return true;
}

//------------------------------------------------------------------------------
// SubscriptOperator
//------------------------------------------------------------------------------

SubscriptOperator::SubscriptOperator( std::unique_ptr<Expression> expression )
    :m_expression( std::move( expression ) )
{
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

bool SubscriptOperator::Parse( Parser& parser, std::unique_ptr<SubscriptOperator>& token )
{
    if( !parser.ExpectTerminal( Lexer::OPEN_SQUARE ) )
        return false;

    std::unique_ptr<Expression> expression;
    if( !Expect<Expression>( parser, expression ) )
        return false;

    if( !parser.ExpectTerminal( Lexer::CLOSE_SQUARE ) )
        return false;

    token.reset( new SubscriptOperator( std::move( expression ) ) );
    return true;
}

//------------------------------------------------------------------------------
// ArgumentListOperator
//------------------------------------------------------------------------------

ArgumentListOperator::ArgumentListOperator(
        std::vector< std::unique_ptr<Expression> > argument_expressions )
    :m_argumentExpressions( std::move( argument_expressions ) )
{
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

bool ArgumentListOperator::Parse( Parser& parser, std::unique_ptr<ArgumentListOperator>& token )
{
    if( !parser.ExpectTerminal( Lexer::OPEN_ROUND ) )
        return false;

    std::vector< std::unique_ptr<Expression> > argument_expressions;

    std::unique_ptr<Expression> argument;
    if( Expect<AssignmentExpression>( parser, argument ) )
    {
        argument_expressions.push_back( std::move( argument ) );
        while( parser.ExpectTerminal( Lexer::COMMA ) )
        {
            if( !Expect<AssignmentExpression>( parser, argument ) )
                return false;
            argument_expressions.push_back( std::move( argument ) );
        }
    }

    CHECK_PARSER;

    if( !parser.ExpectTerminal( Lexer::CLOSE_ROUND ) )
        return false;

    token.reset( new ArgumentListOperator( std::move( argument_expressions ) ) );
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

bool MemberAccessOperator::Parse( Parser& parser, std::unique_ptr<MemberAccessOperator>& token )
{
    if( !parser.ExpectTerminal( Lexer::PERIOD ) )
        return false;

    std::string identifier;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, identifier ) )
        return false;

    token.reset( new MemberAccessOperator( std::move( identifier ) ) );
    return true;
}

//------------------------------------------------------------------------------
// IncrementalOperator
//------------------------------------------------------------------------------

IncrementalOperator::IncrementalOperator( Lexer::TerminalType terminal_type )
    :m_terminalType( terminal_type )
{
}

IncrementalOperator::~IncrementalOperator()
{
}

void IncrementalOperator::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << GetTerminalString( m_terminalType ) << std::endl;
}

bool IncrementalOperator::Parse( Parser& parser, std::unique_ptr<IncrementalOperator>& token )
{
    if( parser.ExpectTerminal( Lexer::INCREMENT ) )
    {
        token.reset( new IncrementalOperator( Lexer::INCREMENT ) );
        return true;
    }
    if( parser.ExpectTerminal( Lexer::DECREMENT ) )
    {
        token.reset( new IncrementalOperator( Lexer::DECREMENT ) );
        return true;
    }
    return false;
}

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

PrimaryExpression::PrimaryExpression()
{
}

PrimaryExpression::~PrimaryExpression()
{
}

void PrimaryExpression::Print( int depth ) const
{
}

bool PrimaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    //TODO this may require some more logic for handling arrays, functions and structs
    if( Expect<IdentifierExpression>( parser, token ) )
        return true;
    CHECK_PARSER;

    if( Expect<LiteralExpression>( parser, token ) )
        return true;
    CHECK_PARSER;

    if( !parser.ExpectTerminal( Lexer::OPEN_ROUND ) )
        return false;
    if( !Expect<Expression>( parser, token ) )
        return false;
    if( !parser.ExpectTerminal( Lexer::CLOSE_ROUND ) )
        return false;

    return true;
}

//------------------------------------------------------------------------------
// IdentifierExpression
//------------------------------------------------------------------------------

IdentifierExpression::IdentifierExpression( std::string identifier )
    :m_identifier( std::move( identifier ) )
{
}

IdentifierExpression::~IdentifierExpression()
{
}

void IdentifierExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_identifier << "\n";
}

Type IdentifierExpression::GetReturnType() const
{
    assert( false && "complete me" );
    return Type::UNKNOWN_TYPE;
}

bool IdentifierExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( Expect<ConstantValueExpression>( parser, token ) )
        return true;
    CHECK_PARSER;

    std::string identifier;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, identifier ) )
        return false;

    token.reset( new IdentifierExpression( identifier ) );
    return true;
}

//------------------------------------------------------------------------------
// ConstantValueExpression
//------------------------------------------------------------------------------

ConstantValueExpression::ConstantValueExpression( std::shared_ptr<LiteralExpression> value )
    :m_value( std::move( value ) )
{
}

ConstantValueExpression::~ConstantValueExpression()
{
}

void ConstantValueExpression::Print( int depth ) const
{
    m_value->Print( depth );
}

llvm::Value* ConstantValueExpression::CodeGen( CodeGenerator& code_generator ) const
{
    return m_value->CodeGen( code_generator );
}

Type ConstantValueExpression::GetReturnType() const
{
    return m_value->GetReturnType();
}

bool ConstantValueExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::string identifier;
    if( !parser.ExpectTerminal( Lexer::IDENTIFIER, identifier ) )
        return false;

    //TODO undeclared identifier error reporting somewhere here
    //This will probably have to be folded into IdentifierExpression
    std::shared_ptr<LiteralExpression> e;
    if( !parser.GetSymbolTable().GetConstant( identifier, e ) )
        return false;

    token.reset( new ConstantValueExpression( e ) );
    return true;
}

//------------------------------------------------------------------------------
// LiteralExpression
//------------------------------------------------------------------------------

LiteralExpression::LiteralExpression()
{
}

LiteralExpression::~LiteralExpression()
{
}

//TODO
bool LiteralExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    std::unique_ptr<Token> t;
    if( !ExpectAnyOf<FloatingLiteralExpression,
                     IntegralLiteralExpression,
                     BooleanLiteralExpression,
                     //CharacterLiteralExpression,
                     StringLiteralExpression
        >( parser, t ) )
        return false;

    Token* p = t.release();
    token.reset( dynamic_cast<LiteralExpression*>( p ) );
    if( !token )
    {
        delete p;
        return false;
    }
    return true;
}

//------------------------------------------------------------------------------
// IntegralLiteralExpression
//------------------------------------------------------------------------------

IntegralLiteralExpression::IntegralLiteralExpression( long long value )
    :m_value( value )
{
}

IntegralLiteralExpression::~IntegralLiteralExpression()
{
}

void IntegralLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

llvm::Value* IntegralLiteralExpression::CodeGen( CodeGenerator& code_generator ) const
{
    //TODO different sizes of int?
    return llvm::ConstantInt::get( llvm::Type::getInt64Ty( code_generator.GetLLVMContext() ),
                                   m_value );
}

Type IntegralLiteralExpression::GetReturnType() const
{
    //TODO
    return Type::I64;
}

bool IntegralLiteralExpression::Parse( Parser& parser, std::unique_ptr<IntegralLiteralExpression>& token )
{
    std::string string;
    if( !parser.ExpectTerminal( Lexer::INTEGER_LITERAL, string ) )
        return false;

    long long value;
    if( string[0] == '0' )
    {
        if( string.size() > 2 &&
            ( string[1] == 'x' || string[1] == 'X' ) )
        {
            std::istringstream i( string.substr(2) );
            if( !( i >> std::hex >> value ) )
                return false;
        }
        else
        {
            std::istringstream i( string );
            if( !( i >> std::oct >> value ) )
                return false;
        }
    }
    else
    {
        std::istringstream i( string );
        if( !( i >> value ) )
            return false;
    }

    token.reset( new IntegralLiteralExpression( value ) );
    return true;
}

//------------------------------------------------------------------------------
// FloatingLiteralExpression
//------------------------------------------------------------------------------

FloatingLiteralExpression::FloatingLiteralExpression( double value, bool double_precision )
    :m_value( value )
    ,m_doublePrecision( double_precision )
{
}

FloatingLiteralExpression::~FloatingLiteralExpression()
{
}

void FloatingLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

llvm::Value* FloatingLiteralExpression::CodeGen( CodeGenerator& code_generator ) const
{
    if( m_doublePrecision )
        return llvm::ConstantFP::get( llvm::Type::getDoubleTy( code_generator.GetLLVMContext() ),
                                      m_value );
    else
        return llvm::ConstantFP::get( llvm::Type::getFloatTy( code_generator.GetLLVMContext() ),
                                      m_value );
}

Type FloatingLiteralExpression::GetReturnType() const
{
    if( m_doublePrecision )
        return Type::DOUBLE;
    else
        return Type::FLOAT;
}

bool FloatingLiteralExpression::Parse( Parser& parser, std::unique_ptr<FloatingLiteralExpression>& token )
{
    std::string string;
    if( !parser.ExpectTerminal( Lexer::FLOATING_LITERAL, string ) )
        return false;

    double value;
    std::istringstream i( string );
    if( !( i >> value ) )
        return false;

    bool double_precision = *string.rbegin() == 'f' ||
                            *string.rbegin() == 'F';

    token.reset( new FloatingLiteralExpression( value, double_precision ) );
    return true;
}

//------------------------------------------------------------------------------
// BooleanLiteralExpression
//------------------------------------------------------------------------------

BooleanLiteralExpression::BooleanLiteralExpression( bool value )
    :m_value( value )
{
}

BooleanLiteralExpression::~BooleanLiteralExpression()
{
}

void BooleanLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

llvm::Value* BooleanLiteralExpression::CodeGen( CodeGenerator& code_generator ) const
{
    if( m_value )
        return llvm::ConstantInt::getTrue( code_generator.GetLLVMContext() );
    else
        return llvm::ConstantInt::getFalse( code_generator.GetLLVMContext() );
}

Type BooleanLiteralExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool BooleanLiteralExpression::Parse( Parser& parser, std::unique_ptr<BooleanLiteralExpression>& token )
{
    if( parser.ExpectTerminal( Lexer::TRUE ) )
    {
        token.reset( new BooleanLiteralExpression( true ) );
        return true;
    }

    if( parser.ExpectTerminal( Lexer::FALSE ) )
    {
        token.reset( new BooleanLiteralExpression( false ) );
        return true;
    }

    return false;
}

//------------------------------------------------------------------------------
// StringLiteralExpression
//------------------------------------------------------------------------------

StringLiteralExpression::StringLiteralExpression( std::string value )
    :m_value( std::move(value) )
{
}

StringLiteralExpression::~StringLiteralExpression()
{
}

void StringLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

const std::string& StringLiteralExpression::GetString() const
{
    return m_value;
}

Type StringLiteralExpression::GetReturnType() const
{
    return Type::STRING;
}

bool StringLiteralExpression::Parse( Parser& parser, std::unique_ptr<StringLiteralExpression>& token )
{
    std::string string;
    if( !parser.ExpectTerminal( Lexer::STRING_LITERAL, string ) )
        return false;

    token.reset( new StringLiteralExpression( Unescape(string) ) );
    return true;
}

std::string StringLiteralExpression::Unescape( const std::string& string )
{
    assert( string.size() >= 2 && "string to unescape is too short" );
    assert( *string.begin()  == '\"' && "string doesn't start with a \"" );
    assert( *string.rbegin() == '\"' && "string doesn't end with a \"" );
    // We can assume that the string is well formatted
    std::string::const_iterator p = string.begin() + 1;
    std::string ret;
    while( p != string.end() - 1 )
    {
        if( *p == '\\' )
        {
            char c;
            ++p;
            switch( *p )
            {
            case '\'':
                c = '\'';
                break;
            case '\"':
                c = '\"';
                break;
            case '\?':
                c = '\?';
                break;
            case '\\':
                c = '\\';
                break;
            case '\a':
                c = '\a';
                break;
            case '\b':
                c = '\b';
                break;
            case '\f':
                c = '\f';
                break;
            case '\n':
                c = '\n';
                break;
            case '\r':
                c = '\r';
                break;
            case '\t':
                c = '\t';
                break;
            case '\v':
                c = '\v';
                break;
            default:
                //TODO warning here
                c = *p;
            }
            ret += c;
        }
        ret += *p;
        ++p;
    }
    return ret;
}

} // namespace Parser
} // namespace JoeLang
