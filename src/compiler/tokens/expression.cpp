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
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Expression
//------------------------------------------------------------------------------

Expression::Expression( ExpressionTy sub_class_id )
    :m_subClassID( sub_class_id )
{
}

Expression::~Expression()
{
}

void Expression::FoldConstants( std::unique_ptr<Expression>& self )
{
}

bool Expression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    // TODO comma sep expressions
    return parser.Expect<AssignmentExpression>( token );
}

Expression::ExpressionTy Expression::GetSubClassID() const
{
    return m_subClassID;
}

bool Expression::classof( const Expression* e )
{
    // An Expression is always an Expression
    return true;
}

//------------------------------------------------------------------------------
// Assignment Expression
//------------------------------------------------------------------------------

AssignmentExpression::AssignmentExpression(
                        std::unique_ptr<Expression> assignee,
                        std::unique_ptr<AssignmentOperator> assignment_operator,
                        std::unique_ptr<Expression> assigned_expression )
    :Expression( ExpressionTy::AssignmentExpression )
    ,m_assignee          ( std::move(assignee) )
    ,m_assignmentOperator( std::move(assignment_operator) )
    ,m_assignedExpression( std::move(assigned_expression) )
{
    assert( m_assignee &&
            "AssignmentExpression given an invalid assignee" );
    assert( m_assignmentOperator &&
            "AssignmentExpression given an invalid assignment operator" );
    assert( m_assignedExpression &&
            "AssignmentExpression given an invalid assigned expression" );
}

AssignmentExpression::~AssignmentExpression()
{
}

void AssignmentExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_assignee->ResolveIdentifiers( sema );
    m_assignedExpression->ResolveIdentifiers( sema );
}

void AssignmentExpression::PerformSema( SemaAnalyzer& sema )
{
    m_assignedExpression = CastExpression::Create(
                                              m_assignee->GetReturnType(),
                                              std::move(m_assignedExpression) );

    m_assignee->PerformSema( sema );
    m_assignedExpression->PerformSema( sema );
}

Type AssignmentExpression::GetReturnType() const
{
    return m_assignee->GetReturnType();
}

void AssignmentExpression::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Assignment Expression\n";

    m_assignee->Print( depth + 1 );
    m_assignmentOperator->Print( depth + 1 );
    m_assignedExpression->Print( depth + 1 );
}

bool AssignmentExpression::Parse( Parser& parser,
                                  std::unique_ptr<Expression>& token )
{
    // TODO clean this up
    std::unique_ptr<Expression> lhs_expression;
    if( !parser.Expect< ConditionalExpression >( lhs_expression ) )
        return false;

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !parser.Expect< AssignmentOperator >( assignment_operator ) )
    {
        CHECK_PARSER;

        token = std::move( lhs_expression );
        return true;
    }

    std::unique_ptr<Expression> assignment_expression;
    if( !parser.Expect< AssignmentExpression >( assignment_expression ) )
        return false;

    token.reset( new AssignmentExpression( std::move(lhs_expression),
                                           std::move(assignment_operator),
                                           std::move(assignment_expression) ) );
    return true;
}

bool AssignmentExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::AssignmentExpression;
}

bool AssignmentExpression::classof( const AssignmentExpression* e )
{
    return true;
}

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
// ConditionalExpression
//------------------------------------------------------------------------------

ConditionalExpression::ConditionalExpression(
                                  std::unique_ptr<Expression> condition,
                                  std::unique_ptr<Expression> true_expression,
                                  std::unique_ptr<Expression> false_expression )
    :Expression( ExpressionTy::ConditionalExpression )
    ,m_condition( std::move(condition) )
    ,m_trueExpression( std::move(true_expression) )
    ,m_falseExpression( std::move(false_expression) )
{
    assert( m_condition &&
            "ConditionalExpression given a null condition" );
    assert( m_trueExpression &&
            "ConditionalExpression given a true_expression" );
    assert( m_falseExpression &&
            "ConditionalExpression given a null false_expression" );
}

ConditionalExpression::~ConditionalExpression()
{
}

void ConditionalExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_condition->ResolveIdentifiers( sema );
    m_trueExpression->ResolveIdentifiers( sema );
    m_falseExpression->ResolveIdentifiers( sema );
}

void ConditionalExpression::PerformSema( SemaAnalyzer& sema )
{
    //TODO constant folding

    m_condition = CastExpression::Create( Type::BOOL, std::move(m_condition) );

    Type t = GetReturnType();
    if( t == Type::UNKNOWN_TYPE )
        sema.Error( "Incompatable operand types" );
    else
    {
        m_trueExpression = CastExpression::Create(
                                                t,
                                                std::move(m_trueExpression) );
        m_falseExpression = CastExpression::Create(
                                                t,
                                                std::move(m_falseExpression) );
    }

    m_condition->PerformSema( sema );
    m_trueExpression->PerformSema( sema );
    m_falseExpression->PerformSema( sema );
}

void ConditionalExpression::FoldConstants( std::unique_ptr<Expression>& self )
{
    // Simplify the sub expressions
    m_condition->FoldConstants( m_condition );
    m_trueExpression->FoldConstants( m_trueExpression );
    m_falseExpression->FoldConstants( m_falseExpression );

    // See if the condition is a boolean literal
    LiteralExpression* l = nullptr;
    if( isa<LiteralExpression>( m_condition ) )
    {
        l = static_cast<LiteralExpression*>( m_condition.get() );
    }
    else if( isa<IdentifierExpression>( m_condition ) )
    {
        IdentifierExpression* i =
                        static_cast<IdentifierExpression*>( m_condition.get() );
        const std::shared_ptr<Expression>& read_expression = i->GetReadExpression();
        if( isa<LiteralExpression>( read_expression ) )
            l = static_cast<LiteralExpression*>( read_expression.get() );
    }

    // If it wasn't constant, then we've done as much as we can
    if( !l )
        return;

    // If it was constant, we can replace this node with just one of the values
    const GenericValue v = l->GetValue();
    assert( v.GetType() == Type::BOOL &&
            "ConditionalExpression has a non-bool condition" );

    self = v.GetBool() ? std::move(m_trueExpression)
                       : std::move(m_falseExpression);
}

Type ConditionalExpression::GetReturnType() const
{
    return GetCommonType( m_trueExpression->GetReturnType(),
                          m_falseExpression->GetReturnType() );
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

bool ConditionalExpression::Parse( Parser& parser,
                                   std::unique_ptr<Expression>& token )
{
    // Try and parse the condition
    std::unique_ptr<Expression> condition;
    if( !parser.Expect< LogicalOrExpression >( condition ) )
        return false;

    // If the next terminal isn't QUERY then just return the LogicalOrExpression
    if( !parser.ExpectTerminal( TerminalType::QUERY ) )
    {
        token = std::move( condition );
        return true;
    }

    // We've seen the QUERY do must parse the rest of the ternary expression
    std::unique_ptr<Expression> true_expression;
    if( !parser.Expect<Expression>( true_expression ) )
        return false;

    if( !parser.ExpectTerminal( TerminalType::COLON ) )
        return false;

    std::unique_ptr<Expression> false_expression;
    if( !parser.Expect<AssignmentExpression>( false_expression ) )
        return false;

    token.reset( new ConditionalExpression( std::move(condition),
                                            std::move(true_expression),
                                            std::move(false_expression) ) );
    return true;
}

bool ConditionalExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::ConditionalExpression;
}

bool ConditionalExpression::classof( const ConditionalExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// BinaryOperatorExpression
//------------------------------------------------------------------------------

BinaryOperatorExpression::BinaryOperatorExpression(
                                        ExpressionTy sub_class_id,
                                        Op op,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :Expression( sub_class_id )
    ,m_operator( op )
    ,m_leftSide( std::move(left_side) )
    ,m_rightSide( std::move(right_side) )
{
    assert( m_leftSide && "BinaryOperatorExpression given a null lhs" );
    assert( m_rightSide && "BinaryOperatorExpression given a null rhs" );
}

BinaryOperatorExpression::~BinaryOperatorExpression()
{
}

void BinaryOperatorExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_leftSide->ResolveIdentifiers( sema );
    m_rightSide->ResolveIdentifiers( sema );
}

void BinaryOperatorExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = GetReturnType();
    if( t == Type::UNKNOWN_TYPE )
        sema.Error( "Invalid operands to binary operator"
                    + GetTypeString( m_leftSide->GetReturnType() )
                    + GetTypeString( m_rightSide->GetReturnType() ) );
    else
    {
        m_leftSide  = CastExpression::Create( t, std::move(m_leftSide) );
        m_rightSide = CastExpression::Create( t, std::move(m_rightSide) );
    }

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

Type BinaryOperatorExpression::GetReturnType() const
{
    return GetCommonType( m_leftSide->GetReturnType(),
                          m_rightSide->GetReturnType() );
}

void BinaryOperatorExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "binary_operator" << std::endl;
    m_leftSide->Print( depth + 1 );
    m_rightSide->Print( depth + 1 );
}

template< typename ExpressionType, typename SubExpressionType >
bool BinaryOperatorExpression::ParseLeftAssociative( Parser& parser,
                                  std::unique_ptr<Expression>& token,
                                  const OperatorTerminalMap& op_terminal_map )
{
    // Try and parse the sub expression for the left side
    std::unique_ptr<Expression> left;
    if( !parser.Expect<SubExpressionType>( left ) )
        return false;

    // A vector of operators and the next expression
    std::vector< std::pair< Op,
                            std::unique_ptr<Expression> > > rest;
    while( true )
    {
        bool cont = false;
        Op op;
        // Try and match any of the operators
        for( const auto& p : op_terminal_map )
        {
            if( parser.ExpectTerminal( p.first ) )
            {
                op = p.second;
                cont = true;
                break;
            }
        }

        // if we didn't match any of them go and return what we've got
        if( !cont )
            break;

        // We have an operator, there must be an expression here
        std::unique_ptr<Expression> next;
        if( !parser.Expect<SubExpressionType>( next ) )
            return false;

        // Push this operator and expression to the list
        rest.push_back( std::make_pair( op,
                                        std::move( next ) ) );
    }

    // for every operator+expression we have, set the lhs of the new left to
    // the old left and its operator to op and the next subexpression to the rhs
    for( auto& expression : rest )
        left.reset( new ExpressionType( expression.first,
                                        std::move(left),
                                        std::move(expression.second) ) );

    token = std::move( left );
    return true;
}

bool BinaryOperatorExpression::classof( const Expression* e )
{
    return e->GetSubClassID() > ExpressionTy::BinaryOperatorExpression_Start &&
           e->GetSubClassID() < ExpressionTy::BinaryOperatorExpression_End;
}

bool BinaryOperatorExpression::classof( const BinaryOperatorExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// Logical Or Expression
//------------------------------------------------------------------------------

LogicalOrExpression::LogicalOrExpression(
                                        Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::LogicalOrExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

LogicalOrExpression::~LogicalOrExpression()
{
}

void LogicalOrExpression::PerformSema( SemaAnalyzer& sema )
{
    m_leftSide = CastExpression::Create( Type::BOOL, std::move(m_leftSide) );
    m_rightSide = CastExpression::Create( Type::BOOL, std::move(m_rightSide) );

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

Type LogicalOrExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalOrExpression::Parse( Parser& parser,
                                 std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::LOGICAL_OR, Op::LOGICAL_OR }
    };

    return ParseLeftAssociative<LogicalOrExpression, LogicalAndExpression>
            ( parser, token, ops );
}

bool LogicalOrExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::LogicalOrExpression;
}

bool LogicalOrExpression::classof( const LogicalOrExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// LogicalAndExpression
//------------------------------------------------------------------------------

LogicalAndExpression::LogicalAndExpression(
                                        Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::LogicalAndExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

LogicalAndExpression::~LogicalAndExpression()
{
}

void LogicalAndExpression::PerformSema( SemaAnalyzer& sema )
{
    m_leftSide = CastExpression::Create( Type::BOOL, std::move(m_leftSide) );
    m_rightSide = CastExpression::Create( Type::BOOL, std::move(m_rightSide) );

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

Type LogicalAndExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalAndExpression::Parse( Parser& parser,
                                  std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::LOGICAL_AND, Op::LOGICAL_AND }
    };

    return ParseLeftAssociative<LogicalAndExpression, InclusiveOrExpression>
            ( parser, token, ops );
}

bool LogicalAndExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::LogicalAndExpression;
}

bool LogicalAndExpression::classof( const LogicalAndExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// InclusiveOrExpression
//------------------------------------------------------------------------------

InclusiveOrExpression::InclusiveOrExpression(
                                        Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::InclusiveOrExpression,
                               operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

InclusiveOrExpression::~InclusiveOrExpression()
{
}

void InclusiveOrExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = GetReturnType();

    if( !IsIntegral(t) )
        sema.Error( "Invalid operand types to to inclusive or operator: "
                    + GetTypeString( m_leftSide->GetReturnType() )
                    + GetTypeString( m_rightSide->GetReturnType() ) );
    else
    {
        m_leftSide  = CastExpression::Create( t, std::move(m_leftSide) );
        m_rightSide = CastExpression::Create( t, std::move(m_rightSide) );
    }

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

bool InclusiveOrExpression::Parse( Parser& parser,
                                   std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::INCLUSIVE_OR, Op::OR }
    };

    return ParseLeftAssociative<InclusiveOrExpression, ExclusiveOrExpression>
            ( parser, token, ops );
}

bool InclusiveOrExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::InclusiveOrExpression;
}

bool InclusiveOrExpression::classof( const InclusiveOrExpression* e )
{
    return true;
}


//------------------------------------------------------------------------------
// ExclusiveOrExpression
//------------------------------------------------------------------------------

ExclusiveOrExpression::ExclusiveOrExpression(
                                        Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::ExclusiveOrExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

ExclusiveOrExpression::~ExclusiveOrExpression()
{
}

void ExclusiveOrExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = GetReturnType();

    if( !IsIntegral(t) )
        sema.Error( "Invalid operand types to to exclusive or operator: "
                    + GetTypeString( m_leftSide->GetReturnType() )
                    + GetTypeString( m_rightSide->GetReturnType() ) );
    else
    {
        m_leftSide  = CastExpression::Create( t, std::move(m_leftSide) );
        m_rightSide = CastExpression::Create( t, std::move(m_rightSide) );
    }

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

bool ExclusiveOrExpression::Parse( Parser& parser,
                                   std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::EXCLUSIVE_OR, Op::XOR }
    };

    return ParseLeftAssociative<ExclusiveOrExpression, AndExpression>
            ( parser, token, ops );
}

bool ExclusiveOrExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::ExclusiveOrExpression;
}

bool ExclusiveOrExpression::classof( const ExclusiveOrExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// AndExpression
//------------------------------------------------------------------------------

AndExpression::AndExpression( Op operator_terminal,
                              std::unique_ptr<Expression> left_side,
                              std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::AndExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

AndExpression::~AndExpression()
{
}

void AndExpression::PerformSema( SemaAnalyzer& sema )
{
    //TODO constant folding

    Type t = GetReturnType();

    if( !IsIntegral(t) )
        sema.Error( "Invalid operand types to to and operator: "
                    + GetTypeString( m_leftSide->GetReturnType() )
                    + GetTypeString( m_rightSide->GetReturnType() ) );
    else
    {
        m_leftSide  = CastExpression::Create( t, std::move(m_leftSide) );
        m_rightSide = CastExpression::Create( t, std::move(m_rightSide) );
    }

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

bool AndExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::AND, Op::AND }
    };

    return ParseLeftAssociative<AndExpression, EqualityExpression>
            ( parser, token, ops );
}

bool AndExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::AndExpression;
}

bool AndExpression::classof( const AndExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// EqualityExpression
//------------------------------------------------------------------------------

EqualityExpression::EqualityExpression( Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::EqualityExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

EqualityExpression::~EqualityExpression()
{
}

Type EqualityExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool EqualityExpression::Parse( Parser& parser,
                                std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::EQUALITY, Op::EQUAL_TO },
        { TerminalType::NOT_EQUALITY, Op::NOT_EQUAL_TO }
    };

    return ParseLeftAssociative<EqualityExpression, RelationalExpression>
            ( parser, token, ops );
}

bool EqualityExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::EqualityExpression;
}

bool EqualityExpression::classof( const EqualityExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// RelationalExpression
//------------------------------------------------------------------------------

RelationalExpression::RelationalExpression( Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::RelationalExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

RelationalExpression::~RelationalExpression()
{
}

Type RelationalExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool RelationalExpression::Parse( Parser& parser,
                                  std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::LESS_THAN, Op::LESS_THAN },
        { TerminalType::GREATER_THAN, Op::GREATER_THAN },
        { TerminalType::LESS_THAN_EQUALS, Op::LESS_THAN_EQUALS },
        { TerminalType::GREATER_THAN_EQUALS, Op::GREATER_THAN_EQUALS }
    };

    return ParseLeftAssociative<RelationalExpression, ShiftExpression>
            ( parser, token, ops );
}

bool RelationalExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::RelationalExpression;
}

bool RelationalExpression::classof( const RelationalExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// ShiftExpression
//------------------------------------------------------------------------------

ShiftExpression::ShiftExpression( Op operator_terminal,
                                  std::unique_ptr<Expression> left_side,
                                  std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::ShiftExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

ShiftExpression::~ShiftExpression()
{
}

void ShiftExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = GetReturnType();

    if( !IsIntegral(t) )
        sema.Error( "Invalid operand types to to shift operator: "
                    + GetTypeString( m_leftSide->GetReturnType() )
                    + GetTypeString( m_rightSide->GetReturnType() ) );
    else
    {
        m_leftSide  = CastExpression::Create( t, std::move(m_leftSide) );
        m_rightSide = CastExpression::Create( t, std::move(m_rightSide) );
    }

    m_leftSide->PerformSema( sema );
    m_rightSide->PerformSema( sema );
}

bool ShiftExpression::Parse( Parser& parser,
                             std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::LEFT_SHIFT, Op::LEFT_SHIFT },
        { TerminalType::RIGHT_SHIFT, Op::RIGHT_SHIFT }
    };

    return ParseLeftAssociative<ShiftExpression, AdditiveExpression>
            ( parser, token, ops );
}

bool ShiftExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::ShiftExpression;
}

bool ShiftExpression::classof( const ShiftExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// AdditiveExpression
//------------------------------------------------------------------------------

AdditiveExpression::AdditiveExpression( Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::AdditiveExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

AdditiveExpression::~AdditiveExpression()
{
}

bool AdditiveExpression::Parse( Parser& parser,
                                std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::PLUS, Op::PLUS },
        { TerminalType::MINUS, Op::MINUS }
    };

    return ParseLeftAssociative<AdditiveExpression, MultiplicativeExpression>
            ( parser, token, ops );
}

bool AdditiveExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::AdditiveExpression;
}

bool AdditiveExpression::classof( const AdditiveExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// MultiplicativeExpression
//------------------------------------------------------------------------------

MultiplicativeExpression::MultiplicativeExpression(
                                        Op operator_terminal,
                                        std::unique_ptr<Expression> left_side,
                                        std::unique_ptr<Expression> right_side )
    :BinaryOperatorExpression( ExpressionTy::MultiplicativeExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

MultiplicativeExpression::~MultiplicativeExpression()
{
}

bool MultiplicativeExpression::Parse( Parser& parser,
                                      std::unique_ptr<Expression>& token )
{
    const static OperatorTerminalMap ops =
    {
        { TerminalType::MULTIPLY, Op::MULTIPLY },
        { TerminalType::DIVIDE, Op::DIVIDE },
        { TerminalType::MODULO, Op::MODULO }
    };

    return ParseLeftAssociative<MultiplicativeExpression, CastExpression>
            ( parser, token, ops );
}

bool MultiplicativeExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::MultiplicativeExpression;
}

bool MultiplicativeExpression::classof( const MultiplicativeExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// CastExpression
//------------------------------------------------------------------------------

CastExpression::CastExpression( Type cast_type,
                                std::unique_ptr<Expression> expression )
    :Expression( ExpressionTy::CastExpression )
    ,m_castType( cast_type )
    ,m_expression( std::move(expression) )
{
}

CastExpression::~CastExpression()
{
}

void CastExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_expression->ResolveIdentifiers( sema );
}

void CastExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = m_expression->GetReturnType();

    if( m_castType == Type::UNKNOWN_TYPE )
        sema.Error( "Can't cast to an unknown type" );
    else if( m_castType == Type::STRING )
        if( t != Type::STRING )
            sema.Error( "Can't cast " + GetTypeString( t ) + " to string" );

    if( t == Type::STRING )
        sema.Error( "Can't cast string to " + GetTypeString( m_castType ) );
    else if( t == Type::UNKNOWN_TYPE )
        sema.Error( "Can't cast from unknown type" );

    m_expression->PerformSema( sema );
}

Type CastExpression::GetReturnType() const
{
    return m_castType;
}

void CastExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Cast Expression\n";
}

bool CastExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    // TODO implement c-style casts
    // for the time being, forward the parse to a UnaryExpression
    return parser.Expect<UnaryExpression>( token );
}

std::unique_ptr<Expression> CastExpression::Create(
                                  Type cast_type,
                                  std::unique_ptr<Expression> cast_expression )
{
    if( cast_expression->GetReturnType() == cast_type )
        return cast_expression;
    return std::unique_ptr<Expression>( new CastExpression(
                                                cast_type,
                                                std::move(cast_expression) ) );
}

bool CastExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::CastExpression;
}

bool CastExpression::classof( const CastExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression( Op op,
                                  std::unique_ptr<Expression> expression )
    :Expression( ExpressionTy::UnaryExpression )
    ,m_operator( op )
    ,m_expression( std::move(expression) )
{
}

UnaryExpression::~UnaryExpression()
{
}

void UnaryExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_expression->ResolveIdentifiers( sema );
}

void UnaryExpression::PerformSema( SemaAnalyzer& sema )
{
    Type t = m_expression->GetReturnType();
    if( t == Type::STRING )
        sema.Error( "Invalid type to unary operator: string" );
    else if( t == Type::UNKNOWN_TYPE )
        sema.Error( "Invalid type to unary operator: unknown_type" );

    m_expression->PerformSema( sema );
}

Type UnaryExpression::GetReturnType() const
{
    Type t = m_expression->GetReturnType();

    switch( m_operator )
    {
    //TODO should plus perform integer promotion
    //TODO check for bad type matching here?
    case Op::PLUS:
    case Op::MINUS:
    case Op::INCREMENT:
    case Op::DECREMENT:
    case Op::BITWISE_NOT:
        return t;
    case Op::LOGICAL_NOT:
        return Type::BOOL;
    }
    assert( false && "unreachable" );
    return Type::UNKNOWN_TYPE;
}

void UnaryExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Unary Expression\n";
    m_expression->Print( depth + 1 );
}

bool UnaryExpression::Parse( Parser& parser,
                             std::unique_ptr<Expression>& token )
{
    static const std::vector< std::pair<TerminalType, Op> >
            operator_terminal_map =
    {
        { TerminalType::PLUS,        Op::PLUS },
        { TerminalType::MINUS,       Op::MINUS },
        { TerminalType::BITWISE_NOT, Op::BITWISE_NOT },
        { TerminalType::LOGICAL_NOT, Op::LOGICAL_NOT },
        { TerminalType::INCREMENT,   Op::INCREMENT },
        { TerminalType::DECREMENT,   Op::DECREMENT }
    };

    bool found = false;
    Op op;
    for( const auto& p : operator_terminal_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            op = p.second;
            found = true;
            break;
        }

    if( found )
    {
        // Parse the next unary expression
        std::unique_ptr<Expression> unary_expression;
        // we had a unary operator, there should be an expression
        if( !parser.Expect<UnaryExpression>( unary_expression ) )
            return false;

        token.reset( new UnaryExpression( op, std::move(unary_expression) ) );
        return true;
    }

    // if there was no operator just forward the parse to a postfix expression
    return parser.Expect<PostfixExpression>( token );
}

bool UnaryExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::UnaryExpression;
}

bool UnaryExpression::classof( const UnaryExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// PostfixExpression
//------------------------------------------------------------------------------

PostfixExpression::PostfixExpression(
                             std::unique_ptr<Expression> expression,
                             std::unique_ptr<PostfixOperator> postfix_operator )
    :Expression( ExpressionTy::PostfixExpression )
    ,m_expression( std::move(expression) )
    ,m_postfixOperator( std::move(postfix_operator) )
{
}

PostfixExpression::~PostfixExpression()
{
}

void PostfixExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_expression->ResolveIdentifiers( sema );
}

void PostfixExpression::PerformSema( SemaAnalyzer& sema )
{
    assert( "false" && "Complete me" );
    m_expression->PerformSema( sema );
}

Type PostfixExpression::GetReturnType() const
{
    assert( "false" && "Complete me" );
    return Type::UNKNOWN_TYPE;
}

void PostfixExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Postfix Expression\n";
    m_expression->Print( depth + 1 );
    m_postfixOperator->Print( depth + 1 );
}

bool PostfixExpression::Parse( Parser& parser,
                               std::unique_ptr<Expression>& token )
{
    // Try and parse the primary expression
    std::unique_ptr<Expression> primary_expression;
    if( !parser.Expect<PrimaryExpression>( primary_expression ) )
        return false;

    // try and parse all the postfix operators
    std::vector< std::unique_ptr<PostfixOperator> > operators;
    parser.ExpectSequenceOf<PostfixOperator>( operators );
    CHECK_PARSER;

    // set ret to just the primary expression
    std::unique_ptr<Expression> ret = std::move( primary_expression );

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
    return e->GetSubClassID() == ExpressionTy::PostfixExpression;
}

bool PostfixExpression::classof( const PostfixExpression* e )
{
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
// IncrementalOperator
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

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

bool PrimaryExpression::Parse( Parser& parser,
                               std::unique_ptr<Expression>& token )
{
    // Try and parse and identifier
    if( parser.Expect<IdentifierExpression>( token ) )
        return true;
    CHECK_PARSER;

    // Try and parse a literal
    if( parser.Expect<LiteralExpression>( token ) )
        return true;
    CHECK_PARSER;

    // Try and parse a bracketed expression
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;
    if( !parser.Expect<Expression>( token ) )
        return false;
    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
        return false;

    return true;
}

//------------------------------------------------------------------------------
// IdentifierExpression
//------------------------------------------------------------------------------

IdentifierExpression::IdentifierExpression( std::string identifier )
    :Expression( ExpressionTy::IdentifierExpression )
    ,m_identifier( std::move( identifier ) )
    ,m_readExpression( nullptr )
{
    assert( !m_identifier.empty() );
}

IdentifierExpression::~IdentifierExpression()
{
}

void IdentifierExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_readExpression = sema.GetVariable( m_identifier );
    if( !m_readExpression )
        sema.Error( "Undeclared variable: " + m_identifier );
}

Type IdentifierExpression::GetReturnType() const
{
    assert( m_readExpression &&
            "Trying to get the type of an unresolved identifier" );
    return m_readExpression->GetReturnType();
}

void IdentifierExpression::PerformSema( SemaAnalyzer& sema )
{
}

const std::shared_ptr<Expression>&  IdentifierExpression::GetReadExpression() const
{
    return m_readExpression;
}

void IdentifierExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_identifier << "\n";
}

bool IdentifierExpression::Parse( Parser& parser,
                                  std::unique_ptr<Expression>& token )
{
    // Try and parse the identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    token.reset( new IdentifierExpression( identifier ) );
    return true;
}

bool IdentifierExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::IdentifierExpression;
}

bool IdentifierExpression::classof( const IdentifierExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// LiteralExpression
//------------------------------------------------------------------------------

LiteralExpression::LiteralExpression( ExpressionTy sub_class_id )
    :Expression( sub_class_id )
{
}

void LiteralExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
}

void LiteralExpression::PerformSema( SemaAnalyzer& sema )
{
}

bool LiteralExpression::Parse( Parser& parser,
                               std::unique_ptr<Expression>& token )
{
    // Try to parse any of the literals
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<FloatingLiteralExpression,
                            IntegerLiteralExpression,
                            BooleanLiteralExpression,
                            CharacterLiteralExpression,
                            StringLiteralExpression>( t ) )
        return false;

    // Cast to a LiteralExpression
    token.reset( static_cast<LiteralExpression*>( t.release() ) );
    return true;
}

bool LiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() > ExpressionTy::LiteralExpression_Start &&
           e->GetSubClassID() < ExpressionTy::LiteralExpression_End;
}

bool LiteralExpression::classof( const LiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// IntegerlLiteralExpression
//------------------------------------------------------------------------------

IntegerLiteralExpression::IntegerLiteralExpression( unsigned long long value,
                                                    Suffix suffix )
    :LiteralExpression( ExpressionTy::IntegerLiteralExpression )
    ,m_value( value )
    ,m_suffix( suffix )
{
}

IntegerLiteralExpression::~IntegerLiteralExpression()
{
}

Type IntegerLiteralExpression::GetReturnType() const
{
    // TODO vary type if something doesn't fit
    switch( m_suffix )
    {
    case Suffix::CHAR:
        return Type::I8;
    case Suffix::INT:
        return Type::I32;
    case Suffix::SHORT:
        return Type::I16;
    case Suffix::LONG:
        return Type::I64;
    case Suffix::UNSIGNED_CHAR:
        return Type::U8;
    case Suffix::UNSIGNED_INT:
        return Type::U32;
    case Suffix::UNSIGNED_SHORT:
        return Type::U16;
    case Suffix::UNSIGNED_LONG:
        return Type::U64;
    default:
        return Type::I32;
    }
}

GenericValue IntegerLiteralExpression::GetValue() const
{
    switch( m_suffix )
    {
    case Suffix::CHAR:
        return GenericValue( jl_i8(m_value) );
    case Suffix::INT:
        return GenericValue( jl_i32(m_value) );
    case Suffix::SHORT:
        return GenericValue( jl_i16(m_value) );
    case Suffix::LONG:
        return GenericValue( jl_i64(m_value) );
    case Suffix::UNSIGNED_CHAR:
        return GenericValue( jl_u8(m_value) );
    case Suffix::UNSIGNED_INT:
        return GenericValue( jl_u32(m_value) );
    case Suffix::UNSIGNED_SHORT:
        return GenericValue( jl_u16(m_value) );
    case Suffix::UNSIGNED_LONG:
        return GenericValue( jl_u64(m_value) );
    default:
        return GenericValue( jl_i32(m_value) );
    }
}

void IntegerLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

bool IntegerLiteralExpression::ParseInteger( std::string string,
                                             unsigned long long& value,
                                             Suffix& suffix )
{
    if( string.empty() )
        return false;

    std::istringstream ss;
    ss.unsetf( std::ios_base::skipws );

    // If this is a Hex or Oct constant
    if( string[0] == '0' )
    {
        // If this is a Hex constant
        if( string.size() > 2 &&
            ( string[1] == 'x' || string[1] == 'X' ) )
        {
            // Initialize a stringstream with the hex part of this string
            // Read as hexadecimal
            ss.str( string.substr(2) );
            ss.setf( std::ios_base::hex, std::ios::basefield );
            if( !( ss >> value ) )
                return false;
        }
        else
        {
            // Read as octal
            ss.str( string );
            ss.setf( std::ios_base::oct, std::ios::basefield );
            if( !( ss >> value ) )
                return false;
        }
    }
    else
    {
        // Read as decimal
        ss.str( string );
        ss.setf( std::ios_base::dec, std::ios::basefield );
        if( !( ss >> value ) )
            return false;
    }

    // check if there is a suffix
    if( ss.eof() )
    {
        suffix = Suffix::NONE;
        return true;
    }

    // There is a suffix
    if( string[ss.tellg()] == 'u' )
    {
        if( string.size() + 1 == std::size_t(ss.tellg()) )
        {
            if( string[ss.tellg()] == 'i' )
                suffix = Suffix::UNSIGNED_INT;
            else if( string[ss.tellg()] == 'l' )
                suffix = Suffix::UNSIGNED_LONG;
            else if( string[ss.tellg()] == 's' )
                suffix = Suffix::UNSIGNED_SHORT;
            else if( string[ss.tellg()] == 't' )
                suffix = Suffix::UNSIGNED_CHAR;
            else
                return false;
        }
        else
            suffix = Suffix::UNSIGNED;
    }
    else if( string[ss.tellg()] == 'i' )
        suffix = Suffix::INT;
    else if( string[ss.tellg()] == 'l' )
        suffix = Suffix::LONG;
    else if( string[ss.tellg()] == 's' )
        suffix = Suffix::SHORT;
    else if( string[ss.tellg()] == 't' )
        suffix = Suffix::CHAR;
    else
        return false;

    return true;
}

bool IntegerLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<IntegerLiteralExpression>& token )
{
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::INTEGER_LITERAL, string ) )
        return false;

    unsigned long long value;
    Suffix suffix;

    if( !ParseInteger( string, value, suffix ) )
        return false;

    token.reset( new IntegerLiteralExpression( value, suffix ) );
    return true;
}

bool IntegerLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::IntegerLiteralExpression;
}

bool IntegerLiteralExpression::classof( const IntegerLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// FloatingLiteralExpression
//------------------------------------------------------------------------------

FloatingLiteralExpression::FloatingLiteralExpression( double value,
                                                      Suffix suffix )
    :LiteralExpression( ExpressionTy::FloatingLiteralExpression )
    ,m_value( value )
    ,m_suffix( suffix )
{
}

FloatingLiteralExpression::~FloatingLiteralExpression()
{
}

Type FloatingLiteralExpression::GetReturnType() const
{
    if( m_suffix == Suffix::SINGLE )
        return Type::FLOAT;
    else
        return Type::DOUBLE;
}

GenericValue FloatingLiteralExpression::GetValue() const
{
    switch( m_suffix )
    {
    case Suffix::SINGLE:
        return GenericValue( jl_float(m_value) );
    default:
        return GenericValue( jl_double(m_value) );
    }
}

void FloatingLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_value << "\n";
}

bool FloatingLiteralExpression::ParseFloat( std::string string,
                                            double& value,
                                            Suffix& suffix )
{
    std::istringstream ss( string );
    ss.unsetf( std::ios_base::skipws );
    if( !( ss >> value ) )
        return false;

    // check if there is a suffix
    if( ss.eof() )
    {
        suffix = Suffix::NONE;
        return true;
    }

    // There is a suffix
    if( string[ss.tellg()] == 'f' )
        suffix = Suffix::SINGLE;
    else
        return false;

    return true;
}

bool FloatingLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<FloatingLiteralExpression>& token )
{
    // Parse a floating literal
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::FLOATING_LITERAL, string ) )
        return false;

    double value;
    Suffix suffix;
    if( !ParseFloat( string, value, suffix ) )
        return false;

    token.reset( new FloatingLiteralExpression( value, suffix ) );
    return true;
}

bool FloatingLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::FloatingLiteralExpression;
}

bool FloatingLiteralExpression::classof( const FloatingLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// BooleanLiteralExpression
//------------------------------------------------------------------------------

BooleanLiteralExpression::BooleanLiteralExpression( bool value )
    :LiteralExpression( ExpressionTy::BooleanLiteralExpression )
    ,m_value( value )
{
}

BooleanLiteralExpression::~BooleanLiteralExpression()
{
}

Type BooleanLiteralExpression::GetReturnType() const
{
    return Type::BOOL;
}

GenericValue BooleanLiteralExpression::GetValue() const
{
    return GenericValue( jl_bool(m_value) );
}

void BooleanLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << std::boolalpha << m_value << std::noboolalpha << "\n";
}

bool BooleanLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<BooleanLiteralExpression>& token )
{
    // Try to parse 'true'
    if( parser.ExpectTerminal( TerminalType::TRUE ) )
    {
        token.reset( new BooleanLiteralExpression( true ) );
        return true;
    }

    // Try to parse 'false'
    if( parser.ExpectTerminal( TerminalType::FALSE ) )
    {
        token.reset( new BooleanLiteralExpression( false ) );
        return true;
    }

    return false;
}

bool BooleanLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::BooleanLiteralExpression;
}

bool BooleanLiteralExpression::classof( const BooleanLiteralExpression* e )
{
    return true;
}

/**
  * This function unescapes a single character
  * It's used by StringLiteralExpression and CharacterLiteralExpression
  * \param character
  *   the char to unescape
  * \returns the unescaped character
  */
char UnescapeCharacter( char c )
{
    char ret;
    switch( c )
    {
    case '\'':
        ret = '\'';
        break;
    case '\"':
        ret = '\"';
        break;
    case '\?':
        ret = '\?';
        break;
    case '\\':
        ret = '\\';
        break;
    case '\a':
        ret = '\a';
        break;
    case '\b':
        ret = '\b';
        break;
    case '\f':
        ret = '\f';
        break;
    case '\n':
        ret = '\n';
        break;
    case '\r':
        ret = '\r';
        break;
    case '\t':
        ret = '\t';
        break;
    case '\v':
        ret = '\v';
        break;
    default:
        //TODO warning here
        ret = c;
    }

    return ret;
}

//------------------------------------------------------------------------------
// StringLiteralExpression
//------------------------------------------------------------------------------

StringLiteralExpression::StringLiteralExpression( std::string value )
    :LiteralExpression( ExpressionTy::BooleanLiteralExpression )
    ,m_value( std::move(value) )
{
}

StringLiteralExpression::~StringLiteralExpression()
{
}

Type StringLiteralExpression::GetReturnType() const
{
    return Type::STRING;
}

GenericValue StringLiteralExpression::GetValue() const
{
    return GenericValue( jl_string(m_value) );
}

void StringLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\"' << m_value << "\"\n";
}

const std::string& StringLiteralExpression::GetString() const
{
    return m_value;
}

bool StringLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<StringLiteralExpression>& token )
{
    // Parse the escaped string literal into string
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::STRING_LITERAL, string ) )
        return false;

    // Unescape the string
    std::string unescaped_string;
    if( !UnquoteAndUnescapeString( string, unescaped_string ) )
        return false;

    token.reset( new StringLiteralExpression( std::move(unescaped_string) ) );
    return true;
}

bool StringLiteralExpression::UnquoteAndUnescapeString(
                                                const std::string& string,
                                                std::string& unescaped_string )
{
    if( string.size() < 2 )
        return false;
    if( *string.begin() != '\"' )
        return false;
    if( *string.rbegin() != '\"' )
        return false;

    // reserve space for a string with no escaped characters
    unescaped_string.clear();
    unescaped_string.reserve( string.size() - 2 );

    std::string::const_iterator p = string.begin() + 1;
    while( p != string.end() - 1 )
    {
        if( *p == '\\' )
        {
            ++p;
            unescaped_string += UnescapeCharacter( *p );
        }
        unescaped_string += *p;
        ++p;
    }
    return true;
}

bool StringLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::StringLiteralExpression;
}

bool StringLiteralExpression::classof( const StringLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// CharacterLiteralExpression
//------------------------------------------------------------------------------

CharacterLiteralExpression::CharacterLiteralExpression( char value )
    :LiteralExpression( ExpressionTy::CharacterLiteralExpression )
    ,m_value( value )
{
}

CharacterLiteralExpression::~CharacterLiteralExpression()
{
}

Type CharacterLiteralExpression::GetReturnType() const
{
    return Type::I8;
}

GenericValue CharacterLiteralExpression::GetValue() const
{
    return GenericValue( jl_i8(m_value) );
}

void CharacterLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\'' << m_value << '\'' << "\n";
}

bool CharacterLiteralExpression::Parse(
                            Parser& parser,
                            std::unique_ptr<CharacterLiteralExpression>& token )
{
    // Parse the escaped char into string
    std::string string;
    if( !parser.ExpectTerminal( TerminalType::CHARACTER_LITERAL, string ) )
        return false;

    char unescaped_char;
    if( !UnquoteAndUnescapeChar( string, unescaped_char ) )
        return false;

    token.reset( new CharacterLiteralExpression( unescaped_char ) );
    return true;
}

bool CharacterLiteralExpression::UnquoteAndUnescapeChar(
                                                const std::string& string,
                                                char& unescaped_char )
{
    if( string.size() < 3 )
        return false;
    if( *string.begin() != '\'' )
        return false;
    if( *string.rbegin() != '\'' )
        return false;

    if( string[1] == '\\' )
    {
        if( string.size() != 4 )
            return false;
        unescaped_char = UnescapeCharacter( string[2] );
        return true;
    }

    unescaped_char = UnescapeCharacter( string[1] );
    return true;
}

bool CharacterLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == ExpressionTy::CharacterLiteralExpression;
}

bool CharacterLiteralExpression::classof( const CharacterLiteralExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
