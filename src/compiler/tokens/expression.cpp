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

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/assignment_operator.hpp>
#include <compiler/tokens/literal_expression.hpp>
#include <compiler/tokens/postfix_operator.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Expression
//------------------------------------------------------------------------------

Expression::Expression( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

Expression::~Expression()
{
}


bool Expression::IsLValue() const
{
    return false;
}

llvm::Value* Expression::CodeGenPointerTo( CodeGenerator& code_gen ) const
{
    assert( false && "Complete me" );
    return nullptr;
}

bool Expression::Parse( Parser& parser, Expression_up& token )
{
    // TODO comma sep expressions
    return parser.Expect<AssignmentExpression>( token );
}

bool Expression::classof( const Token* t )
{
    return t->GetSubClassID() >= TokenTy::Expression_Start &&
           t->GetSubClassID() <= TokenTy::Expression_End;
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
                        Expression_up assignee,
                        Op assignment_operator,
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
    m_AssigneePtr = m_Assignee.get();
}

AssignmentExpression::~AssignmentExpression()
{
}

bool AssignmentExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_Assignee->ResolveIdentifiers( sema ) &&
           m_AssignedExpression->ResolveIdentifiers( sema );
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
        sema.Error( "Trying to assign to a Const expression" );
    }

    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        break;
    case Op::SHL_EQUALS:
        m_AssignedExpression.reset(
                  new ShiftExpression( BinaryOperatorExpression::Op::LEFT_SHIFT,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::SHR_EQUALS:
        m_AssignedExpression.reset(
                 new ShiftExpression( BinaryOperatorExpression::Op::RIGHT_SHIFT,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::AND_EQUALS:
        m_AssignedExpression.reset(
                   new AndExpression( BinaryOperatorExpression::Op::AND,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::XOR_EQUALS:
        m_AssignedExpression.reset(
           new ExclusiveOrExpression( BinaryOperatorExpression::Op::XOR,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::OR_EQUALS:
        m_AssignedExpression.reset(
           new InclusiveOrExpression( BinaryOperatorExpression::Op::OR,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::PLUS_EQUALS:
        m_AssignedExpression.reset(
                    new AdditiveExpression( BinaryOperatorExpression::Op::PLUS,
                                            std::move(m_Assignee),
                                            std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::MINUS_EQUALS:
        m_AssignedExpression.reset(
                    new AdditiveExpression( BinaryOperatorExpression::Op::MINUS,
                                            std::move(m_Assignee),
                                            std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::MULTIPLY_EQUALS:
        m_AssignedExpression.reset(
          new MultiplicativeExpression( BinaryOperatorExpression::Op::MULTIPLY,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::DIVIDE_EQUALS:
        m_AssignedExpression.reset(
          new MultiplicativeExpression( BinaryOperatorExpression::Op::DIVIDE,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::MODULO_EQUALS:
        m_AssignedExpression.reset(
          new MultiplicativeExpression( BinaryOperatorExpression::Op::MODULO,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    }
    // Assuming that we can assign to the same as ReturnType;
    m_AssignedExpression = CastExpression::Create(
                                              m_AssigneePtr->GetReturnType(),
                                              std::move(m_AssignedExpression) );
    good &= m_AssignedExpression->PerformSema( sema );

    return good;
}

llvm::Value* AssignmentExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_AssigneePtr &&
            "Trying to codegen an assignmentexpression with null assigneeptr" );
    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        code_gen.CreateVariableAssignment( *m_AssigneePtr,
                                           *m_AssignedExpression );
        break;
    default:
        assert( false && "unhandled assignment operator" );
    }

    // Return the new value of the variable
    return m_AssigneePtr->CodeGen( code_gen );
}

llvm::Value* AssignmentExpression::CodeGenPointerTo(
                                                  CodeGenerator& code_gen) const
{
    assert( m_AssigneePtr &&
            "Trying to codegen an assignmentexpression with null assigneeptr" );
    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        code_gen.CreateVariableAssignment( *m_AssigneePtr,
                                           *m_AssignedExpression );
        break;
    default:
        assert( false && "unhandled assignment operator" );
    }

    // Return the new value of the variable
    return m_AssigneePtr->CodeGenPointerTo( code_gen );
}

Type AssignmentExpression::GetReturnType() const
{
    return m_AssigneePtr->GetReturnType();
}

Type AssignmentExpression::GetUnderlyingType() const
{
    return m_AssigneePtr->GetUnderlyingType();
}

const std::vector<unsigned>& AssignmentExpression::GetArrayExtents() const
{
    return m_AssigneePtr->GetArrayExtents();
}

bool AssignmentExpression::IsConst() const
{
    return false;
}

void AssignmentExpression::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Assignment Expression\n";

    m_AssigneePtr->Print( depth + 1 );
    m_AssignedExpression->Print( depth + 1 );
}

bool AssignmentExpression::Parse( Parser& parser,
                                  Expression_up& token )
{
    Expression_up lhs_expression;
    if( !parser.Expect< ConditionalExpression >( lhs_expression ) )
        return false;

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !parser.Expect< AssignmentOperator >( assignment_operator ) )
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

bool ConditionalExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_Condition->ResolveIdentifiers( sema ) &&
           m_TrueExpression->ResolveIdentifiers( sema ) &&
           m_FalseExpression->ResolveIdentifiers( sema );
}

bool ConditionalExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO check that the true and false expressions are of equal array size
    bool good = true;

    m_Condition = CastExpression::Create( Type::BOOL, std::move(m_Condition) );

    Type t = GetReturnType();
    if( t == Type::UNKNOWN )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here, so report it
        if( m_TrueExpression->GetReturnType() != Type::UNKNOWN &&
            m_FalseExpression->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Incompatable operand types in conditional expression "+
                        GetTypeString( m_TrueExpression->GetReturnType() ) +
                        " and " +
                        GetTypeString( m_FalseExpression->GetReturnType() ) );
    }
    else
    {
        m_TrueExpression = CastExpression::Create(
                                                t,
                                                std::move(m_TrueExpression) );
        m_FalseExpression = CastExpression::Create(
                                                t,
                                                std::move(m_FalseExpression) );
    }

    good &= m_Condition->PerformSema( sema );
    good &= m_TrueExpression->PerformSema( sema );
    good &= m_FalseExpression->PerformSema( sema );

    return good;
}

llvm::Value* ConditionalExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateSelect( *m_Condition,
                                  *m_TrueExpression,
                                  *m_FalseExpression );
}

Type ConditionalExpression::GetReturnType() const
{
    return GetCommonType( m_TrueExpression->GetReturnType(),
                          m_FalseExpression->GetReturnType() );
}

Type ConditionalExpression::GetUnderlyingType() const
{
    return GetCommonType( m_TrueExpression->GetUnderlyingType(),
                          m_FalseExpression->GetUnderlyingType() );
}

const std::vector<unsigned>& ConditionalExpression::GetArrayExtents() const
{
    // The array extents should be the same between the two sides
    return m_TrueExpression->GetArrayExtents();
}

bool ConditionalExpression::IsConst() const
{
    /// TODO only check the taken select
    return m_Condition->IsConst() &&
           m_TrueExpression->IsConst() &&
           m_FalseExpression->IsConst();

}

void ConditionalExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Conditional Expression\n";

    m_Condition->Print( depth + 1 );
    m_TrueExpression->Print( depth + 1 );
    m_FalseExpression->Print( depth + 1 );
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

//------------------------------------------------------------------------------
// BinaryOperatorExpression
//------------------------------------------------------------------------------

BinaryOperatorExpression::BinaryOperatorExpression(
                                        TokenTy sub_class_id,
                                        Op op,
                                        Expression_up left_side,
                                        Expression_up right_side )
    :Expression( sub_class_id )
    ,m_Operator( op )
    ,m_LeftSide( std::move(left_side) )
    ,m_RightSide( std::move(right_side) )
{
    assert( m_LeftSide && "BinaryOperatorExpression given a null lhs" );
    assert( m_RightSide && "BinaryOperatorExpression given a null rhs" );
}

BinaryOperatorExpression::~BinaryOperatorExpression()
{
}

bool BinaryOperatorExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_LeftSide->ResolveIdentifiers( sema ) &&
           m_RightSide->ResolveIdentifiers( sema );
}

bool BinaryOperatorExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO operands can't be arrays
    bool good = true;

    Type t = GetCommonType( m_RightSide->GetReturnType(),
                            m_LeftSide->GetReturnType() );
    if( t == Type::UNKNOWN )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operands to binary operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

llvm::Value* BinaryOperatorExpression::CodeGen( CodeGenerator& code_gen ) const
{
    switch( m_Operator )
    {
    case Op::LOGICAL_OR:
        return code_gen.CreateLOr(  *m_LeftSide, *m_RightSide );
    case Op::LOGICAL_AND:
        return code_gen.CreateLAnd( *m_LeftSide, *m_RightSide );
    case Op::OR:
        return code_gen.CreateOr(   *m_LeftSide, *m_RightSide );
    case Op::XOR:
        return code_gen.CreateXor(  *m_LeftSide, *m_RightSide );
    case Op::AND:
        return code_gen.CreateAnd(  *m_LeftSide, *m_RightSide );
    case Op::EQUAL_TO:
        return code_gen.CreateEq(   *m_LeftSide, *m_RightSide );
    case Op::NOT_EQUAL_TO:
        return code_gen.CreateNeq(  *m_LeftSide, *m_RightSide );
    case Op::LESS_THAN:
        return code_gen.CreateLT(   *m_LeftSide, *m_RightSide );
    case Op::GREATER_THAN:
        return code_gen.CreateGT(   *m_LeftSide, *m_RightSide );
    case Op::LESS_THAN_EQUALS:
        return code_gen.CreateLTE(  *m_LeftSide, *m_RightSide );
    case Op::GREATER_THAN_EQUALS:
        return code_gen.CreateGTE(  *m_LeftSide, *m_RightSide );
    case Op::LEFT_SHIFT:
        return code_gen.CreateShl(  *m_LeftSide, *m_RightSide );
    case Op::RIGHT_SHIFT:
        return code_gen.CreateShr(  *m_LeftSide, *m_RightSide );
    case Op::PLUS:
        return code_gen.CreateAdd(  *m_LeftSide, *m_RightSide );
    case Op::MINUS:
        return code_gen.CreateSub(  *m_LeftSide, *m_RightSide );
    case Op::MULTIPLY:
        return code_gen.CreateMul(  *m_LeftSide, *m_RightSide );
    case Op::DIVIDE:
        return code_gen.CreateDiv(  *m_LeftSide, *m_RightSide );
    case Op::MODULO:
        return code_gen.CreateMod(  *m_LeftSide, *m_RightSide );
    default:
        assert( false &&
                "Trying to generate code for unhandled binary operator" );
        return nullptr;
    }
}

Type BinaryOperatorExpression::GetReturnType() const
{
    return GetCommonType( m_LeftSide->GetReturnType(),
                          m_RightSide->GetReturnType() );
}

Type BinaryOperatorExpression::GetUnderlyingType() const
{
    return GetReturnType();
}

const std::vector<unsigned>& BinaryOperatorExpression::GetArrayExtents() const
{
    // Can't binary op between two arrays
    assert( m_LeftSide->GetArrayExtents().size() == 0 &&
            "Binary operator left side is an array" );
    assert( m_RightSide->GetArrayExtents().size() == 0 &&
            "Binary operator right side is an array" );
    return m_LeftSide->GetArrayExtents();
}

bool BinaryOperatorExpression::IsConst() const
{
    return m_LeftSide->IsConst() &&
           m_RightSide->IsConst();
}

void BinaryOperatorExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "binary_operator" << std::endl;
    m_LeftSide->Print( depth + 1 );
    m_RightSide->Print( depth + 1 );
}

template< typename TokenType, typename SubTokenType >
bool BinaryOperatorExpression::ParseLeftAssociative( Parser& parser,
                                  Expression_up& token,
                                  const OperatorTerminalMap& op_terminal_map )
{
    // Try and parse the sub expression for the left side
    Expression_up left;
    if( !parser.Expect<SubTokenType>( left ) )
        return false;

    // A vector of operators and the next expression
    std::vector< std::pair< Op,
                            Expression_up > > rest;
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
        Expression_up next;
        if( !parser.Expect<SubTokenType>( next ) )
            return false;

        // Push this operator and expression to the list
        rest.push_back( std::make_pair( op, std::move( next ) ) );
    }

    // for every operator+expression we have, set the lhs of the new left to
    // the old left and its operator to op and the next subexpression to the rhs
    for( auto& expression : rest )
        left.reset( new TokenType( expression.first,
                                        std::move(left),
                                        std::move(expression.second) ) );

    token = std::move( left );
    return true;
}

bool BinaryOperatorExpression::classof( const Expression* e )
{
    return e->GetSubClassID() >= TokenTy::BinaryOperatorExpression_Start &&
           e->GetSubClassID() <= TokenTy::BinaryOperatorExpression_End;
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
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::LogicalOrExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

LogicalOrExpression::~LogicalOrExpression()
{
}

bool LogicalOrExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    m_LeftSide = CastExpression::Create( Type::BOOL, std::move(m_LeftSide) );
    m_RightSide = CastExpression::Create( Type::BOOL, std::move(m_RightSide) );

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

Type LogicalOrExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalOrExpression::Parse( Parser& parser,
                                 Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::LogicalOrExpression;
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
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::LogicalAndExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

LogicalAndExpression::~LogicalAndExpression()
{
}

bool LogicalAndExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    m_LeftSide = CastExpression::Create( Type::BOOL, std::move(m_LeftSide) );
    m_RightSide = CastExpression::Create( Type::BOOL, std::move(m_RightSide) );

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

Type LogicalAndExpression::GetReturnType() const
{
    return Type::BOOL;
}

bool LogicalAndExpression::Parse( Parser& parser,
                                  Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::LogicalAndExpression;
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
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::InclusiveOrExpression,
                               operator_terminal,
                               std::move( left_side ),
                               std::move( right_side ) )
{
}

InclusiveOrExpression::~InclusiveOrExpression()
{
}

bool InclusiveOrExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;
    Type t = GetReturnType();

    if( !IsIntegral(t) )
    {
        good = false;
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operand types to to inclusive or operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

bool InclusiveOrExpression::Parse( Parser& parser,
                                   Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::InclusiveOrExpression;
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
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::ExclusiveOrExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

ExclusiveOrExpression::~ExclusiveOrExpression()
{
}

bool ExclusiveOrExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    Type t = GetReturnType();

    if( !IsIntegral(t) )
    {
        good = false;
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operand types to to exclusive or operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

bool ExclusiveOrExpression::Parse( Parser& parser,
                                   Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::ExclusiveOrExpression;
}

bool ExclusiveOrExpression::classof( const ExclusiveOrExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// AndExpression
//------------------------------------------------------------------------------

AndExpression::AndExpression( Op operator_terminal,
                              Expression_up left_side,
                              Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::AndExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

AndExpression::~AndExpression()
{
}

bool AndExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    Type t = GetReturnType();

    if( !IsIntegral(t) )
    {
        good = false;
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operand types to to and operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );

    return good;
}

bool AndExpression::Parse( Parser& parser, Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::AndExpression;
}

bool AndExpression::classof( const AndExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// EqualityExpression
//------------------------------------------------------------------------------

EqualityExpression::EqualityExpression( Op operator_terminal,
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::EqualityExpression,
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
                                Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::EqualityExpression;
}

bool EqualityExpression::classof( const EqualityExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// RelationalExpression
//------------------------------------------------------------------------------

RelationalExpression::RelationalExpression( Op operator_terminal,
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::RelationalExpression,
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
                                  Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::RelationalExpression;
}

bool RelationalExpression::classof( const RelationalExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// ShiftExpression
//------------------------------------------------------------------------------

ShiftExpression::ShiftExpression( Op operator_terminal,
                                  Expression_up left_side,
                                  Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::ShiftExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

ShiftExpression::~ShiftExpression()
{
}

bool ShiftExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    Type t = GetReturnType();

    if( !IsIntegral(t) )
    {
        good = false;
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operand types to to shift operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );
    return good;
}

bool ShiftExpression::Parse( Parser& parser,
                             Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::ShiftExpression;
}

bool ShiftExpression::classof( const ShiftExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// AdditiveExpression
//------------------------------------------------------------------------------

AdditiveExpression::AdditiveExpression( Op operator_terminal,
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::AdditiveExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

AdditiveExpression::~AdditiveExpression()
{
}

bool AdditiveExpression::Parse( Parser& parser,
                                Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::AdditiveExpression;
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
                                        Expression_up left_side,
                                        Expression_up right_side )
    :BinaryOperatorExpression( TokenTy::MultiplicativeExpression,
                               operator_terminal,
                               std::move(left_side),
                               std::move(right_side) )
{
}

MultiplicativeExpression::~MultiplicativeExpression()
{
}

bool MultiplicativeExpression::PerformSema( SemaAnalyzer& sema )
{
    //
    // Don't have any special handling for * and /
    //
    if( m_Operator != BinaryOperatorExpression::Op::MODULO )
        return BinaryOperatorExpression::PerformSema( sema );

    bool good = true;

    Type t = GetReturnType();

    if( !IsIntegral(t) )
    {
        good = false;
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN &&
            m_RightSide->GetReturnType() != Type::UNKNOWN )
            sema.Error( "Invalid operand types to to modulo operator: " +
                        GetTypeString( m_LeftSide->GetReturnType() ) + " and " +
                        GetTypeString( m_RightSide->GetReturnType() ) );
    }
    else
    {
        m_LeftSide  = CastExpression::Create( t, std::move(m_LeftSide) );
        m_RightSide = CastExpression::Create( t, std::move(m_RightSide) );
    }

    good &= m_LeftSide->PerformSema( sema );
    good &= m_RightSide->PerformSema( sema );
    return good;
}

bool MultiplicativeExpression::Parse( Parser& parser,
                                      Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::MultiplicativeExpression;
}

bool MultiplicativeExpression::classof( const MultiplicativeExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// CastExpression
//------------------------------------------------------------------------------

CastExpression::CastExpression( Type cast_type,
                                Expression_up expression )
    :Expression( TokenTy::CastExpression )
    ,m_CastType( cast_type )
    ,m_Expression( std::move(expression) )
{
}

CastExpression::~CastExpression()
{
}

bool CastExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_Expression->ResolveIdentifiers( sema );
}

bool CastExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    Type t = m_Expression->GetReturnType();

    if( m_CastType == Type::UNKNOWN )
    {
        good = false;
        sema.Error( "Can't cast to an unknown type" );
    }
    else if( m_CastType == Type::STRING )
        if( t != Type::STRING &&
            t != Type::UNKNOWN )
        {
            good = false;
            sema.Error( "Can't cast " + GetTypeString( t ) + " to string" );
        }

    if( t == Type::STRING && m_CastType != Type::STRING )
    {
        good = false;
        sema.Error( "Can't cast string to " + GetTypeString( m_CastType ) );
    }

    good &= m_Expression->PerformSema( sema );

    return good;
}

llvm::Value* CastExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateCast( *m_Expression, m_CastType );
}

Type CastExpression::GetReturnType() const
{
    if( m_Expression->GetReturnType() == Type::ARRAY )
        return Type::ARRAY;
    return m_CastType;
}

Type CastExpression::GetUnderlyingType() const
{
    return m_CastType;
}

const std::vector<unsigned>& CastExpression::GetArrayExtents() const
{
    return m_Expression->GetArrayExtents();
}

bool CastExpression::IsConst() const
{
    return m_Expression->IsConst();
}

void CastExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Cast Expression\n";
}

bool CastExpression::Parse( Parser& parser, Expression_up& token )
{
    // TODO implement c-style casts
    // for the time being, forward the parse to a UnaryExpression
    return parser.Expect<UnaryExpression>( token );
}

Expression_up CastExpression::Create(
                                  Type cast_type,
                                  Expression_up cast_expression )
{
    if( cast_expression->GetReturnType() == cast_type )
        return cast_expression;
    return Expression_up( new CastExpression( cast_type,
                                              std::move(cast_expression) ) );
}

bool CastExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::CastExpression;
}

bool CastExpression::classof( const CastExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression( Op op,
                                  Expression_up expression )
    :Expression( TokenTy::UnaryExpression )
    ,m_Operator( op )
    ,m_Expression( std::move(expression) )
{
}

UnaryExpression::~UnaryExpression()
{
}

bool UnaryExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_Expression->ResolveIdentifiers( sema );
}

bool UnaryExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO check that expression isn't an array
    bool good = true;

    Type t = m_Expression->GetReturnType();
    if( t == Type::STRING )
    {
        good = false;
        sema.Error( "Invalid type to unary operator: string" );
    }
    if( !IsIntegral( t ) &&
        m_Operator == Op::BITWISE_NOT )
    {
        good = false;
        sema.Error( "Invalid type to bitwise unary not operator" );
    }

    good &= m_Expression->PerformSema( sema );

    return good;
}

llvm::Value* UnaryExpression::CodeGen( CodeGenerator& code_gen ) const
{
    switch( m_Operator )
    {
    case Op::PLUS:
        return m_Expression->CodeGen( code_gen );
    case Op::MINUS:
        return code_gen.CreateNeg( *m_Expression );
    case Op::INCREMENT:
    case Op::DECREMENT:
        assert( false && "Complete me" );
        return nullptr;
    case Op::BITWISE_NOT:
        return code_gen.CreateNot( *m_Expression );
    case Op::LOGICAL_NOT:
        return code_gen.CreateLNot( *m_Expression );
    default:
        assert( false &&
                "Trying to generate code for unhandled unary operator" );
        return nullptr;
    }
}

Type UnaryExpression::GetReturnType() const
{
    Type t = m_Expression->GetReturnType();

    switch( m_Operator )
    {
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
    return Type::UNKNOWN;
}

Type UnaryExpression::GetUnderlyingType() const
{
    return GetReturnType();
}

const std::vector<unsigned>& UnaryExpression::GetArrayExtents() const
{
    assert( m_Expression->GetArrayExtents().size() == 0 &&
            "Unary Expression given an array" );
    return m_Expression->GetArrayExtents();
}

bool UnaryExpression::IsConst() const
{
    return m_Expression->IsConst() &&
           m_Operator != Op::INCREMENT &&
           m_Operator != Op::DECREMENT;
}

void UnaryExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Unary Expression\n";
    m_Expression->Print( depth + 1 );
}

bool UnaryExpression::Parse( Parser& parser,
                             Expression_up& token )
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
        Expression_up unary_expression;
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
    return e->GetSubClassID() == TokenTy::UnaryExpression;
}

bool UnaryExpression::classof( const UnaryExpression* e )
{
    return true;
}

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

bool PostfixExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_Expression->ResolveIdentifiers( sema );
}

bool PostfixExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;
    good &= m_Expression->PerformSema( sema );
    good &= m_PostfixOperator->PerformSema( sema, m_Expression );
    return good;
}

llvm::Value* PostfixExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGen( code_gen, m_Expression );
}

llvm::Value* PostfixExpression::CodeGenPointerTo(
                                                CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGenPointerTo( code_gen, m_Expression );
}

Type PostfixExpression::GetReturnType() const
{
    return m_PostfixOperator->GetReturnType( m_Expression );
}

Type PostfixExpression::GetUnderlyingType() const
{
    return m_PostfixOperator->GetUnderlyingType( m_Expression );
}

const std::vector<unsigned>& PostfixExpression::GetArrayExtents() const
{
    return m_PostfixOperator->GetArrayExtents( m_Expression );
}

bool PostfixExpression::IsConst() const
{
    return m_PostfixOperator->IsConst( *m_Expression );
}

bool PostfixExpression::IsLValue() const
{
    return m_PostfixOperator->IsLValue( *m_Expression );
}

void PostfixExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Postfix Expression\n";
    m_Expression->Print( depth + 1 );
    m_PostfixOperator->Print( depth + 1 );
}

bool PostfixExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try and parse the primary expression
    Expression_up primary_expression;
    if( !parser.Expect<PrimaryExpression>( primary_expression ) )
        return false;

    // try and parse all the postfix operators
    std::vector< std::unique_ptr<PostfixOperator> > operators;
    parser.ExpectSequenceOf<PostfixOperator>( operators );
    CHECK_PARSER;

    // set ret to just the primary expression
    Expression_up ret = std::move( primary_expression );

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

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

bool PrimaryExpression::Parse( Parser& parser,
                               Expression_up& token )
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
    :Expression( TokenTy::IdentifierExpression )
    ,m_Identifier( std::move( identifier ) )
    ,m_Variable( nullptr )
{
    assert( !m_Identifier.empty() );
}

IdentifierExpression::~IdentifierExpression()
{
}

bool IdentifierExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Variable = sema.GetVariable( m_Identifier );
    if( !m_Variable )
    {
        sema.Error( "Undeclared variable: " + m_Identifier );
        return false;
    }
    return true;
}

llvm::Value* IdentifierExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_Variable &&
            "Trying to generate code for an unresolved variable" );
    return code_gen.CreateVariableRead( *m_Variable );
}

llvm::Value* IdentifierExpression::CodeGenPointerTo(
                                                 CodeGenerator& code_gen ) const
{
    assert( m_Variable &&
            "Trying to generate code for an unresolved variable" );
    return m_Variable->GetLLVMPointer();
}

Type IdentifierExpression::GetReturnType() const
{
    if( !m_Variable )
        return Type::UNKNOWN;
    return m_Variable->GetType();
}

Type IdentifierExpression::GetUnderlyingType() const
{
    if( !m_Variable )
        return Type::UNKNOWN;
    return m_Variable->GetUnderlyingType();
}

const std::vector<unsigned>& IdentifierExpression::GetArrayExtents() const
{
    assert( m_Variable &&
            "Trying to get the array extents of an unresolved identifier" );
    return m_Variable->GetArrayExtents();
}

bool IdentifierExpression::IsLValue() const
{
    return true;
}

bool IdentifierExpression::IsConst() const
{
    return m_Variable->IsConst();
}

const std::shared_ptr<Variable>& IdentifierExpression::GetVariable() const
{
    return m_Variable;
}

bool IdentifierExpression::PerformSema( SemaAnalyzer& sema )
{
    this->ResolveIdentifiers( sema );
    return bool(m_Variable);
}

void IdentifierExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_Identifier << "\n";
}

bool IdentifierExpression::Parse( Parser& parser,
                                  Expression_up& token )
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
    return e->GetSubClassID() == TokenTy::IdentifierExpression;
}

bool IdentifierExpression::classof( const IdentifierExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
