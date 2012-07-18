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
#include <limits>
#include <memory>
#include <sstream>
#include <string>
#include <utility>

#include <llvm/DerivedTypes.h>
#include <llvm/Instructions.h>
#include <llvm/IRBuilder.h>
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>

#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/operator.hpp>
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

void Expression::FoldConstants( Expression_up& self )
{
}

bool Expression::IsLValue() const
{
    return false;
}

bool Expression::Parse( Parser& parser, Expression_up& token )
{
    // TODO comma sep expressions
    return parser.Expect<AssignmentExpression>( token );
}

LiteralExpression* Expression::GetLiteral(
                                        const Expression_up& e )
{
    assert( e && "Trying to get the literal expression of nullptr" );

    LiteralExpression* l = nullptr;
    if( isa<LiteralExpression>( e ) )
    {
        // If this is a literal expression, just return that
        l = static_cast<LiteralExpression*>( e.get() );
    }
    else if( isa<IdentifierExpression>( e ) )
    {
        // Otherwise, see if this is a const variable from which we can get an
        // identifier
        IdentifierExpression* i =
                        static_cast<IdentifierExpression*>( e.get() );
        if( i->IsConst() )
        {
            const Expression_up& read_expression = i->GetReadExpression();
            if( isa<LiteralExpression>( read_expression ) )
                l = static_cast<LiteralExpression*>( read_expression.get() );
        }
    }

    return l;
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
}

AssignmentExpression::~AssignmentExpression()
{
}

void AssignmentExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Assignee->ResolveIdentifiers( sema );
    m_AssignedExpression->ResolveIdentifiers( sema );
}

bool AssignmentExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    // Extract the identifier from the assignee
    good &= m_Assignee->PerformSema( sema );

    if( !isa<IdentifierExpression>(m_Assignee) )
    {
        good = false;
        sema.Error( "Trying to assign to a RValue" );
    }
    else
    {
        IdentifierExpression* i =
                        static_cast<IdentifierExpression*>( m_Assignee.get() );
        if( i->IsConst() )
        {
            good = false;
            sema.Error( "Trying to assign to a const value" );
        }
        else
        {
            m_AssigneeVariable = i->GetVariable();
        }
    }

    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        break;
    case Op::SHL_EQUALS:
        m_AssignedExpression.reset(
                    new ShiftExpression(
                                      BinaryOperatorExpression::Op::LEFT_SHIFT,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::SHR_EQUALS:
        m_AssignedExpression.reset(
                    new ShiftExpression(
                                      BinaryOperatorExpression::Op::RIGHT_SHIFT,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::AND_EQUALS:
        m_AssignedExpression.reset(
                    new AndExpression(
                                      BinaryOperatorExpression::Op::AND,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::XOR_EQUALS:
        m_AssignedExpression.reset(
                    new ExclusiveOrExpression(
                                      BinaryOperatorExpression::Op::XOR,
                                      std::move(m_Assignee),
                                      std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::OR_EQUALS:
        m_AssignedExpression.reset(
                    new InclusiveOrExpression(
                                      BinaryOperatorExpression::Op::OR,
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
                    new MultiplicativeExpression(
                                        BinaryOperatorExpression::Op::MULTIPLY,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::DIVIDE_EQUALS:
        m_AssignedExpression.reset(
                    new MultiplicativeExpression(
                                        BinaryOperatorExpression::Op::DIVIDE,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    case Op::MODULO_EQUALS:
        m_AssignedExpression.reset(
                    new MultiplicativeExpression(
                                        BinaryOperatorExpression::Op::MODULO,
                                        std::move(m_Assignee),
                                        std::move(m_AssignedExpression) ) );
        m_AssignmentOperator = Op::EQUALS;
        break;
    }
    m_AssignedExpression = CastExpression::Create(
                                              m_AssigneeVariable->GetType(),
                                              std::move(m_AssignedExpression) );
    good &= m_AssignedExpression->PerformSema( sema );

    return good;
}

llvm::Value* AssignmentExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_AssigneeVariable &&
            "Trying to code_gen to a null variable" );

    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        code_gen.CreateVariableAssignment( *m_AssigneeVariable,
                                           *m_AssignedExpression );
        break;
    default:
        assert( false && "unhandled assignment operator" );
    }

    // Return the new value of the variable
    return code_gen.CreateVariableRead( *m_AssigneeVariable );
}

Type AssignmentExpression::GetReturnType() const
{
    if( m_Assignee )
        return m_Assignee->GetReturnType();
    else
        return m_AssigneeVariable->GetType();
}

Type AssignmentExpression::GetUnderlyingType() const
{
    if( m_Assignee )
        return m_Assignee->GetUnderlyingType();
    else
        return m_AssigneeVariable->GetUnderlyingType();
}

const std::vector<unsigned>& AssignmentExpression::GetArrayExtents() const
{
    if( m_Assignee )
        return m_Assignee->GetArrayExtents();
    else
        return m_AssigneeVariable->GetArrayExtents();
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

    m_Assignee->Print( depth + 1 );
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

void ConditionalExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Condition->ResolveIdentifiers( sema );
    m_TrueExpression->ResolveIdentifiers( sema );
    m_FalseExpression->ResolveIdentifiers( sema );
}

bool ConditionalExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO check that the true and false expressions are of equal array size
    bool good = true;

    m_Condition = CastExpression::Create( Type::BOOL, std::move(m_Condition) );

    Type t = GetReturnType();
    if( t == Type::UNKNOWN_TYPE )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here, so report it
        if( m_TrueExpression->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_FalseExpression->GetReturnType() != Type::UNKNOWN_TYPE )
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

void ConditionalExpression::FoldConstants( Expression_up& self )
{
    // Simplify the sub expressions
    m_Condition->FoldConstants( m_Condition );
    m_TrueExpression->FoldConstants( m_TrueExpression );
    m_FalseExpression->FoldConstants( m_FalseExpression );

    // See if the condition is a boolean literal
    LiteralExpression* l = nullptr;
    if( isa<LiteralExpression>( m_Condition ) )
    {
        l = static_cast<LiteralExpression*>( m_Condition.get() );
    }
    else if( isa<IdentifierExpression>( m_Condition ) )
    {
        IdentifierExpression* i =
                        static_cast<IdentifierExpression*>( m_Condition.get() );
        const Expression_up& read_expression =
                                                         i->GetReadExpression();
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

    self = v.GetBool() ? std::move(m_TrueExpression)
                       : std::move(m_FalseExpression);
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

void BinaryOperatorExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_LeftSide->ResolveIdentifiers( sema );
    m_RightSide->ResolveIdentifiers( sema );
}

bool BinaryOperatorExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO operands can't be arrays
    bool good = true;

    Type t = GetCommonType( m_RightSide->GetReturnType(),
                            m_LeftSide->GetReturnType() );
    if( t == Type::UNKNOWN_TYPE )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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

void BinaryOperatorExpression::FoldConstants(
                                            Expression_up& self )
{
    // If there is a type mismatch, don't fold things
    if( GetReturnType() == Type::UNKNOWN_TYPE )
        return;

    m_LeftSide->FoldConstants( m_LeftSide );
    m_RightSide->FoldConstants( m_RightSide );

    LiteralExpression* l = GetLiteral( m_LeftSide );
    // If it wasn't constant, then we've done as much as we can
    if( !l )
        return;

    LiteralExpression* r = GetLiteral( m_RightSide );
    // If it wasn't constant, then we've done as much as we can
    if( !r )
        return;

    // If it was constant, we can replace this node with just one of the values
    GenericValue v;
    GenericValue lv = l->GetValue();
    GenericValue rv = r->GetValue();

    switch( m_Operator )
    {
    case Op::LOGICAL_OR:
        v = GenericValue::Lor( lv, rv );
        break;
    case Op::LOGICAL_AND:
        v = GenericValue::Land( lv, rv );
        break;
    case Op::OR:
        v = GenericValue::Or( lv, rv );
        break;
    case Op::XOR:
        v = GenericValue::Xor( lv, rv );
        break;
    case Op::AND:
        v = GenericValue::And( lv, rv );
        break;
    case Op::EQUAL_TO:
        v = GenericValue::EqualTo( lv, rv );
        break;
    case Op::NOT_EQUAL_TO:
        v = GenericValue::NotEqualTo( lv, rv );
        break;
    case Op::LESS_THAN:
        v = GenericValue::LessThan( lv, rv );
        break;
    case Op::GREATER_THAN:
        v = GenericValue::GreaterThan( lv, rv );
        break;
    case Op::LESS_THAN_EQUALS:
        v = GenericValue::LessThanEqual( lv, rv );
        break;
    case Op::GREATER_THAN_EQUALS:
        v = GenericValue::GreaterThanEqual( lv, rv );
        break;
    case Op::LEFT_SHIFT:
        v = GenericValue::Shl( lv, rv );
        break;
    case Op::RIGHT_SHIFT:
        v = GenericValue::Shr( lv, rv );
        break;
    case Op::PLUS:
        v = GenericValue::Add( lv, rv );
        break;
    case Op::MINUS:
        v = GenericValue::Sub( lv, rv );
        break;
    case Op::MULTIPLY:
        v = GenericValue::Mul( lv, rv );
        break;
    case Op::DIVIDE:
        v = GenericValue::Div( lv, rv );
        break;
    case Op::MODULO:
        v = GenericValue::Mod( lv, rv );
        break;
    }

    assert( v.GetType() == GetReturnType() &&
            "Return type mismatch in constant folding" );

    self = LiteralExpression::Create( v );
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
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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
        if( m_LeftSide->GetReturnType() != Type::UNKNOWN_TYPE &&
            m_RightSide->GetReturnType() != Type::UNKNOWN_TYPE )
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

void CastExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Expression->ResolveIdentifiers( sema );
}

bool CastExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    Type t = m_Expression->GetReturnType();

    if( m_CastType == Type::UNKNOWN_TYPE )
    {
        good = false;
        sema.Error( "Can't cast to an unknown type" );
    }
    else if( m_CastType == Type::STRING )
        if( t != Type::STRING &&
            t != Type::UNKNOWN_TYPE )
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

void CastExpression::FoldConstants( Expression_up& self )
{
    m_Expression->FoldConstants( m_Expression );

    Type t = m_Expression->GetReturnType();

    // If this is an incompatible cast, return
    if( m_CastType == Type::UNKNOWN_TYPE ||
        t == Type::UNKNOWN_TYPE ||
        ( t == Type::STRING && m_CastType != Type::STRING ) ||
        ( m_CastType == Type::STRING && t != Type::STRING ) )
        return;

    LiteralExpression* l = GetLiteral( m_Expression );

    // If we are casting a literal expression, perform the cast now
    if( l )
    {
        GenericValue v = GenericValue::Cast( m_CastType, l->GetValue() );
        self = LiteralExpression::Create( v );
        return;
    }

    // It may not be a literal expression, but it may be another cast expression
    if( isa<CastExpression>(m_Expression) )
    {
        CastExpression* c = static_cast<CastExpression*>( m_Expression.get() );
        m_Expression = std::move(c->m_Expression);
    }
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

void UnaryExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Expression->ResolveIdentifiers( sema );
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

void UnaryExpression::FoldConstants( Expression_up& self )
{
    m_Expression->FoldConstants( m_Expression );

    LiteralExpression* l = GetLiteral( m_Expression );

    if( l )
    {
        GenericValue v( l->GetValue() );

        switch( m_Operator )
        {
        case Op::PLUS:
            self = LiteralExpression::Create( GenericValue::UnaryPlus( v ) );
            return;
        case Op::MINUS:
            self = LiteralExpression::Create( GenericValue::UnaryMinus( v ) );
            return;
        case Op::BITWISE_NOT:
            self = LiteralExpression::Create( GenericValue::BitwiseNot( v ) );
            return;
        case Op::LOGICAL_NOT:
            self = LiteralExpression::Create( GenericValue::LogicalNot( v ) );
            return;
        default:
            break;
        }
    }
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
    return Type::UNKNOWN_TYPE;
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

void PostfixExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Expression->ResolveIdentifiers( sema );
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

void IdentifierExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Variable = sema.GetVariable( m_Identifier );
    if( !m_Variable )
        sema.Error( "Undeclared variable: " + m_Identifier );
}

llvm::Value* IdentifierExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_Variable &&
            "Trying to generate code for an unresolved variable" );
    return code_gen.CreateVariableRead( *m_Variable );
}

Type IdentifierExpression::GetReturnType() const
{
    if( !m_Variable )
        return Type::UNKNOWN_TYPE;
    return m_Variable->GetType();
}

Type IdentifierExpression::GetUnderlyingType() const
{
    if( !m_Variable )
        return Type::UNKNOWN_TYPE;
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

const Expression_up& IdentifierExpression::GetReadExpression() const
{
    return m_Variable->GetReadExpression();
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

//------------------------------------------------------------------------------
// LiteralExpression
//------------------------------------------------------------------------------

LiteralExpression::LiteralExpression( TokenTy sub_class_id )
    :Expression( sub_class_id )
{
}

void LiteralExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
}

bool LiteralExpression::PerformSema( SemaAnalyzer& sema )
{
    return true;
}

Type LiteralExpression::GetUnderlyingType() const
{
    return GetReturnType();
}

const std::vector<unsigned>& LiteralExpression::GetArrayExtents() const
{
    const static std::vector<unsigned> empty;
    return empty;
}

bool LiteralExpression::IsConst() const
{
    return true;
}

bool LiteralExpression::Parse( Parser& parser,
                               Expression_up& token )
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

std::unique_ptr<LiteralExpression> LiteralExpression::Create(
                                                         const GenericValue& v )
{
    switch( v.GetType() )
    {
    case Type::BOOL:
        return std::unique_ptr<LiteralExpression>(
                                new BooleanLiteralExpression( v.GetBool() ) );
    case Type::I8:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI8(),
                                 IntegerLiteralExpression::Suffix::CHAR ) );
    case Type::I16:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI16(),
                                 IntegerLiteralExpression::Suffix::SHORT ) );
    case Type::I32:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI32(),
                                 IntegerLiteralExpression::Suffix::INT ) );
    case Type::I64:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                                 v.GetI64(),
                                 IntegerLiteralExpression::Suffix::LONG ) );
    case Type::U8:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU8(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_CHAR ) );
    case Type::U16:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU16(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_SHORT ) );
    case Type::U32:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU32(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_INT ) );
    case Type::U64:
        return std::unique_ptr<LiteralExpression>( new IntegerLiteralExpression(
                           v.GetU64(),
                           IntegerLiteralExpression::Suffix::UNSIGNED_LONG ) );
    case Type::FLOAT:
        return std::unique_ptr<LiteralExpression>(new FloatingLiteralExpression(
                                v.GetFloat(),
                                FloatingLiteralExpression::Suffix::SINGLE ) );
    case Type::DOUBLE:
        return std::unique_ptr<LiteralExpression>(new FloatingLiteralExpression(
                                v.GetDouble(),
                                FloatingLiteralExpression::Suffix::NONE ) );
    case Type::STRING:
        return std::unique_ptr<LiteralExpression>( new StringLiteralExpression(
                                                            v.GetString() ) );
    default:
        assert( false &&
                "Trying to create LiteralExpression with an unhandled type" );
    }
    return nullptr;
}

bool LiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() >= TokenTy::LiteralExpression_Start &&
           e->GetSubClassID() <= TokenTy::LiteralExpression_End;
}

bool LiteralExpression::classof( const LiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// IntegerlLiteralExpression
//------------------------------------------------------------------------------

IntegerLiteralExpression::IntegerLiteralExpression( jl_u64 value,
                                                    Suffix suffix )
    :LiteralExpression( TokenTy::IntegerLiteralExpression )
    ,m_Value( value )
    ,m_Suffix( suffix )
{
}

IntegerLiteralExpression::~IntegerLiteralExpression()
{
}

llvm::Value* IntegerLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, GetReturnType() );
}

Type IntegerLiteralExpression::GetReturnType() const
{
    switch( m_Suffix )
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
        return m_Value <= jl_u64(std::numeric_limits<jl_i32>::max())
                    ? Type::I32
                    : m_Value <= jl_u64(std::numeric_limits<jl_u32>::max())
                        ? Type::U32
                        : m_Value <= jl_u64(std::numeric_limits<jl_i64>::max())
                            ? Type::I64
                            : Type::U64;
    }
}

GenericValue IntegerLiteralExpression::GetValue() const
{
    switch( m_Suffix )
    {
    case Suffix::CHAR:
        return GenericValue( jl_i8(m_Value) );
    case Suffix::INT:
        return GenericValue( jl_i32(m_Value) );
    case Suffix::SHORT:
        return GenericValue( jl_i16(m_Value) );
    case Suffix::LONG:
        return GenericValue( jl_i64(m_Value) );
    case Suffix::UNSIGNED_CHAR:
        return GenericValue( jl_u8(m_Value) );
    case Suffix::UNSIGNED_INT:
        return GenericValue( jl_u32(m_Value) );
    case Suffix::UNSIGNED_SHORT:
        return GenericValue( jl_u16(m_Value) );
    case Suffix::UNSIGNED_LONG:
        return GenericValue( jl_u64(m_Value) );
    default:
        return m_Value <= jl_u64(std::numeric_limits<jl_i32>::max())
                    ? GenericValue( jl_i32(m_Value) )
                    : m_Value <= jl_u64(std::numeric_limits<jl_u32>::max())
                        ? GenericValue( jl_u32(m_Value) )
                        : m_Value <= jl_u64(std::numeric_limits<jl_i64>::max())
                            ? GenericValue( jl_i64(m_Value) )
                            : GenericValue( jl_u64(m_Value) );
    }
}

void IntegerLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_Value << "\n";
}

bool IntegerLiteralExpression::ParseInteger( std::string string,
                                             jl_u64& value,
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

    jl_u64 value;
    Suffix suffix;

    if( !ParseInteger( string, value, suffix ) )
        return false;

    token.reset( new IntegerLiteralExpression( value, suffix ) );
    return true;
}

bool IntegerLiteralExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::IntegerLiteralExpression;
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
    :LiteralExpression( TokenTy::FloatingLiteralExpression )
    ,m_Value( value )
    ,m_Suffix( suffix )
{
}

FloatingLiteralExpression::~FloatingLiteralExpression()
{
}

llvm::Value* FloatingLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateFloating( m_Value,
                                    m_Suffix == Suffix::SINGLE ? Type::FLOAT
                                                               : Type::DOUBLE );
}

Type FloatingLiteralExpression::GetReturnType() const
{
    if( m_Suffix == Suffix::SINGLE )
        return Type::FLOAT;
    else
        return Type::DOUBLE;
}

GenericValue FloatingLiteralExpression::GetValue() const
{
    switch( m_Suffix )
    {
    case Suffix::SINGLE:
        return GenericValue( jl_float(m_Value) );
    default:
        return GenericValue( jl_double(m_Value) );
    }
}

void FloatingLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << m_Value << "\n";
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
    return e->GetSubClassID() == TokenTy::FloatingLiteralExpression;
}

bool FloatingLiteralExpression::classof( const FloatingLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// BooleanLiteralExpression
//------------------------------------------------------------------------------

BooleanLiteralExpression::BooleanLiteralExpression( bool value )
    :LiteralExpression( TokenTy::BooleanLiteralExpression )
    ,m_Value( value )
{
}

BooleanLiteralExpression::~BooleanLiteralExpression()
{
}

llvm::Value* BooleanLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, Type::BOOL );
}

Type BooleanLiteralExpression::GetReturnType() const
{
    return Type::BOOL;
}

GenericValue BooleanLiteralExpression::GetValue() const
{
    return GenericValue( jl_bool(m_Value) );
}

void BooleanLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << std::boolalpha << m_Value << std::noboolalpha << "\n";
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
    return e->GetSubClassID() == TokenTy::BooleanLiteralExpression;
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
    :LiteralExpression( TokenTy::BooleanLiteralExpression )
    ,m_Value( std::move(value) )
{
}

StringLiteralExpression::~StringLiteralExpression()
{
}

llvm::Value* StringLiteralExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return code_gen.CreateString( m_Value );
}

Type StringLiteralExpression::GetReturnType() const
{
    return Type::STRING;
}

GenericValue StringLiteralExpression::GetValue() const
{
    return GenericValue( jl_string(m_Value) );
}

void StringLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\"' << m_Value << "\"\n";
}

const std::string& StringLiteralExpression::GetString() const
{
    return m_Value;
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
    return e->GetSubClassID() == TokenTy::StringLiteralExpression;
}

bool StringLiteralExpression::classof( const StringLiteralExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// CharacterLiteralExpression
//------------------------------------------------------------------------------

CharacterLiteralExpression::CharacterLiteralExpression( char value )
    :LiteralExpression( TokenTy::CharacterLiteralExpression )
    ,m_Value( value )
{
}

CharacterLiteralExpression::~CharacterLiteralExpression()
{
}

llvm::Value* CharacterLiteralExpression::CodeGen(
                                                CodeGenerator& code_gen ) const
{
    return code_gen.CreateInteger( m_Value, Type::I8 );
}

Type CharacterLiteralExpression::GetReturnType() const
{
    return Type::I8;
}

GenericValue CharacterLiteralExpression::GetValue() const
{
    return GenericValue( jl_i8(m_Value) );
}

void CharacterLiteralExpression::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << '\'' << m_Value << '\'' << "\n";
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
    return e->GetSubClassID() == TokenTy::CharacterLiteralExpression;
}

bool CharacterLiteralExpression::classof( const CharacterLiteralExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
