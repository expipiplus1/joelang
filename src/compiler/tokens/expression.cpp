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

#include <algorithm>
#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/assignment_operator.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/binary_operator_expression.hpp>
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

    // Assuming that we can assign to the same as ReturnType;
    m_AssignedExpression = CastExpression::Create(
                                              m_Assignee->GetReturnType(),
                                              std::move(m_AssignedExpression) );
    good &= m_AssignedExpression->PerformSema( sema );

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

    return good;
}

llvm::Value* AssignmentExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_AssigneePtr &&
            "Trying to codegen an assignmentexpression with null assigneeptr" );
    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        return code_gen.CreateAssignment( *m_AssigneePtr,
                                          *m_AssignedExpression );
        break;
    default:
        assert( false && "unhandled assignment operator" );
        return nullptr;
    }
}

llvm::Value* AssignmentExpression::CodeGenPointerTo(
                                                  CodeGenerator& code_gen) const
{
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

const ArrayExtents& AssignmentExpression::GetArrayExtents() const
{
    return m_AssigneePtr->GetArrayExtents();
}

bool AssignmentExpression::IsLValue() const
{
    return true;
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

const ArrayExtents& ConditionalExpression::GetArrayExtents() const
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
// CastExpression
//------------------------------------------------------------------------------

CastExpression::CastExpression( Type cast_type,
                                Expression_up expression )
    :Expression( TokenTy::CastExpression )
    ,m_CastType( cast_type )
    ,m_Expression( std::move(expression) )
{
    assert( m_Expression && "CastExpression given a null expression" );
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

    if( t != Type::UNKNOWN )
    {
        if( t == Type::STRING && m_CastType != Type::STRING )
        {
            good = false;
            sema.Error( "Can't cast string to " + GetTypeString( m_CastType ) );
        }
        else if( m_CastType == Type::UNKNOWN )
        {
            // Can't cast to unknown type
            good = false;
            sema.Error( "Can't cast to an unknown type" );
        }
        else if( m_CastType == Type::ARRAY )
        {
            // Can't cast to an array type
            good = false;
            sema.Error( "Can't cast to an array type" );
        }
        else if( t == Type::ARRAY )
        {
            // Can't cast from an array type
            good = false;
            sema.Error( "Can't cast from an array type" );
        }
        else if( m_CastType == Type::STRING )
        {
            // Can only cast string to string
            if( t != Type::STRING )
            {
                good = false;
                sema.Error( "Can't cast " + GetTypeString( t ) + " to string" );
            }
        }
        else if( IsScalarType( m_CastType ) )
        {
            // All scalar types are compatible nothing to check
        }
        else if( IsVectorType( m_CastType ) )
        {
            // Can only cast same size vectors
            // But all vector types are compatible
            if( GetVectorSize( m_CastType ) != GetVectorSize( t ) )
            {
                good = false;
                sema.Error( "Can't cast " + GetTypeString( t ) + " to " +
                            GetTypeString( m_CastType ) );
            }
        }
        else
        {
            std::cout << GetTypeString( m_CastType ) << std::endl;
            assert( false && "Casting to an unhandled type" );
        }
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

const ArrayExtents& CastExpression::GetArrayExtents() const
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

Expression_up CastExpression::Create( Type cast_type,
                                      Expression_up cast_expression )
{
    if( cast_expression->GetReturnType() == cast_type )
        return cast_expression;
    return Expression_up( new CastExpression( cast_type,
                                              std::move(cast_expression) ) );
}

Expression_up CastExpression::CreateBaseTypeCast(
                                                Type cast_type,
                                                Expression_up cast_expression )
{
    Type expression_type = cast_expression->GetReturnType();
    if( IsScalarType( expression_type ) )
        return Create( cast_type, std::move(cast_expression) );

    assert( IsVectorType( expression_type ) &&
            "Unhandled type in CreateBaseTypeCase" );

    // Find the correct cast_type
    return Create( GetVectorType( cast_type,
                                  GetNumElementsInType( expression_type ) ),
                   std::move(cast_expression) );
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

const ArrayExtents& UnaryExpression::GetArrayExtents() const
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

const ArrayExtents& PostfixExpression::GetArrayExtents() const
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

//------------------------------------------------------------------------------
// TypeConstructorExpression
//------------------------------------------------------------------------------

TypeConstructorExpression::TypeConstructorExpression(
                                         Type type,
                                         std::vector<Expression_up> arguments )
    :Expression( TokenTy::TypeConstructorExpression )
    ,m_Type( type )
    ,m_Arguments( std::move(arguments) )
{
#if !defined(NDEBUG)
    assert( type != Type::UNKNOWN &&
            "TypeConstructorExpression given an unknown type" );
    for( const auto& argument : m_Arguments )
        assert( argument && "TypeConstructorExpression given a null argument" );
#endif
}

TypeConstructorExpression::~TypeConstructorExpression()
{
}

bool TypeConstructorExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    bool ret = true;
    for( const auto& argument : m_Arguments )
        ret &= argument->ResolveIdentifiers( sema );
    return ret;
}

bool TypeConstructorExpression::PerformSema( SemaAnalyzer& sema )
{
    //
    // Verify that we have the correct number of parameters for this type
    //
    unsigned num_desired_elements = GetNumElementsInType( m_Type );
    unsigned num_elements = 0;
    for( const auto& argument : m_Arguments )
        num_elements += GetNumElementsInType( argument->GetReturnType() );

    if( num_elements != num_desired_elements )
        sema.Error( "Wrong number of elements in constructor" );

    //
    // Verify that all the parameters can be converted into the correct base
    // type
    //
    for( auto& argument : m_Arguments )
        argument = CastExpression::CreateBaseTypeCast( GetElementType( m_Type ),
                                                       std::move(argument) );

    bool ret = true;

    for( const auto& argument : m_Arguments )
        ret &= argument->PerformSema( sema );

    return ret;
}

llvm::Value* TypeConstructorExpression::CodeGen( CodeGenerator& code_gen ) const
{
    if( IsVectorType( m_Type ) )
    {
        return code_gen.CreateVectorConstructor( m_Type, m_Arguments );
    }
    else
    {
        assert( IsScalarType( m_Type ) &&
                "Trying to construct an unhandled type" );
        assert( m_Arguments.size() == 1 &&
                "Trying to construct scalar type with wrong number of "
                "arguments" );
        return code_gen.CreateScalarConstructor( m_Type, *m_Arguments[0] );
    }
}

Type TypeConstructorExpression::GetReturnType() const
{
    return m_Type;
}

Type TypeConstructorExpression::GetUnderlyingType() const
{
    return m_Type;
}

const ArrayExtents& TypeConstructorExpression::GetArrayExtents() const
{
    static const ArrayExtents empty = {};
    return empty;
}

bool TypeConstructorExpression::IsConst() const
{
    for( const auto& argument : m_Arguments )
        if( !argument->IsConst() )
            return false;
    return true;
}

bool TypeConstructorExpression::IsLValue() const
{
    return false;
}

void TypeConstructorExpression::Print( int depth ) const
{
}

bool TypeConstructorExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try and parse the primary expression
    if( parser.Expect<PrimaryExpression>( token ) )
        return true;
    CHECK_PARSER;

    std::unique_ptr<TypeSpecifier> type_specifier;
    if( !parser.Expect<TypeSpecifier>(type_specifier) )
        return false;

    //
    // match opening bracket or brace
    //
    bool round_brackets = parser.ExpectTerminal( TerminalType::OPEN_ROUND );
    if( !round_brackets &&
        !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    std::vector<Expression_up> arguments;
    parser.ExpectListOf<AssignmentExpression, TerminalType::COMMA>( arguments );
    CHECK_PARSER;

    //
    // match closing bracket or brace
    //
    if( !parser.ExpectTerminal( round_brackets ? TerminalType::CLOSE_ROUND
                                               : TerminalType::CLOSE_BRACE ) )
        return false;

    Type type = type_specifier->GetType();
    token.reset( new TypeConstructorExpression( type, std::move(arguments) ) );
    return true;
}

bool TypeConstructorExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::TypeConstructorExpression;
}

bool TypeConstructorExpression::classof( const TypeConstructorExpression* e )
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

const ArrayExtents& IdentifierExpression::GetArrayExtents() const
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
