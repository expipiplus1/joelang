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

#include "binary_operator_expression.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <utility>

#include <joelang/types.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/assignment_operator.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

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

    const CompleteType& left_type = m_LeftSide->GetType();
    const CompleteType& right_type = m_RightSide->GetType();

    CompleteType t = GetCommonType( left_type, right_type );
    if( t.IsUnknown() )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operands to binary operator: " +
                        left_type.GetString() + " and " +
                        right_type.GetString() );
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

CompleteType BinaryOperatorExpression::GetType() const
{
    return GetCommonType( m_LeftSide->GetType(),
                          m_RightSide->GetType() );
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

CompleteType LogicalOrExpression::GetType() const
{
    /// todo vectors of bool
    return CompleteType( Type::BOOL );
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

CompleteType LogicalAndExpression::GetType() const
{
    return CompleteType( Type::BOOL );
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
    const CompleteType& t = GetType();

    if( !t.IsIntegral() )
    {
        good = false;
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operand types to to inclusive or operator: " +
                        m_LeftSide->GetType().GetString() + " and " +
                        m_RightSide->GetType().GetString() );
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
    const CompleteType& t = GetType();

    if( !t.IsIntegral() )
    {
        good = false;
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operand types to to enclusive or operator: " +
                        m_LeftSide->GetType().GetString() + " and " +
                        m_RightSide->GetType().GetString() );
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
    const CompleteType& t = GetType();

    if( !t.IsIntegral() )
    {
        good = false;
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operand types to to and operator: " +
                        m_LeftSide->GetType().GetString() + " and " +
                        m_RightSide->GetType().GetString() );
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

CompleteType EqualityExpression::GetType() const
{
    return CompleteType( Type::BOOL );
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

CompleteType RelationalExpression::GetType() const
{
    return CompleteType( Type::BOOL );
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
    const CompleteType& t = GetType();

    if( !t.IsIntegral() )
    {
        good = false;
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operand types to to shift operator: " +
                        m_LeftSide->GetType().GetString() + " and " +
                        m_RightSide->GetType().GetString() );
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

    const CompleteType& t = GetType();

    if( !t.IsIntegral() )
    {
        good = false;
        if( !m_LeftSide->GetType().IsUnknown() &&
            !m_RightSide->GetType().IsUnknown() )
            sema.Error( "Invalid operand types to to modulo operator: " +
                        m_LeftSide->GetType().GetString() + " and " +
                        m_RightSide->GetType().GetString() );
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

} // namespace Compiler
} // namespace JoeLang
