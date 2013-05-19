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
#include <memory>
#include <set>
#include <string>
#include <utility>

#include <joelang/types.hpp>
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/token.hpp>
#include <compiler/tokens/expressions/assignment_operator.hpp>
#include <compiler/tokens/expressions/binary_operator_expression.hpp>
#include <compiler/tokens/expressions/literal_expression.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>

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
    if( IsLValue() )
        assert( false && "Complete me" );
    assert( false && "Trying to write to an rvalue" );
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
                                              m_Assignee->GetType(),
                                              std::move(m_AssignedExpression),
                                              false );
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

    //
    // Get the swizzle data if there is any
    //
    Swizzle swizzle;
    if( PostfixExpression* p = dyn_cast<PostfixExpression>(m_AssigneePtr) )
        if( MemberAccessOperator* m =
                             dyn_cast<MemberAccessOperator>(&p->GetOperator()) )
            if( m->IsSwizzle() )
                swizzle = m->GetSwizzle();

    switch( m_AssignmentOperator )
    {
    case Op::EQUALS:
        return code_gen.CreateAssignment( *m_AssigneePtr,
                                          *m_AssignedExpression,
                                          swizzle );
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

void AssignmentExpression::Write( ShaderWriter& shader_writer ) const
{
    // Todo this isn't right, need to handle += and friends properly
    shader_writer << *m_AssigneePtr << " = " << *m_AssignedExpression;
}

CompleteType AssignmentExpression::GetType() const
{
    return m_AssigneePtr->GetType();
}

std::set<Function_sp> AssignmentExpression::GetCallees() const
{
    //assert( false && "Do assignment properly" );
    auto ret = m_AssigneePtr->GetCallees();
    auto f   = m_AssignedExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> AssignmentExpression::GetVariables() const
{
    //assert( false && "Do assignment properly" );
    auto ret = m_Assignee->GetVariables();
    auto f   = m_AssignedExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> AssignmentExpression::GetWrittenToVariables(
                                                        bool is_assigned ) const
{
    auto ret = m_Assignee->GetWrittenToVariables( true );
    auto f   = m_AssignedExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    return ret;
}

bool AssignmentExpression::IsLValue() const
{
    return true;
}

bool AssignmentExpression::IsConst() const
{
    return false;
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

    m_Condition = CastExpression::Create( Type::BOOL,
                                          std::move(m_Condition),
                                          false );

    const CompleteType& t = GetType();
    if( t.IsUnknown() )
    {
        good = false;

        // If both of the sub expressions are fine, then we know the problem's
        // here, so report it
        if( !m_TrueExpression->GetType().IsUnknown() &&
            !m_FalseExpression->GetType().IsUnknown() )
            sema.Error( "Incompatable operand types in conditional expression "+
                        m_TrueExpression->GetType().GetString() +
                        " and " +
                        m_FalseExpression->GetType().GetString() );
    }
    else
    {
        m_TrueExpression = CastExpression::Create(
                                                t,
                                                std::move(m_TrueExpression),
                                                false );
        m_FalseExpression = CastExpression::Create(
                                                t,
                                                std::move(m_FalseExpression),
                                                false );
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

void ConditionalExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << "(" << *m_Condition << " ? " << *m_TrueExpression <<
                                            " : " << *m_FalseExpression << ")";
}

CompleteType ConditionalExpression::GetType() const
{
    return GetCommonType( m_TrueExpression->GetType(),
                          m_FalseExpression->GetType() );
}

std::set<Function_sp> ConditionalExpression::GetCallees() const
{
    auto ret = m_Condition->GetCallees();
    auto f   = m_TrueExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> ConditionalExpression::GetVariables() const
{
    auto ret = m_Condition->GetVariables();
    auto f   = m_TrueExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> ConditionalExpression::GetWrittenToVariables(
                                                        bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a conditional expression" );
    auto ret = m_Condition->GetWrittenToVariables( false );
    auto f   = m_TrueExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    f = m_FalseExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    return ret;
}

bool ConditionalExpression::IsConst() const
{
    /// TODO only check the taken select
    return m_Condition->IsConst() &&
           m_TrueExpression->IsConst() &&
           m_FalseExpression->IsConst();

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

CastExpression::CastExpression( CompleteType cast_type,
                                Expression_up expression,
                                bool is_explicit )
    :Expression( TokenTy::CastExpression )
    ,m_CastType( std::move(cast_type) )
    ,m_Expression( std::move(expression) )
    ,m_IsExplicit( is_explicit )
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
    // todo this in a better way
    bool good;

    const CompleteType& t = m_Expression->GetType();
    
    if( t.IsUnknown() )
    {
        sema.Error( "Trying to cast from unknown type" );
        return false;
    }

    assert( !m_CastType.IsUnknown() && "Trying to cast to unknown type" );
    
    if( t == m_CastType )
        good = true;
    
    if( t.IsScalarType() )
        good = CanCastFromScalar( sema );
    else if( t.IsVectorType() )
        good = CanCastFromVector( sema );
    else if( t.IsMatrixType() )
        good = CanCastFromMatrix( sema );
    else if( t.IsStructType() )
        good = CanCastFromStruct( sema );
    else if( t.IsArrayType() )
        good = CanCastFromArray ( sema );
    else if( t.GetType() == Type::STRING )
        good = CanCastFromString( sema );
    else if( t.IsVoid() )
        good = CanCastFromVoid( sema );
    else 
        assert( false && "Trying to cast from unhandled type" );
    
    good &= m_Expression->PerformSema( sema );

    return good;
}

bool CastExpression::CanCastFromScalar( SemaAnalyzer& sema )
{
    //
    // We can cast from a scalar to a scalar, vector or matrix
    //
    if( m_CastType.IsScalarType() ||
        m_CastType.IsVectorType() ||
        m_CastType.IsMatrixType() )
        return true;
    
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromVector( SemaAnalyzer& sema )
{
    CompleteType t = m_Expression->GetType();
    
    //
    // We issue a warning if a cast to a scalar isn't explicit
    //
    if( m_CastType.IsScalarType() )
    {
        if( !IsExplicit() )
            sema.Warning( "Implicit cast of a vector to a scalar type" );
        return true;
    }
    
    //
    // We can't cast to a larger vector, and issue a warning if casting to a 
    // smaller vector
    //
    if( m_CastType.IsVectorType() )
    {
        if( m_CastType.GetVectorSize() > t.GetVectorSize() )
        {
            sema.Error( "Can't cast a vector to a larger vector type" );
            return false;
        }
        
        if( m_CastType.GetVectorSize() < t.GetVectorSize() )
            sema.Warning( "Casting a vector to a smaller vector type" );    
        return true;
    }
    
    //
    // We can only cast to a matrix if they are of the same size
    //
    if( m_CastType.IsMatrixType() )
    {
        if( m_CastType.GetMatrixWidth() * m_CastType.GetMatrixHeight() !=
            t.GetVectorSize() )
        {
            sema.Error( "Can't cast a vector to a matrix of different size" );
            return false;
        }
        if( !IsExplicit() )
            sema.Warning( "Casting a vector to a matrix" );
        return true;
    }
    
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromMatrix( SemaAnalyzer& sema )
{
    CompleteType t = m_Expression->GetType();
    //
    // Warn if casting to a scalar
    //
    if( m_CastType.IsScalarType() )
    {
        if( !IsExplicit() )
            sema.Warning( "Casting a matrix to a scalar" );
        return true;
    }
    
    //
    // Can only cast to a vector of the same size
    //
    if( m_CastType.IsVectorType() )
    {
        if( t.GetMatrixWidth() * t.GetMatrixHeight() !=
            m_CastType.GetVectorSize() )
        {
            sema.Error( "Can't cast a matrix to a vector of different size" );
            return false;
        }
        if( !IsExplicit() )
            sema.Warning( "Casting a matrix to a vector" );
        return true;
    }
    
    //
    // We can't cast to a larger matrix, and issue a warning if casting to a 
    // smaller matrix
    //
    if( m_CastType.IsMatrixType() )
    {
        if( t.GetMatrixWidth()  < m_CastType.GetMatrixWidth() ||
            t.GetMatrixHeight() < m_CastType.GetMatrixHeight() )
        {
            sema.Error( 
                  "Can't cast a matrix to a matrix type of larger dimensions" );
            return false;
        }
        
        if( ( t.GetMatrixWidth()  > m_CastType.GetMatrixWidth() ||
              t.GetMatrixHeight() > m_CastType.GetMatrixHeight() ) &&
            !IsExplicit() ) 
            sema.Warning( 
                    "Casting a matrix to a matrix type of smaller dimensions" );
        return true;
    }
    
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromStruct( SemaAnalyzer& sema )
{
    //
    // Can't cast from a struct
    // todo allow casting from structs
    //
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromArray( SemaAnalyzer& sema )
{
    //
    // Can't cast from an array
    //
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromString( SemaAnalyzer& sema )
{
    //
    // Can't cast from string
    // todo, perhaps cast to a char[]
    //
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() + 
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::CanCastFromVoid( SemaAnalyzer& sema )
{
    //
    // Can't cast from void
    //
    sema.Error( "Can't cast " + m_Expression->GetType().GetString() +
                " to " + m_CastType.GetString() );
    return false;
}

bool CastExpression::IsExplicit() const
{
    return m_IsExplicit;
}

llvm::Value* CastExpression::CodeGen( CodeGenerator& code_gen ) const
{
    /// todo casting arrays
    return code_gen.CreateCast( *m_Expression, m_CastType );
}

void CastExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << m_CastType << "(" << *m_Expression << ")";
}

CompleteType CastExpression::GetType() const
{
    return m_CastType;
}

std::set<Function_sp> CastExpression::GetCallees() const
{
    return m_Expression->GetCallees();
}

std::set<Variable_sp> CastExpression::GetVariables() const
{
    return m_Expression->GetVariables();
}

std::set<Variable_sp> CastExpression::GetWrittenToVariables(
                                                    bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a cast expression" );
    return m_Expression->GetWrittenToVariables( is_assigned );
}

bool CastExpression::IsConst() const
{
    return m_Expression->IsConst();
}

bool CastExpression::Parse( Parser& parser, Expression_up& token )
{
    // TODO implement c-style casts
    // for the time being, forward the parse to a UnaryExpression
    return parser.Expect<UnaryExpression>( token );
}

Expression_up CastExpression::Create( Type cast_type,
                                      Expression_up expression,
                                      bool is_explicit )
{
    return Create( CompleteType( cast_type ),
                   std::move( expression ),
                   is_explicit );
}

Expression_up CastExpression::Create( const CompleteType& cast_type,
                                      Expression_up expression,
                                      bool is_explicit )
{
    // If this would be a null implicit cast don't bother creating one
    if( expression->GetType() == cast_type && !is_explicit )
        return expression;

    return Expression_up( new CastExpression( cast_type,
                                              std::move(expression),
                                              is_explicit ) );
}

Expression_up CastExpression::CreateBaseTypeCast(
                                                Type cast_type,
                                                Expression_up cast_expression,
                                                bool is_explicit )
{
    Type expression_type = cast_expression->GetType().GetType();
    if( IsScalarType( expression_type ) )
        return Create( cast_type, std::move(cast_expression), is_explicit );

    assert( IsVectorType( expression_type ) &&
            "Unhandled type in CreateBaseTypeCase" );

    // Find the correct cast_type
    return Create( GetVectorType( cast_type,
                                  GetNumElementsInType( expression_type ) ),
                   std::move(cast_expression),
                   is_explicit );
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

    const CompleteType& t = m_Expression->GetType();
    if( t.GetType() == Type::STRING )
    {
        good = false;
        sema.Error( "Invalid type to unary operator: string" );
    }
    if( !t.IsIntegral() &&
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

void UnaryExpression::Write( ShaderWriter& shader_writer ) const
{
    static const std::map<Op, std::string> op_string_map =
    {
        { Op::PLUS,        "+" },
        { Op::MINUS,       "-" },
        { Op::INCREMENT,   "++" },
        { Op::DECREMENT,   "--" },
        { Op::BITWISE_NOT, "~" },
        { Op::LOGICAL_NOT, "!" }
    };
    shader_writer << "(" << op_string_map.at(m_Operator) << *m_Expression << ")";
}

CompleteType UnaryExpression::GetType() const
{
    /// Todo vector and matrix bool things

    CompleteType t = m_Expression->GetType();

    switch( m_Operator )
    {
    case Op::PLUS:
    case Op::MINUS:
    case Op::INCREMENT:
    case Op::DECREMENT:
    case Op::BITWISE_NOT:
        return t;
    case Op::LOGICAL_NOT:
        return CompleteType( Type::BOOL );
    }
    assert( false && "unreachable" );
    return CompleteType();
}

std::set<Function_sp> UnaryExpression::GetCallees() const
{
    return m_Expression->GetCallees();
}

std::set<Variable_sp> UnaryExpression::GetVariables() const
{
    return m_Expression->GetVariables();
}

std::set<Variable_sp> UnaryExpression::GetWrittenToVariables(
                                                    bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a unaryexpression" );
    return m_Expression->GetWrittenToVariables( is_assigned );
}

bool UnaryExpression::IsConst() const
{
    return m_Expression->IsConst() &&
           m_Operator != Op::INCREMENT &&
           m_Operator != Op::DECREMENT;
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

PostfixOperator& PostfixExpression::GetOperator()
{
    return *m_PostfixOperator;
}

bool PostfixExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    return m_PostfixOperator->ResolveIdentifiers( sema, *m_Expression );
}

bool PostfixExpression::PerformSema( SemaAnalyzer& sema )
{
    return m_PostfixOperator->PerformSema( sema, *m_Expression );
}

llvm::Value* PostfixExpression::CodeGen( CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGen( code_gen, *m_Expression );
}

llvm::Value* PostfixExpression::CodeGenPointerTo(
                                                CodeGenerator& code_gen ) const
{
    return m_PostfixOperator->CodeGenPointerTo( code_gen, *m_Expression );
}

void PostfixExpression::Write( ShaderWriter& shader_writer ) const
{
    m_PostfixOperator->Write( shader_writer, *m_Expression );
}

CompleteType PostfixExpression::GetType() const
{
    return m_PostfixOperator->GetType( *m_Expression );
}

std::set<Function_sp> PostfixExpression::GetCallees() const
{
    return m_PostfixOperator->GetCallees( *m_Expression );
}

std::set<Variable_sp> PostfixExpression::GetVariables() const
{
    return m_PostfixOperator->GetVariables( *m_Expression );
}

std::set<Variable_sp> PostfixExpression::GetWrittenToVariables(
                                                     bool is_assigned ) const
{
    return m_PostfixOperator->GetWrittenToVariables( *m_Expression,
                                                     is_assigned );
}

bool PostfixExpression::IsConst() const
{
    return m_PostfixOperator->IsConst( *m_Expression );
}

bool PostfixExpression::IsLValue() const
{
    return m_PostfixOperator->IsLValue( *m_Expression );
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
        num_elements += argument->GetType().GetVectorSize();

    if( num_elements != num_desired_elements )
        sema.Error( "Wrong number of elements in constructor" );

    //
    // Verify that all the parameters can be converted into the correct base
    // type, if there's only one argument, then this is an explicit cast
    // todo matrices
    //
    for( auto& argument : m_Arguments )
        argument = CastExpression::CreateBaseTypeCast(
                                                      GetElementType( m_Type ),
                                                      std::move(argument),
                                                      m_Arguments.size() == 1 );

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

void TypeConstructorExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << CompleteType(m_Type) << "(";
    bool first = true;
    for( const auto& argument : m_Arguments )
    {
        if( !first )
            shader_writer << ", ";
        else
            first = false;
        shader_writer << *argument;
    }
    shader_writer << ")";
}

CompleteType TypeConstructorExpression::GetType() const
{
    /// todo constructing arrays
    return CompleteType( m_Type );
}

std::set<Function_sp> TypeConstructorExpression::GetCallees() const
{
    std::set<Function_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetCallees();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

std::set<Variable_sp> TypeConstructorExpression::GetVariables() const
{
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetVariables();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

std::set<Variable_sp> TypeConstructorExpression::GetWrittenToVariables(
                                                        bool is_assigned) const
{
    assert( !is_assigned && "Trying to assign to a constructor expression");
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetWrittenToVariables( is_assigned );
        ret.insert( f.begin(), f.end() );
    }
    return ret;
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
    {
        parser.Error( "Expected closing ')' in primary expression" );
        return false;
    }

    return true;
}

//------------------------------------------------------------------------------
// IdentifierExpression
//------------------------------------------------------------------------------

IdentifierExpression::IdentifierExpression( std::string identifier )
    :Expression( TokenTy::IdentifierExpression )
    ,m_Identifier( std::move( identifier ) )
{
    assert( !m_Identifier.empty() &&
            "IdentifierExpression given an empty identifier" );
}

IdentifierExpression::~IdentifierExpression()
{
}

bool IdentifierExpression::ResolveIdentifiers( SemaAnalyzer& sema )
{
    m_Variable = sema.GetVariable( m_Identifier );
    if( !m_Variable )
    {
        if( sema.HasFunctionNamed( m_Identifier ) )
            sema.Error( "Using function as variable " + m_Identifier );
        else
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

void IdentifierExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer.WriteVariableName( m_Variable );
}

CompleteType IdentifierExpression::GetType() const
{
    // todo should this assert
    if( !m_Variable )
        return CompleteType();
    return m_Variable->GetType();
}

std::set<Function_sp> IdentifierExpression::GetCallees() const
{
    return std::set<Function_sp>{};
}

std::set<Variable_sp> IdentifierExpression::GetVariables() const
{
    assert( m_Variable &&
            "Trying to get the variables of an unresolved identifier" );
    return { m_Variable };
}

std::set<Variable_sp> IdentifierExpression::GetWrittenToVariables(
                                                     bool is_assigned ) const
{
    assert( m_Variable &&
            "Trying to get the variables of an unresolved identifier" );

    return is_assigned ? std::set<Variable_sp>{ m_Variable } : std::set<Variable_sp>{};
}

const std::string& IdentifierExpression::GetIdentifier() const
{
    return m_Identifier;
}

bool IdentifierExpression::IsLValue() const
{
    return true;
}

bool IdentifierExpression::IsConst() const
{
    assert( m_Variable && "Trying to get constness of an unresolved variable" );
    return m_Variable->IsConst();
}

const std::shared_ptr<Variable>& IdentifierExpression::GetVariable() const
{
    assert( m_Variable && "Trying to get an unresolved variable" );
    return m_Variable;
}

bool IdentifierExpression::PerformSema( SemaAnalyzer& sema )
{
    return bool(m_Variable);
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
