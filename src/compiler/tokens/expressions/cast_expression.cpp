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

#include "cast_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/tokens/expressions/unary_expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/type_node.hpp>

namespace JoeLang
{
namespace Compiler
{

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

bool CastExpression::PerformSemaNoRecurse( SemaAnalyzer& sema )
{
    const CompleteType& t = m_Expression->GetType();

    if( t.IsUnknown() )
    {
        sema.Error( "Trying to cast from unknown type" );
        return false;
    }

    assert( !m_CastType.IsUnknown() && "Trying to cast to unknown type" );

    if( t == m_CastType )
        return true;

    if( t.IsScalarType() )
        return CanCastFromScalar( sema );
    else if( t.IsVectorType() )
        return CanCastFromVector( sema );
    else if( t.IsMatrixType() )
        return CanCastFromMatrix( sema );
    else if( t.IsStructType() )
        return CanCastFromStruct( sema );
    else if( t.IsArrayType() )
        return CanCastFromArray ( sema );
    else if( t.GetType() == Type::STRING )
        return CanCastFromString( sema );
    else if( t.IsVoid() )
        return CanCastFromVoid( sema );
    else
        assert( false && "Trying to cast from unhandled type" );
    return false;
}

bool CastExpression::PerformSema( SemaAnalyzer& sema )
{
    if( !m_Expression->PerformSema( sema ) )
        return false;
    return PerformSemaNoRecurse( sema );
}

const Node& CastExpression::GenerateCodeDag( NodeManager& node_manager ) const
{
    //
    // Don't generate a cast here if it wouldn't do anything
    //
    if( m_Expression->GetType() == m_CastType )
        return m_Expression->GenerateCodeDag( node_manager );
                
    const TypeNode& type = m_CastType.GenerateCodeDag( node_manager );
    const Node& expression = m_Expression->GenerateCodeDag( node_manager );
    return node_manager.MakeNode( NodeType::Cast, {expression, type} );
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
        if( m_CastType.GetNumElements() > t.GetNumElements() )
        {
            sema.Error( "Can't cast a vector to a larger vector type" );
            return false;
        }
        
        if( m_CastType.GetNumElements() < t.GetNumElements() )
            sema.Warning( "Casting a vector to a smaller vector type" );    
        return true;
    }
    
    //
    // We can only cast to a matrix if they are of the same size
    // Or we can fill up the diagonal (if this is explicit)
    //
    if( m_CastType.IsMatrixType() )
    {
        if( t.GetNumElements() ==
                            JoeMath::Min( m_CastType.GetNumMatrixRows(),
                                          m_CastType.GetNumMatrixColumns() ) &&
            IsExplicit() )
        {

        }
        else if( m_CastType.GetNumElements() != t.GetNumElements() )
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
        if( t.GetNumElements() != m_CastType.GetNumElements() )
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
        if( t.GetNumMatrixColumns()  < m_CastType.GetNumMatrixColumns() ||
            t.GetNumMatrixRows() < m_CastType.GetNumMatrixRows() )
        {
            sema.Error( 
                  "Can't cast a matrix to a matrix type of larger dimensions" );
            return false;
        }
        
        if( ( t.GetNumMatrixColumns()  > m_CastType.GetNumMatrixColumns() ||
              t.GetNumMatrixRows() > m_CastType.GetNumMatrixRows() ) &&
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
    if( m_CastType == m_Expression->GetType() )
        shader_writer << *m_Expression;
    else
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

CastExpression_up CastExpression::Create( Type cast_type,
                                          Expression_up expression,
                                          bool is_explicit )
{
    return Create( CompleteType( cast_type ),
                   std::move( expression ),
                   is_explicit );
}

CastExpression_up CastExpression::Create( const CompleteType& cast_type,
                                          Expression_up expression,
                                          bool is_explicit )
{
    // If this would be a null implicit cast don't bother creating one
    //if( expression->GetType() == cast_type && !is_explicit )
        //return expression;

    return CastExpression_up( new CastExpression( cast_type,
                                                  std::move(expression),
                                                  is_explicit ) );
}

CastExpression_up CastExpression::CreateBaseTypeCast(
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

} // namespace Compiler
} // namespace JoeLang
