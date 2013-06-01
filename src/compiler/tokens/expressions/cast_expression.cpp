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

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/type_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/tokens/expressions/unary_expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

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

const ExpressionNode& CastExpression::GenerateCodeDag( NodeManager& node_manager ) const
{
    //
    // Don't generate a cast here if it wouldn't do anything
    //
    if( m_Expression->GetType() == m_CastType )
        return m_Expression->GenerateCodeDag( node_manager );
                
    const CompleteType& from_type = m_Expression->GetType();
    
    const ExpressionNode& expression = m_Expression->GenerateCodeDag( node_manager );
    
    if( from_type.IsScalarType() )
        return GenerateCastFromScalar( expression, m_CastType, node_manager );
    if( from_type.IsVectorType() )
        return GenerateCastFromVector( expression, m_CastType, node_manager );
    if( from_type.IsMatrixType() )
        return GenerateCastFromMatrix( expression, m_CastType, node_manager );
    if( from_type.IsArrayType() )
        return GenerateCastFromArray( expression, m_CastType, node_manager );
    if( from_type.IsStructType() )
        return GenerateCastFromStruct( expression, m_CastType, node_manager );
    
    assert( false && "Trying to cast from an unhandled type" );
    std::abort();
}

const ExpressionNode& CastExpression::GenerateCastFromScalar( const ExpressionNode& expression, 
                                                              const CompleteType &to_type, 
                                                              NodeManager &node_manager )
{
    const TypeNode& to_type_node = to_type.GenerateCodeDag( node_manager );
    
    if( to_type.IsScalarType() )
    {
        // If this is already the same size
        return node_manager.MakeExpressionNode( NodeType::Cast, 
                                                {expression, to_type_node} );
    }
    
    if( to_type.IsVectorType() )
    {
        // 
        // Create a splat
        //
        unsigned num_elements = to_type.GetNumElements();
        std::vector<Node_ref> arguments;
        arguments.reserve( num_elements + 1 );
        for( unsigned i = 0; i < num_elements; ++i )
            arguments.push_back( expression );
        CompleteType uncast_type = CompleteType( GetVectorType( expression.GetType().GetType(),
                                                                to_type.GetNumElements() ) );
        const TypeNode& uncast_type_node = node_manager.MakeTypeNode( uncast_type );
        arguments.push_back( uncast_type_node );
        const ExpressionNode& uncast_node =
            node_manager.MakeExpressionNode( NodeType::VectorConstructor, std::move( arguments ) );
        return node_manager.MakeExpressionNode( NodeType::Cast, 
                                                {uncast_node, to_type_node} );
    }
    
    if( to_type.IsMatrixType() )
    {
        std::vector<Node_ref> columns;
        unsigned num_columns = to_type.GetNumMatrixColumns();
        CompleteType column_type = CompleteType( to_type.GetMatrixColumnType() );
        for( unsigned i = 0; i < num_columns; ++i )
            columns.push_back( GenerateCastFromScalar( expression, column_type, node_manager ) );
        columns.push_back( to_type_node );
        return node_manager.MakeExpressionNode( NodeType::MatrixConstructor, columns );
    }
    
    assert( false && "Generating code for an unhandled cast" );
    std::abort();
}

const ExpressionNode& CastExpression::GenerateCastFromVector( const ExpressionNode& expression,
                                                              const CompleteType& to_type,
                                                              NodeManager& node_manager )
{
    const TypeNode& to_type_node = to_type.GenerateCodeDag( node_manager );
    unsigned from_size = expression.GetType().GetNumElements();
    
    if( to_type.IsScalarType() )
    {
        //
        // Extract the first element and cast that
        //
        const ExpressionNode& index = node_manager.MakeConstant( 0 );
        const ExpressionNode& first_element = node_manager.MakeExpressionNode( 
                                                  NodeType::ExtractElement, { expression, index } );
        return node_manager.MakeExpressionNode( NodeType::Cast, { first_element, to_type_node } );
    }
    
    if( to_type.IsVectorType() )
    {
        const unsigned to_size = to_type.GetNumElements();
        if( to_size == from_size )
            // They are already the same size, and the writer can take care of the rest
            return node_manager.MakeExpressionNode( NodeType::Cast, { expression, to_type_node } );
        
        assert( to_size < from_size && "Trying to cast a vector to a larger one" );
        
        const Swizzle reduce_swizzle( 0, 1, to_size > 2 ? 2 : 0xff, to_size > 3 ? 3 : 0xff );
        const SwizzleNode& reduced_expression = node_manager.MakeSwizzleNode( expression, 
                                                                              reduce_swizzle );
        return node_manager.MakeExpressionNode( NodeType::Cast, 
                                                { reduced_expression, to_type_node } );
    }
    
    if( to_type.IsMatrixType() )
    {
        unsigned num_rows = to_type.GetNumMatrixRows();
        unsigned num_columns = to_type.GetNumMatrixColumns(); 
        
        const CompleteType& to_column_type = CompleteType( to_type.GetMatrixColumnType() );
        const TypeNode& to_column_type_node = to_column_type.GenerateCodeDag( node_manager );
        
        std::vector<Node_ref> columns;
        
        std::vector<Node_ref> elements;
        for( unsigned i = 0; i < from_size; ++i )
        {
            const Node& index = node_manager.MakeConstant( i );
            const ExpressionNode& element = 
                node_manager.MakeExpressionNode( NodeType::ExtractElement, { expression, index } );
            elements.push_back( element );
        }
        
        if( from_size == JoeMath::Min( num_rows, num_columns ) )
        {
            //
            // the case where the vector is the right size to fill up the diagonal
            //
            unsigned i = 0;
            for( const Node& element : elements )
            {
                const ExpressionNode& index = node_manager.MakeConstant( i++ );
                const ZeroNode& zero = node_manager.MakeZero( to_type.GetMatrixColumnType() );
                columns.push_back( node_manager.MakeNode( NodeType::InsertElement,
                                                          { zero, element, index }));
            }
        }
        
        if( from_size == num_rows * num_columns )
        {
            //
            // the case where the vector is the right size to fill the whole vector
            //
            unsigned i = 0;
            for( unsigned c = 0; c < num_columns; ++c )
            {
                std::vector<Node_ref> column_nodes;
                for( unsigned r = 0; r < num_rows; ++r )
                    column_nodes.push_back( elements[i++] );
                column_nodes.push_back( to_column_type_node );
                const ExpressionNode& column = 
                    node_manager.MakeExpressionNode( NodeType::VectorConstructor, column_nodes );
                columns.push_back( column );
            }
        }
        
        assert( columns.size() == num_columns && "Wrong number of columns" );
        
        columns.push_back( to_type_node );
        return node_manager.MakeExpressionNode( NodeType::MatrixConstructor, columns );
    }
    
    assert( false && "Casting to unhandled type" );
    std::abort();
}

const ExpressionNode& CastExpression::GenerateCastFromMatrix( const ExpressionNode& expression,
                                                              const CompleteType& to_type,
                                                              NodeManager& node_manager )
{
    const TypeNode& to_type_node = to_type.GenerateCodeDag( node_manager );
    const CompleteType& from_type = expression.GetType();
    unsigned from_num_rows = from_type.GetNumMatrixRows();
    unsigned from_num_columns = from_type.GetNumMatrixColumns();
    
    if( to_type.IsScalarType() )
    {
        //
        // Extract the first element and cast that
        //
        const ExpressionNode& index = node_manager.MakeConstant( 0 );
        const ExpressionNode& column = 
            node_manager.MakeExpressionNode( NodeType::ExtractColumn, { expression, index } );
        const ExpressionNode& element =
            node_manager.MakeExpressionNode( NodeType::ExtractElement, { column, index } );
        return node_manager.MakeExpressionNode( NodeType::Cast, { element, to_type_node } );
    }
    
    if( to_type.IsVectorType() )
    {
        unsigned to_size = to_type.GetNumElements();   
        const CompleteType& from_column_type = 
           CompleteType( expression.GetType().GetMatrixColumnType() );
        const TypeNode& from_column_type_node = from_column_type.GenerateCodeDag( node_manager );
        
        //
        // The vector must be the rght size
        //
        assert( to_size == from_num_rows * from_num_columns && 
                "Casting to a vector of the wrong type" );
        std::vector<Node_ref> elements;
        elements.reserve( to_size + 1 );
        
        for( unsigned c = 0; c < from_num_columns; ++c ) 
        {
            const ExpressionNode& column_index = node_manager.MakeConstant( c );
            const ExpressionNode& column = 
                node_manager.MakeExpressionNode( NodeType::ExtractColumn, 
                                                 { expression, column_index } );
            for( unsigned r = 0; r < from_num_rows; ++r ) 
            {
                const ExpressionNode& row_index = node_manager.MakeConstant( r );
                const ExpressionNode& element = 
                    node_manager.MakeExpressionNode( NodeType::ExtractElement, 
                                                     { column, row_index } );
                elements.push_back( element );
            }
        }
        
        elements.push_back( from_column_type_node );
        const ExpressionNode& uncast_vector = 
            node_manager.MakeExpressionNode( NodeType::VectorConstructor,  std::move( elements ) );
        return node_manager.MakeExpressionNode( NodeType::Cast, { uncast_vector, to_type_node } );
    }
    
    if( to_type.IsMatrixType() )
    {
        if( to_type.GetNumElements() == from_num_columns * from_num_rows )
            return node_manager.MakeExpressionNode( NodeType::Cast, { expression, to_type_node } );
        
        //
        // We have to take the top left part of the matrix
        //
        
        unsigned to_num_columns = to_type.GetNumMatrixColumns();   
        unsigned to_num_rows    = to_type.GetNumMatrixRows();   
        assert( to_num_columns <= from_num_columns && "Trying to cast to a larger matrix" );
        assert( to_num_rows <= from_num_rows && "Trying to cast to a larger matrix" );
        
        const CompleteType& to_column_type = CompleteType( to_type.GetMatrixColumnType() );
        
        std::vector<Node_ref> columns;
        columns.reserve( to_num_columns + 1 );
        
        for( unsigned c = 0; c < from_num_columns; ++c ) 
        {
            const ExpressionNode& column_index = node_manager.MakeConstant( c );
            const ExpressionNode& from_column = 
                node_manager.MakeExpressionNode( NodeType::ExtractColumn, 
                                                 { expression, column_index } );
            
            columns.push_back( GenerateCastFromVector( from_column, 
                                                       to_column_type, 
                                                       node_manager ) );
        }
        
        columns.push_back( to_type_node );
        return node_manager.MakeExpressionNode( NodeType::MatrixConstructor, 
                                                std::move( columns ) );
    }
    
    assert( false && "Trying to cast to an unhandled type" );
    std::abort();
}

const ExpressionNode& CastExpression::GenerateCastFromStruct( const ExpressionNode& expression,
                                                              const CompleteType& to_type,
                                                              NodeManager& node_manager )
{
    assert( false && "Complete me" );
    std::abort();
}

const ExpressionNode& CastExpression::GenerateCastFromArray ( const ExpressionNode& expression,
                                                              const CompleteType& to_type,
                                                              NodeManager& node_manager )
{
    assert( false && "Complete me" );
    std::abort();
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
