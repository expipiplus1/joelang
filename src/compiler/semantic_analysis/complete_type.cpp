/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include "complete_type.hpp"

#include <sstream>
#include <string>
#include <utility>
#include <vector>

#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/writers/shader_writer.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

CompleteType::CompleteType()
    :m_BaseType( Type::UNKNOWN )
{
}

CompleteType::CompleteType( Type base_type,
                            ArrayExtents array_extents )
    :m_BaseType( base_type )
    ,m_ArrayExtents( std::move(array_extents) )
{
}

Type CompleteType::GetType() const
{
    if( !m_ArrayExtents.empty() )
        return Type::ARRAY;
    return m_BaseType;
}

const ArrayExtents& CompleteType::GetArrayExtents() const
{
    return m_ArrayExtents;
}

Type CompleteType::GetBaseType() const
{
    return m_BaseType;
}

bool CompleteType::IsArrayType() const
{
    return !m_ArrayExtents.empty();
}

bool CompleteType::IsUnknown() const
{
    return m_BaseType == Type::UNKNOWN;
}

bool CompleteType::IsVoid() const
{
    return m_BaseType == Type::VOID;
}

bool CompleteType::IsFloatingPoint() const
{
    return Compiler::IsFloatingPoint( m_BaseType ) && m_ArrayExtents.empty();
}

bool CompleteType::IsIntegral() const
{
    return Compiler::IsIntegral( m_BaseType ) && m_ArrayExtents.empty();
}

bool CompleteType::IsMatrixType() const
{
    return Compiler::IsMatrixType( m_BaseType ) && m_ArrayExtents.empty();
}

bool CompleteType::IsStructType() const
{
    // no structs yet
    return false;
}

bool CompleteType::IsVectorType() const
{
    return Compiler::IsVectorType( m_BaseType ) && m_ArrayExtents.empty();
}

bool CompleteType::IsScalarType() const
{
    return Compiler::IsScalarType( m_BaseType ) && m_ArrayExtents.empty();
}

bool CompleteType::IsSigned() const
{
    return Compiler::IsSigned( m_BaseType ) && m_ArrayExtents.empty();
}

Type CompleteType::GetElementType() const
{
    assert( ( IsMatrixType() || IsVectorType() || IsScalarType() ) &&
            "Trying to get the element type of a non-vector or non-matrix "
            "type" );
    return Compiler::GetScalarType( m_BaseType );
}

Type CompleteType::GetMatrixColumnType() const
{
    assert( IsMatrixType() &&
            "Trying to get the element type of a non-matrix type" );
    return Compiler::GetMatrixColumnType( m_BaseType );
}

unsigned CompleteType::GetNumMatrixColumns() const
{
    assert( IsMatrixType() &&
            "Trying to get the number of columns in a non-matrix type" );
    return GetNumColumnsInType( m_BaseType );
}

unsigned CompleteType::GetNumMatrixRows() const
{
    assert( IsMatrixType() &&
            "Trying to get the number of rows in a non-matrix type" );
    return GetNumRowsInType( m_BaseType );
}

unsigned CompleteType::GetNumElements() const
{
    assert( !IsArrayType() && "Complete me" );
    return GetNumElementsInType( m_BaseType );
}

std::string CompleteType::GetString() const
{
    std::stringstream string( GetTypeString( m_BaseType ) );
    for( unsigned e : m_ArrayExtents )
        string << "[" << e << "]";
    return string.str();
}

void CompleteType::Write( ShaderWriter& shader_writer ) const
{
    if( !HasGLSLType( m_BaseType ) )
    {
        shader_writer.Error( "Type " + GetTypeString( m_BaseType ) +
                             " has no GLSL equivalent" );
        return;
    }
    shader_writer << GetGLSLTypeString( m_BaseType );
    for( unsigned extent : m_ArrayExtents )
        shader_writer << "[" << extent << "]";
}

bool CompleteType::operator == ( const CompleteType& other ) const
{
    return m_BaseType == other.m_BaseType &&
           m_ArrayExtents == other.m_ArrayExtents;
}

bool CompleteType::operator != ( const CompleteType& other ) const
{
    return !operator==( other );
}

} // namespace Compiler
} // namespace JoeLang
