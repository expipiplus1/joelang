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

#include "generic_value.hpp"

#include <cassert>
#include <string>
#include <utility>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>
#include <joelang/types.hpp>


namespace JoeLang
{
namespace Compiler
{

GenericValue::GenericValue()
    :m_Type( Type::UNKNOWN )
{
}

GenericValue::GenericValue( const GenericValue& g )
    :m_Type( Type::UNKNOWN )
{
    *this = g;
}

GenericValue::GenericValue( Type type )
    :m_Type( type )
{
    switch( m_Type )
    {
    case Type::STRING:
        new(&m_StringValue) std::string;
        break;
    case Type::ARRAY:
        new(&m_ArrayValue) std::vector<GenericValue>;
        break;
    default:
        break;
    }
}

const GenericValue& GenericValue::operator = ( const GenericValue& g )
{
    FreeData();


#define COPY_DATA(Type, TYPE) \
    case TYPE: \
        m_##Type##Value = g.m_##Type##Value; \
        break;

#define COPY_DATA_N(Type, TYPE) \
    COPY_DATA( Type,      TYPE ) \
    COPY_DATA( Type##2,   TYPE##2 ) \
    COPY_DATA( Type##3,   TYPE##3 ) \
    COPY_DATA( Type##4,   TYPE##4 ) \
    COPY_DATA( Type##2x2, TYPE##2x2 ) \
    COPY_DATA( Type##2x3, TYPE##2x3 ) \
    COPY_DATA( Type##2x4, TYPE##2x4 ) \
    COPY_DATA( Type##3x2, TYPE##3x2 ) \
    COPY_DATA( Type##3x3, TYPE##3x3 ) \
    COPY_DATA( Type##3x4, TYPE##3x4 ) \
    COPY_DATA( Type##4x2, TYPE##4x2 ) \
    COPY_DATA( Type##4x3, TYPE##4x3 ) \
    COPY_DATA( Type##4x4, TYPE##4x4 )


    switch( g.m_Type )
    {
    case Type::UNKNOWN:
        break;

    COPY_DATA_N(Bool,   Type::BOOL)
    COPY_DATA_N(Char,   Type::CHAR)
    COPY_DATA_N(Short,  Type::SHORT)
    COPY_DATA_N(Int,    Type::INT)
    COPY_DATA_N(Long,   Type::LONG)
    COPY_DATA_N(UChar,  Type::UCHAR)
    COPY_DATA_N(UShort, Type::USHORT)
    COPY_DATA_N(UInt,   Type::UINT)
    COPY_DATA_N(ULong,  Type::ULONG)
    COPY_DATA_N(Float,  Type::FLOAT)
    COPY_DATA_N(Double, Type::DOUBLE)

    case Type::STRING:
        new(&m_StringValue) std::string;
        m_StringValue = g.m_StringValue;
        break;
    case Type::ARRAY:
        new(&m_ArrayValue) std::vector<GenericValue>;
        m_ArrayValue = g.m_ArrayValue;
        break;
    default:
        assert( false && "Assigning from GenericValue with an unhandled Type" );
        break;
    }

    m_Type = g.m_Type;

    return *this;

#undef COPY_DATA_N
#undef COPY_DATA
}

#define TYPE_CONSTRUCTOR_AND_GETTER(type, Type, TYPE) \
GenericValue::GenericValue( type   type##_value   ) \
    :m_Type( TYPE ) \
    ,m_##Type##Value( type##_value ) \
{ \
} \
 \
type GenericValue::Get##Type() const \
{ \
    assert( m_Type == TYPE && \
            "Trying to get the wrong type from a GenericValue" ); \
    return m_##Type##Value; \
}

#define TYPE_CONSTRUCTOR_AND_GETTER_N(type, Type, TYPE) \
    TYPE_CONSTRUCTOR_AND_GETTER( type,      Type,      TYPE ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2,   Type##2,   TYPE##2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3,   Type##3,   TYPE##3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4,   Type##4,   TYPE##4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x2, Type##2x2, TYPE##2x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x3, Type##2x3, TYPE##2x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x4, Type##2x4, TYPE##2x4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x2, Type##3x2, TYPE##3x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x3, Type##3x3, TYPE##3x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x4, Type##3x4, TYPE##3x4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x2, Type##4x2, TYPE##4x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x3, Type##4x3, TYPE##4x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x4, Type##4x4, TYPE##4x4 )

    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_bool,   Bool,   Type::BOOL )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_char,   Char,   Type::CHAR )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_short,  Short,  Type::SHORT )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_int,    Int,    Type::INT )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_long,   Long,   Type::LONG )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_uchar,  UChar,  Type::UCHAR )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_ushort, UShort, Type::USHORT )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_uint,   UInt,   Type::UINT )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_ulong,  ULong,  Type::ULONG )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_float,  Float,  Type::FLOAT )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_double, Double, Type::DOUBLE )

#undef TYPE_CONSTRUCTOR_AND_GETTER_N
#undef TYPE_CONSTRUCTOR_AND_GETTER

GenericValue::GenericValue( jl_string&& string_value )
    :m_Type( Type::STRING )
    ,m_StringValue( reinterpret_cast<const char*>(string_value.data),
                    string_value.size )
{
    delete[] string_value.data;
    string_value.size = 0;
    string_value.data = nullptr;
}

GenericValue::GenericValue( std::string string_value )
    :m_Type( Type::STRING )
    ,m_StringValue( std::move(string_value) )
{
}

GenericValue::GenericValue( std::vector<GenericValue> array_value )
    :m_Type( Type::ARRAY )
    ,m_ArrayValue( std::move(array_value) )
{
#ifndef NDEBUG
    assert( !m_ArrayValue.empty() &&
            "GenericValue given an empty array value" );
    const ArrayExtents& extents = m_ArrayValue[0].GetArrayExtents();
    Type underlying_type = m_ArrayValue[0].GetUnderlyingType();
    for( unsigned i = 1; i < m_ArrayValue.size(); ++i )
    {
        assert( underlying_type == m_ArrayValue[i].GetUnderlyingType() &&
                "Array genericvalue created with mismatched types" );
        assert( extents == m_ArrayValue[i].GetArrayExtents() &&
                "Array genericvalue created with mismatched array extents" );
    }
#endif
}

GenericValue::~GenericValue()
{
    FreeData();
}

llvm::Constant* GenericValue::CodeGen( CodeGenerator& code_gen ) const
{
#define CODE_INTEGER( Type, TYPE ) \
    case TYPE: \
        return code_gen.CreateInteger( m_##Type##Value, TYPE );

#define CODE_INTEGER_VECTOR( Type, TYPE, n ) \
    case TYPE: \
    { \
        std::vector<jl_ulong> values( &m_##Type##Value[0], \
                                      &m_##Type##Value[0]+(n) ); \
        return code_gen.CreateIntegerVector( values, TYPE ); \
    }
    
#define CODE_INTEGER_MATRIX( Type, TYPE, n ) \
    case TYPE: \
    { \
        std::vector<jl_ulong> values( &m_##Type##Value[0][0], \
                                      &m_##Type##Value[0][0]+(n) ); \
        return code_gen.CreateIntegerMatrix( values, TYPE ); \
    }
    
#define CODE_INTEGER_N( Type, TYPE ) \
    CODE_INTEGER( Type, TYPE ) \
    CODE_INTEGER_VECTOR( Type##2, TYPE##2, 2 ) \
    CODE_INTEGER_VECTOR( Type##3, TYPE##3, 3 ) \
    CODE_INTEGER_VECTOR( Type##4, TYPE##4, 4 ) \
    CODE_INTEGER_MATRIX( Type##2x2, TYPE##2x2, 2 * 2 ) \
    CODE_INTEGER_MATRIX( Type##2x3, TYPE##2x3, 2 * 3 ) \
    CODE_INTEGER_MATRIX( Type##2x4, TYPE##2x4, 2 * 4 ) \
    CODE_INTEGER_MATRIX( Type##3x2, TYPE##3x2, 3 * 2 ) \
    CODE_INTEGER_MATRIX( Type##3x3, TYPE##3x3, 3 * 3 ) \
    CODE_INTEGER_MATRIX( Type##3x4, TYPE##3x4, 3 * 4 ) \
    CODE_INTEGER_MATRIX( Type##4x2, TYPE##4x2, 4 * 2 ) \
    CODE_INTEGER_MATRIX( Type##4x3, TYPE##4x3, 4 * 3 ) \
    CODE_INTEGER_MATRIX( Type##4x4, TYPE##4x4, 4 * 4 )
    
#define CODE_FLOATING( Type, TYPE ) \
    case TYPE: \
        return code_gen.CreateFloating( m_##Type##Value, TYPE );

#define CODE_FLOATING_VECTOR( Type, TYPE, n ) \
    case TYPE: \
    { \
        std::vector<jl_double> values( &m_##Type##Value[0], \
                                       &m_##Type##Value[0]+(n) ); \
        return code_gen.CreateFloatingVector( values, TYPE ); \
    }
    
#define CODE_FLOATING_MATRIX( Type, TYPE, n ) \
    case TYPE: \
    { \
        std::vector<jl_double> values( &m_##Type##Value[0][0], \
                                       &m_##Type##Value[0][0]+(n) ); \
        return code_gen.CreateFloatingMatrix( values, TYPE ); \
    }
    
#define CODE_FLOATING_N( Type, TYPE ) \
    CODE_FLOATING( Type, TYPE ) \
    CODE_FLOATING_VECTOR( Type##2, TYPE##2, 2 ) \
    CODE_FLOATING_VECTOR( Type##3, TYPE##3, 3 ) \
    CODE_FLOATING_VECTOR( Type##4, TYPE##4, 4 ) \
    CODE_FLOATING_MATRIX( Type##2x2, TYPE##2x2, 2 * 2 ) \
    CODE_FLOATING_MATRIX( Type##2x3, TYPE##2x3, 2 * 3 ) \
    CODE_FLOATING_MATRIX( Type##2x4, TYPE##2x4, 2 * 4 ) \
    CODE_FLOATING_MATRIX( Type##3x2, TYPE##3x2, 3 * 2 ) \
    CODE_FLOATING_MATRIX( Type##3x3, TYPE##3x3, 3 * 3 ) \
    CODE_FLOATING_MATRIX( Type##3x4, TYPE##3x4, 3 * 4 ) \
    CODE_FLOATING_MATRIX( Type##4x2, TYPE##4x2, 4 * 2 ) \
    CODE_FLOATING_MATRIX( Type##4x3, TYPE##4x3, 4 * 3 ) \
    CODE_FLOATING_MATRIX( Type##4x4, TYPE##4x4, 4 * 4 )

    switch( m_Type )
    {
    CODE_INTEGER_N( Bool,   Type::BOOL )
    CODE_INTEGER_N( Char,   Type::CHAR )
    CODE_INTEGER_N( Short,  Type::SHORT )
    CODE_INTEGER_N( Int,    Type::INT )
    CODE_INTEGER_N( Long,   Type::LONG )
    CODE_INTEGER_N( UChar,  Type::UCHAR )
    CODE_INTEGER_N( UShort, Type::USHORT )
    CODE_INTEGER_N( UInt,   Type::UINT )
    CODE_INTEGER_N( ULong,  Type::ULONG )

    CODE_FLOATING_N( Float,  Type::FLOAT );
    CODE_FLOATING_N( Double, Type::DOUBLE );

    case Type::STRING:
        return code_gen.CreateString( m_StringValue );
    case Type::ARRAY:
        return code_gen.CreateArray( m_ArrayValue );
    default:
        assert( false && "Trying to codegen an unhandled type" );
    }
    return nullptr;

#undef CODE_FLOATING_N
#undef CODE_FLOATING_VECTOR
#undef CODE_FLOATING
#undef CODE_INTEGER_N
#undef CODE_INTEGER_VECTOR
#undef CODE_INTEGER
}

template<typename T>
static void WriteValue( ShaderWriter& shader_writer, T t )
{
    std::string suffix = GetGLSLTypeSuffix( JoeLangType<T>::value );
    shader_writer << t << suffix;
}

template<typename Scalar, JoeMath::u32 Rows, JoeMath::u32 Columns>
static void WriteValue( ShaderWriter& shader_writer,
                   const JoeMath::Matrix<Scalar, Rows, Columns>& m )
{
    shader_writer << GetGLSLTypeString(
                   JoeLangType<JoeMath::Matrix<Scalar, Rows, Columns>>::value )
                  << "(";
    std::string suffix = GetGLSLTypeSuffix( JoeLangType<Scalar>::value );

    bool first = true;
    for( unsigned i = 0; i < Columns; ++i )
        for( unsigned j = 0; j < Rows; ++j )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;

            shader_writer << m.GetColumn(i)[j] << suffix;
        }
    shader_writer << ")";
}

void GenericValue::Write( ShaderWriter& shader_writer ) const
{

#define WRITE( Type, TYPE ) \
    case TYPE: \
        WriteValue( shader_writer, m_##Type##Value ); \
        break; \

#define WRITE_N( Type, TYPE ) \
    WRITE( Type, TYPE ) \
    WRITE( Type##2, TYPE##2 ) \
    WRITE( Type##3, TYPE##3 ) \
    WRITE( Type##4, TYPE##4 ) \
    WRITE( Type##2x2, TYPE##2x2 ) \
    WRITE( Type##2x3, TYPE##2x3 ) \
    WRITE( Type##2x4, TYPE##2x4 ) \
    WRITE( Type##3x2, TYPE##3x2 ) \
    WRITE( Type##3x3, TYPE##3x3 ) \
    WRITE( Type##3x4, TYPE##3x4 ) \
    WRITE( Type##4x2, TYPE##4x2 ) \
    WRITE( Type##4x3, TYPE##4x3 ) \
    WRITE( Type##4x4, TYPE##4x4 )

    switch( m_Type )
    {
    WRITE_N( Bool,   Type::BOOL )
    WRITE_N( Char,   Type::CHAR )
    WRITE_N( Short,  Type::SHORT )
    WRITE_N( Int,    Type::INT )
    WRITE_N( Long,   Type::LONG )
    WRITE_N( UChar,  Type::UCHAR )
    WRITE_N( UShort, Type::USHORT )
    WRITE_N( UInt,   Type::UINT )
    WRITE_N( ULong,  Type::ULONG )

    WRITE_N( Float,  Type::FLOAT )
    WRITE_N( Double, Type::DOUBLE )

    case Type::STRING:
        shader_writer << "\"" << m_StringValue << "\"";
        break;
    case Type::ARRAY:
    {
        shader_writer << GetGLSLTypeString(GetUnderlyingType()) << "[]" << "(";
        bool first = true;
        for( const GenericValue& g : m_ArrayValue )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;
            shader_writer << g;
        }
        shader_writer << ")";
        break;
    }
    default:
        assert( false && "Trying to write an unhandled type" );
    }
}

CompleteType GenericValue::GetType() const
{
    return CompleteType( GetUnderlyingType(),
                         GetArrayExtents() );
}

Type GenericValue::GetUnderlyingType() const
{
    if( m_Type == Type::ARRAY )
    {
        assert( !m_ArrayValue.empty() &&
                "Trying to get the underlying type of an empty array "
                "genericvalue" );
        return m_ArrayValue[0].GetUnderlyingType();
    }
    return m_Type;
}

ArrayExtents GenericValue::GetArrayExtents() const
{
    if( m_Type == Type::ARRAY )
    {
        assert( !m_ArrayValue.empty() &&
                "Trying to get the array extents of an empty array "
                "genericvalue" );
        ArrayExtents ret = { static_cast<unsigned>( m_ArrayValue.size() ) };
        const ArrayExtents& sub_extents = m_ArrayValue[0].GetArrayExtents();
        ret.insert( ret.end(), sub_extents.begin(), sub_extents.end() );
        return ret;
    }
    return {};
}

const std::string& GenericValue::GetString() const
{
    assert( m_Type == Type::STRING &&
            "Trying to get the string value from a non-string GenericValue" );
    return m_StringValue;
}

const std::vector<GenericValue>& GenericValue::GetArray() const
{
    assert( m_Type == Type::ARRAY &&
            "Trying to get the array value from a non-array GenericValue" );
    return m_ArrayValue;
}

void GenericValue::FreeData()
{
    using std::string;
    using std::vector;

    switch( m_Type )
    {
    case Type::ARRAY:
        m_ArrayValue.~vector();
        break;
    case Type::STRING:
        m_StringValue.~string();
        break;
    default:
        break;
    }

    m_Type = Type::UNKNOWN;
}

} // namespace Compiler
} // namespace JoeLang
