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

#include <compiler/code_generator.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/type_properties.hpp>
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

    switch( g.m_Type )
    {
    case Type::UNKNOWN:
        break;
    case Type::BOOL:
        m_BoolValue = g.m_BoolValue;
        break;
    case Type::CHAR:
        m_I8Value = g.m_I8Value;
        break;
    case Type::SHORT:
        m_I16Value = g.m_I16Value;
        break;
    case Type::INT:
        m_I32Value = g.m_I32Value;
        break;
    case Type::LONG:
        m_I64Value = g.m_I64Value;
        break;
    case Type::UCHAR:
        m_U8Value = g.m_U8Value;
        break;
    case Type::USHORT:
        m_U16Value = g.m_U16Value;
        break;
    case Type::UINT:
        m_U32Value = g.m_U32Value;
        break;
    case Type::ULONG:
        m_U64Value = g.m_U64Value;
        break;
    case Type::FLOAT:
        m_FloatValue = g.m_FloatValue;
        break;
    case Type::FLOAT2:
        m_Float2Value = g.m_Float2Value;
        break;
    case Type::FLOAT3:
        m_Float3Value = g.m_Float3Value;
        break;
    case Type::FLOAT4:
        m_Float4Value = g.m_Float4Value;
        break;
    case Type::DOUBLE:
        m_DoubleValue = g.m_DoubleValue;
        break;
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
}

GenericValue::GenericValue( jl_bool   bool_value   )
    :m_Type( Type::BOOL )
    ,m_BoolValue( bool_value )
{
}

GenericValue::GenericValue( jl_char     i8_value     )
    :m_Type( Type::CHAR )
    ,m_I8Value( i8_value )
{
}

GenericValue::GenericValue( jl_short    i16_value    )
    :m_Type( Type::SHORT )
    ,m_I16Value( i16_value )
{
}

GenericValue::GenericValue( jl_int    i32_value    )
    :m_Type( Type::INT )
    ,m_I32Value( i32_value )
{
}

GenericValue::GenericValue( jl_long    i64_value    )
    :m_Type( Type::LONG )
    ,m_I64Value( i64_value )
{
}

GenericValue::GenericValue( jl_uchar     u8_value     )
    :m_Type( Type::UCHAR )
    ,m_U8Value( u8_value )
{
}

GenericValue::GenericValue( jl_ushort    u16_value    )
    :m_Type( Type::USHORT )
    ,m_U16Value( u16_value )
{
}

GenericValue::GenericValue( jl_uint    u32_value    )
    :m_Type( Type::UINT )
    ,m_U32Value( u32_value )
{
}

GenericValue::GenericValue( jl_ulong    u64_value    )
    :m_Type( Type::ULONG )
    ,m_U64Value( u64_value )
{
}

GenericValue::GenericValue( jl_float  float_value  )
    :m_Type( Type::FLOAT )
    ,m_FloatValue( float_value )
{
}

GenericValue::GenericValue( jl_float2 float2_value  )
    :m_Type( Type::FLOAT2 )
    ,m_Float2Value( float2_value )
{
}

GenericValue::GenericValue( jl_float3 float3_value  )
    :m_Type( Type::FLOAT3 )
    ,m_Float3Value( float3_value )
{
}

GenericValue::GenericValue( jl_float4 float4_value  )
    :m_Type( Type::FLOAT4 )
    ,m_Float4Value( float4_value )
{
}

GenericValue::GenericValue( jl_double double_value )
    :m_Type( Type::DOUBLE )
    ,m_DoubleValue( double_value )
{
}

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
    switch( m_Type )
    {
    case Type::BOOL:
        return code_gen.CreateInteger( m_BoolValue, Type::BOOL );
    case Type::CHAR:
        return code_gen.CreateInteger( m_I8Value, Type::CHAR );
    case Type::SHORT:
        return code_gen.CreateInteger( m_I16Value, Type::SHORT );
    case Type::INT:
        return code_gen.CreateInteger( m_I32Value, Type::INT );
    case Type::LONG:
        return code_gen.CreateInteger( m_I64Value, Type::LONG );
    case Type::UCHAR:
        return code_gen.CreateInteger( m_U8Value,  Type::UCHAR );
    case Type::USHORT:
        return code_gen.CreateInteger( m_U16Value, Type::USHORT );
    case Type::UINT:
        return code_gen.CreateInteger( m_U32Value, Type::UINT );
    case Type::ULONG:
        return code_gen.CreateInteger( m_U64Value, Type::ULONG );
    case Type::FLOAT:
        return code_gen.CreateFloating( m_FloatValue, Type::FLOAT );
    case Type::FLOAT2:
    {
        std::vector<double> values( &m_Float2Value[0], &m_Float2Value[0]+2 );
        return code_gen.CreateFloatingVector( values, Type::FLOAT2 );
    }
    case Type::FLOAT3:
    {
        std::vector<double> values( &m_Float3Value[0], &m_Float3Value[0]+3 );
        return code_gen.CreateFloatingVector( values, Type::FLOAT3 );
    }
    case Type::FLOAT4:
    {
        std::vector<double> values( &m_Float4Value[0], &m_Float4Value[0]+4 );
        return code_gen.CreateFloatingVector( values, Type::FLOAT4 );
    }
    case Type::DOUBLE:
        return code_gen.CreateFloating( m_DoubleValue, Type::DOUBLE );
    case Type::STRING:
        return code_gen.CreateString( m_StringValue );
    case Type::ARRAY:
        return code_gen.CreateArray( m_ArrayValue );
    default:
        assert( false && "Trying to codegen an unhandled type" );
    }
    return nullptr;
}

void GenericValue::Write( ShaderWriter& shader_writer ) const
{
    switch( m_Type )
    {
    case Type::BOOL:
        shader_writer << m_BoolValue;
        break;
    case Type::CHAR:
        shader_writer << m_I8Value;
        break;
    case Type::SHORT:
        shader_writer << m_I16Value;
        break;
    case Type::INT:
        shader_writer << m_I32Value;
        break;
    case Type::LONG:
        shader_writer << m_I64Value;
        break;
    case Type::UCHAR:
        shader_writer << m_U8Value;
        break;
    case Type::USHORT:
        shader_writer << m_U16Value;
        break;
    case Type::UINT:
        shader_writer << m_U32Value;
        break;
    case Type::ULONG:
        shader_writer << m_U64Value;
        break;
    case Type::FLOAT:
        shader_writer << m_FloatValue << "f";
        break;
    case Type::FLOAT2:
    {
        shader_writer << GetGLSLTypeString( Type::FLOAT2 ) << "(";
        bool first = true;
        for( unsigned i = 0; i < 2; ++i )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;
            shader_writer << m_Float2Value[i] << "f";
        }
        shader_writer << ")";
        break;
    }
    case Type::FLOAT3:
    {
        shader_writer << GetGLSLTypeString( Type::FLOAT3 ) << "(";
        bool first = true;
        for( unsigned i = 0; i < 3; ++i )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;
            shader_writer << m_Float3Value[i] << "f";
        }
        shader_writer << ")";
        break;
    }
    case Type::FLOAT4:
    {
        shader_writer << GetGLSLTypeString( Type::FLOAT4 ) << "(";
        bool first = true;
        for( unsigned i = 0; i < 4; ++i )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;
            shader_writer << m_Float4Value[i] << "f";
        }
        shader_writer << ")";
        break;
    }
    case Type::DOUBLE:
        shader_writer << m_DoubleValue << "d";
        break;
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

jl_bool GenericValue::GetBool() const
{
    assert( m_Type == Type::BOOL &&
            "Trying to get the bool value from a non-bool GenericValue" );
    return m_BoolValue;
}

jl_char GenericValue::GetI8() const
{
    assert( m_Type == Type::CHAR &&
            "Trying to get the i8 value from a non-i8 GenericValue" );
    return m_I8Value;
}

jl_short GenericValue::GetI16() const
{
    assert( m_Type == Type::SHORT &&
            "Trying to get the i16 value from a non-i16 GenericValue" );
    return m_I16Value;
}

jl_int GenericValue::GetI32() const
{
    assert( m_Type == Type::INT &&
            "Trying to get the i32 value from a non-i32 GenericValue" );
    return m_I32Value;
}

jl_long GenericValue::GetI64() const
{
    assert( m_Type == Type::LONG &&
            "Trying to get the i64 value from a non-i64 GenericValue" );
    return m_I64Value;
}

jl_uchar GenericValue::GetU8() const
{
    assert( m_Type == Type::UCHAR &&
            "Trying to get the u8 value from a non-u8 GenericValue" );
    return m_U8Value;
}

jl_ushort GenericValue::GetU16() const
{
    assert( m_Type == Type::USHORT &&
            "Trying to get the u16 value from a non-u16 GenericValue" );
    return m_U16Value;
}

jl_uint GenericValue::GetU32() const
{
    assert( m_Type == Type::UINT &&
            "Trying to get the u32 value from a non-u32 GenericValue" );
    return m_U32Value;
}

jl_ulong GenericValue::GetU64() const
{
    assert( m_Type == Type::ULONG &&
            "Trying to get the u64 value from a non-u64 GenericValue" );
    return m_U64Value;
}

jl_float GenericValue::GetFloat() const
{
    assert( m_Type == Type::FLOAT &&
            "Trying to get the float value from a non-float GenericValue" );
    return m_FloatValue;
}

jl_float2 GenericValue::GetFloat2() const
{
    assert( m_Type == Type::FLOAT2 &&
            "Trying to get the float2 value from a non-float GenericValue" );
    return m_Float2Value;
}

jl_float3 GenericValue::GetFloat3() const
{
    assert( m_Type == Type::FLOAT3 &&
            "Trying to get the float3 value from a non-float GenericValue" );
    return m_Float3Value;
}

jl_float4 GenericValue::GetFloat4() const
{
    assert( m_Type == Type::FLOAT4 &&
            "Trying to get the float4 value from a non-float GenericValue" );
    return m_Float4Value;
}

jl_double GenericValue::GetDouble() const
{
    assert( m_Type == Type::DOUBLE &&
            "Trying to get the double value from a non-double GenericValue" );
    return m_DoubleValue;
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
