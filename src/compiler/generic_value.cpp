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
#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

GenericValue::GenericValue()
    :m_Type( Type::UNKNOWN_TYPE )
{
}

GenericValue::GenericValue( const GenericValue& g )
    :m_Type( Type::UNKNOWN_TYPE )
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
    case Type::UNKNOWN_TYPE:
        break;
    case Type::BOOL:
        m_BoolValue = g.m_BoolValue;
        break;
    case Type::I8:
        m_I8Value = g.m_I8Value;
        break;
    case Type::I16:
        m_I16Value = g.m_I16Value;
        break;
    case Type::I32:
        m_I32Value = g.m_I32Value;
        break;
    case Type::I64:
        m_I64Value = g.m_I64Value;
        break;
    case Type::U8:
        m_U8Value = g.m_U8Value;
        break;
    case Type::U16:
        m_U16Value = g.m_U16Value;
        break;
    case Type::U32:
        m_U32Value = g.m_U32Value;
        break;
    case Type::U64:
        m_U64Value = g.m_U64Value;
        break;
    case Type::FLOAT:
        m_FloatValue = g.m_FloatValue;
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

GenericValue::GenericValue( jl_i8     i8_value     )
    :m_Type( Type::I8 )
    ,m_I8Value( i8_value )
{
}

GenericValue::GenericValue( jl_i16    i16_value    )
    :m_Type( Type::I16 )
    ,m_I16Value( i16_value )
{
}

GenericValue::GenericValue( jl_i32    i32_value    )
    :m_Type( Type::I32 )
    ,m_I32Value( i32_value )
{
}

GenericValue::GenericValue( jl_i64    i64_value    )
    :m_Type( Type::I64 )
    ,m_I64Value( i64_value )
{
}

GenericValue::GenericValue( jl_u8     u8_value     )
    :m_Type( Type::U8 )
    ,m_U8Value( u8_value )
{
}

GenericValue::GenericValue( jl_u16    u16_value    )
    :m_Type( Type::U16 )
    ,m_U16Value( u16_value )
{
}

GenericValue::GenericValue( jl_u32    u32_value    )
    :m_Type( Type::U32 )
    ,m_U32Value( u32_value )
{
}

GenericValue::GenericValue( jl_u64    u64_value    )
    :m_Type( Type::U64 )
    ,m_U64Value( u64_value )
{
}

GenericValue::GenericValue( jl_float  float_value  )
    :m_Type( Type::FLOAT )
    ,m_FloatValue( float_value )
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
    /// TODO verify that the values are of the same extents and types
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
    case Type::I8:
        return code_gen.CreateInteger( m_I8Value, Type::I8 );
    case Type::I16:
        return code_gen.CreateInteger( m_I16Value, Type::I16 );
    case Type::I32:
        return code_gen.CreateInteger( m_I32Value, Type::I32 );
    case Type::I64:
        return code_gen.CreateInteger( m_I64Value, Type::I64 );
    case Type::U8:
        return code_gen.CreateInteger( m_U8Value,  Type::U8 );
    case Type::U16:
        return code_gen.CreateInteger( m_U16Value, Type::U16 );
    case Type::U32:
        return code_gen.CreateInteger( m_U32Value, Type::U32 );
    case Type::U64:
        return code_gen.CreateInteger( m_U64Value, Type::U64 );
    case Type::FLOAT:
        return code_gen.CreateFloating( m_FloatValue, Type::FLOAT );
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

Type GenericValue::GetType() const
{
    return m_Type;
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

std::vector<unsigned> GenericValue::GetArrayExtents() const
{
    if( m_Type == Type::ARRAY )
    {
        assert( !m_ArrayValue.empty() &&
                "Trying to get the array extents of an empty array "
                "genericvalue" );
        std::vector<unsigned> ret =
                               { static_cast<unsigned>( m_ArrayValue.size() ) };
        const std::vector<unsigned>& sub_extents =
                                              m_ArrayValue[0].GetArrayExtents();
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

jl_i8 GenericValue::GetI8() const
{
    assert( m_Type == Type::I8 &&
            "Trying to get the i8 value from a non-i8 GenericValue" );
    return m_I8Value;
}

jl_i16 GenericValue::GetI16() const
{
    assert( m_Type == Type::I16 &&
            "Trying to get the i16 value from a non-i16 GenericValue" );
    return m_I16Value;
}

jl_i32 GenericValue::GetI32() const
{
    assert( m_Type == Type::I32 &&
            "Trying to get the i32 value from a non-i32 GenericValue" );
    return m_I32Value;
}

jl_i64 GenericValue::GetI64() const
{
    assert( m_Type == Type::I64 &&
            "Trying to get the i64 value from a non-i64 GenericValue" );
    return m_I64Value;
}

jl_u8 GenericValue::GetU8() const
{
    assert( m_Type == Type::U8 &&
            "Trying to get the u8 value from a non-u8 GenericValue" );
    return m_U8Value;
}

jl_u16 GenericValue::GetU16() const
{
    assert( m_Type == Type::U16 &&
            "Trying to get the u16 value from a non-u16 GenericValue" );
    return m_U16Value;
}

jl_u32 GenericValue::GetU32() const
{
    assert( m_Type == Type::U32 &&
            "Trying to get the u32 value from a non-u32 GenericValue" );
    return m_U32Value;
}

jl_u64 GenericValue::GetU64() const
{
    assert( m_Type == Type::U64 &&
            "Trying to get the u64 value from a non-u64 GenericValue" );
    return m_U64Value;
}

jl_float GenericValue::GetFloat() const
{
    assert( m_Type == Type::FLOAT &&
            "Trying to get the float value from a non-float GenericValue" );
    return m_FloatValue;
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

    m_Type = Type::UNKNOWN_TYPE;
}

} // namespace Compiler
} // namespace JoeLang
