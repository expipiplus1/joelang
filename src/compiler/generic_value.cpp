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

#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

GenericValue::GenericValue()
    :m_type( Type::UNKNOWN_TYPE )
    ,m_stringValue()
{
}

GenericValue::GenericValue( const GenericValue& g )
    :m_stringValue()
{
    *this = g;
}

GenericValue::GenericValue( Type type )
    :m_type( type )
    ,m_stringValue()
{
}

const GenericValue& GenericValue::operator = ( const GenericValue& g )
{
    m_type = g.m_type;

    switch( g.m_type )
    {
    case Type::BOOL:
        m_boolValue = g.m_boolValue;
        break;
    case Type::I8:
        m_i8Value = g.m_i8Value;
        break;
    case Type::I16:
        m_i16Value = g.m_i16Value;
        break;
    case Type::I32:
        m_i32Value = g.m_i32Value;
        break;
    case Type::I64:
        m_i64Value = g.m_i64Value;
        break;
    case Type::U8:
        m_u8Value = g.m_u8Value;
        break;
    case Type::U16:
        m_u16Value = g.m_u16Value;
        break;
    case Type::U32:
        m_u32Value = g.m_u32Value;
        break;
    case Type::U64:
        m_u64Value = g.m_u64Value;
        break;
    case Type::FLOAT:
        m_floatValue = g.m_floatValue;
        break;
    case Type::DOUBLE:
        m_doubleValue = g.m_doubleValue;
        break;
    case Type::STRING:
        m_stringValue = g.m_stringValue;
        break;
    default:
        break;
    }

    return *this;
}

GenericValue::GenericValue( jl_bool   bool_value   )
    :m_type( Type::BOOL )
    ,m_boolValue( bool_value )
{
}

GenericValue::GenericValue( jl_i8     i8_value     )
    :m_type( Type::I8 )
    ,m_i8Value( i8_value )
{
}

GenericValue::GenericValue( jl_i16    i16_value    )
    :m_type( Type::I16 )
    ,m_i16Value( i16_value )
{
}

GenericValue::GenericValue( jl_i32    i32_value    )
    :m_type( Type::I32 )
    ,m_i32Value( i32_value )
{
}

GenericValue::GenericValue( jl_i64    i64_value    )
    :m_type( Type::I64 )
    ,m_i64Value( i64_value )
{
}

GenericValue::GenericValue( jl_u8     u8_value     )
    :m_type( Type::U8 )
    ,m_u8Value( u8_value )
{
}

GenericValue::GenericValue( jl_u16    u16_value    )
    :m_type( Type::U16 )
    ,m_u16Value( u16_value )
{
}

GenericValue::GenericValue( jl_u32    u32_value    )
    :m_type( Type::U32 )
    ,m_u32Value( u32_value )
{
}

GenericValue::GenericValue( jl_u64    u64_value    )
    :m_type( Type::U64 )
    ,m_u64Value( u64_value )
{
}

GenericValue::GenericValue( jl_float  float_value  )
    :m_type( Type::FLOAT )
    ,m_floatValue( float_value )
{
}

GenericValue::GenericValue( jl_double double_value )
    :m_type( Type::DOUBLE )
    ,m_doubleValue( double_value )
{
}

GenericValue::GenericValue( std::string string_value )
    :m_type( Type::STRING )
    ,m_stringValue( std::move(string_value) )
{
}

GenericValue::~GenericValue()
{
    using std::string;
    if( m_type == Type::STRING )
        m_stringValue.~string();
}

Type GenericValue::GetType() const
{
    return m_type;
}

jl_bool GenericValue::GetBool() const
{
    assert( m_type == Type::BOOL &&
            "Trying to get the bool value from a non-bool GenericValue" );
    return m_boolValue;
}

jl_i8 GenericValue::GetI8() const
{
    assert( m_type == Type::I8 &&
            "Trying to get the i8 value from a non-i8 GenericValue" );
    return m_i8Value;
}

jl_i16 GenericValue::GetI16() const
{
    assert( m_type == Type::I16 &&
            "Trying to get the i16 value from a non-i16 GenericValue" );
    return m_i16Value;
}

jl_i32 GenericValue::GetI32() const
{
    assert( m_type == Type::I32 &&
            "Trying to get the i32 value from a non-i32 GenericValue" );
    return m_i32Value;
}

jl_i64 GenericValue::GetI64() const
{
    assert( m_type == Type::I64 &&
            "Trying to get the i64 value from a non-i64 GenericValue" );
    return m_i64Value;
}

jl_u8 GenericValue::GetU8() const
{
    assert( m_type == Type::U8 &&
            "Trying to get the u8 value from a non-u8 GenericValue" );
    return m_u8Value;
}

jl_u16 GenericValue::GetU16() const
{
    assert( m_type == Type::U16 &&
            "Trying to get the u16 value from a non-u16 GenericValue" );
    return m_u16Value;
}

jl_u32 GenericValue::GetU32() const
{
    assert( m_type == Type::U32 &&
            "Trying to get the u32 value from a non-u32 GenericValue" );
    return m_u32Value;
}

jl_u64 GenericValue::GetU64() const
{
    assert( m_type == Type::U64 &&
            "Trying to get the u64 value from a non-u64 GenericValue" );
    return m_u64Value;
}

jl_float GenericValue::GetFloat() const
{
    assert( m_type == Type::FLOAT &&
            "Trying to get the float value from a non-float GenericValue" );
    return m_floatValue;
}

jl_double GenericValue::GetDouble() const
{
    assert( m_type == Type::DOUBLE &&
            "Trying to get the double value from a non-double GenericValue" );
    return m_doubleValue;
}

const jl_string& GenericValue::GetString() const
{
    assert( m_type == Type::STRING &&
            "Trying to get the string value from a non-string GenericValue" );
    return m_stringValue;
}

GenericValue GenericValue::Lor( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to lor GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue || g2.m_boolValue;
        break;
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to lor GenericValues of non-bool type" );
    default:
        assert( false && "Trying to lor GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Land( const GenericValue& g1,
                                 const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to land GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue || g2.m_boolValue;
        break;
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to land GenericValues of non-bool type" );
    default:
        assert( false && "Trying to land GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Or( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to or GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue | g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value | g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value | g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value | g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value | g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value | g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value | g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value | g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value | g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to or GenericValues of non-integer type" );
    default:
        assert( false && "Trying to or GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Xor( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to xor GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue ^ g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value ^ g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value ^ g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value ^ g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value ^ g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value ^ g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value ^ g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value ^ g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value ^ g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to xor GenericValues of non-integer type" );
    default:
        assert( false && "Trying to xor GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::And( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to and GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue & g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value & g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value & g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value & g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value & g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value & g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value & g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value & g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value & g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to and GenericValues of non-integer type" );
    default:
        assert( false && "Trying to and GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::EqualTo( const GenericValue& g1,
                                    const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare equality of GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue == g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value == g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value == g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value == g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value == g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value == g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value == g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value == g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value == g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue == g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue == g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue == g2.m_stringValue;
        break;
    default:
        assert( false &&
                "Trying to compare equality of GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::NotEqualTo( const GenericValue& g1,
                                       const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare equality of GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue != g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value != g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value != g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value != g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value != g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value != g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value != g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value != g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value != g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue != g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue != g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue != g2.m_stringValue;
        break;
    default:
        assert( false &&
                "Trying to compare equality of GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::LessThan( const GenericValue& g1,
                                     const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue < g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value < g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value < g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value < g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value < g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value < g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value < g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value < g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value < g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue < g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue < g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue < g2.m_stringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::GreaterThan( const GenericValue& g1,
                                        const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue > g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value > g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value > g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value > g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value > g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value > g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value > g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value > g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value > g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue > g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue > g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue > g2.m_stringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::LessThanEqual( const GenericValue& g1,
                                          const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue <= g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value <= g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value <= g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value <= g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value <= g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value <= g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value <= g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value <= g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value <= g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue <= g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue <= g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue <= g2.m_stringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::GreaterThanEqual( const GenericValue& g1,
                                             const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue >= g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_boolValue = g1.m_i8Value >= g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_boolValue = g1.m_i16Value >= g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_boolValue = g1.m_i32Value >= g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_boolValue = g1.m_i64Value >= g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_boolValue = g1.m_u8Value >= g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_boolValue = g1.m_u16Value >= g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_boolValue = g1.m_u32Value >= g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_boolValue = g1.m_u64Value >= g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_boolValue = g1.m_floatValue >= g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_boolValue = g1.m_doubleValue >= g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_boolValue = g1.m_stringValue >= g2.m_stringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Shl( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to shift GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue << g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value << g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value << g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value << g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value << g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value << g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value << g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value << g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value << g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to shift GenericValues of non-integer type" );
    default:
        assert( false && "Trying to shift GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Shr( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to shift GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue >> g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value >> g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value >> g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value >> g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value >> g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value >> g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value >> g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value >> g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value >> g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to shift GenericValues of non-integer type" );
    default:
        assert( false && "Trying to shift GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Add( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to add GenericValues of differing type" );

        GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue + g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value + g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value + g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value + g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value + g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value + g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value + g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value + g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value + g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_floatValue = g1.m_floatValue + g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_doubleValue = g1.m_doubleValue + g2.m_doubleValue;
        break;
    case Type::STRING:
        ret.m_stringValue = g1.m_stringValue + g2.m_stringValue;
        break;
    default:
        assert( false && "Trying to add GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Sub( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to sub GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue - g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value - g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value - g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value - g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value - g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value - g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value - g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value - g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value - g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_floatValue = g1.m_floatValue - g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_doubleValue = g1.m_doubleValue - g2.m_doubleValue;
        break;
    case Type::STRING:
        assert( false && "Trying to sub GenericValues of string type" );
    default:
        assert( false && "Trying to sub GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Mul( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to mul GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue * g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value * g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value * g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value * g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value * g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value * g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value * g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value * g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value * g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_floatValue = g1.m_floatValue * g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_doubleValue = g1.m_doubleValue * g2.m_doubleValue;
        break;
    case Type::STRING:
        assert( false && "Trying to mul GenericValues of string type" );
    default:
        assert( false && "Trying to mul GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Div( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to div GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue / g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value / g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value / g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value / g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value / g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value / g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value / g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value / g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value / g2.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_floatValue = g1.m_floatValue / g2.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_doubleValue = g1.m_doubleValue / g2.m_doubleValue;
        break;
    case Type::STRING:
        assert( false && "Trying to div GenericValues of string type" );
    default:
        assert( false && "Trying to div GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Mod( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_type == g2.m_type &&
            "Trying to mod GenericValues of differing type" );

    GenericValue ret( g1.m_type );

    switch( g1.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = g1.m_boolValue % g2.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = g1.m_i8Value % g2.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = g1.m_i16Value % g2.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = g1.m_i32Value % g2.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = g1.m_i64Value % g2.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = g1.m_u8Value % g2.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = g1.m_u16Value % g2.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = g1.m_u32Value % g2.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = g1.m_u64Value % g2.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
        // fmod here?
    case Type::STRING:
        assert( false && "Trying to mod GenericValues of non-integer type" );
    default:
        assert( false && "Trying to mod GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::UnaryPlus( const GenericValue& g )
{
    assert( g.m_type != Type::UNKNOWN_TYPE &&
                        "Trying to unary plus GenericValue of unknown type" );
    assert( g.m_type != Type::STRING &&
                        "Trying to unary plus GenericValue of string type" );
    return g;
}

GenericValue GenericValue::UnaryMinus( const GenericValue& g )
{
    GenericValue ret( g.m_type );

    switch( g.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = -g.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = -g.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = -g.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = -g.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = -g.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = -g.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = -g.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = -g.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = -g.m_u64Value;
        break;
    case Type::FLOAT:
        ret.m_floatValue = -g.m_floatValue;
        break;
    case Type::DOUBLE:
        ret.m_doubleValue = -g.m_doubleValue;
        break;
    case Type::STRING:
        assert( false && "Trying to negate GenericValue of non-integer type" );
    default:
        assert( false && "Trying to negate GenericValue of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::LogicalNot( const GenericValue& g )
{
    GenericValue ret( g.m_type );

    switch( g.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = !g.m_boolValue;
        break;
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::U8:
    case Type::U16:
    case Type::U32:
    case Type::U64:
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false && "Trying to negate GenericValue of non-bool type" );
    default:
        assert( false && "Trying to negate GenericValue of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::BitwiseNot( const GenericValue& g )
{
    GenericValue ret( g.m_type );

    switch( g.m_type )
    {
    case Type::BOOL:
        ret.m_boolValue = ~g.m_boolValue;
        break;
    case Type::I8:
        ret.m_i8Value = ~g.m_i8Value;
        break;
    case Type::I16:
        ret.m_i16Value = ~g.m_i16Value;
        break;
    case Type::I32:
        ret.m_i32Value = ~g.m_i32Value;
        break;
    case Type::I64:
        ret.m_i64Value = ~g.m_i64Value;
        break;
    case Type::U8:
        ret.m_u8Value = ~g.m_u8Value;
        break;
    case Type::U16:
        ret.m_u16Value = ~g.m_u16Value;
        break;
    case Type::U32:
        ret.m_u32Value = ~g.m_u32Value;
        break;
    case Type::U64:
        ret.m_u64Value = ~g.m_u64Value;
        break;
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        assert( false &&
                "Trying to bitwise-not GenericValue of non-integer type" );
    default:
        assert( false && "Trying to bitwise-not GenericValue of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Cast( Type t, const GenericValue& g )
{
    assert( t != Type::UNKNOWN_TYPE &&
                 "Trying to cast a GenericValue to an unknown type" );
    assert( g.m_type != Type::UNKNOWN_TYPE &&
                 "Trying to cast a GenericValue from an unknown type" );

    union
    {
        jl_u64 i;
        jl_double f;
    };
    bool integral = false;

    if( IsIntegral( g.m_type ) )
    {
        switch( g.m_type )
        {
        case Type::BOOL:
            i = g.GetBool();
            break;
        case Type::I8:
            i = g.GetI8();
            break;
        case Type::I16:
            i = g.GetI16();
            break;
        case Type::I32:
            i = g.GetI32();
            break;
        case Type::I64:
            i = g.GetI64();
            break;
        case Type::U8:
            i = g.GetU8();
            break;
        case Type::U16:
            i = g.GetU16();
            break;
        case Type::U32:
            i = g.GetU32();
            break;
        case Type::U64:
            i = g.GetU64();
            break;
        default:
            assert( false && "Unhandled integer type in GenericValue casting" );
        }
        integral = true;
    }
    else if( IsFloatingPoint( g.m_type ) )
    {
        switch( g.m_type )
        {
        case Type::FLOAT:
            f = g.GetFloat();
            break;
        case Type::DOUBLE:
            f = g.GetDouble();
            break;
        default:
            assert( false &&
                    "Unhandled floating point type in GenericValue casting" );
        }
    }
    else
    {
        assert( t == Type::STRING &&
                "Trying to cast a string to a numerical type in GenericValue" );
        return g;
    }

    switch( t )
    {
    case Type::BOOL:
        return GenericValue( jl_bool(integral ? i : f) );
    case Type::I8:
        return GenericValue( jl_i8(integral ? i : f) );
    case Type::I16:
        return GenericValue( jl_i16(integral ? i : f) );
    case Type::I32:
        return GenericValue( jl_i32(integral ? i : f) );
    case Type::I64:
        return GenericValue( jl_i64(integral ? i : f) );
    case Type::U8:
        return GenericValue( jl_u8(integral ? i : f) );
    case Type::U16:
        return GenericValue( jl_u16(integral ? i : f) );
    case Type::U32:
        return GenericValue( jl_u32(integral ? i : f) );
    case Type::U64:
        return GenericValue( jl_u64(integral ? i : f) );
    case Type::FLOAT:
        return GenericValue( jl_float(integral ? i : f) );
    case Type::DOUBLE:
        return GenericValue( jl_double(integral ? i : f) );
    default:
        assert(
            false &&
            "Trying to cast to an unhandled type in GenericValue" );
        return GenericValue();
    }
}

} // namespace Compiler
} // namespace JoeLang
