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
    if( m_Type == Type::STRING )
        new(&m_StringValue) std::string;
}

GenericValue::GenericValue( const GenericValue& g )
    :m_Type( Type::UNKNOWN_TYPE )
{
    *this = g;
}

GenericValue::GenericValue( Type type )
    :m_Type( type )
{
    if( m_Type == Type::STRING )
        new(&m_StringValue) std::string;
}

const GenericValue& GenericValue::operator = ( const GenericValue& g )
{
    switch( g.m_Type )
    {
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
        if( m_Type != Type::STRING )
            new(&m_StringValue) std::string;
        m_StringValue = g.m_StringValue;
        break;
    default:
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

GenericValue::GenericValue( jl_string string_value )
    :m_Type( Type::STRING )
    ,m_StringValue( string_value.data, string_value.size )
{
}

GenericValue::GenericValue( std::string string_value )
    :m_Type( Type::STRING )
    ,m_StringValue( std::move(string_value) )
{
}

GenericValue::~GenericValue()
{
    using std::string;
    if( m_Type == Type::STRING )
    {
        m_StringValue.~string();
    }
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
    default:
        assert( false && "Trying to codegen an unhandled type" );
    }
    return nullptr;
}

Type GenericValue::GetType() const
{
    return m_Type;
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

GenericValue GenericValue::Lor( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to lor GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue || g2.m_BoolValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to land GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue || g2.m_BoolValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to or GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue | g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value | g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value | g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value | g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value | g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value | g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value | g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value | g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value | g2.m_U64Value;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to xor GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue ^ g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value ^ g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value ^ g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value ^ g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value ^ g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value ^ g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value ^ g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value ^ g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value ^ g2.m_U64Value;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to and GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue & g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value & g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value & g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value & g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value & g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value & g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value & g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value & g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value & g2.m_U64Value;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare equality of GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue == g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value == g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value == g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value == g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value == g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value == g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value == g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value == g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value == g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue == g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue == g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue == g2.m_StringValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare equality of GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue != g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value != g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value != g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value != g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value != g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value != g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value != g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value != g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value != g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue != g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue != g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue != g2.m_StringValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue < g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value < g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value < g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value < g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value < g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value < g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value < g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value < g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value < g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue < g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue < g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue < g2.m_StringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::GreaterThan( const GenericValue& g1,
                                        const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue > g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value > g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value > g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value > g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value > g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value > g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value > g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value > g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value > g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue > g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue > g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue > g2.m_StringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::LessThanEqual( const GenericValue& g1,
                                          const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue <= g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value <= g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value <= g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value <= g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value <= g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value <= g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value <= g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value <= g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value <= g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue <= g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue <= g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue <= g2.m_StringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::GreaterThanEqual( const GenericValue& g1,
                                             const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to compare GenericValues of differing type" );

    GenericValue ret( Type::BOOL );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue >= g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_BoolValue = g1.m_I8Value >= g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_BoolValue = g1.m_I16Value >= g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_BoolValue = g1.m_I32Value >= g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_BoolValue = g1.m_I64Value >= g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_BoolValue = g1.m_U8Value >= g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_BoolValue = g1.m_U16Value >= g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_BoolValue = g1.m_U32Value >= g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_BoolValue = g1.m_U64Value >= g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_BoolValue = g1.m_FloatValue >= g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_BoolValue = g1.m_DoubleValue >= g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_BoolValue = g1.m_StringValue >= g2.m_StringValue;
        break;
    default:
        assert( false && "Trying to compare GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Shl( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to shift GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue << g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value << g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value << g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value << g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value << g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value << g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value << g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value << g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value << g2.m_U64Value;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to shift GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue >> g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value >> g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value >> g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value >> g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value >> g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value >> g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value >> g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value >> g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value >> g2.m_U64Value;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to add GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue + g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value + g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value + g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value + g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value + g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value + g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value + g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value + g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value + g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_FloatValue = g1.m_FloatValue + g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_DoubleValue = g1.m_DoubleValue + g2.m_DoubleValue;
        break;
    case Type::STRING:
        ret.m_StringValue = g1.m_StringValue + g2.m_StringValue;
        break;
    default:
        assert( false && "Trying to add GenericValues of unknown type" );
    }
    return ret;
}

GenericValue GenericValue::Sub( const GenericValue& g1, const GenericValue& g2 )
{
    assert( g1.m_Type == g2.m_Type &&
            "Trying to sub GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue - g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value - g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value - g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value - g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value - g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value - g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value - g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value - g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value - g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_FloatValue = g1.m_FloatValue - g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_DoubleValue = g1.m_DoubleValue - g2.m_DoubleValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to mul GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue * g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value * g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value * g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value * g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value * g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value * g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value * g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value * g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value * g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_FloatValue = g1.m_FloatValue * g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_DoubleValue = g1.m_DoubleValue * g2.m_DoubleValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to div GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue / g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value / g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value / g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value / g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value / g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value / g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value / g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value / g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value / g2.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_FloatValue = g1.m_FloatValue / g2.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_DoubleValue = g1.m_DoubleValue / g2.m_DoubleValue;
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
    assert( g1.m_Type == g2.m_Type &&
            "Trying to mod GenericValues of differing type" );

    GenericValue ret( g1.m_Type );

    switch( g1.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = g1.m_BoolValue % g2.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = g1.m_I8Value % g2.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = g1.m_I16Value % g2.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = g1.m_I32Value % g2.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = g1.m_I64Value % g2.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = g1.m_U8Value % g2.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = g1.m_U16Value % g2.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = g1.m_U32Value % g2.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = g1.m_U64Value % g2.m_U64Value;
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
    assert( g.m_Type != Type::UNKNOWN_TYPE &&
                        "Trying to unary plus GenericValue of unknown type" );
    assert( g.m_Type != Type::STRING &&
                        "Trying to unary plus GenericValue of string type" );
    return g;
}

GenericValue GenericValue::UnaryMinus( const GenericValue& g )
{
    GenericValue ret( g.m_Type );

    switch( g.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = -g.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = -g.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = -g.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = -g.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = -g.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = -g.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = -g.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = -g.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = -g.m_U64Value;
        break;
    case Type::FLOAT:
        ret.m_FloatValue = -g.m_FloatValue;
        break;
    case Type::DOUBLE:
        ret.m_DoubleValue = -g.m_DoubleValue;
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
    GenericValue ret( g.m_Type );

    switch( g.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = !g.m_BoolValue;
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
    GenericValue ret( g.m_Type );

    switch( g.m_Type )
    {
    case Type::BOOL:
        ret.m_BoolValue = ~g.m_BoolValue;
        break;
    case Type::I8:
        ret.m_I8Value = ~g.m_I8Value;
        break;
    case Type::I16:
        ret.m_I16Value = ~g.m_I16Value;
        break;
    case Type::I32:
        ret.m_I32Value = ~g.m_I32Value;
        break;
    case Type::I64:
        ret.m_I64Value = ~g.m_I64Value;
        break;
    case Type::U8:
        ret.m_U8Value = ~g.m_U8Value;
        break;
    case Type::U16:
        ret.m_U16Value = ~g.m_U16Value;
        break;
    case Type::U32:
        ret.m_U32Value = ~g.m_U32Value;
        break;
    case Type::U64:
        ret.m_U64Value = ~g.m_U64Value;
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
    assert( g.m_Type != Type::UNKNOWN_TYPE &&
                 "Trying to cast a GenericValue from an unknown type" );

    union
    {
        jl_u64 i;
        jl_double f;
    };
    bool integral = false;

    if( IsIntegral( g.m_Type ) )
    {
        switch( g.m_Type )
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
    else if( IsFloatingPoint( g.m_Type ) )
    {
        switch( g.m_Type )
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
