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

#include <string>
#include <utility>

#include <engine/types.hpp>

namespace JoeLang
{
namespace Compiler
{

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

} // namespace Compiler
} // namespace JoeLang
