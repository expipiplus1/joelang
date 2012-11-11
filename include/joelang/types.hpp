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

#pragma once

#include <cstdint>
#include <string>
#include <type_traits>

#include <joemath/joemath.hpp>

// windows.h may have defined VOID
#undef VOID

namespace JoeLang
{

enum class Type
{
    // Unknown type
    UNKNOWN,

    // void
    VOID,

    //Any array
    ARRAY,

    // Boolean type
    BOOL,

    // Signed integer types
    CHAR,
    SHORT,
    INT,
    LONG,

    // Unsigned integer types
    UCHAR,
    USHORT,
    UINT,
    ULONG,

    // Floating point types
    FLOAT,
    FLOAT2,
    FLOAT3,
    FLOAT4,

    DOUBLE,

    // String types
    STRING
};

using jl_bool   = bool;
using jl_char   = std::int8_t;
using jl_short  = std::int16_t;
using jl_int    = std::int32_t;
using jl_long   = std::int64_t;
using jl_uchar  = std::uint8_t;
using jl_ushort = std::uint16_t;
using jl_uint   = std::uint32_t;
using jl_ulong  = std::uint64_t;
using jl_float  = float;
using jl_double = double;
using jl_float2 = JoeMath::float2;
using jl_float3 = JoeMath::float3;
using jl_float4 = JoeMath::float4;

template<typename T>
struct JoeLangType
{
private:
    static constexpr
    Type GetType()
    {
        return    std::is_same<T, jl_bool>::value
                ? Type::BOOL
                : std::is_same<T, jl_char>::value
                ? Type::CHAR
                : std::is_same<T, jl_short>::value
                ? Type::SHORT
                : std::is_same<T, jl_int>::value
                ? Type::INT
                : std::is_same<T, jl_long>::value
                ? Type::LONG
                : std::is_same<T, jl_uchar>::value
                ? Type::UCHAR
                : std::is_same<T, jl_ushort>::value
                ? Type::USHORT
                : std::is_same<T, jl_uint>::value
                ? Type::UINT
                : std::is_same<T, jl_ulong>::value
                ? Type::ULONG
                : std::is_same<T, jl_float>::value
                ? Type::FLOAT
                : std::is_same<T, jl_float2>::value
                ? Type::FLOAT2
                : std::is_same<T, jl_float3>::value
                ? Type::FLOAT3
                : std::is_same<T, jl_float4>::value
                ? Type::FLOAT4
                : std::is_same<T, jl_double>::value
                ? Type::DOUBLE
                : std::is_same<T, std::string>::value
                ? Type::STRING
                : Type::UNKNOWN;
    }

public:
    const static
    Type value = GetType();
};

} // namespace JoeLang
