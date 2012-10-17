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
    S8,
    S16,
    S32,
    S64,

    // Unsigned integer types
    U8,
    U16,
    U32,
    U64,

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
using jl_s8     = std::int8_t;
using jl_s16    = std::int16_t;
using jl_s32    = std::int32_t;
using jl_s64    = std::int64_t;
using jl_u8     = std::uint8_t;
using jl_u16    = std::uint16_t;
using jl_u32    = std::uint32_t;
using jl_u64    = std::uint64_t;
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
                : std::is_same<T, jl_s8>::value
                ? Type::S8
                : std::is_same<T, jl_s16>::value
                ? Type::S16
                : std::is_same<T, jl_s32>::value
                ? Type::S32
                : std::is_same<T, jl_s64>::value
                ? Type::S64
                : std::is_same<T, jl_u8>::value
                ? Type::U8
                : std::is_same<T, jl_u16>::value
                ? Type::U16
                : std::is_same<T, jl_u32>::value
                ? Type::U32
                : std::is_same<T, jl_u64>::value
                ? Type::U64
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
