/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#pragma once

#include <cstdint>
#include <type_traits>

namespace JoeLang
{

enum class Type
{
    // Unknown type
    UNKNOWN_TYPE,

    // Boolean type
    BOOL,

    // Signed integer types
    I8,
    I16,
    I32,
    I64,

    // Unsigned integer types
    U8,
    U16,
    U32,
    U64,

    // Floating point types
    FLOAT,
    DOUBLE,
};

template<Type t> struct TypeOfJoeLangType
{ static_assert(t!=Type::UNKNOWN_TYPE,
                "Trying to get the type of an unknown JoeLang::Type"); };
template<>struct TypeOfJoeLangType<Type::BOOL>  { typedef bool          type; };
template<>struct TypeOfJoeLangType<Type::I8>    { typedef std::int8_t   type; };
template<>struct TypeOfJoeLangType<Type::I16>   { typedef std::int16_t  type; };
template<>struct TypeOfJoeLangType<Type::I32>   { typedef std::int32_t  type; };
template<>struct TypeOfJoeLangType<Type::I64>   { typedef std::int64_t  type; };
template<>struct TypeOfJoeLangType<Type::U8>    { typedef std::uint8_t  type; };
template<>struct TypeOfJoeLangType<Type::U16>   { typedef std::uint16_t type; };
template<>struct TypeOfJoeLangType<Type::U32>   { typedef std::uint32_t type; };
template<>struct TypeOfJoeLangType<Type::U64>   { typedef std::uint64_t type; };
template<>struct TypeOfJoeLangType<Type::FLOAT> { typedef float         type; };
template<>struct TypeOfJoeLangType<Type::DOUBLE>{ typedef double        type; };


template<typename T>
struct JoeLangType
{
private:
    static
    constexpr
    Type GetFloatingPointType()
    {
        return sizeof(T) == 4
            ? Type::FLOAT
            : sizeof(T) == 8
                ? Type::DOUBLE
                : Type::UNKNOWN_TYPE;
    }

    static
    constexpr
    Type GetUnsignedIntegralType()
    {
        return sizeof(T) == 1
            ? Type::U8
            : sizeof(T) == 2
                ? Type::U16
                : sizeof(T) == 4
                    ? Type::U32
                    : sizeof(T) == 8
                        ? Type::U64
                        : Type::UNKNOWN_TYPE;
    }

    static
    constexpr
    Type GetSignedIntegralType()
    {
        return sizeof(T) == 1
            ? Type::I8
            : sizeof(T) == 2
                ? Type::I16
                : sizeof(T) == 4
                    ? Type::I32
                    : sizeof(T) == 8
                        ? Type::I64
                        : Type::UNKNOWN_TYPE;
    }

    static
    constexpr
    Type GetIntegralType()
    {
        return std::is_signed<T>::value
            ? GetSignedIntegralType()
            : GetUnsignedIntegralType();
    }

    static
    constexpr
    Type GetType()
    {
        return std::is_floating_point<T>::value
                ? GetFloatingPointType()
                : std::is_integral<T>::value
                    ? GetIntegralType()
                    : std::is_same<bool,T>::value
                        ? Type::BOOL
                        : Type::UNKNOWN_TYPE;
    }

public:
    const
    static
    Type value = GetType();
};

} // namespace JoeLang
