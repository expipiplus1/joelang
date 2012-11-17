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

    // String types
    STRING,

    // Boolean type
    BOOL,
    BOOL1,
    BOOL2,
    BOOL3,
    BOOL4,
    BOOL1x1,
    BOOL1x2,
    BOOL1x3,
    BOOL1x4,
    BOOL2x1,
    BOOL2x2,
    BOOL2x3,
    BOOL2x4,
    BOOL3x1,
    BOOL3x2,
    BOOL3x3,
    BOOL3x4,
    BOOL4x1,
    BOOL4x2,
    BOOL4x3,
    BOOL4x4,

    // Signed integer types
    CHAR,
    CHAR1,
    CHAR2,
    CHAR3,
    CHAR4,
    CHAR1x1,
    CHAR1x2,
    CHAR1x3,
    CHAR1x4,
    CHAR2x1,
    CHAR2x2,
    CHAR2x3,
    CHAR2x4,
    CHAR3x1,
    CHAR3x2,
    CHAR3x3,
    CHAR3x4,
    CHAR4x1,
    CHAR4x2,
    CHAR4x3,
    CHAR4x4,
    
    SHORT,
    SHORT1,
    SHORT2,
    SHORT3,
    SHORT4,
    SHORT1x1,
    SHORT1x2,
    SHORT1x3,
    SHORT1x4,
    SHORT2x1,
    SHORT2x2,
    SHORT2x3,
    SHORT2x4,
    SHORT3x1,
    SHORT3x2,
    SHORT3x3,
    SHORT3x4,
    SHORT4x1,
    SHORT4x2,
    SHORT4x3,
    SHORT4x4,
    
    INT,
    INT1,
    INT2,
    INT3,
    INT4,
    INT1x1,
    INT1x2,
    INT1x3,
    INT1x4,
    INT2x1,
    INT2x2,
    INT2x3,
    INT2x4,
    INT3x1,
    INT3x2,
    INT3x3,
    INT3x4,
    INT4x1,
    INT4x2,
    INT4x3,
    INT4x4,
    
    LONG,
    LONG1,
    LONG2,
    LONG3,
    LONG4,
    LONG1x1,
    LONG1x2,
    LONG1x3,
    LONG1x4,
    LONG2x1,
    LONG2x2,
    LONG2x3,
    LONG2x4,
    LONG3x1,
    LONG3x2,
    LONG3x3,
    LONG3x4,
    LONG4x1,
    LONG4x2,
    LONG4x3,
    LONG4x4,

    // Unsigned integer types
    UCHAR,
    UCHAR1,
    UCHAR2,
    UCHAR3,
    UCHAR4,
    UCHAR1x1,
    UCHAR1x2,
    UCHAR1x3,
    UCHAR1x4,
    UCHAR2x1,
    UCHAR2x2,
    UCHAR2x3,
    UCHAR2x4,
    UCHAR3x1,
    UCHAR3x2,
    UCHAR3x3,
    UCHAR3x4,
    UCHAR4x1,
    UCHAR4x2,
    UCHAR4x3,
    UCHAR4x4,
    
    USHORT,
    USHORT1,
    USHORT2,
    USHORT3,
    USHORT4,
    USHORT1x1,
    USHORT1x2,
    USHORT1x3,
    USHORT1x4,
    USHORT2x1,
    USHORT2x2,
    USHORT2x3,
    USHORT2x4,
    USHORT3x1,
    USHORT3x2,
    USHORT3x3,
    USHORT3x4,
    USHORT4x1,
    USHORT4x2,
    USHORT4x3,
    USHORT4x4,
    
    UINT,
    UINT1,
    UINT2,
    UINT3,
    UINT4,
    UINT1x1,
    UINT1x2,
    UINT1x3,
    UINT1x4,
    UINT2x1,
    UINT2x2,
    UINT2x3,
    UINT2x4,
    UINT3x1,
    UINT3x2,
    UINT3x3,
    UINT3x4,
    UINT4x1,
    UINT4x2,
    UINT4x3,
    UINT4x4,
    
    ULONG,
    ULONG1,
    ULONG2,
    ULONG3,
    ULONG4,
    ULONG1x1,
    ULONG1x2,
    ULONG1x3,
    ULONG1x4,
    ULONG2x1,
    ULONG2x2,
    ULONG2x3,
    ULONG2x4,
    ULONG3x1,
    ULONG3x2,
    ULONG3x3,
    ULONG3x4,
    ULONG4x1,
    ULONG4x2,
    ULONG4x3,
    ULONG4x4,


    // Floating point types
    FLOAT,
    FLOAT1,
    FLOAT2,
    FLOAT3,
    FLOAT4,
    FLOAT1x1,
    FLOAT1x2,
    FLOAT1x3,
    FLOAT1x4,
    FLOAT2x1,
    FLOAT2x2,
    FLOAT2x3,
    FLOAT2x4,
    FLOAT3x1,
    FLOAT3x2,
    FLOAT3x3,
    FLOAT3x4,
    FLOAT4x1,
    FLOAT4x2,
    FLOAT4x3,
    FLOAT4x4,

    DOUBLE,
    DOUBLE1,
    DOUBLE2,
    DOUBLE3,
    DOUBLE4,
    DOUBLE1x1,
    DOUBLE1x2,
    DOUBLE1x3,
    DOUBLE1x4,
    DOUBLE2x1,
    DOUBLE2x2,
    DOUBLE2x3,
    DOUBLE2x4,
    DOUBLE3x1,
    DOUBLE3x2,
    DOUBLE3x3,
    DOUBLE3x4,
    DOUBLE4x1,
    DOUBLE4x2,
    DOUBLE4x3,
    DOUBLE4x4,
};

using jl_bool      = bool;
using jl_bool1     = JoeMath::Vector<jl_bool, 1>;
using jl_bool2     = JoeMath::Vector<jl_bool, 2>;
using jl_bool3     = JoeMath::Vector<jl_bool, 3>;
using jl_bool4     = JoeMath::Vector<jl_bool, 4>;
using jl_bool1x1   = JoeMath::Matrix<jl_bool, 1, 1>;
using jl_bool1x2   = JoeMath::Matrix<jl_bool, 1, 2>;
using jl_bool1x3   = JoeMath::Matrix<jl_bool, 1, 3>;
using jl_bool1x4   = JoeMath::Matrix<jl_bool, 1, 4>;
using jl_bool2x1   = JoeMath::Matrix<jl_bool, 2, 1>;
using jl_bool2x2   = JoeMath::Matrix<jl_bool, 2, 2>;
using jl_bool2x3   = JoeMath::Matrix<jl_bool, 2, 3>;
using jl_bool2x4   = JoeMath::Matrix<jl_bool, 2, 4>;
using jl_bool3x1   = JoeMath::Matrix<jl_bool, 3, 1>;
using jl_bool3x2   = JoeMath::Matrix<jl_bool, 3, 2>;
using jl_bool3x3   = JoeMath::Matrix<jl_bool, 3, 3>;
using jl_bool3x4   = JoeMath::Matrix<jl_bool, 3, 4>;
using jl_bool4x1   = JoeMath::Matrix<jl_bool, 4, 1>;
using jl_bool4x2   = JoeMath::Matrix<jl_bool, 4, 2>;
using jl_bool4x3   = JoeMath::Matrix<jl_bool, 4, 3>;
using jl_bool4x4   = JoeMath::Matrix<jl_bool, 4, 4>;

using jl_char      = int8_t;
using jl_char1     = JoeMath::Vector<jl_char, 1>;
using jl_char2     = JoeMath::Vector<jl_char, 2>;
using jl_char3     = JoeMath::Vector<jl_char, 3>;
using jl_char4     = JoeMath::Vector<jl_char, 4>;
using jl_char1x1   = JoeMath::Matrix<jl_char, 1, 1>;
using jl_char1x2   = JoeMath::Matrix<jl_char, 1, 2>;
using jl_char1x3   = JoeMath::Matrix<jl_char, 1, 3>;
using jl_char1x4   = JoeMath::Matrix<jl_char, 1, 4>;
using jl_char2x1   = JoeMath::Matrix<jl_char, 2, 1>;
using jl_char2x2   = JoeMath::Matrix<jl_char, 2, 2>;
using jl_char2x3   = JoeMath::Matrix<jl_char, 2, 3>;
using jl_char2x4   = JoeMath::Matrix<jl_char, 2, 4>;
using jl_char3x1   = JoeMath::Matrix<jl_char, 3, 1>;
using jl_char3x2   = JoeMath::Matrix<jl_char, 3, 2>;
using jl_char3x3   = JoeMath::Matrix<jl_char, 3, 3>;
using jl_char3x4   = JoeMath::Matrix<jl_char, 3, 4>;
using jl_char4x1   = JoeMath::Matrix<jl_char, 4, 1>;
using jl_char4x2   = JoeMath::Matrix<jl_char, 4, 2>;
using jl_char4x3   = JoeMath::Matrix<jl_char, 4, 3>;
using jl_char4x4   = JoeMath::Matrix<jl_char, 4, 4>;

using jl_short     = int16_t;
using jl_short1    = JoeMath::Vector<jl_short, 1>;
using jl_short2    = JoeMath::Vector<jl_short, 2>;
using jl_short3    = JoeMath::Vector<jl_short, 3>;
using jl_short4    = JoeMath::Vector<jl_short, 4>;
using jl_short1x1  = JoeMath::Matrix<jl_short, 1, 1>;
using jl_short1x2  = JoeMath::Matrix<jl_short, 1, 2>;
using jl_short1x3  = JoeMath::Matrix<jl_short, 1, 3>;
using jl_short1x4  = JoeMath::Matrix<jl_short, 1, 4>;
using jl_short2x1  = JoeMath::Matrix<jl_short, 2, 1>;
using jl_short2x2  = JoeMath::Matrix<jl_short, 2, 2>;
using jl_short2x3  = JoeMath::Matrix<jl_short, 2, 3>;
using jl_short2x4  = JoeMath::Matrix<jl_short, 2, 4>;
using jl_short3x1  = JoeMath::Matrix<jl_short, 3, 1>;
using jl_short3x2  = JoeMath::Matrix<jl_short, 3, 2>;
using jl_short3x3  = JoeMath::Matrix<jl_short, 3, 3>;
using jl_short3x4  = JoeMath::Matrix<jl_short, 3, 4>;
using jl_short4x1  = JoeMath::Matrix<jl_short, 4, 1>;
using jl_short4x2  = JoeMath::Matrix<jl_short, 4, 2>;
using jl_short4x3  = JoeMath::Matrix<jl_short, 4, 3>;
using jl_short4x4  = JoeMath::Matrix<jl_short, 4, 4>;
 
using jl_int       = int32_t;
using jl_int1      = JoeMath::Vector<jl_int, 1>;
using jl_int2      = JoeMath::Vector<jl_int, 2>;
using jl_int3      = JoeMath::Vector<jl_int, 3>;
using jl_int4      = JoeMath::Vector<jl_int, 4>;
using jl_int1x1    = JoeMath::Matrix<jl_int, 1, 1>;
using jl_int1x2    = JoeMath::Matrix<jl_int, 1, 2>;
using jl_int1x3    = JoeMath::Matrix<jl_int, 1, 3>;
using jl_int1x4    = JoeMath::Matrix<jl_int, 1, 4>;
using jl_int2x1    = JoeMath::Matrix<jl_int, 2, 1>;
using jl_int2x2    = JoeMath::Matrix<jl_int, 2, 2>;
using jl_int2x3    = JoeMath::Matrix<jl_int, 2, 3>;
using jl_int2x4    = JoeMath::Matrix<jl_int, 2, 4>;
using jl_int3x1    = JoeMath::Matrix<jl_int, 3, 1>;
using jl_int3x2    = JoeMath::Matrix<jl_int, 3, 2>;
using jl_int3x3    = JoeMath::Matrix<jl_int, 3, 3>;
using jl_int3x4    = JoeMath::Matrix<jl_int, 3, 4>;
using jl_int4x1    = JoeMath::Matrix<jl_int, 4, 1>;
using jl_int4x2    = JoeMath::Matrix<jl_int, 4, 2>;
using jl_int4x3    = JoeMath::Matrix<jl_int, 4, 3>;
using jl_int4x4    = JoeMath::Matrix<jl_int, 4, 4>;
  
using jl_long      = int64_t;
using jl_long1     = JoeMath::Vector<jl_long, 1>;
using jl_long2     = JoeMath::Vector<jl_long, 2>;
using jl_long3     = JoeMath::Vector<jl_long, 3>;
using jl_long4     = JoeMath::Vector<jl_long, 4>;
using jl_long1x1   = JoeMath::Matrix<jl_long, 1, 1>;
using jl_long1x2   = JoeMath::Matrix<jl_long, 1, 2>;
using jl_long1x3   = JoeMath::Matrix<jl_long, 1, 3>;
using jl_long1x4   = JoeMath::Matrix<jl_long, 1, 4>;
using jl_long2x1   = JoeMath::Matrix<jl_long, 2, 1>;
using jl_long2x2   = JoeMath::Matrix<jl_long, 2, 2>;
using jl_long2x3   = JoeMath::Matrix<jl_long, 2, 3>;
using jl_long2x4   = JoeMath::Matrix<jl_long, 2, 4>;
using jl_long3x1   = JoeMath::Matrix<jl_long, 3, 1>;
using jl_long3x2   = JoeMath::Matrix<jl_long, 3, 2>;
using jl_long3x3   = JoeMath::Matrix<jl_long, 3, 3>;
using jl_long3x4   = JoeMath::Matrix<jl_long, 3, 4>;
using jl_long4x1   = JoeMath::Matrix<jl_long, 4, 1>;
using jl_long4x2   = JoeMath::Matrix<jl_long, 4, 2>;
using jl_long4x3   = JoeMath::Matrix<jl_long, 4, 3>;
using jl_long4x4   = JoeMath::Matrix<jl_long, 4, 4>;

using jl_uchar     = uint8_t;
using jl_uchar1    = JoeMath::Vector<jl_uchar, 1>;
using jl_uchar2    = JoeMath::Vector<jl_uchar, 2>;
using jl_uchar3    = JoeMath::Vector<jl_uchar, 3>;
using jl_uchar4    = JoeMath::Vector<jl_uchar, 4>;
using jl_uchar1x1  = JoeMath::Matrix<jl_uchar, 1, 1>;
using jl_uchar1x2  = JoeMath::Matrix<jl_uchar, 1, 2>;
using jl_uchar1x3  = JoeMath::Matrix<jl_uchar, 1, 3>;
using jl_uchar1x4  = JoeMath::Matrix<jl_uchar, 1, 4>;
using jl_uchar2x1  = JoeMath::Matrix<jl_uchar, 2, 1>;
using jl_uchar2x2  = JoeMath::Matrix<jl_uchar, 2, 2>;
using jl_uchar2x3  = JoeMath::Matrix<jl_uchar, 2, 3>;
using jl_uchar2x4  = JoeMath::Matrix<jl_uchar, 2, 4>;
using jl_uchar3x1  = JoeMath::Matrix<jl_uchar, 3, 1>;
using jl_uchar3x2  = JoeMath::Matrix<jl_uchar, 3, 2>;
using jl_uchar3x3  = JoeMath::Matrix<jl_uchar, 3, 3>;
using jl_uchar3x4  = JoeMath::Matrix<jl_uchar, 3, 4>;
using jl_uchar4x1  = JoeMath::Matrix<jl_uchar, 4, 1>;
using jl_uchar4x2  = JoeMath::Matrix<jl_uchar, 4, 2>;
using jl_uchar4x3  = JoeMath::Matrix<jl_uchar, 4, 3>;
using jl_uchar4x4  = JoeMath::Matrix<jl_uchar, 4, 4>;

using jl_ushort    = uint16_t;
using jl_ushort1   = JoeMath::Vector<jl_ushort, 1>;
using jl_ushort2   = JoeMath::Vector<jl_ushort, 2>;
using jl_ushort3   = JoeMath::Vector<jl_ushort, 3>;
using jl_ushort4   = JoeMath::Vector<jl_ushort, 4>;
using jl_ushort1x1 = JoeMath::Matrix<jl_ushort, 1, 1>;
using jl_ushort1x2 = JoeMath::Matrix<jl_ushort, 1, 2>;
using jl_ushort1x3 = JoeMath::Matrix<jl_ushort, 1, 3>;
using jl_ushort1x4 = JoeMath::Matrix<jl_ushort, 1, 4>;
using jl_ushort2x1 = JoeMath::Matrix<jl_ushort, 2, 1>;
using jl_ushort2x2 = JoeMath::Matrix<jl_ushort, 2, 2>;
using jl_ushort2x3 = JoeMath::Matrix<jl_ushort, 2, 3>;
using jl_ushort2x4 = JoeMath::Matrix<jl_ushort, 2, 4>;
using jl_ushort3x1 = JoeMath::Matrix<jl_ushort, 3, 1>;
using jl_ushort3x2 = JoeMath::Matrix<jl_ushort, 3, 2>;
using jl_ushort3x3 = JoeMath::Matrix<jl_ushort, 3, 3>;
using jl_ushort3x4 = JoeMath::Matrix<jl_ushort, 3, 4>;
using jl_ushort4x1 = JoeMath::Matrix<jl_ushort, 4, 1>;
using jl_ushort4x2 = JoeMath::Matrix<jl_ushort, 4, 2>;
using jl_ushort4x3 = JoeMath::Matrix<jl_ushort, 4, 3>;
using jl_ushort4x4 = JoeMath::Matrix<jl_ushort, 4, 4>;

using jl_uint      = uint32_t;
using jl_uint1     = JoeMath::Vector<jl_uint, 1>;
using jl_uint2     = JoeMath::Vector<jl_uint, 2>;
using jl_uint3     = JoeMath::Vector<jl_uint, 3>;
using jl_uint4     = JoeMath::Vector<jl_uint, 4>;
using jl_uint1x1   = JoeMath::Matrix<jl_uint, 1, 1>;
using jl_uint1x2   = JoeMath::Matrix<jl_uint, 1, 2>;
using jl_uint1x3   = JoeMath::Matrix<jl_uint, 1, 3>;
using jl_uint1x4   = JoeMath::Matrix<jl_uint, 1, 4>;
using jl_uint2x1   = JoeMath::Matrix<jl_uint, 2, 1>;
using jl_uint2x2   = JoeMath::Matrix<jl_uint, 2, 2>;
using jl_uint2x3   = JoeMath::Matrix<jl_uint, 2, 3>;
using jl_uint2x4   = JoeMath::Matrix<jl_uint, 2, 4>;
using jl_uint3x1   = JoeMath::Matrix<jl_uint, 3, 1>;
using jl_uint3x2   = JoeMath::Matrix<jl_uint, 3, 2>;
using jl_uint3x3   = JoeMath::Matrix<jl_uint, 3, 3>;
using jl_uint3x4   = JoeMath::Matrix<jl_uint, 3, 4>;
using jl_uint4x1   = JoeMath::Matrix<jl_uint, 4, 1>;
using jl_uint4x2   = JoeMath::Matrix<jl_uint, 4, 2>;
using jl_uint4x3   = JoeMath::Matrix<jl_uint, 4, 3>;
using jl_uint4x4   = JoeMath::Matrix<jl_uint, 4, 4>;

using jl_ulong     = uint64_t;
using jl_ulong1    = JoeMath::Vector<jl_ulong, 1>;
using jl_ulong2    = JoeMath::Vector<jl_ulong, 2>;
using jl_ulong3    = JoeMath::Vector<jl_ulong, 3>;
using jl_ulong4    = JoeMath::Vector<jl_ulong, 4>;
using jl_ulong1x1  = JoeMath::Matrix<jl_ulong, 1, 1>;
using jl_ulong1x2  = JoeMath::Matrix<jl_ulong, 1, 2>;
using jl_ulong1x3  = JoeMath::Matrix<jl_ulong, 1, 3>;
using jl_ulong1x4  = JoeMath::Matrix<jl_ulong, 1, 4>;
using jl_ulong2x1  = JoeMath::Matrix<jl_ulong, 2, 1>;
using jl_ulong2x2  = JoeMath::Matrix<jl_ulong, 2, 2>;
using jl_ulong2x3  = JoeMath::Matrix<jl_ulong, 2, 3>;
using jl_ulong2x4  = JoeMath::Matrix<jl_ulong, 2, 4>;
using jl_ulong3x1  = JoeMath::Matrix<jl_ulong, 3, 1>;
using jl_ulong3x2  = JoeMath::Matrix<jl_ulong, 3, 2>;
using jl_ulong3x3  = JoeMath::Matrix<jl_ulong, 3, 3>;
using jl_ulong3x4  = JoeMath::Matrix<jl_ulong, 3, 4>;
using jl_ulong4x1  = JoeMath::Matrix<jl_ulong, 4, 1>;
using jl_ulong4x2  = JoeMath::Matrix<jl_ulong, 4, 2>;
using jl_ulong4x3  = JoeMath::Matrix<jl_ulong, 4, 3>;
using jl_ulong4x4  = JoeMath::Matrix<jl_ulong, 4, 4>;

using jl_float     = float;
using jl_float1    = JoeMath::Vector<jl_float, 1>;
using jl_float2    = JoeMath::Vector<jl_float, 2>;
using jl_float3    = JoeMath::Vector<jl_float, 3>;
using jl_float4    = JoeMath::Vector<jl_float, 4>;
using jl_float1x1  = JoeMath::Matrix<jl_float, 1, 1>;
using jl_float1x2  = JoeMath::Matrix<jl_float, 1, 2>;
using jl_float1x3  = JoeMath::Matrix<jl_float, 1, 3>;
using jl_float1x4  = JoeMath::Matrix<jl_float, 1, 4>;
using jl_float2x1  = JoeMath::Matrix<jl_float, 2, 1>;
using jl_float2x2  = JoeMath::Matrix<jl_float, 2, 2>;
using jl_float2x3  = JoeMath::Matrix<jl_float, 2, 3>;
using jl_float2x4  = JoeMath::Matrix<jl_float, 2, 4>;
using jl_float3x1  = JoeMath::Matrix<jl_float, 3, 1>;
using jl_float3x2  = JoeMath::Matrix<jl_float, 3, 2>;
using jl_float3x3  = JoeMath::Matrix<jl_float, 3, 3>;
using jl_float3x4  = JoeMath::Matrix<jl_float, 3, 4>;
using jl_float4x1  = JoeMath::Matrix<jl_float, 4, 1>;
using jl_float4x2  = JoeMath::Matrix<jl_float, 4, 2>;
using jl_float4x3  = JoeMath::Matrix<jl_float, 4, 3>;
using jl_float4x4  = JoeMath::Matrix<jl_float, 4, 4>;

using jl_double    = double;
using jl_double1   = JoeMath::Vector<jl_double, 1>;
using jl_double2   = JoeMath::Vector<jl_double, 2>;
using jl_double3   = JoeMath::Vector<jl_double, 3>;
using jl_double4   = JoeMath::Vector<jl_double, 4>;
using jl_double1x1 = JoeMath::Matrix<jl_double, 1, 1>;
using jl_double1x2 = JoeMath::Matrix<jl_double, 1, 2>;
using jl_double1x3 = JoeMath::Matrix<jl_double, 1, 3>;
using jl_double1x4 = JoeMath::Matrix<jl_double, 1, 4>;
using jl_double2x1 = JoeMath::Matrix<jl_double, 2, 1>;
using jl_double2x2 = JoeMath::Matrix<jl_double, 2, 2>;
using jl_double2x3 = JoeMath::Matrix<jl_double, 2, 3>;
using jl_double2x4 = JoeMath::Matrix<jl_double, 2, 4>;
using jl_double3x1 = JoeMath::Matrix<jl_double, 3, 1>;
using jl_double3x2 = JoeMath::Matrix<jl_double, 3, 2>;
using jl_double3x3 = JoeMath::Matrix<jl_double, 3, 3>;
using jl_double3x4 = JoeMath::Matrix<jl_double, 3, 4>;
using jl_double4x1 = JoeMath::Matrix<jl_double, 4, 1>;
using jl_double4x2 = JoeMath::Matrix<jl_double, 4, 2>;
using jl_double4x3 = JoeMath::Matrix<jl_double, 4, 3>;
using jl_double4x4 = JoeMath::Matrix<jl_double, 4, 4>;


template<typename T>
struct JoeLangType
{
private:
    static constexpr
    Type GetType()
    {
#define MATCHN(a, b, n) std::is_same<T, a##n>::value ? Type::b##n :
        
#define MATCH(a, b) MATCHN(a, b, ) \
                    MATCHN(a, b, 1) \
                    MATCHN(a, b, 2) \
                    MATCHN(a, b, 3) \
                    MATCHN(a, b, 4) \
                    MATCHN(a, b, 1x1) \
                    MATCHN(a, b, 1x2) \
                    MATCHN(a, b, 1x3) \
                    MATCHN(a, b, 1x4) \
                    MATCHN(a, b, 2x1) \
                    MATCHN(a, b, 2x2) \
                    MATCHN(a, b, 2x3) \
                    MATCHN(a, b, 2x4) \
                    MATCHN(a, b, 3x1) \
                    MATCHN(a, b, 3x2) \
                    MATCHN(a, b, 3x3) \
                    MATCHN(a, b, 3x4) \
                    MATCHN(a, b, 4x1) \
                    MATCHN(a, b, 4x2) \
                    MATCHN(a, b, 4x3) \
                    MATCHN(a, b, 4x4)
        
        return MATCH(jl_bool,   BOOL)
               MATCH(jl_char,   CHAR)
               MATCH(jl_short,  SHORT)
               MATCH(jl_int,    INT)
               MATCH(jl_long,   LONG)
               MATCH(jl_uchar,  UCHAR)
               MATCH(jl_ushort, USHORT)
               MATCH(jl_uint,   UINT)
               MATCH(jl_ulong,  ULONG)
               MATCH(jl_float,  FLOAT)
               MATCH(jl_double, DOUBLE)
               std::is_same<T, std::string>::value ? Type::STRING :
               Type::UNKNOWN;
    }

public:
    const static
    Type value = GetType();
};

} // namespace JoeLang
