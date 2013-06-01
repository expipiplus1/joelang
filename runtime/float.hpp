/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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

#include "types.hpp"

extern "C"
{
    jl_float dot_float ( jl_float  v1, jl_float  v2 );
    jl_float dot_float2( jl_float2 v1, jl_float2 v2 );
    jl_float dot_float3( jl_float3 v1, jl_float3 v2 );
    jl_float dot_float4( jl_float4 v1, jl_float4 v2 );

    jl_float  normalize_float ( jl_float  v );
    jl_float2 normalize_float2( jl_float2 v );
    jl_float3 normalize_float3( jl_float3 v );
    jl_float4 normalize_float4( jl_float4 v );


    jl_float2   mul_float2x2_float2( jl_float2x2 m1, jl_float2 m2 );
    jl_float2x2 mul_float2x2_float2x2( jl_float2x2 m1, jl_float2x2 m2 );
    jl_float3x2 mul_float2x2_float3x2( jl_float2x2 m1, jl_float3x2 m2 );
    jl_float4x2 mul_float2x2_float4x2( jl_float2x2 m1, jl_float4x2 m2 );

    jl_float3   mul_float2x3_float2( jl_float2x3 m1, jl_float2 m2 );
    jl_float2x3 mul_float2x3_float2x2( jl_float2x3 m1, jl_float2x2 m2 );
    jl_float3x3 mul_float2x3_float3x2( jl_float2x3 m1, jl_float3x2 m2 );
    jl_float4x3 mul_float2x3_float4x2( jl_float2x3 m1, jl_float4x2 m2 );

    jl_float4   mul_float2x4_float2( jl_float2x4 m1, jl_float2 m2 );
    jl_float2x4 mul_float2x4_float2x2( jl_float2x4 m1, jl_float2x2 m2 );
    jl_float3x4 mul_float2x4_float3x2( jl_float2x4 m1, jl_float3x2 m2 );
    jl_float4x4 mul_float2x4_float4x2( jl_float2x4 m1, jl_float4x2 m2 );


    jl_float2   mul_float3x2_float3( jl_float3x2 m1, jl_float3 m2 );
    jl_float2x2 mul_float3x2_float2x3( jl_float3x2 m1, jl_float2x3 m2 );
    jl_float3x2 mul_float3x2_float3x3( jl_float3x2 m1, jl_float3x3 m2 );
    jl_float4x2 mul_float3x2_float4x3( jl_float3x2 m1, jl_float4x3 m2 );

    jl_float3   mul_float3x3_float3( jl_float3x3 m1, jl_float3 m2 );
    jl_float2x3 mul_float3x3_float2x3( jl_float3x3 m1, jl_float2x3 m2 );
    jl_float3x3 mul_float3x3_float3x3( jl_float3x3 m1, jl_float3x3 m2 );
    jl_float4x3 mul_float3x3_float4x3( jl_float3x3 m1, jl_float4x3 m2 );

    jl_float4   mul_float3x4_float3( jl_float3x4 m1, jl_float3 m2 );
    jl_float2x4 mul_float3x4_float2x3( jl_float3x4 m1, jl_float2x3 m2 );
    jl_float3x4 mul_float3x4_float3x3( jl_float3x4 m1, jl_float3x3 m2 );
    jl_float4x4 mul_float3x4_float4x3( jl_float3x4 m1, jl_float4x3 m2 );


    jl_float2   mul_float4x2_float4( jl_float4x2 m1, jl_float4 m2 );
    jl_float2x2 mul_float4x2_float2x4( jl_float4x2 m1, jl_float2x4 m2 );
    jl_float3x2 mul_float4x2_float3x4( jl_float4x2 m1, jl_float3x4 m2 );
    jl_float4x2 mul_float4x2_float4x4( jl_float4x2 m1, jl_float4x4 m2 );

    jl_float3   mul_float4x3_float4( jl_float4x3 m1, jl_float4 m2 );
    jl_float2x3 mul_float4x3_float2x4( jl_float4x3 m1, jl_float2x4 m2 );
    jl_float3x3 mul_float4x3_float3x4( jl_float4x3 m1, jl_float3x4 m2 );
    jl_float4x3 mul_float4x3_float4x4( jl_float4x3 m1, jl_float4x4 m2 );

    jl_float4   mul_float4x4_float4( jl_float4x4 m1, jl_float4 m2 );
    jl_float2x4 mul_float4x4_float2x4( jl_float4x4 m1, jl_float2x4 m2 );
    jl_float3x4 mul_float4x4_float3x4( jl_float4x4 m1, jl_float3x4 m2 );
    jl_float4x4 mul_float4x4_float4x4( jl_float4x4 m1, jl_float4x4 m2 );

    //
    // These treat the vector as a row vector and return a row vector
    //
    jl_float2   mul_float2_float2x2( jl_float2 m1, jl_float2x2 m2 );
    jl_float3   mul_float2_float3x2( jl_float2 m1, jl_float3x2 m2 );
    jl_float4   mul_float2_float4x2( jl_float2 m1, jl_float4x2 m2 );

    jl_float2   mul_float3_float2x3( jl_float3 m1, jl_float2x3 m2 );
    jl_float3   mul_float3_float3x3( jl_float3 m1, jl_float3x3 m2 );
    jl_float4   mul_float3_float4x3( jl_float3 m1, jl_float4x3 m2 );

    jl_float2   mul_float4_float2x4( jl_float4 m1, jl_float2x4 m2 );
    jl_float3   mul_float4_float3x4( jl_float4 m1, jl_float3x4 m2 );
    jl_float4   mul_float4_float4x4( jl_float4 m1, jl_float4x4 m2 );
}
