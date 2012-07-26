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

#include "string.hpp"

#include <cstring>

bool String_Equal( jl_string a, jl_string b )
{
    return a.size == b.size &&
           memcmp( a.data, b.data, a.size ) == 0;
}

bool String_NotEqual( jl_string a, jl_string b )
{
    return !String_Equal( a, b );
}

jl_string String_Concat( jl_string a, jl_string b )
{
    jl_string ret;
    ret.size = a.size + b.size;
    jl_u8* data = new jl_u8[ret.size];
    memcpy( data, a.data, a.size );
    memcpy( data + a.size, b.data, b.size );
    ret.data = data;
    return ret;
}

/*
void String_Copy( jl_string& to, const jl_string& from )
{
    to = String_Concat( to, to );
    String_Destroy( to );
    to.size = from.size;
    jl_u8* data = new jl_u8[to.size];
    memcpy( data, from.data, from.size );
    to.data = data;
}

void String_Move( jl_string& to, jl_string&& from )
{
    auto s    = to.size;
    to.size   = from.size;
    from.size = s;

    auto d    = to.data;
    to.data   = from.data;
    from.data = d;
}
*/

jl_string String_Create( jl_u32 size, const jl_u8* data )
{
    jl_string ret;
    ret.size = size;
    jl_u8* ret_data = new jl_u8[ret.size];
    memcpy( ret_data, data, ret.size );
    ret.data = data;
    return ret;
}

void String_Destroy( jl_string a )
{
    if( a.data )
        delete[] a.data;
}
