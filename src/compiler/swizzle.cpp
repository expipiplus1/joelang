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

#include "swizzle.hpp"

#include <array>
#include <cassert>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Swizzle
//------------------------------------------------------------------------------

Swizzle::Swizzle()
    :m_SwizzleIndices{{ 0xff, 0xff, 0xff, 0xff }}
{

}

Swizzle::Swizzle( unsigned char index_1,
                  unsigned char index_2,
                  unsigned char index_3,
                  unsigned char index_4 )
    :m_SwizzleIndices{{ index_1, index_2, index_3, index_4 }}
{
    assert( ( index_1 == 0xff || index_1 < 4 ) &&
            "Swizzle index out of range" );
    assert( ( index_2 == 0xff || index_2 < 4 ) &&
            "Swizzle index out of range" );
    assert( ( index_3 == 0xff || index_3 < 4 ) &&
            "Swizzle index out of range" );
    assert( ( index_4 == 0xff || index_4 < 4 ) &&
            "Swizzle index out of range" );
#ifndef NDEBUG
    for( unsigned i = GetSize(); i < 4; ++i )
        assert( m_SwizzleIndices[i] == 0xff && "unused swizzle isn't 0xff" );
#endif
}

//Swizzle& Swizzle::operator = ( Swizzle other )
//{
    //for( unsigned i = 0; i < 4; ++i )
        //m_SwizzleIndices[i] = other.m_SwizzleIndices[i];
//}

unsigned Swizzle::GetSize() const
{
    for( unsigned i = 0; i < 4; ++i )
        if( m_SwizzleIndices[i] == 0xff )
            return i;
    return 4;
}

unsigned char Swizzle::GetIndex( unsigned index ) const
{
    assert( index < GetSize() &&
            "Trying to get an out of bounds swizzle index" );
    return m_SwizzleIndices[index];
}

bool Swizzle::HasDuplicates() const
{
    for( unsigned i = 1; i < GetSize(); ++i )
        for( unsigned j =0; j < i; ++j )
            if( GetIndex(i) == GetIndex(j) )
                return true;
    return false;
}

bool Swizzle::IsValid() const
{
    return GetSize() != 0;
}

std::array<unsigned char, 4>::const_iterator Swizzle::begin() const
{
    return m_SwizzleIndices.begin();
}

std::array<unsigned char, 4>::const_iterator Swizzle::end() const
{
    return begin() + GetSize();
}



} // namespace Compiler
} // namespace JoeLang
