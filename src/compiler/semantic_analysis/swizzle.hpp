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

#include <array>
#include <string>

namespace JoeLang
{
namespace Compiler
{

/**
  * \class Swizzle
  * \brief holds onto swizzle data
  */
class Swizzle
{
public:
    Swizzle();

    explicit
    Swizzle( unsigned char index_1,
             unsigned char index_2 = 0xff,
             unsigned char index_3 = 0xff,
             unsigned char index_4 = 0xff );

    //Swizzle& operator = ( Swizzle other );

    //
    // returns the number of swizzle indices we have
    //
    unsigned GetSize() const;

    //
    // Get the specified swizzle index, this asserts that index < swizzle size
    //
    unsigned char GetIndex( unsigned index ) const;
    
    //
    // returns the string as an xyzw swizzle
    //
    std::string GetString() const;

    //
    //
    //
    bool HasDuplicates() const;

    bool IsValid() const;

    std::array<unsigned char, 4>::const_iterator begin() const;
    std::array<unsigned char, 4>::const_iterator end() const;

private:
    //
    // todo check if just storing an int would be better here
    //
    std::array<unsigned char, 4> m_SwizzleIndices;
};

} // namespace Compiler

} // namespace JoeLang
