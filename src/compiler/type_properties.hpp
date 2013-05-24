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

#include <string>

namespace JoeLang
{
enum class Type;

namespace Compiler
{

class CompleteType;

// todo review all this mess

/**
  * This returns the type according to the integer promotion rules
  */
CompleteType GetCommonType( const CompleteType& t1, const CompleteType& t2 );

Type GetVectorType( Type base, unsigned size );

/** Reduces matrix or vector types to their scalar types **/
Type GetScalarType( Type t );

/** Reduces matrix types to their vector type **/
Type GetMatrixElementType( Type t );

bool IsFloatingPoint( Type t );

bool IsIntegral( Type t );

bool IsSigned( Type t );

bool IsVectorType( Type t );

bool IsMatrixType( Type t );

bool IsScalarType( Type t );

/**
  * This asserts that t is not Type::UNKNOWN
  */
unsigned GetNumElementsInType( Type t );

unsigned GetNumRowsInType( Type t );

unsigned GetNumColumnsInType( Type t );

/**
  * \param t
  *   The type to get the size of
  * \returns the number of bytes used to store a t
  */
std::size_t SizeOf( Type t );

/**
  * This asserts if t is not integral or t is bool
  */
Type MakeUnsigned( Type t );

const std::string& GetTypeString( Type t );

bool HasGLSLType( Type t );

const std::string& GetGLSLTypeString( Type t );
std::string GetGLSLTypeSuffix( Type t );

} //namespace Compiler
} // namespace JoeLang
