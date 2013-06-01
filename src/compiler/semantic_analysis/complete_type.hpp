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
#include <vector>

namespace JoeLang
{
enum class Type;

namespace Compiler
{

using ArrayExtents = std::vector<unsigned>;
class ShaderWriter;

/**
  * \class CompleteType
  * \brief A class to hold all the information a type could need
  */
class CompleteType
{
public:
    CompleteType();
    CompleteType( Type base_type, ArrayExtents array_extents = {} );
    
    /**
      * This doesn't return the base type, for example this could return
      * Type::ARRAY
      */
    Type GetType() const;

    const ArrayExtents& GetArrayExtents() const;

    Type GetBaseType() const;

    bool IsArrayType() const;

    bool IsUnknown() const;
    bool IsVoid() const;

    bool IsFloatingPoint() const;
    bool IsIntegral() const;

    bool IsStructType() const;
    bool IsMatrixType() const;
    bool IsVectorType() const;
    bool IsScalarType() const;
    bool IsString() const;

    bool IsSigned() const;

    Type GetElementType() const;

    unsigned GetNumElements() const;

    unsigned GetNumMatrixRows() const;
    unsigned GetNumMatrixColumns() const;

    Type GetMatrixColumnType() const;

    std::string GetString() const;

    void Write( ShaderWriter& shader_writer ) const;

    bool operator == ( const CompleteType& other ) const;
    bool operator != ( const CompleteType& other ) const;
private:
    Type         m_BaseType;
    ArrayExtents m_ArrayExtents;
};

} // namespace Compiler
} // namespace JoeLang
