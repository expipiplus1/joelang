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

#include <runtime/types.hpp>

namespace llvm
{
    class Constant;
}

namespace JoeLang
{

enum class Type;

namespace Compiler
{

using ArrayExtents = std::vector<unsigned>;
class CompleteType;
class CodeGenerator;
class ShaderWriter;

class GenericValue
{
public:
    GenericValue();
    GenericValue( const GenericValue& g );
    explicit
    GenericValue( Type type );
    const GenericValue& operator = ( const GenericValue& g );

#define TYPE_CONSTRUCTOR_AND_GETTER(type, Type) \
    explicit \
    GenericValue( type type##_value ); \
    type Get##Type() const;

#define TYPE_CONSTRUCTOR_AND_GETTER_N(type, Type) \
    TYPE_CONSTRUCTOR_AND_GETTER( type, Type ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2, Type##2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3, Type##3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4, Type##4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x2, Type##2x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x3, Type##2x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##2x4, Type##2x4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x2, Type##3x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x3, Type##3x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##3x4, Type##3x4 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x2, Type##4x2 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x3, Type##4x3 ) \
    TYPE_CONSTRUCTOR_AND_GETTER( type##4x4, Type##4x4 )

    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_bool,   Bool )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_char,   Char )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_short,  Short )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_int,    Int )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_long,   Long )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_uchar,  UChar )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_ushort, UShort )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_uint,   UInt )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_ulong,  ULong )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_float,  Float )
    TYPE_CONSTRUCTOR_AND_GETTER_N( jl_double, Double )

#undef TYPE_CONSTRUCTOR_AND_GETTER_N
#undef TYPE_CONSTRUCTOR_AND_GETTER

    explicit
    GenericValue( jl_string&& string_value );
    explicit
    GenericValue( std::string string_value );
    explicit
    GenericValue( std::vector<GenericValue> array );

    ~GenericValue();

    llvm::Constant* CodeGen( CodeGenerator& code_gen ) const;
    void Write( ShaderWriter& shader_writer ) const;

    CompleteType GetType() const;
    Type GetUnderlyingType() const;
    ArrayExtents GetArrayExtents() const;

    const std::string& GetString() const;
    const std::vector<GenericValue>& GetArray() const;

private:
    /**
      * Frees the current value if it has a destructor
      */
    void FreeData();

    Type m_Type;

#define TYPE_N(type, Type) \
    type      m_##Type##Value; \
    type##2   m_##Type##2##Value; \
    type##3   m_##Type##3##Value; \
    type##4   m_##Type##4##Value; \
    type##2x2 m_##Type##2x2##Value; \
    type##2x3 m_##Type##2x3##Value; \
    type##2x4 m_##Type##2x4##Value; \
    type##3x2 m_##Type##3x2##Value; \
    type##3x3 m_##Type##3x3##Value; \
    type##3x4 m_##Type##3x4##Value; \
    type##4x2 m_##Type##4x2##Value; \
    type##4x3 m_##Type##4x3##Value; \
    type##4x4 m_##Type##4x4##Value;

    union
    {
        TYPE_N( jl_bool,   Bool )
        TYPE_N( jl_char,   Char )
        TYPE_N( jl_short,  Short )
        TYPE_N( jl_int,    Int )
        TYPE_N( jl_long,   Long )
        TYPE_N( jl_uchar,  UChar )
        TYPE_N( jl_ushort, UShort )
        TYPE_N( jl_uint,   UInt )
        TYPE_N( jl_ulong,  ULong )
        TYPE_N( jl_float,  Float )
        TYPE_N( jl_double, Double )

        std::string m_StringValue;
        std::vector<GenericValue> m_ArrayValue;
    };

#undef TYPE_N

};

} // namespace Compiler
} // namespace JoeLang
