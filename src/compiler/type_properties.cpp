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

#include "type_properties.hpp"

#include <cassert>
#include <array>
#include <map>
#include <string>
#include <vector>

#include <compiler/complete_type.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{

namespace Compiler
{

CompleteType GetCommonType( const CompleteType& t1, const CompleteType& t2 )
{
    const static std::vector<Type> promotion_ordering =
    {
        Type::DOUBLE,
        Type::FLOAT,
        Type::ULONG,
        Type::LONG,
        Type::UINT,
        Type::INT,
        Type::USHORT,
        Type::SHORT,
        Type::UCHAR,
        Type::CHAR,
        Type::BOOL
    };

    if( t1.GetArrayExtents() != t2.GetArrayExtents() )
        return CompleteType();

    // If they are array types they have to have identical types (no implicit
    // conversion of arrays)
    if( t1.IsArrayType() )
    {
        if( t1.GetBaseType() == t2.GetBaseType() )
            return t1;
        else
            return CompleteType();
    }

    //
    // strings can only be converted into strings
    //
    if( t1.GetBaseType() == Type::STRING )
    {
        if( t2.GetBaseType() == Type::STRING )
            return t1;
        else
            return CompleteType();
    }

    Type t1_scalar_type = t1.GetVectorElementType();
    Type t2_scalar_type = t2.GetVectorElementType();
    Type common_scalar_type = Type::UNKNOWN;

    for( Type t : promotion_ordering )
        if( t1_scalar_type == t || t2_scalar_type == t )
        {
            common_scalar_type = t;
            break;
        }


    //
    // Vectors can be implicitly cast to a smaller vector type, this will
    // issue a warning
    //

    //
    // If they are both vectors, shrink one of them
    // otherwise expand the scalar type
    //
    unsigned result_size;

    if( t1.IsVectorType() && t2.IsVectorType() )
        result_size = JoeMath::Min( t1.GetVectorSize(),
                                    t2.GetVectorSize() );
    else
        result_size = JoeMath::Max( t1.GetVectorSize(),
                                    t2.GetVectorSize() );

    return CompleteType( GetVectorType( common_scalar_type,
                                        result_size ) );
}

Type GetVectorType( Type base, unsigned size )
{
#define CREATE_VECTOR_MAPPING(type) \
    {{ type, type##2, type##3, type##4 }}
    const static std::array< std::array< Type, 4 >, 11 > t =
    {{
        CREATE_VECTOR_MAPPING(Type::BOOL),
        CREATE_VECTOR_MAPPING(Type::CHAR),
        CREATE_VECTOR_MAPPING(Type::INT),
        CREATE_VECTOR_MAPPING(Type::SHORT),
        CREATE_VECTOR_MAPPING(Type::LONG),
        CREATE_VECTOR_MAPPING(Type::UCHAR),
        CREATE_VECTOR_MAPPING(Type::UINT),
        CREATE_VECTOR_MAPPING(Type::USHORT),
        CREATE_VECTOR_MAPPING(Type::ULONG),
        CREATE_VECTOR_MAPPING(Type::FLOAT),
        CREATE_VECTOR_MAPPING(Type::DOUBLE)
    }};
#undef CREATE_VECTOR_MAPPING

    assert( size <= 4 && size != 0 && "Trying to get an invalid vector size " );
    
    for( auto& a : t )
    {
        if( a[0] == base )
            return a[size-1]; 
    }
    assert( false && "Trying to make an unhandled vector type" );
    return Type::UNKNOWN;
}

Type GetScalarType( Type t )
{
#define RETURN_BASE(type) \
    case type: \
    case type##2: \
    case type##3: \
    case type##4: \
        return type

    switch( t )
    {
    RETURN_BASE(Type::BOOL);
    RETURN_BASE(Type::CHAR);
    RETURN_BASE(Type::SHORT);
    RETURN_BASE(Type::INT);
    RETURN_BASE(Type::LONG);
    RETURN_BASE(Type::UCHAR);
    RETURN_BASE(Type::USHORT);
    RETURN_BASE(Type::UINT);
    RETURN_BASE(Type::ULONG);
    RETURN_BASE(Type::FLOAT);
    RETURN_BASE(Type::DOUBLE);
    case Type::STRING:
        return Type::STRING;
    default:
        assert( false && "Trying to get the scalar type of an unknown type" );
        return Type::UNKNOWN;
    }

#undef RETURN_BASE
}

bool IsIntegral( Type t )
{
    t = GetScalarType( t );
    return t == Type::BOOL ||
           t == Type::CHAR   ||
           t == Type::SHORT  ||
           t == Type::INT  ||
           t == Type::LONG  ||
           t == Type::UCHAR   ||
           t == Type::USHORT  ||
           t == Type::UINT  ||
           t == Type::ULONG;
}

bool IsFloatingPoint( Type t )
{
    t = GetScalarType( t );
    return t == Type::DOUBLE ||
           t == Type::FLOAT;
}

bool IsSigned( Type t )
{
    t = GetScalarType( t );
    return t == Type::LONG ||
           t == Type::INT ||
           t == Type::SHORT ||
           t == Type::CHAR;
}

bool IsVectorType( Type t )
{
#define EQUALS_VECTOR_TYPES(type) \
    t == type##2 || \
    t == type##3 || \
    t == type##4
    return EQUALS_VECTOR_TYPES(Type::BOOL) ||
           EQUALS_VECTOR_TYPES(Type::CHAR) ||
           EQUALS_VECTOR_TYPES(Type::SHORT) ||
           EQUALS_VECTOR_TYPES(Type::INT) ||
           EQUALS_VECTOR_TYPES(Type::LONG) ||
           EQUALS_VECTOR_TYPES(Type::UCHAR) ||
           EQUALS_VECTOR_TYPES(Type::USHORT) ||
           EQUALS_VECTOR_TYPES(Type::UINT) ||
           EQUALS_VECTOR_TYPES(Type::ULONG) ||
           EQUALS_VECTOR_TYPES(Type::FLOAT) ||
           EQUALS_VECTOR_TYPES(Type::DOUBLE);
}

bool IsScalarType( Type t )
{
    return GetNumElementsInType( t ) == 1 && !IsVectorType( t );
}

unsigned GetNumElementsInType( Type t )
{
#define MATCH_N(n) \
    case Type::BOOL##n: \
    case Type::CHAR##n: \
    case Type::SHORT##n: \
    case Type::INT##n: \
    case Type::LONG##n: \
    case Type::UCHAR##n: \
    case Type::USHORT##n: \
    case Type::UINT##n: \
    case Type::ULONG##n: \
    case Type::FLOAT##n: \
    case Type::DOUBLE##n:

    switch( t )
    {
    MATCH_N(4)
        return 4;
    MATCH_N(3)
        return 3;
    MATCH_N(2)
        return 2;
    MATCH_N( )
    case Type::STRING:
        return 1;
    case Type::VOID:
        return 0;
    default:
        assert( false &&
                "Trying to get the number of elements in an unhandled type" );
    }
    return 0;

#undef MATCH_N
}

std::size_t SizeOf( Type t )
{

#define TYPE_SIZE_N(type, size) \
    case Type::type: return (size); \
    case Type::type##2: return (size) * 2; \
    case Type::type##3: return (size) * 3; \
    case Type::type##4: return (size) * 4

    switch( t )
    {
    TYPE_SIZE_N(BOOL,   1);
    TYPE_SIZE_N(CHAR,   1);
    TYPE_SIZE_N(SHORT,  2);
    TYPE_SIZE_N(INT,    4);
    TYPE_SIZE_N(LONG,   8);
    TYPE_SIZE_N(UCHAR,  1);
    TYPE_SIZE_N(USHORT, 2);
    TYPE_SIZE_N(UINT,   4);
    TYPE_SIZE_N(ULONG,  8);
    TYPE_SIZE_N(FLOAT,  4);
    TYPE_SIZE_N(DOUBLE, 8);

    default:
        assert( false && "Trying to get the size of an unhandled type" );
    }
    return 0;

#undef TYPE_SIZE_N
}

Type MakeUnsigned( Type t )
{
    assert( IsIntegral( t ) && t != Type::BOOL &&
            "Trying to make an invalid type unsigned" );
    switch( t )
    {
    case Type::CHAR:
        return Type::UCHAR;
    case Type::SHORT:
        return Type::USHORT;
    case Type::INT:
        return Type::UINT;
    case Type::LONG:
        return Type::ULONG;
    default:
        // t is already unsigned;
        return t;
    }
}

const std::string& GetTypeString( Type t )
{
#define STR(s) #s
#define TYPE_STRINGS(type, name) \
    { type, STR(name) }, \
    { type##2, STR(name##2) }, \
    { type##3, STR(name##3) }, \
    { type##4, STR(name##4) }

    const static std::map<Type, std::string> string_map =
    {
        TYPE_STRINGS(Type::BOOL,   bool),
        TYPE_STRINGS(Type::CHAR,   char),
        TYPE_STRINGS(Type::SHORT,  short),
        TYPE_STRINGS(Type::INT,    int),
        TYPE_STRINGS(Type::LONG,   long),
        TYPE_STRINGS(Type::UCHAR,  uchar),
        TYPE_STRINGS(Type::USHORT, ushort),
        TYPE_STRINGS(Type::UINT,   uint),
        TYPE_STRINGS(Type::ULONG,  ulong),
        TYPE_STRINGS(Type::FLOAT,  float),
        TYPE_STRINGS(Type::DOUBLE, double),
        { Type::UNKNOWN,      "unknown type" },
        { Type::STRING,       "string" },
        { Type::ARRAY,        "array" },
        { Type::VOID,         "void" }
    };

#undef TYPE_STRINGS
#undef STR

    return string_map.at(t);
}

bool HasGLSLType( Type t )
{
    const static std::set<Type> glsl_types =
    {
        Type::FLOAT,
        Type::FLOAT2,
        Type::FLOAT3,
        Type::FLOAT4,
        Type::UINT,
        Type::INT,
        Type::BOOL,
        Type::VOID,
    };

    return glsl_types.find( t ) != glsl_types.end();
}

const std::string& GetGLSLTypeString( Type t )
{
    const static std::map<Type, std::string> string_map =
    {
        { Type::FLOAT,        "float" },
        { Type::FLOAT2,       "vec2" },
        { Type::FLOAT3,       "vec3" },
        { Type::FLOAT4,       "vec4" },

        { Type::UINT,         "uint" },
        { Type::INT,          "int" },
        { Type::BOOL,         "bool" },
        { Type::VOID,         "void" },
    };

    assert( string_map.find(t) != string_map.end() &&
            "Trying to get a type not supported by glsl" );

    return string_map.at(t);
}

std::string GetGLSLTypeSuffix( Type t )
{
    const static std::map<Type, std::string> suffix_map =
    {
        { Type::FLOAT,        "f" },
        { Type::UINT,         "u" },
    };

    auto s = suffix_map.find(t);

    if( s == suffix_map.end() )
        return "";

    return s->second;
}

} // namespace Compiler
} // namespace JoeLang
