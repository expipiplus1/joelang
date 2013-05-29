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

#include <array>
#include <cassert>
#include <map>
#include <string>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
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

    //
    // There is no common type between arrays of different sizes
    //
    if( t1.GetArrayExtents() != t2.GetArrayExtents() )
        return CompleteType();

    //
    // If they are array types they have to have identical types (no implicit
    // conversion of arrays)
    //
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

    Type t1_scalar_type = t1.GetElementType();
    Type t2_scalar_type = t2.GetElementType();
    Type common_scalar_type = Type::UNKNOWN;

    for( Type t : promotion_ordering )
        if( t1_scalar_type == t || t2_scalar_type == t )
        {
            common_scalar_type = t;
            break;
        }


    assert( ( t1.IsScalarType() || t1.IsVectorType() || t1.IsMatrixType() ) &&
            "No structs or array's here, please" );
    assert( ( t2.IsScalarType() || t2.IsVectorType() || t2.IsMatrixType() ) &&
            "No structs or array's here, please" );

    //
    // If both are scalars, return the common scalar type
    //
    if( t1.IsScalarType() && t2.IsScalarType() )
        return CompleteType( common_scalar_type );

    //
    // If only one type is a scalar
    //
    if( t1.IsScalarType() ^ t2.IsScalarType() )
    {
        //
        // we need to smear the scalar to the other type
        //
        if( t1.IsVectorType() || t2.IsVectorType() )
        {
            unsigned result_size;
            result_size = JoeMath::Max( t1.GetNumElements(),
                                        t2.GetNumElements() );
            return CompleteType( GetVectorType( common_scalar_type,
                                                result_size ) );
        }

        if( t1.IsMatrixType() || t2.IsMatrixType() )
        {
            const CompleteType& m = t1.IsMatrixType() ? t1 : t2;
            return CompleteType( GetMatrixType( common_scalar_type,
                                                m.GetNumMatrixColumns(),
                                                m.GetNumMatrixRows() ) );
        }
    }

    //
    // If only one type is a vector
    //
    if( t1.IsVectorType() ^ t2.IsVectorType() )
    {
        //
        // There is no common type between vectors and matrices
        //
        return CompleteType();
    }

    assert( t1.IsVectorType() == t2.IsVectorType() &&
            t1.IsMatrixType() == t2.IsMatrixType() &&
            "t1 and t2 have to be the same class of type" );

    if( t1.IsMatrixType() )
    {
        //
        // Only if they're the same size
        //
        unsigned t1_rows    = t1.GetNumMatrixRows();
        unsigned t1_columns = t1.GetNumMatrixColumns();
        unsigned t2_rows    = t2.GetNumMatrixRows();
        unsigned t2_columns = t2.GetNumMatrixColumns();

        if( t1_rows == t2_rows && t1_columns == t2_columns )
            return CompleteType( GetMatrixType( common_scalar_type,
                                                t1_columns,
                                                t1_rows ) );
        return CompleteType();
    }

    if( t1.IsVectorType() )
    {
        //
        // Vectors can be implicitly cast to a smaller vector type, this will
        // issue a warning
        //
        unsigned size = JoeMath::Min( t1.GetNumElements(),
                                      t2.GetNumElements() );

        return CompleteType( GetVectorType( common_scalar_type,
                                            size ) );
    }

    assert( false && "How did we get here?" );
    return CompleteType();
}

Type GetMatrixType( Type base, unsigned columns, unsigned rows )
{
#define CREATE_MATRIX_MAPPING(type) \
    {{ type, type##2x2, type##2x3, type##2x4, \
             type##3x2, type##3x3, type##3x4, \
             type##4x2, type##4x3, type##4x4 }}

    const static std::array< std::array< Type, 10 >, 11 > t =
    {{
        CREATE_MATRIX_MAPPING(Type::BOOL),
        CREATE_MATRIX_MAPPING(Type::CHAR),
        CREATE_MATRIX_MAPPING(Type::INT),
        CREATE_MATRIX_MAPPING(Type::SHORT),
        CREATE_MATRIX_MAPPING(Type::LONG),
        CREATE_MATRIX_MAPPING(Type::UCHAR),
        CREATE_MATRIX_MAPPING(Type::UINT),
        CREATE_MATRIX_MAPPING(Type::USHORT),
        CREATE_MATRIX_MAPPING(Type::ULONG),
        CREATE_MATRIX_MAPPING(Type::FLOAT),
        CREATE_MATRIX_MAPPING(Type::DOUBLE)
    }};
#undef CREATE_MATRIX_MAPPING

    assert( columns > 1  && "Trying to make a too small matrix" );
    assert( columns <= 4 && "Trying to make a too big matrix" );
    assert( rows    > 1  && "Trying to make a too small matrix" );
    assert( rows    <= 4 && "Trying to make a too big matrix" );

    unsigned index = 1 + (columns - 2) * 3 + (rows - 2);

    for( auto& a : t )
    {
        if( a[0] == base )
            return a[index];
    }

    assert( false && "Trying to make an unhandled vector type" );
    return Type::UNKNOWN;
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
    case type##2x2: \
    case type##2x3: \
    case type##2x4: \
    case type##3x2: \
    case type##3x3: \
    case type##3x4: \
    case type##4x2: \
    case type##4x3: \
    case type##4x4: \
        return type;

    switch( t )
    {
    RETURN_BASE(Type::BOOL)
    RETURN_BASE(Type::CHAR)
    RETURN_BASE(Type::SHORT)
    RETURN_BASE(Type::INT)
    RETURN_BASE(Type::LONG)
    RETURN_BASE(Type::UCHAR)
    RETURN_BASE(Type::USHORT)
    RETURN_BASE(Type::UINT)
    RETURN_BASE(Type::ULONG)
    RETURN_BASE(Type::FLOAT)
    RETURN_BASE(Type::DOUBLE)
    case Type::STRING:
        return Type::STRING;
    default:
        assert( false && "Trying to get the scalar type of an unknown type" );
        return Type::UNKNOWN;
    }

#undef RETURN_BASE
}

Type GetMatrixColumnType( Type t )
{
    assert( IsMatrixType(t) &&
            "Trying to get the element type of a non-matrix type" );

#define RETURN_ELEMENT(type) \
    case type##2x2: \
    case type##2x3: \
    case type##2x4: \
    case type##3x2: \
    case type##3x3: \
    case type##3x4: \
    case type##4x2: \
    case type##4x3: \
    case type##4x4: \
        return GetVectorType( type, GetNumRowsInType( t ) );

    switch( t )
    {
    RETURN_ELEMENT(Type::BOOL);
    RETURN_ELEMENT(Type::CHAR);
    RETURN_ELEMENT(Type::SHORT);
    RETURN_ELEMENT(Type::INT);
    RETURN_ELEMENT(Type::LONG);
    RETURN_ELEMENT(Type::UCHAR);
    RETURN_ELEMENT(Type::USHORT);
    RETURN_ELEMENT(Type::UINT);
    RETURN_ELEMENT(Type::ULONG);
    RETURN_ELEMENT(Type::FLOAT);
    RETURN_ELEMENT(Type::DOUBLE);
    default:
        assert( false &&
                "Trying to get the element type of an unhandled type" );
        return Type::UNKNOWN;
    }

#undef RETURN_ELEMENT
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

bool IsMatrixType( Type t )
{
#define EQUALS_MATRIX_TYPES(type) \
    t == type##2x2 || \
    t == type##2x3 || \
    t == type##2x4 || \
    t == type##3x2 || \
    t == type##3x3 || \
    t == type##3x4 || \
    t == type##4x2 || \
    t == type##4x3 || \
    t == type##4x4

    return EQUALS_MATRIX_TYPES(Type::BOOL) ||
           EQUALS_MATRIX_TYPES(Type::CHAR) ||
           EQUALS_MATRIX_TYPES(Type::SHORT) ||
           EQUALS_MATRIX_TYPES(Type::INT) ||
           EQUALS_MATRIX_TYPES(Type::LONG) ||
           EQUALS_MATRIX_TYPES(Type::UCHAR) ||
           EQUALS_MATRIX_TYPES(Type::USHORT) ||
           EQUALS_MATRIX_TYPES(Type::UINT) ||
           EQUALS_MATRIX_TYPES(Type::ULONG) ||
           EQUALS_MATRIX_TYPES(Type::FLOAT) ||
           EQUALS_MATRIX_TYPES(Type::DOUBLE);

#undef EQUALS_MATRIX_TYPES
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

#undef EQUALS_VECTOR_TYPES
}

bool IsScalarType( Type t )
{
    return GetNumElementsInType( t ) == 1 &&
           !IsVectorType( t ) &&
           !IsMatrixType( t );
}

unsigned GetNumElementsInType( Type t )
{
    if( IsMatrixType( t ) )
        return GetNumColumnsInType( t ) * GetNumColumnsInType( t );

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

unsigned GetNumRowsInType( Type t )
{
    assert( IsMatrixType(t) &&
            "Trying to get the number of rows of a non-matrix type" );

#define RETURN_ROWS(type) \
    case type##2x2: \
    case type##3x2: \
    case type##4x2: \
        return 2; \
    case type##2x3: \
    case type##3x3: \
    case type##4x3: \
        return 3; \
    case type##2x4: \
    case type##3x4: \
    case type##4x4: \
        return 4;

    switch( t )
    {
    RETURN_ROWS(Type::BOOL);
    RETURN_ROWS(Type::CHAR);
    RETURN_ROWS(Type::SHORT);
    RETURN_ROWS(Type::INT);
    RETURN_ROWS(Type::LONG);
    RETURN_ROWS(Type::UCHAR);
    RETURN_ROWS(Type::USHORT);
    RETURN_ROWS(Type::UINT);
    RETURN_ROWS(Type::ULONG);
    RETURN_ROWS(Type::FLOAT);
    RETURN_ROWS(Type::DOUBLE);
    default:
        assert( false &&
                "Trying to get the number of rows of an unhandled type" );
        return 0;
    }

#undef RETURN_ROWS
}

unsigned GetNumColumnsInType( Type t )
{
    assert( IsMatrixType(t) &&
            "Trying to get the number of columns of a non-matrix type" );

#define RETURN_COLUMNS(type) \
    case type##2x2: \
    case type##2x3: \
    case type##2x4: \
        return 2; \
    case type##3x2: \
    case type##3x3: \
    case type##3x4: \
        return 3; \
    case type##4x2: \
    case type##4x3: \
    case type##4x4: \
        return 4;

    switch( t )
    {
    RETURN_COLUMNS(Type::BOOL);
    RETURN_COLUMNS(Type::CHAR);
    RETURN_COLUMNS(Type::SHORT);
    RETURN_COLUMNS(Type::INT);
    RETURN_COLUMNS(Type::LONG);
    RETURN_COLUMNS(Type::UCHAR);
    RETURN_COLUMNS(Type::USHORT);
    RETURN_COLUMNS(Type::UINT);
    RETURN_COLUMNS(Type::ULONG);
    RETURN_COLUMNS(Type::FLOAT);
    RETURN_COLUMNS(Type::DOUBLE);
    default:
        assert( false &&
                "Trying to get the number of columns of an unhandled type" );
        return 0;
    }

#undef RETURN_COLUMNS
}

std::size_t SizeOf( Type t )
{

#define TYPE_SIZE_N(type, size) \
    case Type::type: return (size); \
    case Type::type##2: return (size) * 2; \
    case Type::type##3: return (size) * 3; \
    case Type::type##4: return (size) * 4; \
    case Type::type##2x2: return (size) * 2*2; \
    case Type::type##2x3: return (size) * 2*3; \
    case Type::type##2x4: return (size) * 2*4; \
    case Type::type##3x2: return (size) * 3*2; \
    case Type::type##3x3: return (size) * 3*3; \
    case Type::type##3x4: return (size) * 3*4; \
    case Type::type##4x2: return (size) * 4*2; \
    case Type::type##4x3: return (size) * 4*3; \
    case Type::type##4x4: return (size) * 4*4

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
    { type##4, STR(name##4) }, \
    { type##2x2, STR(name##2x2) }, \
    { type##2x3, STR(name##2x3) }, \
    { type##2x4, STR(name##2x4) }, \
    { type##3x2, STR(name##3x2) }, \
    { type##3x3, STR(name##3x3) }, \
    { type##3x4, STR(name##3x4) }, \
    { type##4x2, STR(name##4x2) }, \
    { type##4x3, STR(name##4x3) }, \
    { type##4x4, STR(name##4x4) }

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
        Type::FLOAT2x2,
        Type::FLOAT2x3,
        Type::FLOAT2x4,
        Type::FLOAT3x2,
        Type::FLOAT3x3,
        Type::FLOAT3x4,
        Type::FLOAT4x2,
        Type::FLOAT4x3,
        Type::FLOAT4x4,
        Type::UINT,
        Type::UINT2,
        Type::UINT3,
        Type::UINT4,
        Type::INT,
        Type::INT2,
        Type::INT3,
        Type::INT4,
        Type::BOOL,
        Type::BOOL2,
        Type::BOOL3,
        Type::BOOL4,
        
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

        { Type::FLOAT2x2,     "mat2x2" },
        { Type::FLOAT2x3,     "mat2x3" },
        { Type::FLOAT2x4,     "mat2x4" },
        { Type::FLOAT3x2,     "mat3x2" },
        { Type::FLOAT3x3,     "mat3x3" },
        { Type::FLOAT3x4,     "mat3x4" },
        { Type::FLOAT4x2,     "mat4x2" },
        { Type::FLOAT4x3,     "mat4x3" },
        { Type::FLOAT4x4,     "mat4x4" },

        { Type::UINT,         "uint" },
        { Type::UINT2,        "uvec2" },
        { Type::UINT3,        "uvec3" },
        { Type::UINT4,        "uvec4" },
        
        { Type::INT,         "int" },
        { Type::INT2,        "ivec2" },
        { Type::INT3,        "ivec3" },
        { Type::INT4,        "ivec4" },
        
        { Type::BOOL,         "bool" },
        { Type::BOOL2,        "bvec2" },
        { Type::BOOL3,        "bvec3" },
        { Type::BOOL4,        "bvec4" },
        
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
