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

#include <map>
#include <string>
#include <vector>

#include <compiler/complete_type.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <engine/types.hpp>

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
        Type::U64,
        Type::I64,
        Type::U32,
        Type::I32,
        Type::U16,
        Type::I16,
        Type::U8,
        Type::I8,
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

    // Can't convert a vector into a scalar
    if( IsVectorType( t1.GetBaseType() ) != IsVectorType( t2.GetBaseType() ) )
        return CompleteType();

    // Vectors have to be the same size
    if( GetNumElementsInType( t1.GetBaseType() ) !=
                                      GetNumElementsInType( t2.GetBaseType() ) )
        return CompleteType();

    if( t1.GetBaseType() == Type::STRING )
    {
        if( t2.GetBaseType() == Type::STRING )
            return t1;
        else
            return CompleteType();
    }

    Type t1_scalar_type = GetScalarType( t1.GetBaseType() );
    Type t2_scalar_type = GetScalarType( t2.GetBaseType() );
    Type common_scalar_type = Type::UNKNOWN;

    for( Type t : promotion_ordering )
        if( t1_scalar_type == t || t2_scalar_type == t )
        {
            common_scalar_type = t;
            break;
        }

    if( IsScalarType( t1.GetBaseType() )  )
        return CompleteType( common_scalar_type );

    return CompleteType( GetVectorType( common_scalar_type,
                                         GetVectorSize( t1.GetBaseType() ) ) );
}

Type GetVectorType( Type base, unsigned size )
{
    //TODO do this in a better way
    switch( size )
    {
    case 4:
        switch( base )
        {
        case Type::FLOAT:
            return Type::FLOAT4;
        default:
            assert( false &&
                    "Trying to get the vector type of an unhandled base" );
        }
    default:
        assert( false && "Trying to get the vector type of an unhandled size" );
    }
    return Type::UNKNOWN;
}

Type GetScalarType( Type t )
{
    switch( t )
    {
    case Type::BOOL:
        return Type::BOOL;
    case Type::I8:
        return Type::I8;
    case Type::I16:
        return Type::I16;
    case Type::I32:
        return Type::I32;
    case Type::I64:
        return Type::I64;
    case Type::U8:
        return Type::U8;
    case Type::U16:
        return Type::U16;
    case Type::U32:
        return Type::U32;
    case Type::U64:
        return Type::U64;
    case Type::FLOAT:
    case Type::FLOAT4:
        return Type::FLOAT;
    case Type::DOUBLE:
        return Type::DOUBLE;
    case Type::STRING:
        return Type::STRING;
    default:
        return Type::UNKNOWN;
    }
}

bool IsIntegral( Type t )
{
    return t == Type::BOOL ||
           t == Type::I8   ||
           t == Type::I16  ||
           t == Type::I32  ||
           t == Type::I64  ||
           t == Type::U8   ||
           t == Type::U16  ||
           t == Type::U32  ||
           t == Type::U64;
}

bool IsFloatingPoint( Type t )
{
    return t == Type::DOUBLE ||
           t == Type::FLOAT ||
           t == Type::FLOAT4;
}

bool IsSigned( Type t )
{
    return t == Type::I64 ||
           t == Type::I32 ||
           t == Type::I16 ||
           t == Type::I8;
}

bool IsVectorType( Type t )
{
    return t == Type::FLOAT4;
}

bool IsScalarType( Type t )
{
    return GetNumElementsInType( t ) == 1 && !IsVectorType( t );
}

unsigned GetVectorSize( Type t )
{
    switch( t )
    {
    case Type::FLOAT4:
        return 4;
    default:
        assert( false && "Trying to get the vector size of a non-vector type" );
    }
    return 0;
}

Type GetElementType( Type t )
{
    assert( t != Type::ARRAY && "Trying to get the base type of Type::ARRAY" );

    switch( t )
    {
    case Type::FLOAT4:
        return Type::FLOAT;
    default:
        return t;
    }
}

unsigned GetNumElementsInType( Type t )
{
    switch( t )
    {
    case Type::FLOAT4:
        return 4;
    case Type::U64:
    case Type::I64:
    case Type::U32:
    case Type::I32:
    case Type::U16:
    case Type::I16:
    case Type::U8:
    case Type::I8:
    case Type::BOOL:
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::STRING:
        return 1;
    default:
        assert( false &&
                "Trying to get the number of elements in an unhandled type" );
    }
    return 0;
}

std::size_t SizeOf( Type t )
{
    switch( t )
    {
    case Type::FLOAT4:
        return 16;
    case Type::DOUBLE:
    case Type::U64:
    case Type::I64:
        return 8;
    case Type::FLOAT:
    case Type::U32:
    case Type::I32:
        return 4;
    case Type::U16:
    case Type::I16:
        return 2;
    case Type::U8:
    case Type::I8:
    case Type::BOOL:
        return 1;
    default:
        assert( false && "Trying to get the size of an unhandled type" );
    }
    return 0;
}

Type MakeUnsigned( Type t )
{
    assert( IsIntegral( t ) && t != Type::BOOL &&
            "Trying to make an invalid type unsigned" );
    switch( t )
    {
    case Type::I8:
        return Type::U8;
    case Type::I16:
        return Type::U16;
    case Type::I32:
        return Type::U32;
    case Type::I64:
        return Type::U64;
    default:
        // t is already unsigned;
        return t;
    }
}

const std::string& GetTypeString( Type t )
{
    const static std::map<Type, std::string> string_map =
    {
        { Type::UNKNOWN, "unknown type" },
        { Type::DOUBLE,       "double" },
        { Type::FLOAT,        "float" },
        { Type::U64,          "u64" },
        { Type::I64,          "i64" },
        { Type::U32,          "u32" },
        { Type::I32,          "i32" },
        { Type::U16,          "u16" },
        { Type::I16,          "i16" },
        { Type::U8,           "u8" },
        { Type::I8,           "i8" },
        { Type::BOOL,         "bool" },
        { Type::STRING,       "string" },
        { Type::ARRAY,        "array" },
    };

    return string_map.at(t);
}

} // namespace Compiler
} // namespace JoeLang