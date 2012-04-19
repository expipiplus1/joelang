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

#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>

#include <engine/types.hpp>

namespace JoeLang
{

Type GetCommonType( Type t1, Type t2 )
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

    if( t1 == Type::STRING )
    {
        if( t2 == Type::STRING )
            return Type::STRING;
        else
            return Type::UNKNOWN_TYPE;
    }

    for( Type t : promotion_ordering )
        if( t1 == t || t2 == t )
            return t;

    assert( false && "Trying to get the common type of unknown types" );
    return Type::UNKNOWN_TYPE;
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
           t == Type::FLOAT;
}

bool IsSigned( Type t )
{
    return t == Type::I64 ||
           t == Type::I32 ||
           t == Type::I16 ||
           t == Type::I8;
}

std::size_t SizeOf( Type t )
{
    if( t == Type::DOUBLE ||
        t == Type::U64 ||
        t == Type::I64 )
        return 8;

    if( t == Type::FLOAT ||
        t == Type::U32 ||
        t == Type::I32 )
        return 4;

    if( t == Type::U16 ||
        t == Type::I16 )
        return 2;

    if( t == Type::U8 ||
        t == Type::I8 ||
        t == Type::BOOL )
        return 1;

    return 0;
}

llvm::Type* GetLLVMType( Type t, llvm::LLVMContext& c )
{
    if( t == Type::DOUBLE )
        return llvm::Type::getDoubleTy( c );
    if( t == Type::FLOAT )
        return llvm::Type::getFloatTy( c );
    if( t == Type::BOOL )
        return llvm::Type::getInt1Ty( c );
    if( IsIntegral( t ) )
        return llvm::Type::getIntNTy( c, SizeOf(t)*8 );
    return nullptr;
}

const std::string& GetTypeString( Type t )
{
    const static std::map<Type, std::string> string_map =
    {
        { Type::UNKNOWN_TYPE, "unknown type" },
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
    };

    return string_map.at(t);
}

} // namespace JoeLang
