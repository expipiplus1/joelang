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

#include <engine/types.hpp>

namespace llvm
{
    class Type;
    class LLVMContext;
}

namespace JoeLang
{

/**
  * This returns the type according to the integer promotion rules
  *
  *
  */
Type GetCommonType( Type t1, Type t2 );

bool IsFloatingPoint( Type t );

bool IsIntegral( Type t );

bool IsSigned( Type t );

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

/**
  * This asserts if one tries to get the llvm type of an invalid Type
  */
llvm::Type* GetLLVMType( Type t, llvm::LLVMContext& c );

const std::string& GetTypeString( Type t );

} // namespace JoeLang
