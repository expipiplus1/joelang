/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

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

#include "code_generator.hpp"

#include <cassert>
#include <string>

#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/Function.h>

#include <runtime/types.hpp>

namespace JoeLang
{
namespace Compiler
{

template<typename T>
struct HasSimpleReturn : std::false_type
{};

template<> struct HasSimpleReturn<jl_bool> : std::true_type {};

template<typename T>
std::function<T()> CodeGenerator::WrapExpression(
                                                const Expression& expression,
                                                llvm::Function*& function )
{
    function = WrapExpressionCommon( expression, HasSimpleReturn<T>::value );

    void* function_ptr = m_ExecutionEngine.getPointerToFunction(function);

    if( HasSimpleReturn<T>::value )
    {
        return std::function<T()>( reinterpret_cast<T(*)()>( function_ptr ) );
    }
    else
    {
        return [function_ptr]() -> T
        {
            T t[1];
            reinterpret_cast<void(*)(T*)>( function_ptr )(t);
            return t[0];
        };
    }
}

std::function<std::string()> CodeGenerator::WrapStringExpression(
                                                const Expression& expression,
                                                llvm::Function*& function )
{
    function = WrapExpressionCommon( expression, true );

    void* function_ptr = m_ExecutionEngine.getPointerToFunction(function);

    // Cast from string to std::string and destroy original
    const auto ToString = [function_ptr]()
    {
        jl_string s = reinterpret_cast<jl_string(*)()>(function_ptr)();

        std::string ret( reinterpret_cast<const char*>(s.data), s.size);
        delete[] s.data;
        return ret;
    };
    return ToString;
}

#define INSTANTIATE_WRAPPER( type ) \
template std::function<type()> CodeGenerator::WrapExpression<type>( \
                                                            const Expression&, \
                                                            llvm::Function*& );

#define INSTANTIATE_WRAPPER_N( type ) \
    INSTANTIATE_WRAPPER( type ) \
    INSTANTIATE_WRAPPER( type##2 ) \
    INSTANTIATE_WRAPPER( type##3 ) \
    INSTANTIATE_WRAPPER( type##4 ) \
    INSTANTIATE_WRAPPER( type##2x2 ) \
    INSTANTIATE_WRAPPER( type##2x3 ) \
    INSTANTIATE_WRAPPER( type##2x4 ) \
    INSTANTIATE_WRAPPER( type##3x2 ) \
    INSTANTIATE_WRAPPER( type##3x3 ) \
    INSTANTIATE_WRAPPER( type##3x4 ) \
    INSTANTIATE_WRAPPER( type##4x2 ) \
    INSTANTIATE_WRAPPER( type##4x3 ) \
    INSTANTIATE_WRAPPER( type##4x4 )

INSTANTIATE_WRAPPER_N( jl_bool )
INSTANTIATE_WRAPPER_N( jl_char )
INSTANTIATE_WRAPPER_N( jl_short )
INSTANTIATE_WRAPPER_N( jl_int )
INSTANTIATE_WRAPPER_N( jl_long )
INSTANTIATE_WRAPPER_N( jl_uchar )
INSTANTIATE_WRAPPER_N( jl_ushort )
INSTANTIATE_WRAPPER_N( jl_uint )
INSTANTIATE_WRAPPER_N( jl_ulong )
INSTANTIATE_WRAPPER_N( jl_float )
INSTANTIATE_WRAPPER_N( jl_double )

} // namespace Compiler
} // namespace JoeLang

