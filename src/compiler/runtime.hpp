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

#pragma once

#include <vector>

#include <llvm/IRBuilder.h>

#ifdef __i686
#define ARCH_I686
#elif __x86_64
#define ARCH_X86_64
#else
#error Incompatible Arch
#endif

namespace llvm
{
    class Function;
    class LLVMContext;
    class Module;
    class StructType;
    class Type;
    class Value;
}

namespace JoeLang
{

enum class Type;

namespace Compiler
{

class Runtime
{
public:
    Runtime();
    ~Runtime();

    llvm::LLVMContext&  GetLLVMContext();
    llvm::Module*       GetModule();

    //
    // String functions
    //
    llvm::Value*        CreateStringEqualCall(
                                           llvm::Value* lhs,
                                           llvm::Value* rhs,
                                           llvm::IRBuilder<>& builder) const;
    llvm::Value*        CreateStringNotEqualCall(
                                           llvm::Value* lhs,
                                           llvm::Value* rhs,
                                           llvm::IRBuilder<>& builder) const;
    llvm::Value*        CreateStringConcatCall(
                                           llvm::Value* lhs,
                                           llvm::Value* rhs,
                                           llvm::IRBuilder<>& builder) const;

    llvm::Type*         GetLLVMType(
                        Type base_type,
                        const std::vector<unsigned>& array_extents = {} ) const;
private:
    enum class ReturnType
    {
        DEFAULT, /// return the type as it is
        POINTER, /// return by hidden first poiner argument
        INTEGER, /// return by casting into an integer
    };

    enum class ParamType
    {
        DEFAULT, /// Pass this param as it is
        EXPAND,  /// Expand this struct param
        POINTER, /// Pass this param by pointer
    };

    struct ParamValue
    {
        llvm::Value* value;
        ParamType    param_type;
    };

    llvm::Value*        CreateCall( llvm::Function* function,
                                    ReturnType return_type,
                                    const std::vector<ParamValue>& param_types,
                                    llvm::IRBuilder<>& builder ) const;

    llvm::LLVMContext&  m_LLVMContext;

    llvm::Module*       m_RuntimeModule;

    llvm::StructType*   m_StringType;
    ParamType           m_StringPassType;
    ReturnType          m_StringReturnType;

    llvm::Function*     m_StringEqualFunction;
    llvm::Function*     m_StringNotEqualFunction;
    llvm::Function*     m_StringConcatFunction;
};

} // namespace Compiler
} // namespace JoeLang
