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

#include <string>
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

enum class RuntimeFunction
{
    STRING_EQUAL,
    STRING_NOTEQUAL,
    STRING_CONCAT,
    STRING_DESTROY,
};

class Runtime
{
public:
    Runtime();
    ~Runtime();

    llvm::LLVMContext&  GetLLVMContext();
    llvm::Module*       GetModule();

    llvm::Value*        CreateRuntimeCall( RuntimeFunction function,
                                           std::vector<llvm::Value*> params,
                                           llvm::IRBuilder<>& builder ) const;

    llvm::Type*         GetLLVMType(
                        Type base_type,
                        const std::vector<unsigned>& array_extents = {} ) const;
private:
    enum class ReturnType
    {
        DEFAULT, /// return the type as it is
        POINTER, /// return by hidden first poiner argument
        INTEGER, /// return by casting into an integer
        IGNORE,  /// Ignore this value (used for void)
    };

    enum class ParamType
    {
        DEFAULT, /// Pass this param as it is
        EXPAND,  /// Expand this struct param
        POINTER, /// Pass this param by pointer
        IGNORE,  /// Ignore this value (used for void)
    };

    struct ParamValue
    {
        llvm::Value* value;
        Type         type;
    };

    struct TypeInformation
    {
        ReturnType   returnType;
        ParamType    passType;
    };

    struct FunctionInfo
    {
        const std::string name;
        Type returnType;
        const std::vector<Type> paramTypes;
    };

    llvm::Value*        CreateCall( llvm::Function* function,
                                    Type return_type,
                                    const std::vector<ParamValue>& param_types,
                                    llvm::IRBuilder<>& builder ) const;

    llvm::LLVMContext&  m_LLVMContext;

    llvm::Module*       m_RuntimeModule;

    llvm::StructType*   m_StringType;

    std::map<RuntimeFunction, llvm::Function*> m_Functions;

    const static
    std::map<Type, TypeInformation> s_TypeInformationMap;

    const static
    std::map<RuntimeFunction, FunctionInfo> s_FunctionInfos;
};

} // namespace Compiler
} // namespace JoeLang
