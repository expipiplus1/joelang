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

#include <memory>
#include <string>
#include <vector>

#include <llvm/IR/IRBuilder.h>

#ifdef __i386
#define ARCH_I386
#elif __x86_64
#define ARCH_X86_64
#else
#error Incompatible Arch
#endif

namespace llvm
{
    class ExecutionEngine;
    class Function;
    class FunctionPassManager;
    class LLVMContext;
    class Module;
    class PassManager;
    class StructType;
    class Type;
    class Value;
}

namespace JoeLang
{

class Context;
enum class Type;

namespace Compiler
{

typedef std::vector<unsigned> ArrayExtents;
class CodeGenerator;
class CompleteType;
class Function;
using Function_sp = std::shared_ptr<Function>;

enum class RuntimeFunction
{
    NONE,

    STRING_EQUAL,
    STRING_NOTEQUAL,
    STRING_CONCAT,
    STRING_COPY,
    STRING_DESTROY,

    FLOAT_DOT,
    FLOAT2_DOT,
    FLOAT3_DOT,
    FLOAT4_DOT,

    FLOAT_NORMALIZE,
    FLOAT2_NORMALIZE,
    FLOAT3_NORMALIZE,
    FLOAT4_NORMALIZE,


    FLOAT2x2_FLOAT2_MUL,
    FLOAT2x2_FLOAT2x2_MUL,
    FLOAT2x2_FLOAT3x2_MUL,
    FLOAT2x2_FLOAT4x2_MUL,

    FLOAT2x3_FLOAT2_MUL,
    FLOAT2x3_FLOAT2x2_MUL,
    FLOAT2x3_FLOAT3x2_MUL,
    FLOAT2x3_FLOAT4x2_MUL,

    FLOAT2x4_FLOAT2_MUL,
    FLOAT2x4_FLOAT2x2_MUL,
    FLOAT2x4_FLOAT3x2_MUL,
    FLOAT2x4_FLOAT4x2_MUL,


    FLOAT3x2_FLOAT3_MUL,
    FLOAT3x2_FLOAT2x3_MUL,
    FLOAT3x2_FLOAT3x3_MUL,
    FLOAT3x2_FLOAT4x3_MUL,

    FLOAT3x3_FLOAT3_MUL,
    FLOAT3x3_FLOAT2x3_MUL,
    FLOAT3x3_FLOAT3x3_MUL,
    FLOAT3x3_FLOAT4x3_MUL,

    FLOAT3x4_FLOAT3_MUL,
    FLOAT3x4_FLOAT2x3_MUL,
    FLOAT3x4_FLOAT3x3_MUL,
    FLOAT3x4_FLOAT4x3_MUL,


    FLOAT4x2_FLOAT4_MUL,
    FLOAT4x2_FLOAT2x4_MUL,
    FLOAT4x2_FLOAT3x4_MUL,
    FLOAT4x2_FLOAT4x4_MUL,

    FLOAT4x3_FLOAT4_MUL,
    FLOAT4x3_FLOAT2x4_MUL,
    FLOAT4x3_FLOAT3x4_MUL,
    FLOAT4x3_FLOAT4x4_MUL,

    FLOAT4x4_FLOAT4_MUL,
    FLOAT4x4_FLOAT2x4_MUL,
    FLOAT4x4_FLOAT3x4_MUL,
    FLOAT4x4_FLOAT4x4_MUL,

    FLOAT2_FLOAT2x2_MUL,
    FLOAT2_FLOAT3x2_MUL,
    FLOAT2_FLOAT4x2_MUL,

    FLOAT3_FLOAT2x3_MUL,
    FLOAT3_FLOAT3x3_MUL,
    FLOAT3_FLOAT4x3_MUL,

    FLOAT4_FLOAT2x4_MUL,
    FLOAT4_FLOAT3x4_MUL,
    FLOAT4_FLOAT4x4_MUL,
};

class Runtime
{
public:
    Runtime( const JoeLang::Context& joelang_context );
    ~Runtime();

    const JoeLang::Context& GetJoeLangContext() const;
    llvm::LLVMContext&      GetLLVMContext();
    llvm::ExecutionEngine&  GetExecutionEngine();
    llvm::Module&           GetModule();
    CodeGenerator           CreateCodeGenerator();

    std::vector<Function_sp> GetRuntimeFunctions() const;

    /**
      * Runs some optimizations on the module
      */
    void OptimizeModule();

    llvm::Value*        CreateRuntimeCall( RuntimeFunction function,
                                           std::vector<llvm::Value*> params,
                                           llvm::IRBuilder<>& builder );

    //
    // This will flatten values, and repackage it as a to_type
    //
    llvm::Value*        CreateDeepCopy( const std::vector<llvm::Value*>& values,
                                        llvm::Type* to_type,
                                        llvm::IRBuilder<>& builder );

    llvm::Value*        CreateDeepCopy( llvm::Value* value,
                                        llvm::Type* to_type,
                                        llvm::IRBuilder<>& builder );


    llvm::Type*         GetLLVMType( const CompleteType& type ) const;
    llvm::Type*         GetLLVMType( Type type ) const;

    //
    // The runtime often has slightly different types
    //
    llvm::Type*         GetRuntimeLLVMType( Type type ) const;
    
private:
    enum class ReturnType
    {
        DEFAULT, /// return the type as it is
        POINTER, /// return by hidden first poiner argument
        INTEGER, /// return by casting into an integer
        IGNORE,  /// Ignore this value (used for void)
        STRUCT,  /// Expand this value into a struct, for example float3 becomes
                 /// { <float x 2>, float }
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
        const std::string bitcodeName;
        const std::string runtimeName;
        const Type returnType;
        const std::vector<Type> paramTypes;
    };

    llvm::Value*        CreateCall( llvm::Function* function,
                                    Type return_type,
                                    const std::vector<ParamValue>& param_types,
                                    llvm::IRBuilder<>& builder );

    bool FindRuntimeFunctions();
    bool FindInternalTypes();
    bool FindRuntimeTypes();
    std::vector<std::string> VerifyRuntimeFunctions();

    //
    // This will generate a Function for each of the runtime functions
    //
    bool GenerateFunctionWrappers();
    Function_sp GenerateFunctionWrapper( RuntimeFunction runtime_function );

    void InitializeOptimizers();

    /**
      * Runs some optimizations on the function
      */
    void OptimizeFunction( llvm::Function& function );

    //
    // This will store the value, and load it back as a to_type
    //
    llvm::Value*        CreatePointerCastCopy( llvm::Value* value,
                                               llvm::Type* to_type,
                                               llvm::IRBuilder<>& builder );

    //
    // The JoeLang Context to which this runtime belongs
    //
    const JoeLang::Context& m_JoeLangContext;

    //
    // The LLVMContext which belongs to this JoeLang Context
    //
    llvm::LLVMContext  m_LLVMContext;

    //
    // The llvm module into which we put everything, initialized from runtime.bc
    //
    llvm::Module*       m_Module;

    //
    // The ExecutionEngine which runs the module
    //
    std::unique_ptr<llvm::ExecutionEngine>     m_ExecutionEngine;

    //
    // Optimization
    //
    std::unique_ptr<llvm::FunctionPassManager> m_LLVMFunctionPassManager;
    std::unique_ptr<llvm::PassManager>         m_LLVMModulePassManager;

    //
    // The types as used in the runtime
    //
    std::map<Type, llvm::Type*> m_RuntimeTypeMap;

    //
    // The mapping of joelang types to llvm types
    //
    std::map<Type, llvm::Type*> m_InternalTypeMap;

    //
    // The list of functions in the runtime
    //
    std::map<RuntimeFunction, llvm::Function*> m_Functions;

    //
    // The list of functions and their wrappers
    //
    std::map<RuntimeFunction, Function_sp> m_FunctionWrappers;

    //
    // A list of types and how they are passed to and from functions
    //
    const static
    std::map<Type, TypeInformation> s_TypeInformationMap;

    //
    // A mapping between the runtime functions and information describing them
    //
    const static
    std::map<RuntimeFunction, FunctionInfo> s_FunctionInfos;
};

} // namespace Compiler
} // namespace JoeLang
