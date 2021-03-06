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

#include <memory>
#include <string>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/semantic.hpp>
#include <compiler/support/generic_value.hpp>

namespace llvm
{
    class Value;
    class Argument;
}

namespace JoeLang
{

enum class Type;

namespace Compiler
{

class CodeGenerator;
class Expression;
using Expression_up = std::unique_ptr<Expression>;

/**
  * \class Variable
  * \brief A class to handle reading from and writing to variables
  */
class Variable
{
public:
    /**
      * This constructor asserts if the varible is const and hasn't been given
      * an initializer, and on a bunch of other things
      */
    Variable( CompleteType type,
              Semantic semantic,
              bool is_const,
              bool is_uniform,
              bool is_varying,
              bool is_in,
              bool is_out,
              bool is_global,
              bool is_parameter,
              GenericValue initializer,
              std::string name );

    void CodeGen( CodeGenerator& code_gen );

    void SetParameterPointer( llvm::Value* parameter_pointer );

    llvm::Value* GetLLVMPointer() const;

    const CompleteType& GetType() const;

    const Semantic& GetSemantic() const;

    const std::string& GetName() const;

    const GenericValue& GetInitializer() const;

    Type GetUnderlyingType() const;

    /** \returns true if this variable is const **/
    bool IsConst() const;

    bool IsVarying() const;

    bool IsUniform() const;

    bool IsIn() const;

    bool IsOut() const;

    bool IsGlobal() const;

    bool IsParameter() const;
private:
    CompleteType m_Type;
    Semantic m_Semantic;
    bool m_IsConst;
    bool m_IsUniform;
    bool m_IsVarying;
    bool m_IsIn;
    bool m_IsOut;
    bool m_IsGlobal;
    bool m_IsParameter;
    GenericValue m_Initializer;
    std::string m_Name;

    /// This holds the pointer to the variable, or the llvm::Value* representing
    /// The argument if it's an argument once it's been codegened
    llvm::Value* m_LLVMPointer = nullptr;
};

} // namespace Compiler
} // namespace JoeLang
