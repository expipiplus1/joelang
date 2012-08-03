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

#include <compiler/generic_value.hpp>

namespace llvm
{
    class Value;
}

namespace JoeLang
{

enum class Type;

namespace Compiler
{

class CodeGenerator;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
typedef std::shared_ptr<Expression> Expression_sp;

/**
  * \class Variable
  * \brief A class to handle reading from and writing to variables
  */
class Variable
{
public:
    /**
      * This constructor asserts if the varible is const and hasn't been given
      * an initializer
      */
    Variable( Type type,
              ArrayExtents array_extents,
              bool is_const,
              bool is_global = false,
              bool is_parameter = false,
              GenericValue initializer = GenericValue(),
              std::string name = "" );

    void CodeGen( CodeGenerator& code_gen );

    llvm::Value* GetLLVMPointer() const;

    Type GetType() const;

    Type GetUnderlyingType() const;

    const ArrayExtents& GetArrayExtents() const;

    /** \returns true if this variable is const **/
    bool IsConst() const;

private:
    Type m_Type;
    ArrayExtents m_ArrayExtents;
    bool m_IsConst;
    bool m_IsGlobal;
    bool m_IsParameter;
    GenericValue m_Initializer;
    std::string m_Name;

    /// TODO handle non global variables
    /// This holds the pointer to the variable, or the llvm::Value* representing
    /// The argument if it's an argument once it's been codegened
    llvm::Value* m_LLVMPointer = nullptr;
};

} // namespace Compiler
} // namespace JoeLang
