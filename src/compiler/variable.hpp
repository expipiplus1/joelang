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
#include <vector>

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
              std::vector<Expression_sp> array_dimension_sizes,
              bool is_const,
              bool is_global,
              std::unique_ptr<Expression> initializer = nullptr );

    void CodeGen( CodeGenerator& code_gen );

    llvm::Value* GetLLVMPointer() const;

    Type GetType() const;

    Type GetUnderlyingType() const;

    const std::vector<Expression_sp>& GetArrayExtents() const;

    /** \returns true if this variable is const **/
    bool IsConst() const;

    const std::unique_ptr<Expression>& GetReadExpression() const;

private:
    Type m_type;
    std::vector<Expression_sp> m_arrayDimensionSizes;
    bool m_isConst;
    bool m_isGlobal;
    std::unique_ptr<Expression> m_initializer;

    // TODO handle non global variables
    llvm::Value* m_llvmPointer = nullptr;
};

} // namespace Compiler
} // namespace JoeLang
