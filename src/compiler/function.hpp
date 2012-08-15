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

#include <compiler/complete_type.hpp>

namespace llvm
{
    class Function;
}

namespace JoeLang
{

namespace Compiler
{
using ArrayExtents = std::vector<unsigned>;
class CodeGenerator;
class CompoundStatement;
using CompoundStatement_up = std::unique_ptr<CompoundStatement>;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

/**
  * \class Function
  * \brief A class to handle functions
  */
class Function
{
public:
    Function( std::string identifier,
              CompleteType base_return_type,
              std::vector<CompleteType> parameters );

    const std::string& GetIdentifier() const;

    const CompleteType& GetReturnType() const;

    const std::vector<CompleteType>& GetParameterTypes() const;

    std::size_t GetNumParams() const;

    llvm::Function* GetLLVMFunction() const;

    /**
      * This asserts that the parameter types match
      */
    void SetParameters( std::vector<Variable_sp> parameters );

    /**
      * This asserts that the function is undefined
      */
    void SetDefinition( CompoundStatement_up definition );

    bool HasDefinition() const;

    /**
      * Generates a string for the function signature
      */
    std::string GetSignatureString() const;

    bool HasSameParameterTypes(
                           const std::vector<CompleteType>& other_types ) const;

    void CodeGenDeclaration( CodeGenerator& code_gen );

    void CodeGenDefinition( CodeGenerator& code_gen );

    void Write( ShaderWriter& shader_writer ) const;
private:
    std::string m_Identifier;
    CompleteType m_ReturnType;
    std::vector<CompleteType> m_ParameterTypes;
    std::vector<Variable_sp>  m_Parameters;

    CompoundStatement_up m_Definition;

    llvm::Function* m_LLVMFunction;
};

} // namespace Compiler
} // namespace JoeLang
