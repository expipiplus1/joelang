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
#include <set>
#include <string>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/semantic.hpp>

namespace llvm
{
    class Function;
}

namespace JoeLang
{

namespace Compiler
{
// todo, write tool to sort these
using ArrayExtents = std::vector<unsigned>;
class CodeGenerator;
class CompoundStatement;
using CompoundStatement_up = std::unique_ptr<CompoundStatement>;
class Function;
using Function_sp = std::shared_ptr<Function>;
class NodeManager;
enum class RuntimeFunction;
class StatementNode; 
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
              Semantic semantic,
              std::vector<CompleteType> parameters );

    Function(std::string identifier,
              CompleteType return_type,
              std::vector<CompleteType> parameter_types,
              llvm::Function* llvm_function,
              RuntimeFunction runtime_function );

    ~Function();
    
    void GenerateCodeDag( NodeManager& node_manager );
    
    const StatementNode& GetCodeDag() const;
    
    void SetCodeDag( const StatementNode& code );

    const std::string& GetIdentifier() const;

    const CompleteType& GetReturnType() const;

    const std::vector<CompleteType>& GetParameterTypes() const;

    std::size_t GetNumParams() const;

    llvm::Function* GetLLVMFunction() const;

    /**
      * Get the parameter variables
      */
    const std::vector<Variable_sp>& GetParameters() const;

    /**
      * This asserts that the parameter types match
      */
    void SetParameters( std::vector<Variable_sp> parameters );

    /**
      * This asserts that the function is undefined
      */
    void SetDefinition( CompoundStatement_up definition );

    bool HasDefinition() const;

    bool HasLLVMFunction() const;

    bool IsRuntimeFunction() const;

    RuntimeFunction GetRuntimeFunction() const;

    /**
      * Returns all the functions called directly by this one.
      * this asserts that we have a definition
      */
    std::set<Function_sp> GetCallees() const;

    /**
      * Returns all functions below this one in the call graph
      */
    std::set<const Function*> GetFunctionDependencies( bool& recursion ) const;

    /**
      * Returns all the variables referenced in this function
      * this asserts that we have a definition
      */
    std::set<Variable_sp> GetVariables() const;

    /**
      * Returns all the variables written to in this function
      * this asserts that we have a definition
      */
    std::set<Variable_sp> GetWrittenToVariables() const;

    /**
      * Returns this function's semantic
      */
    const Semantic& GetSemantic() const;

    /**
      * Generates a string for the function signature
      */
    std::string GetSignatureString() const;

    bool HasSameParameterTypes(
                           const std::vector<CompleteType>& other_types ) const;

    void CodeGenDeclaration( CodeGenerator& code_gen );

    void CodeGenDefinition( CodeGenerator& code_gen );

    void WriteDefinition( ShaderWriter& shader_writer ) const;

    void WriteDeclaration( ShaderWriter& shader_writer ) const;
private:
    /** This will write the declaration sans the ';' **/
    void WriteHeader( ShaderWriter& shader_writer ) const;

    std::string               m_Identifier;
    CompleteType              m_ReturnType;
    Semantic                  m_Semantic;
    std::vector<CompleteType> m_ParameterTypes;
    std::vector<Variable_sp>  m_Parameters;

    CompoundStatement_up      m_Definition;

    llvm::Function*           m_LLVMFunction;

    RuntimeFunction           m_RuntimeFunction;
    
    const StatementNode*      m_CodeDag;
};

} // namespace Compiler
} // namespace JoeLang
