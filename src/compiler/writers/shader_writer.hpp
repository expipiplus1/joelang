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
#include <sstream>
#include <string>
#include <type_traits>
#include <vector>

namespace JoeLang
{

class Context;

namespace Compiler
{

class CompleteType;
class EntryFunction;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class GenericValue;
class Function;
using Function_sp = std::shared_ptr<Function>;
class PostfixOperator;
enum class RuntimeFunction;
class Statement;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

enum class IdentifierType
{
    VARIABLE,
    IN_VARYING,
    OUT_VARYING,
    UNIFORM,
    FUNCTION
};

/**
  * \class ShaderWriter
  * \brief A class to walk over an ast and generate a glsl shader
  */
class ShaderWriter
{
public:
    explicit
    ShaderWriter( const Context& context );

    std::string GenerateGLSL( const EntryFunction& entry_function );

    void WriteFunction( const Function& function );

    ShaderWriter& operator<<( const CompleteType& value );
    ShaderWriter& operator<<( const Expression& value );
    ShaderWriter& operator<<( const Statement& value );
    ShaderWriter& operator<<( const GenericValue& value );

    template<typename T>
    ShaderWriter& operator<<( const T& value );

    void PushIndentation();
    /** This asserts that m_Indentation is > 0 **/
    void PopIndentation();

    /** Writes '\n' to the shader **/
    void NewLine( unsigned num_lines = 1 );

    /** Records an error while writing the shader **/
    void Error( const std::string& error_string = "" );

    /** Records an warning while writing the shader **/
    void Warning( const std::string& warning_string );

    /**
      * This will mangle an identifier to ensure that there are no conflicts
      * Variables are prefixed  with "_"
      * In varyings are prefixed with "i_"
      * Out varyings are prefixed with "o_"
      * Functions are prefixed with "f_"
      * \param identifier
      *   The identifier to mangle
      * \param identifier_type
      *   What kind of identifier this is
      * \returns the mangled identifier
      */
    static
    std::string Mangle( const std::string& identifier,
                        IdentifierType identifier_type );

    /**
     * This will mangle a variables name according to what it is
     * @param v
     *   The variable to mangle
     * @returns the mangled identifier of the variable
     */
    static
    std::string MangleVariable( const Variable& v );

    /**
     * This will write a variable name with the correct prefix
     */
    void WriteVariableName( const Variable_sp v );

    void WriteRuntimeFunctionCall(
                                  RuntimeFunction runtime_function,
                                  const std::vector<Expression_up>& arguments );
private:
    void GenerateShader( const EntryFunction& entry_function );

    /** Writes #version 150 to the shader **/
    void WriteGLSLVersion();

    /**
      * Writes all the varyings which are input to the shader
      * \returns the set of variables written
      **/
    std::set<Variable_sp> WriteInputVaryings(
                                       const EntryFunction& entry_function,
                                       const std::set<Variable_sp>& variables );

    /**
      * Writes all the varyings which are output from the shader
      * \returns the set of variables written
      **/
    std::set<Variable_sp> WriteOutputVaryings(
                                       const EntryFunction& entry_function,
                                       const std::set<Variable_sp>& variables );

    /**
      * Writes all the global variables used by the shader
      * \returns the set of variables written
      **/
    std::set<Variable_sp> WriteGlobalVariables(
                                       const std::set<Variable_sp>& variables );

    /** Writes the declaration for a particular variable **/
    void WriteVariableDeclaration( Variable_sp variable );

    /** Writes all the function declarations **/
    void WriteFunctionDeclarations( const std::set<Function_sp> functions );

    /** Writes all the function definitions **/
    void WriteFunctionDefinitions( const std::set<Function_sp> functions );

    /** Writes main(){...} **/
    void WriteMainFunction( const EntryFunction& entry_function,
                            const std::set<Variable_sp>& input_variables,
                            const std::set<Variable_sp>& output_variables,
                            const std::set<Variable_sp>& global_variables );

    /**
      * Gather all the variables used by any of the functions
      */
    static
    std::set<Variable_sp> GatherVariables(
                                       const std::set<Function_sp>& functions,
                                       const EntryFunction& entry_function );

    /**
      * Gather all the variables written to by any of the functions
      */
    static
    std::set<Variable_sp> GatherWrittenToVariables(
                                       const std::set<Function_sp>& functions );

    /**
      * Gather all the variables used in the parameters for the entry_function
      */
    static
    std::set<Variable_sp> GatherParameterVariables( 
                                          const EntryFunction& entry_function );

    /**
      * The context that this belongs to
      */
    const Context& m_Context;

    /**
      * The shader under construction
      */
    std::stringstream m_Shader;

    /**
      * The current indentation
      */
    unsigned m_Indentation = 0;

    /**
      * The set of variables which are written to in the shader being compiled
      */
    std::set<Variable_sp> m_WrittenToVariables;

    /**
      * This is false if Error has been called
      */
    bool m_Good = true;

    const static std::string s_GLSLVersion;
};

} // namespace Compiler
} // namespace JoeLang

#include "shader_writer-inl.hpp"
