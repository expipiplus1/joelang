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
#include <sstream>
#include <type_traits>

namespace JoeLang
{

class Context;

namespace Compiler
{

class CompleteType;
class EntryFunction;
class Expression;
class Function;
class PostfixOperator;
class Statement;

enum class IdentifierType
{
    VARIABLE,
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
    ShaderWriter& operator<<( const PostfixOperator& value );
    ShaderWriter& operator<<( const Statement& value );

    template<typename T>
    ShaderWriter& operator<<( const T& value );

    void PushIndentation();
    /** This asserts that m_Indentation is > 0 **/
    void PopIndentation();

    /** Writes '\n' to the shader **/
    void NewLine( unsigned num_lines = 1 );

    /** Records an error while writing the shader **/
    void Error( const std::string& error_string = "" );

    /**
      * This will mangle an identifier to ensure that there are no conflicts
      * Variables are prefixed  with "_"
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
private:
    void GenerateFragmentShader( const EntryFunction& entry_function );

    /** Writes #version 150 to the shader **/
    void WriteGLSLVersion();

    /** Writes all the output varyings **/
    void WriteOutputVariables( const EntryFunction& entry_function );

    /** Writes main(){...} **/
    void WriteMainFunction( const EntryFunction& entry_function );

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
      * The set of function signatures that we've processed
      */
    std::set<std::string> m_FunctionSignatures;

    const static std::string s_GLSLVersion;
};

} // namespace Compiler
} // namespace JoeLang

#include "inl/shader_writer-inl.hpp"
