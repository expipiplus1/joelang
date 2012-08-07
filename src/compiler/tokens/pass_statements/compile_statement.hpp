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

#include <compiler/tokens/pass_statements/pass_statement.hpp>

namespace JoeLang
{

namespace Compiler
{

class CodeGenerator;
class CompileStatement;
using CompileStatement_up = std::unique_ptr<CompileStatement>;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class Parser;
class SemaAnalyzer;

/**
  * \class CompileStatement
  * \ingroup Tokens
  * \brief Matches a shader compile statement
  *
  * CompileStatement = ('vertexshader' | 'pixelshader') '='
  *                                                   'compile' FunctionCall ';'
  */
class CompileStatement : public JoeLang::Compiler::PassStatement
{
public:
    enum class ShaderDomain
    {
        VERTEX,
        FRAGMENT
    };

    /**
      * This asserts that identifier is not empty and that none of the arguments
      * are null
      * \param domain
      *   The domain of the shader
      * \param identifier
      *   The function identifier
      * \param arguments
      *   The arguments to the functions
      */
    CompileStatement( ShaderDomain domain,
                      std::string identifier,
                      std::vector<Expression_up> arguments );
    virtual
    ~CompileStatement();

    void PerformSema( SemaAnalyzer& sema );

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, CompileStatement_up& token );

private:
    ShaderDomain m_Domain;
    std::string m_Identifier;
    std::vector<Expression_up> m_Arguments;
};

} // namespace Compiler
} // namespace JoeLang
