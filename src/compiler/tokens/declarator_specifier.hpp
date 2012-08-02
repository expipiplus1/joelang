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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{
class CodeGenerator;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
typedef std::shared_ptr<Expression> Expression_sp;
class Parameter;
class Parser;
class SemaAnalyzer;

/**
  * \class ArraySpecifier
  * \ingroup Tokens
  * \brief Matches a ArraySpecifier
  *
  * ArraySpecifier = '[' Expression ']'
  */
class ArraySpecifier : public JoeLang::Compiler::Token
{
public:
    /** This asserts that expression is not null **/
    ArraySpecifier    ( Expression_up expression );
    virtual
    ~ArraySpecifier   ();

    void PerformSema( SemaAnalyzer& sema );

    Expression_up GetExpression();

    virtual
    void Print( int depth ) const override;

    /**
      * Parses an array specifier
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse ( Parser& parser, std::unique_ptr<ArraySpecifier>& token );

private:
    Expression_up m_Expression;
};

/**
  * \class FunctionSpecifier
  * \ingroup Tokens
  * \brief Matches a FunctionSpecifier
  *
  * FunctionSpecifier = '(' (Parameter(,Parameter)*)? ')'
  */
class FunctionSpecifier : public JoeLang::Compiler::Token
{
public:
    /** This asserts that no parameter is null **/
    FunctionSpecifier    ( std::vector<std::unique_ptr<Parameter>> parameters );
    virtual
    ~FunctionSpecifier   ();

    bool PerformSema( SemaAnalyzer& sema );

    virtual
    void Print( int depth ) const override;

    /**
      * Parses a function specifier
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse ( Parser& parser, std::unique_ptr<FunctionSpecifier>& token );

private:
    std::vector<std::unique_ptr<Parameter> > m_Parameters;
};

} // namespace Compiler
} // namespace JoeLang
