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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{
class DirectDeclarator;
class Expression;
class Parser;
class SemaAnalyzer;

/**
  * \class Declarator
  * \ingroup Tokens
  * \brief Matches a declarator with optional initializer
  *
  * Declarator = DirectDeclarator ( '=' AssignmentExpression )?
  */
class Declarator : public JoeLang::Compiler::Token
{
public:
    /** This constructor asserts on a null direct_declarator **/
    Declarator  ( std::unique_ptr<DirectDeclarator> direct_declarator,
                  std::unique_ptr<Expression> initializer = nullptr );
    virtual
    ~Declarator ();

    /**
      * Performs semantic ananysis on the declarator
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    void PerformSema( SemaAnalyzer& sema );

    virtual
    void Print( int depth ) const override;

    /**
      * Parses a declarator
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse          ( Parser& parser, std::unique_ptr<Declarator>& token );

private:
    std::unique_ptr<DirectDeclarator> m_directDeclarator;
    std::unique_ptr<Expression> m_initializer;
};

/**
  * \class DirectDeclarator
  * \ingroup Tokens
  * \brief Matches a Declarator
  *
  * DirectDeclarator = identifier
  */
class DirectDeclarator : public JoeLang::Compiler::Token
{
public:
    DirectDeclarator    ( std::string identifier );
    virtual
    ~DirectDeclarator   ();

    virtual
    void Print( int depth ) const override;

    /**
      * Parses a direct declarator
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse ( Parser& parser, std::unique_ptr<DirectDeclarator>& token );

private:
    std::string m_identifier;
};

} // namespace Compiler
} // namespace JoeLang
