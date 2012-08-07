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
#include <vector>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{

namespace Compiler
{

class SemaAnalyzer;
class DeclarationBase;
class Parser;

/**
  * \class TranslationUnit
  * \ingroup Tokens
  * \brief Matches a whole translation unit
  *
  * TranslationUnit = (DeclarationBase)* EOF
  */
class TranslationUnit : public JoeLang::Compiler::Token
{
public:
    using DeclarationVector = std::vector< std::unique_ptr<DeclarationBase> >;

    /**
      * This constructor asserts on any null declaration
      * \param declarations
      *   A vector of the top level declarations that appear in this translation
      *   unit
      */
    TranslationUnit( DeclarationVector declarations );
    virtual
    ~TranslationUnit();

    /**
      * Perform semantic analysis on the translation unit and all the
      * declarations
      * \param sema
      *   The sema to contain the symbol table and other things
      */
    void PerformSema( SemaAnalyzer& sema );

    /** \returns The top level declarations of this translation unit **/
    const DeclarationVector& GetDeclarations() const;

    /**
      * Parses a translation unit
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                std::unique_ptr<TranslationUnit>& token );

private:
    /** The vector of all the top level declarations **/
    DeclarationVector m_Declarations;
};

} // namespace Compiler
} // namespace JoeLang

