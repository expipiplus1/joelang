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
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{
class ArraySpecifier;
using ArrayExtents = std::vector<unsigned>;
class CodeGenerator;
class Declarator;
using Declarator_up = std::unique_ptr<Declarator>;
class DeclSpecs;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class FunctionSpecifier;
using FunctionSpecifier_up = std::unique_ptr<FunctionSpecifier>;
class Initializer;
class Parser;
class SemaAnalyzer;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

/**
  * \class InitDeclarator
  * \ingroup Tokens
  * \brief Matches a declarator with optional initializer
  *
  * InitDeclarator = Declarator ( '=' AssignmentExpression )?
  */
class InitDeclarator : public JoeLang::Compiler::Token
{
public:
    /** This constructor asserts on a null direct_declarator **/
    InitDeclarator  ( std::unique_ptr<Declarator> declarator,
                      std::unique_ptr<Initializer> initializer = nullptr );
    virtual
    ~InitDeclarator ();

    /**
      * Performs semantic ananysis on the declarator
      * This also codegens the variable
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      * \param decl_specs
      *   The declaration specifiers for this declarator
      */
    void PerformSema( SemaAnalyzer& sema, const DeclSpecs& decl_specs );

    /** This asserts that there is no initializer, we don't want to every split
      * a declarator from its initializer **/
    Declarator_up TakeDeclarator();

    /**
      * \returns true if declarator is a function declarator
      */
    bool IsFunctionDeclarator() const;

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
    bool Parse       ( Parser& parser, std::unique_ptr<InitDeclarator>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const InitDeclarator* d );
private:
    std::unique_ptr<Declarator>  m_Declarator;
    std::unique_ptr<Initializer> m_Initializer;
    std::shared_ptr<Variable>    m_Variable;
    bool                         m_IsGlobal;
};

/**
  * \class Declarator
  * \ingroup Tokens
  * \brief Matches a Declarator with a param list or array specifiers or nothing
  *
  * Declarator = identifier FunctionSpecifier? ArraySpecifier*
  */
class Declarator : public JoeLang::Compiler::Token
{
public:
    using ArraySpecifierVector = std::vector< std::unique_ptr<ArraySpecifier> >;

    /** If given a function_body it asserts that it has a function specifier **/
    Declarator    ( std::string identifier,
                    FunctionSpecifier_up function_specifier,
                    ArraySpecifierVector array_specifiers );
    virtual
    ~Declarator   ();

    /**
      * Performs semantic ananysis on the declarator. This will declare this
      * declarator to sema if it's a function.
      * \param sema
      *   The SemaAnalyzer which contains the symbol table and things
      * \param decl_specs
      *   The declaration specifiers associated with this Declarator
      */
    bool PerformSema( SemaAnalyzer& sema, const DeclSpecs& decl_specs );

    /**
      * Registers the names and types of the function parameters with sema
      * This assers that this declarator is a Function declarator
      */
    void DeclareFunctionParameters( SemaAnalyzer& sema ) const;

    /**
      * This asserts that this is a function declarator
      * \returns the parameter types
      */
    std::vector<CompleteType> GetFunctionParameterTypes() const;

    /**
      * This asserts that this is a function declarator
      * \returns the parameters
      */
    std::vector<Variable_sp> GetFunctionParameters() const;

    bool IsFunctionDeclarator() const;

    /**
      * \returns the identifier
      */
    const std::string& GetIdentifier() const;

    /** \returns a vector of the sizes of the array dimensions **/
    const ArrayExtents& GetArrayExtents() const;

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
    bool Parse ( Parser& parser, std::unique_ptr<Declarator>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const Declarator* d );
private:
    std::string          m_Identifier;
    FunctionSpecifier_up m_FunctionSpecifier;
    ArraySpecifierVector m_ArraySpecifiers;
    ArrayExtents         m_ArrayExtents;
};

} // namespace Compiler
} // namespace JoeLang
