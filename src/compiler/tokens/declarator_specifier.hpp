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

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{
typedef std::vector<unsigned> ArrayExtents;
class CodeGenerator;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
class Parameter;
class Parser;
class SemaAnalyzer;
class Semantic;
class SemanticSpecifier;
using SemanticSpecifier_up = std::unique_ptr<SemanticSpecifier>;
enum class SemanticType;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

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

    /**
      * Extracts the values from a list of array specifiers
      * \param specifiers
      *   The array specifiers
      * \param sema
      *   The semantic analyzer used to evaluate the expression
      */
    static
    ArrayExtents GetArrayExtents(
                std::vector<std::unique_ptr<ArraySpecifier> >& specifiers,
                SemaAnalyzer& sema );

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

    static
    bool classof( const Token* t );
    static
    bool classof( const ArraySpecifier* d );
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

    /**
      * Registers the parameters as variables with sema
      */
    void DeclareParameters( SemaAnalyzer& sema );

    std::vector<CompleteType> GetParameterTypes() const;

    std::vector<Variable_sp>  GetParameters() const;

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

    static
    bool classof( const Token* t );
    static
    bool classof( const FunctionSpecifier* d );
private:
    std::vector<std::unique_ptr<Parameter> > m_Parameters;
};

/**
  * \class SemanticSpecifier
  * \ingroup Tokens
  * \brief Matches a semantic specifier
  *
  * SemanticSpecifier =  ':' identifier ( '[' Expression ']' )
  */
class SemanticSpecifier : public JoeLang::Compiler::Token
{
public:
    /** This constructor asserts on an empty string **/
    SemanticSpecifier  ( std::string string, 
                         Expression_up index_expression = nullptr );
    virtual
    ~SemanticSpecifier ();

    bool HasIndex() const;

    /** This must only be called after PerformSema **/
    Semantic GetSemantic() const;

    /**
      * Performs semantic analysis on the semantic
      * This will resolve the index expression if there is one
      */
    void PerformSema( SemaAnalyzer& sema );

    static
    bool Parse       ( Parser& parser, SemanticSpecifier_up& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const SemanticSpecifier* d );
private:
    std::string   m_String;
    Expression_up m_IndexExpression;

    unsigned      m_Index;
    SemanticType  m_SemanticType;
};

} // namespace Compiler
} // namespace JoeLang
