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
#include <vector>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{
class CompleteType;
typedef std::vector<unsigned> ArrayExtents;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
class Parser;
class SemaAnalyzer;

/**
  * \class Initializer
  * \ingroup Tokens
  * \brief Matches an initializer list
  *
  * Initializer = AssignmentExpression
  *             | '{' AssignmentExpression ( ',' AssignmentExpression )* '}'
  */
class Initializer : public JoeLang::Compiler::Token
{
public:
    /**
      * Constructs an Initializer from other initializer lists
      * This asserts that sub_lists isn't empty
      */
    explicit
    Initializer    ( std::vector<std::unique_ptr<Initializer> > sub_lists );

    /**
      * Constructs an Initializer from an expression
      * This asserts that expression isn't null
      */
    explicit
    Initializer    ( Expression_up expression );

    virtual
    ~Initializer   ();

    /**
      * Performs semantic ananysis on the expressions
      *
      * This checks that the initializers are all of the same type, and that
      * there is at least one initializer.
      * \param sema
      *   The SemaAnalyzer which contains the symbol table and things
      * \param desired_type
      *   The underlying type of the initializer
      * \returns true if this is a valid initializer list
      */
    bool PerformSema( SemaAnalyzer& sema, const CompleteType& desired_type );

    const ArrayExtents& GetArrayExtents() const;

    /**
      * \returns true if this initializer represents just an expression
      */
    bool IsExpression() const;

    /**
      * This asserts that m_Expression is non-null
      * \returns the expression represented by this initializer
      */
    const Expression& GetExpression() const;

    /**
      * This asserts that m_SubInitializers is not empty
      * \returns the sub initializers
      */
    const std::vector<std::unique_ptr<Initializer> >&
                                                     GetSubInitializers() const;

    /**
      * \returns true if this is a list containing a single expression
      */
    bool CanReduceToExpression();

    /**
      * Reduces an initializer list containing a single expression to an
      * initializer list representing that expression
      * This asserts that this is a list containing a single expression
      */
    void ReduceToExpression();

    virtual
    void Print( int depth ) const override;

    /**
      * Parses an initializer list
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse ( Parser& parser, std::unique_ptr<Initializer>& token );

private:
    std::vector<std::unique_ptr<Initializer> > m_SubInitializers;
    Expression_up                              m_Expression;
    ArrayExtents                               m_ArrayExtents;
};

} // namespace Compiler
} // namespace JoeLang
