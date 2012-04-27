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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

class Expression;
class Parser;

/**
  * \class AssignmentOperator
  * \ingroup Tokens
  * \brief matches an assignment operator
  *
  * AssignmentExpression = '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' |
  *                        '>>=' | '&=' | '^=' | '|='
  */
class AssignmentOperator : public JoeLang::Compiler::Token
{
public:
    /** \enum Op
      *   An enum for any kind of assignment operator **/
    enum class Op
    {
        EQUALS,
        MULTIPLY_EQUALS,
        DIVIDE_EQUALS,
        MODULO_EQUALS,
        PLUS_EQUALS,
        MINUS_EQUALS,
        SHL_EQUALS,
        SHR_EQUALS,
        AND_EQUALS,
        XOR_EQUALS,
        OR_EQUALS
    };

    /**
      * \param assignment_operator
      *   The operator
      */
    AssignmentOperator( Op assignment_operator );
    virtual
    ~AssignmentOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<AssignmentOperator>& token );
private:
    Op m_operator;
};

/**
  * \defgroup PostfixOperators
  * \ingroup Tokens
  */

/**
  * \class PostfixOperator
  * \ingroup PostfixOperators
  * \brief Matches any postfix operator
  *
  * PostfixOperator =   SubscriptOperator
  *                   | ArgumentListOperator
  *                   | MemberAccessOperator
  *                   | IncrementOrDecrementOperator
  */
class PostfixOperator : public JoeLang::Compiler::Token
{
public:
    PostfixOperator( );
    virtual
    ~PostfixOperator();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<PostfixOperator>& token );
};

/**
  * \class SubscriptOperator
  * \ingroup PostfixOperators
  * \brief Matches an array subscript operator
  *
  * SubscriptOperator = '[' Expression ']'
  */
class SubscriptOperator : public JoeLang::Compiler::PostfixOperator
{

public:
    /**
      * This constructor asserts on a null expression
      * \param expression
      *   The index expression
      */
    SubscriptOperator( std::unique_ptr<Expression> expression );
    virtual
    ~SubscriptOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<SubscriptOperator>& token );
private:
    std::unique_ptr<Expression> m_expression;
};

/**
  * \class ArgumentListOperator
  * \ingroup PostfixOperators
  * \brief Matches an argument list
  *
  * ArgumentListOperator = '(' ( AssignmentExpression
  *                                 (',' AssignmentExpression)* )? ')'
  */
class ArgumentListOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    using ArgumentExpressionVector = std::vector< std::unique_ptr<Expression> >;

    ArgumentListOperator( ArgumentExpressionVector argument_expressions );
    virtual
    ~ArgumentListOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<ArgumentListOperator>& token );
private:
    ArgumentExpressionVector m_argumentExpressions;
};

/**
  * \class MemberAccessOperator
  * \ingroup PostfixOperators
  * \brief Matches a member accessor
  *
  * MemberAccessOperator = '.' identifier
  */
class MemberAccessOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    /**
      * This constructor asserts on a null identifier
      * \param identifier
      *   The identifier for the member access
      */
    MemberAccessOperator( std::string identifier );
    virtual
    ~MemberAccessOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<MemberAccessOperator>& token );
private:
    std::string m_identifier;
};

/**
  * \class IncrementOrDecrementOperator
  * \ingroup PostfixOperators
  * \brief Matches a post increment or post decrement operator
  *
  * IncrementOrDecrementOperator = '++' | '--'
  */
class IncrementOrDecrementOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    enum class Op
    {
        INCREMENT,
        DECREMENT
    };
    IncrementOrDecrementOperator( Op terminal_type );
    virtual
    ~IncrementOrDecrementOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IncrementOrDecrementOperator>& token );
private:
    Op m_operator;
};

} // namespace Compiler
} // namespace JoeLang