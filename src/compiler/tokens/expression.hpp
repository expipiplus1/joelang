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
#include <utility>
#include <vector>

#include <compiler/tokens/token.hpp>

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

namespace JoeLang
{
    enum class Type;

    namespace Compiler
    {
        enum class TerminalType;

        class Parser;
        class SemaAnalyzer;

        class AssignmentOperator;
        class PostfixOperator;
    } // namespace Compiler
} // namespace JoeLang

namespace JoeLang
{
namespace Compiler
{

/**
  * \defgroup Expressions
  * \ingroup Tokens
    m_leftSide->PerformSema
  * Expressions parse part of expressions in statements.
  * The static member function Expression::Parse must take a pointer to the
  * abstract base class expression rather than to the more specific base class
  */

/**
  * \class Expression
  * \ingroup Expressions
  * \brief Abstract base class for all expressions
  *
  * Expression = AssignmentExpression
  */
class Expression : public JoeLang::Compiler::Token
{
public:
    Expression( );
    virtual
    ~Expression();

    virtual
    void PerformSema( SemaAnalyzer& sema );

    virtual
    Type GetReturnType() const;

    /**
      * Parses any expression
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class AssignmentExpression
  * \ingroup Expressions
  * \brief matches an assignment expression
  *
  * AssignmentExpression =   ConditionalExpression
  *                        | UnaryExpression AssignmentOperator
  *                          AssignmentExpression
  */
class AssignmentExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This constructor asserts on any null pointers
      * \param unary_expression
      *   The assignee
      * \param assignment_operator
      *   The assignment operator
      * \param assignment_expression
      *   The assigned Expression
      */
    AssignmentExpression(
                        std::unique_ptr<Expression> assignee,
                        std::unique_ptr<AssignmentOperator> assignment_operator,
                        std::unique_ptr<Expression> assigned_expression );
    virtual
    ~AssignmentExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
private:
    std::unique_ptr<Expression>         m_assignee;
    std::unique_ptr<AssignmentOperator> m_assignmentOperator;
    std::unique_ptr<Expression>         m_assignedExpression;
};

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
  * \class ConditionalExpression
  * \ingroup Expressions
  * \brief matches a ternary conditional expression
  *
  * ConditionalExpression = LogicalOrExpression ('?' expression ':'
  *                         ConditionalExpression )?
  */
class ConditionalExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This constructor asserts on any null pointers
      * \param condition
      *   The boolean expression on which to choose
      * \param true_expression
      *   The expression to return if condition is true
      * \param false_expression
      *   The expression to return if condition is false
      */
    ConditionalExpression( std::unique_ptr<Expression> condition,
                           std::unique_ptr<Expression> true_expression,
                           std::unique_ptr<Expression> false_expression );
    virtual
    ~ConditionalExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );

private:
    std::unique_ptr<Expression> m_condition;
    std::unique_ptr<Expression> m_trueExpression;
    std::unique_ptr<Expression> m_falseExpression;
};

/**
  * \class BinaryOperatorExpression
  * \ingroup Expressions
  * \brief A parent class for all left associative binary expressions
  *
  * BinaryOperatorExpression = SubExpressionType (operator SubExpressionType)*
  */
class BinaryOperatorExpression : public JoeLang::Compiler::Expression
{
public:
    /** \enum Op
      *   An enum for any kind of binary operator **/
    enum class Op
    {
        LOGICAL_OR,
        LOGICAL_AND,
        OR,
        XOR,
        AND,
        EQUAL_TO,
        NOT_EQUAL_TO,
        LESS_THAN,
        GREATER_THAN,
        LESS_THAN_EQUALS,
        GREATER_THAN_EQUALS,
        LEFT_SHIFT,
        RIGHT_SHIFT,
        PLUS,
        MINUS,
        MULTIPLY,
        DIVIDE,
        MODULO
    };
    using OperatorTerminalMap = std::vector< std::pair<TerminalType, Op> >;

    /**
      * This constructor asserts on any null pointers
      * \param op
      *   The operator
      * \param left_side
      *   The expression to the left of the operator
      * \param right_side
      *   The expression to the right of the operator
      */
    BinaryOperatorExpression( Op operator_terminal,
                              std::unique_ptr<Expression> left_side,
                              std::unique_ptr<Expression> right_side );
    virtual
    ~BinaryOperatorExpression();

    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void Print( int depth ) const;

    /**
      * Function to parse a left associative expression
      * \tparam ExpressionType
      *   The type of expression being parsed
      * \tparam SubExpressionType
      *   The subexpression expected either side of the operator
      * \param parser
      *   The parser
      * \param token
      *   The parsed expression
      * \param operator_terminals
      *   A vector of possible operators for the expression
      */
    template< typename ExpressionType, typename SubExpressionType >
    static
    bool ParseLeftAssociative( Parser& parser,
                               std::unique_ptr<Expression>& token,
                               const OperatorTerminalMap& operator_terminals );

private:
    Op m_operator;
    std::unique_ptr<Expression> m_leftSide;
    std::unique_ptr<Expression> m_rightSide;
};


/**
  * \class LogicalOrExpression
  * \ingroup Expressions
  * \brief Matches a logical or expression
  *
  * LogicalOrExpression = LogicalAndExpression ('||' LogicalAndExpression)*
  */
class LogicalOrExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    LogicalOrExpression( Op operator_terminal,
                         std::unique_ptr<Expression>  left_side,
                         std::unique_ptr<Expression>  right_side );
    virtual
    ~LogicalOrExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class LogicalAndExpression
  * \ingroup Expressions
  * \brief Matches a logical and expression
  *
  * LogicalAndExpression = InclusiveOrExpression ('&&' InclusiveOrExpression)*
  */
class LogicalAndExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    LogicalAndExpression( Op operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );
    virtual
    ~LogicalAndExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class InclusiveOrExpression
  * \ingroup Expressions
  * \brief Matches an or expression
  *
  * InclusiveOrExpression = ExclusiveOrExpression ('|' ExclusiveOrExpression)*
  */
class InclusiveOrExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    virtual
    ~InclusiveOrExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    InclusiveOrExpression( Op operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

/**
  * \class ExclusiveOrExpression
  * \ingroup Expressions
  * \brief Matches an xor expression
  *
  * ExclusiveOrExpression = AndExpression ('^' AndExpression)*
  */
class ExclusiveOrExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    ExclusiveOrExpression( Op operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );
    virtual
    ~ExclusiveOrExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class AndExpression
  * \ingroup Expressions
  * \brief Matches an and expression
  *
  * AndExpression = EqualityExpression ('&' EqualityExpression)*
  */
class AndExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    AndExpression( Op operator_terminal,
                   std::unique_ptr<Expression> left_side,
                   std::unique_ptr<Expression> right_side );
    virtual
    ~AndExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class EqualityExpression
  * \ingroup Expressions
  * \brief Matches an equality expression
  *
  * EqualityExpression = RelationalExpression
  *                      (('==' | '!=') RelationalExpression)*
  */
class EqualityExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    EqualityExpression( Op operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );
    virtual
    ~EqualityExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );
};

/**
  * \class RelationalExpression
  * \ingroup Expressions
  * \brief Matches a relational expression
  *
  * RelationalExpression = ShiftExpression
  *                         (('<' | '>' | '<=' | '>=') ShiftExpression)
  */
class RelationalExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    RelationalExpression( Op operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );
    virtual
    ~RelationalExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class ShiftExpression
  * \ingroup Expressions
  * \brief Matches a shift expression
  *
  * ShiftExpression = AdditiveExpression (('<<' | '>>') AdditiveExpression)*
  */
class ShiftExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    ShiftExpression( Op operator_terminal,
                     std::unique_ptr<Expression> left_side,
                     std::unique_ptr<Expression> right_side );
    virtual
    ~ShiftExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class AdditiveExpression
  * \ingroup Expressions
  * \brief Matches an additive expression
  *
  * AdditiveExpression = MultiplicativeExpression
  *                      (('+' | '-') MultiplicativeExpression)*
  */
class AdditiveExpression : public JoeLang::Compiler::BinaryOperatorExpression
{
public:
    AdditiveExpression( Op operator_terminal,
                        std::unique_ptr<Expression> left_side,
                        std::unique_ptr<Expression> right_side );
    virtual
    ~AdditiveExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class MultiplicativeExpression
  * \ingroup Expressions
  * \brief Matches a multuplicative expression
  *
  * MultiplicativeExpression = CastExpression (('+' | '-') CastExpression)*
  */
class MultiplicativeExpression : public Compiler::BinaryOperatorExpression
{
public:
    MultiplicativeExpression( Op operator_terminal,
                              std::unique_ptr<Expression> left_side,
                              std::unique_ptr<Expression> right_side );
    virtual
    ~MultiplicativeExpression();

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class CastExpression
  * \ingroup Expressions
  * \brief Matches a c-style cast expression
  *
  * This token is unimplemented and will just parse a UnaryExpression
  *
  * CastExpression = '(' Type ')' UnaryExpression
  */
class CastExpression : public JoeLang::Compiler::Expression
{
public:
    explicit
    CastExpression( Type cast_type );
    virtual
    ~CastExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );

private:
    Type m_castType;
};

/**
  * \class UnaryExpression
  * \ingroup Expressions
  * \brief Matches a unary operator expression
  *
  * UnaryExpression =   PostfixExpression
  *                   | ('+' | '-' | '~' | '!' | '++' | '--') UnaryExpression
  */
class UnaryExpression : public JoeLang::Compiler::Expression
{
public:
    /** \enum Op
      *   An enum for any kind of unary operator **/
    enum class Op
    {
        PLUS,
        MINUS,
        BITWISE_NOT,
        LOGICAL_NOT,
        INCREMENT,
        DECREMENT
    };

    /** This constructor asserts on a null expression
      * \param op
      *   The unary Operator
      * \param expression
      *   The expression
      */
    UnaryExpression( Op op,
                     std::unique_ptr<Expression> expression );
    virtual
    ~UnaryExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
private:
    Op m_operator;
    std::unique_ptr<Expression> m_expression;
};

/**
  * \class PostfixExpression
  * \ingroup Expressions
  * \brief Matches a postfix expression
  *
  * PostfixExpression = PrimaryExpression ( PostfixOperator )*
  */
class PostfixExpression : public JoeLang::Compiler::Expression
{
public:
    PostfixExpression( std::unique_ptr<Expression> expression,
                       std::unique_ptr<PostfixOperator> postfix_operator );
    virtual
    ~PostfixExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
private:
    std::unique_ptr<Expression> m_expression;
    std::unique_ptr<PostfixOperator> m_postfixOperator;
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
  * \brief Matches a pose increment or post decrement operator
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

/**
  * \class PrimaryExpression
  * \ingroup Expressions
  * \brief Matches a primary expression
  *
  * PrimaryExpression just forwards the parse to any of the sub expressions and
  * is never directly created
  *
  * PrimaryExpression =   IdentifierExpression
  *                     | LiteralExpression
  *                     | '(' Expression ')'
  */
class PrimaryExpression : public JoeLang::Compiler::Expression
{
public:
    PrimaryExpression();
    virtual
    ~PrimaryExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class IdentifierExpression
  * \ingroup Expressions
  * \brief Matches an identifier
  *
  * IdentifierExpression = identifier
  */
class IdentifierExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This constructor asserts on a null identifier
      * \param identifier
      *   The identifier
      */
    IdentifierExpression( std::string identifier );
    virtual
    ~IdentifierExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );

private:
    std::string                 m_identifier;
    std::shared_ptr<Expression> m_readExpression;
};

/**
  * \class LiteralExpression
  * \ingroup Expressions
  * \brief Matches any kind of literal
  *
  * LiteralExpression =   IntegerLiteralExpression
  *                     | FloatingLiteralExpression
  *                     | StringLiteralExpressionon
  *                     | CharacterLiteralExpression
  *                     | BooleanLiteralExpression
  */
class LiteralExpression : public JoeLang::Compiler::Expression
{
public:
    LiteralExpression();
    virtual
    ~LiteralExpression();

    /**
      * Does nothing
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<Expression>& token );
};

/**
  * \class IntegerLiteralExpression
  * \ingroup Expressions
  * \brief Matches integer literals
  *
  * IntegerLiteralExpression = integer_literal
  */
class IntegerLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    /** \enum Suffix
      *   A enumeration of the possible integer suffixes **/
    enum class Suffix
    {
        NONE,
        CHAR,
        INT,
        LONG,
        SHORT,
        UNSIGNED,
        UNSIGNED_CHAR,
        UNSIGNED_INT,
        UNSIGNED_LONG,
        UNSIGNED_SHORT
    };

    /**
      * \param value
      *   The value of this literal
      * \param suffix
      *   The integer suffix
      */
    explicit
    IntegerLiteralExpression( unsigned long long value,
                              Suffix suffix );
    virtual
    ~IntegerLiteralExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IntegerLiteralExpression>& token );
private:
    /**
      * This function parses an integer into value and suffix
      * \param string
      *   The string to parse to an integer
      * \param value
      *   The value of the integer
      * \param suffix
      *   The suffix at the end of the string
      * \returns true if parsed successfully
      */
    static
    bool ParseInteger( std::string string,
                       unsigned long long&  value,
                       Suffix&     suffix );

    unsigned long long m_value;
    Suffix m_suffix;
};

/**
  * \class FloatingLiteralExpression
  * \ingroup Expressions
  * \brief Matches floating literals
  *
  * FloatingLiteralExpression = floating_literal
  */
class FloatingLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    /**
      * \enum Suffix
      *   An enumeration of the possible floating point suffixes
      * \todo implement half and fixed
      */
    enum class Suffix
    {
        NONE,
        SINGLE,
    };

    FloatingLiteralExpression( double value, Suffix suffix );
    virtual
    ~FloatingLiteralExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<FloatingLiteralExpression>& token );
private:
    /**
      * This function parses an float into value and suffix
      * \param string
      *   The string to parse to an float
      * \param value
      *   The value of the float
      * \param suffix
      *   The suffix at the end of the string
      * \returns true if parsed successfully
      */
    static
    bool ParseFloat( std::string string,
                     double& value,
                     Suffix& suffix );

    double m_value;
    Suffix m_suffix;
};

/**
  * \class BooleanLiteralExpression
  * \ingroup Expressions
  * \brief Matches boolean literals
  *
  * BooleanLiteralExpression = 'true' | 'false'
  */
class BooleanLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    BooleanLiteralExpression( bool value );
    virtual
    ~BooleanLiteralExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<BooleanLiteralExpression>& token );
private:
    bool m_value;
};

/**
  * \class StringLiteralExpression
  * \ingroup Expressions
  * \brief Matches string literals
  *
  * StringLiteralExpression = string_literal
  */
class StringLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    StringLiteralExpression( std::string value );
    virtual
    ~StringLiteralExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    const std::string& GetString() const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<StringLiteralExpression>& token );


private:
    /**
      * This function parses an unescaped and quoted string into it's value
      * \param string
      *   The quoted and escaped string
      * \param unescaped_string
      *   The value of the literal
      * \returns true if parsed successfully
      */
    static
    bool UnquoteAndUnescapeString( const std::string& string,
                                   std::string& unescaped_string );

    std::string m_value;
};

/**
  * \class CharacterLiteralExpression
  * \ingroup Expressions
  * \brief Matches character literals
  *
  * CharacterLiteralExpression = character_literal
  */
class CharacterLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    CharacterLiteralExpression( char value );
    virtual
    ~CharacterLiteralExpression();

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<CharacterLiteralExpression>& token );
private:
    /**
      * This function parses an unescaped and quoted char into it's value
      * \param character
      *   The quoted and escaped character
      * \param unescaped_char
      *   The value of the literal
      * \returns true if parsed successfully
      */
    static
    bool UnquoteAndUnescapeChar( const std::string& character,
                                 char& unescaped_char );

    char m_value;
};

} // namespace Compiler
} // namespace JoeLang
