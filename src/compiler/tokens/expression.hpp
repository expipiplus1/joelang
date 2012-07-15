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

#include <compiler/tokens/operator.hpp>
#include <compiler/tokens/token.hpp>
#include <engine/types.hpp>

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

namespace llvm
{
    class Value;
};

namespace JoeLang
{
    enum class Type;

    namespace Compiler
    {
        enum class TerminalType;

        class CodeGenerator;
        class Expression;
        typedef std::unique_ptr<Expression> Expression_up;
        typedef std::shared_ptr<Expression> Expression_sp;
        class GenericValue;
        class Parser;
        class SemaAnalyzer;
        class Variable;

        class LiteralExpression;
    } // namespace Compiler
} // namespace JoeLang

namespace JoeLang
{
namespace Compiler
{

/**
  * \defgroup Expressions
  * \ingroup Tokens
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
    Expression( TokenTy sub_class_id );
    virtual
    ~Expression();

    /**
      * Recurses down and resolves IdentifierExpressions using the symbol table
      * in sema.
      */
    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) = 0;

    /**
      * Performs type checking and things.
      * \param sema
      *   The semantic analyzer to use
      * \returns true if there were no errors parsing
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) = 0;

    /**
      * Performs constant folding.
      * \param self
      *   A reference to a pointer to this. If this can be replaced by a
      *   sub-expression then it's put into self.
      *   Care must be taken to avoid destroying self too early.
      */
    virtual
    void FoldConstants( Expression_up& self );

    /**
      * Generates an llvm value for this expression
      * \param code_gen
      *   A reference to a CodeGenerator
      * \returns the llvm value representing this expression
      */
    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const = 0;

    /**
      * \returns the type which this expression evaluates to
      */
    virtual
    Type GetReturnType() const = 0;

    /**
      * \returns the underling type of this expression, with array extents
      *   removed
      */
    virtual
    Type GetUnderlyingType() const = 0;

    /**
      * \returns the array extents of this expression's return type
      */
    virtual
    std::vector<Expression_sp> GetArrayExtents() const = 0;

    /**
      * \returns true if the Expression represents a l-value
      * by default this returns false
      */
    virtual
    bool IsLValue() const;

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
                Expression_up& token );

    /**
      * Checks if e represents a LiteralExpression, either by being one, or
      * being a constant variable
      * \param e
      *   The expression to check
      * \returns nullptr if it wasn't a literal expression
      */
    static
    LiteralExpression* GetLiteral( const Expression_up& e );

    /** Used for casting **/
    static
    bool classof( const Token* t );
    static
    bool classof( const Expression* e );
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
    using Op = AssignmentOperator::Op;

    /**
      * This constructor asserts on any null pointers
      * \param assignee
      *   The assignee
      * \param assignment_operator
      *   The assignment operator
      * \param assigned_expression
      *   The assigned Expression
      */
    AssignmentExpression(
                        Expression_up assignee,
                        Op assignment_operator,
                        Expression_up assigned_expression );
    virtual
    ~AssignmentExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const AssignmentExpression* e );
private:
    Expression_up m_Assignee;
    std::shared_ptr<Variable>   m_AssigneeVariable;
    Op                          m_AssignmentOperator;
    Expression_up m_AssignedExpression;
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
    ConditionalExpression( Expression_up condition,
                           Expression_up true_expression,
                           Expression_up false_expression );
    virtual
    ~ConditionalExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void FoldConstants( Expression_up& self ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const ConditionalExpression* e );
private:
    Expression_up m_Condition;
    Expression_up m_TrueExpression;
    Expression_up m_FalseExpression;
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
    BinaryOperatorExpression( TokenTy sub_class_id,
                              Op op,
                              Expression_up left_side,
                              Expression_up right_side );
    virtual
    ~BinaryOperatorExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void FoldConstants( Expression_up& self ) override final;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override final;

    virtual
    Type GetReturnType() const override;

    /// \returns GetReturnType
    virtual
    Type GetUnderlyingType() const override final;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override final;

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
                               Expression_up& token,
                               const OperatorTerminalMap& operator_terminals );

    static
    bool classof( const Expression* e );
    static
    bool classof( const BinaryOperatorExpression* e );
protected:
    Op m_Operator;
    Expression_up m_LeftSide;
    Expression_up m_RightSide;
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
                         Expression_up  left_side,
                         Expression_up  right_side );
    virtual
    ~LogicalOrExpression();

    /**
      * Casts both operands to bool
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    /**
      * \returns Type::BOOL
      */
    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const LogicalOrExpression* e );
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
                          Expression_up left_side,
                          Expression_up right_side );
    virtual
    ~LogicalAndExpression();

    /**
      * Casts both operands to bool
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    /**
      * Returns bool
      */
    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const LogicalAndExpression* e );
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
    InclusiveOrExpression( Op operator_terminal,
                           Expression_up left_side,
                           Expression_up right_side );

    virtual
    ~InclusiveOrExpression();

    /**
      * Verifies that both operands are integral
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser, Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const InclusiveOrExpression* e );
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
                          Expression_up left_side,
                          Expression_up right_side );
    virtual
    ~ExclusiveOrExpression();

    /**
      * Verifies that both operands are integral
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const ExclusiveOrExpression* e );
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
                   Expression_up left_side,
                   Expression_up right_side );
    virtual
    ~AndExpression();

    /**
      * Verifies that both operands are integral
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const AndExpression* e );
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
                          Expression_up left_side,
                          Expression_up right_side );
    virtual
    ~EqualityExpression();

    /**
      * \returns Type::BOOL
      */
    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const EqualityExpression* e );
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
                          Expression_up left_side,
                          Expression_up right_side );
    virtual
    ~RelationalExpression();

    /**
      * \returns Type::BOOL
      */
    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const RelationalExpression* e );
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
                     Expression_up left_side,
                     Expression_up right_side );
    virtual
    ~ShiftExpression();

    /**
      * Verifies that both operands are integral
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const ShiftExpression* e );
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
                        Expression_up left_side,
                        Expression_up right_side );
    virtual
    ~AdditiveExpression();

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const AdditiveExpression* e );
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
                              Expression_up left_side,
                              Expression_up right_side );
    virtual
    ~MultiplicativeExpression();

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const MultiplicativeExpression* e );
};

/**
  * \class CastExpression
  * \ingroup Expressions
  * \brief Matches a c-style cast expression
  *
  * This token is unimplemented and will just parse a UnaryExpression.
  * The class is used for representing casts though.
  *
  * CastExpression = '(' Type ')' UnaryExpression
  */
class CastExpression : public JoeLang::Compiler::Expression
{
public:
    explicit
    CastExpression( Type cast_type,
                    Expression_up expression );
    virtual
    ~CastExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void FoldConstants( Expression_up& self ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    Expression_up Create(
                                  Type cast_type,
                                  Expression_up cast_expression );

    static
    bool classof( const Expression* e );
    static
    bool classof( const CastExpression* e );

private:
    Type m_CastType;
    Expression_up m_Expression;
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
                     Expression_up expression );
    virtual
    ~UnaryExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    void FoldConstants( Expression_up& self ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const UnaryExpression* e );
private:
    Op m_Operator;
    Expression_up m_Expression;
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
    PostfixExpression( Expression_up expression,
                       std::unique_ptr<PostfixOperator> postfix_operator );
    virtual
    ~PostfixExpression();

    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const PostfixExpression* e );
private:
    Expression_up m_Expression;
    std::unique_ptr<PostfixOperator> m_PostfixOperator;
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
    static
    bool Parse( Parser& parser,
                Expression_up& token );
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
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    Type GetReturnType() const override;

    virtual
    Type GetUnderlyingType() const override;

    virtual
    std::vector<Expression_sp> GetArrayExtents() const override;

    /** \returns true if this identifier represents a constant expression **/
    bool IsConst() const;

    /** \returns true **/
    virtual
    bool IsLValue() const override;

    const std::shared_ptr<Variable>& GetVariable() const;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    const Expression_up& GetReadExpression() const;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const IdentifierExpression* e );
private:
    std::string                 m_Identifier;
    std::shared_ptr<Variable>   m_Variable;
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
    LiteralExpression( TokenTy sub_class_id );

    /**
      * Does nothing
      */
    virtual
    void ResolveIdentifiers( SemaAnalyzer& sema ) override;

    /**
      * Does nothing
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override final;

    /// \returns GetReturnType
    virtual
    Type GetUnderlyingType() const override final;

    /// \returns {}
    virtual
    std::vector<Expression_sp> GetArrayExtents() const override final;

    virtual
    GenericValue GetValue() const = 0;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    std::unique_ptr<LiteralExpression> Create( const GenericValue& v );

    static
    bool classof( const Expression* e );
    static
    bool classof( const LiteralExpression* e );
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
    IntegerLiteralExpression( jl_u64 value,
                              Suffix suffix );
    virtual
    ~IntegerLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    Type GetReturnType() const override;

    virtual
    GenericValue GetValue() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IntegerLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const IntegerLiteralExpression* e );
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
                       jl_u64&  value,
                       Suffix&     suffix );

    jl_u64 m_Value;
    Suffix m_Suffix;
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
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    Type GetReturnType() const override;

    virtual
    GenericValue GetValue() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<FloatingLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const FloatingLiteralExpression* e );
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

    double m_Value;
    Suffix m_Suffix;
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
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    Type GetReturnType() const override;

    virtual
    GenericValue GetValue() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<BooleanLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const BooleanLiteralExpression* e );
private:
    bool m_Value;
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
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    Type GetReturnType() const override;

    virtual
    GenericValue GetValue() const override;

    virtual
    void Print( int depth ) const;

    const std::string& GetString() const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<StringLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const StringLiteralExpression* e );
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

    std::string m_Value;
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
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    Type GetReturnType() const override;

    virtual
    void Print( int depth ) const;

    virtual
    GenericValue GetValue() const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<CharacterLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const CharacterLiteralExpression* e );
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

    char m_Value;
};

} // namespace Compiler
} // namespace JoeLang
