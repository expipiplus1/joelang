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

#include <compiler/complete_type.hpp>
#include <compiler/tokens/expressions/assignment_operator.hpp>
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
    typedef std::vector<unsigned> ArrayExtents;

    enum class Type;

    namespace Compiler
    {
        enum class TerminalType;

        class CodeGenerator;
        class Expression;
        typedef std::unique_ptr<Expression> Expression_up;
        class GenericValue;
        class Function;
        using Function_sp = std::shared_ptr<Function>;
        class Parser;
        class PostfixOperator;
        class SemaAnalyzer;
        class Variable;
        using Variable_sp = std::shared_ptr<Variable>;

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
    bool ResolveIdentifiers( SemaAnalyzer& sema ) = 0;

    /**
      * Performs type checking and things.
      * \param sema
      *   The semantic analyzer to use
      * \returns true if there were no errors parsing
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) = 0;

    /**
      * Generates an llvm value for this expression
      * \param code_gen
      *   A reference to a CodeGenerator
      * \returns the llvm value representing this expression
      */
    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const = 0;

    /**
      * Generates an llvm value for a pointer to the value of this expression
      * \param code_gen
      *   A reference to a CodeGenerator
      * \returns the llvm value representing this expression's value's pointer
      */
    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const;

    /**
      * \returns the CompleteType which this expression evaluates to
      */
    virtual
    CompleteType GetType() const = 0;

    /**
      * \returns true if the Expression represents a l-value
      * by default this returns false
      */
    virtual
    bool IsLValue() const;

    /**
      * \returns true if this is a constexpr
      */
    virtual
    bool IsConst() const = 0;

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
    AssignmentExpression( Expression_up assignee,
                          Op assignment_operator,
                          Expression_up assigned_expression );
    virtual
    ~AssignmentExpression();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    /**
      * For expressions such as 'a += b' this is inteerpreted as 'a = a+b', with
      * m_AssignedExpression being replaced with a+b and ownership of a being
      * given to m_AssignedExpression and a pointer being kept in m_AssigneePtr,
      * In the case of 'a = b' m_Assignee keeps the ownership;
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

    virtual
    bool IsLValue() const override;

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
    /// This points to the assignee, to hold onto it after ownership has been
    /// given to AssignedExpression for expressions such as a += b;
    Expression*   m_AssigneePtr = nullptr;
    Op            m_AssignmentOperator;
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
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

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
    /** This asserts that expression is not null **/
    explicit
    CastExpression( CompleteType cast_type,
                    Expression_up expression );
    virtual
    ~CastExpression();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    Expression_up Create( const CompleteType& cast_type,
                          Expression_up expression );

    static
    Expression_up Create( Type cast_type, Expression_up expression );

    /**
      * Casts vectors to a different base type, preserving size
      */
    static
    Expression_up CreateBaseTypeCast( Type base_type,
                                      Expression_up cast_expression );

    static
    bool classof( const Expression* e );
    static
    bool classof( const CastExpression* e );

private:
    CompleteType m_CastType;
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
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

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
  * PostfixExpression = TypeConstructorExpression ( PostfixOperator )*
  */
class PostfixExpression : public JoeLang::Compiler::Expression
{
public:
    /** This asserts on a null expression or operator **/
    PostfixExpression( Expression_up expression,
                       std::unique_ptr<PostfixOperator> postfix_operator );
    virtual
    ~PostfixExpression();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;


    virtual
    bool IsConst() const override;

    virtual
    bool IsLValue() const override;

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
  * \class TypeConstructorExpression
  * \ingroup Expressions
  * \brief Matches a type constructor and arguments
  *
  * TypeConstructorExpression = PrimaryExpression
  *                           | TypeSpecifier ArgumentListOperator
  *                           | TypeSpecifier '{' ( AssignmentExpression
  *                                           (',' AssignmentExpression)* )? '}'
  */
class TypeConstructorExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This asserts type isn't Type::UNKNOWN and that the arguments are not
      * null.
      */
    TypeConstructorExpression( Type type,
                               std::vector<Expression_up> arguments );
    virtual
    ~TypeConstructorExpression();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

    virtual
    bool IsLValue() const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const TypeConstructorExpression* e );
private:
    Type                       m_Type;
    std::vector<Expression_up> m_Arguments;
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
  * \brief Matches an identifier for either a function or an variable
  *
  * An IdentifierExpression can match a variable or a function. Because
  * functions can be overloaded it doesn't ask sema for the function, but
  * instead just holds onto the identifier, for the ArgumentListOperator to deal
  * with; If this is a function ArgumentListExpression must not call performsema
  * on it;
  *
  * IdentifierExpression = identifier
  */
class IdentifierExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This constructor asserts on an empty identifier
      * \param identifier
      *   The identifier
      */
    IdentifierExpression( std::string identifier );
    virtual
    ~IdentifierExpression();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    const std::string& GetIdentifier() const;

    virtual
    CompleteType GetType() const override;

    /** \returns true **/
    virtual
    bool IsLValue() const override;

    virtual
    bool IsConst() const override;

    const std::shared_ptr<Variable>& GetVariable() const;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const override;

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
    std::string m_Identifier;
    Variable_sp m_Variable;
};

} // namespace Compiler
} // namespace JoeLang
