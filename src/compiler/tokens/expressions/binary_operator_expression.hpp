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

#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>

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
        class GenericValue;
        class Parser;
        class SemaAnalyzer;
    } // namespace Compiler
} // namespace JoeLang

namespace JoeLang
{
namespace Compiler
{

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
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value*CodeGen( CodeGenerator& code_gen ) const override final;

    virtual
    CompleteType GetType() const override;

    virtual
    bool IsConst() const override;

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
      * \returns CompleteType( Type::BOOL )
      */
    virtual
    CompleteType GetType() const override;

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
    CompleteType GetType() const override;

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
    CompleteType GetType() const override;

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
    CompleteType GetType() const override;

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

} // namespace Compiler
} // namespace JoeLang
