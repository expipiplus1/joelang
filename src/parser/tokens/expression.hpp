/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#pragma once

#include <memory>
#include <string>
#include <vector>

#include <parser/tokens/token.hpp>

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

namespace llvm
{
    class Value;
}

namespace JoeLang
{

enum class Type;

namespace Lexer
{
    enum TerminalType : int;
}

namespace Parser
{

class CodeGenerator;
class Parser;

class AssignmentOperator;
class UnaryOperator;
class PostfixOperator;

//------------------------------------------------------------------------------
// Expression
// All Classes deriving from Expression will fill a pointer to Expression when
// Parse is called, because they're used in a polymorphic way.
//------------------------------------------------------------------------------

class Expression : public JoeLang::Parser::Token
{
public:
    virtual
    ~Expression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const;

    virtual
    Type GetReturnType() const;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    Expression( );
};

//------------------------------------------------------------------------------
// AssignmentExpression
//------------------------------------------------------------------------------

class AssignmentExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~AssignmentExpression();

    virtual
    void Print( int depth ) const;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse(Parser& parser, std::unique_ptr<Expression> &token );

protected:
    AssignmentExpression( std::unique_ptr<Expression> unary_expression,
                          std::unique_ptr<AssignmentOperator> assignment_operator,
                          std::unique_ptr<Expression> assignment_expression );

private:
    std::unique_ptr<Expression> m_assignee;
    std::unique_ptr<AssignmentOperator> m_assignmentOperator;
    std::unique_ptr<Expression> m_assignment;
};

//------------------------------------------------------------------------------
// AssignmentOperator
//------------------------------------------------------------------------------

class AssignmentOperator : public JoeLang::Parser::Token
{
public:
    virtual
    ~AssignmentOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse(Parser& parser, std::unique_ptr<AssignmentOperator> &token );

protected:
    AssignmentOperator( Lexer::TerminalType terminal_type );

private:
    Lexer::TerminalType m_terminalType;
};

//------------------------------------------------------------------------------
// ConditionalExpression
//------------------------------------------------------------------------------

class ConditionalExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~ConditionalExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    ConditionalExpression( std::unique_ptr<Expression> condition,
                           std::unique_ptr<Expression> true_expression,
                           std::unique_ptr<Expression> false_expression );

private:
    std::unique_ptr<Expression> m_condition;
    std::unique_ptr<Expression> m_trueExpression;
    std::unique_ptr<Expression> m_falseExpression;
};

//------------------------------------------------------------------------------
// BinaryOperatorExpression
//------------------------------------------------------------------------------

class BinaryOperatorExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~BinaryOperatorExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    template< typename ExpressionType, typename SubExpressionType >
    static
    bool ParseLeftAssociative( Parser& parser, std::unique_ptr<Expression>& token,
                                      const std::vector<Lexer::TerminalType>& operator_terminals );

    template< typename ExpressionType, typename SubExpressionType >
    static
    bool ParseRightAssociative( Parser& parser, std::unique_ptr<Expression>& token,
                                       const std::vector<Lexer::TerminalType>& operator_terminals );

protected:
    BinaryOperatorExpression( Lexer::TerminalType operator_terminal,
                              std::unique_ptr<Expression> left_side,
                              std::unique_ptr<Expression> right_side );

    Lexer::TerminalType m_operatorTerminal;
    std::unique_ptr<Expression> m_leftSide;
    std::unique_ptr<Expression> m_rightSide;

private:
};

//------------------------------------------------------------------------------
// LogicalOrExpression
//------------------------------------------------------------------------------

class LogicalOrExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~LogicalOrExpression();

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    LogicalOrExpression( Lexer::TerminalType operator_terminal,
                         std::unique_ptr<Expression> left_side,
                         std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// LogicalAndExpression
//------------------------------------------------------------------------------

class LogicalAndExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~LogicalAndExpression();

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    LogicalAndExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// InclusiveOrExpression
//------------------------------------------------------------------------------

class InclusiveOrExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~InclusiveOrExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    InclusiveOrExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// ExclusiveOrExpression
//------------------------------------------------------------------------------

class ExclusiveOrExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~ExclusiveOrExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    ExclusiveOrExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// AndExpression
//------------------------------------------------------------------------------

class AndExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~AndExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    AndExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// EqualityExpression
//------------------------------------------------------------------------------

class EqualityExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~EqualityExpression();

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    EqualityExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// RelationalExpression
//------------------------------------------------------------------------------

class RelationalExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~RelationalExpression();

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    RelationalExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// ShiftExpression
//------------------------------------------------------------------------------

class ShiftExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~ShiftExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    ShiftExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// AdditiveExpression
//------------------------------------------------------------------------------

class AdditiveExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~AdditiveExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    AdditiveExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// MultiplicativeExpression
//------------------------------------------------------------------------------

class MultiplicativeExpression : public JoeLang::Parser::BinaryOperatorExpression
{
public:
    virtual
    ~MultiplicativeExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    MultiplicativeExpression( Lexer::TerminalType operator_terminal,
                          std::unique_ptr<Expression> left_side,
                          std::unique_ptr<Expression> right_side );

    friend class BinaryOperatorExpression;
};

//------------------------------------------------------------------------------
// CastExpression
//------------------------------------------------------------------------------

class CastExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~CastExpression();

    virtual
    void Print( int depth ) const;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    explicit CastExpression( Type cast_type );

private:
    Type m_castType;
};

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

class UnaryExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~UnaryExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    UnaryExpression( std::unique_ptr<UnaryOperator> unary_operator,
                     std::unique_ptr<Expression> expression );

private:
    std::unique_ptr<UnaryOperator> m_unaryOperator;
    std::unique_ptr<Expression> m_expression;
};

//------------------------------------------------------------------------------
// UnaryOperator
//------------------------------------------------------------------------------

class UnaryOperator : public JoeLang::Parser::Token
{
public:
    virtual
    ~UnaryOperator();

    virtual
    void Print( int depth ) const;

    Lexer::TerminalType GetTerminalType() const;

    static
    bool Parse(Parser& parser, std::unique_ptr<UnaryOperator> &token );

protected:
    UnaryOperator( Lexer::TerminalType terminal_type );

private:
    Lexer::TerminalType m_terminalType;
};

//------------------------------------------------------------------------------
// PostfixExpression
//------------------------------------------------------------------------------

class PostfixExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~PostfixExpression();

    virtual
    void Print( int depth ) const;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    PostfixExpression( std::unique_ptr<Expression> expression,
                       std::unique_ptr<PostfixOperator> postfix_operator );

private:
    std::unique_ptr<Expression> m_expression;
    std::unique_ptr<PostfixOperator> m_postfixOperator;
};

//------------------------------------------------------------------------------
// PostfixOperator
//------------------------------------------------------------------------------

class PostfixOperator : public JoeLang::Parser::Token
{
public:
    virtual
    ~PostfixOperator();

    static
    bool Parse( Parser& parser, std::unique_ptr<PostfixOperator>& token );

protected:
    PostfixOperator( );
};

//------------------------------------------------------------------------------
// SubscriptOperator
//------------------------------------------------------------------------------
class SubscriptOperator : public JoeLang::Parser::PostfixOperator
{
public:
    virtual
    ~SubscriptOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<SubscriptOperator>& token );

protected:
    SubscriptOperator( std::unique_ptr<Expression> expression );

private:
    std::unique_ptr<Expression> m_expression;
};

//------------------------------------------------------------------------------
// ArgumentListOperator
//------------------------------------------------------------------------------
class ArgumentListOperator : public JoeLang::Parser::PostfixOperator
{
public:
    virtual
    ~ArgumentListOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<ArgumentListOperator>& token );

protected:
    ArgumentListOperator( std::vector< std::unique_ptr<Expression> > argument_expressions );

private:
    std::vector< std::unique_ptr<Expression> > m_argumentExpressions;
};

//------------------------------------------------------------------------------
// MemberAccessOperator
//------------------------------------------------------------------------------
class MemberAccessOperator : public JoeLang::Parser::PostfixOperator
{
public:
    virtual
    ~MemberAccessOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<MemberAccessOperator>& token );

protected:
    MemberAccessOperator( std::string identifier );

private:
    std::string m_identifier;
};

//------------------------------------------------------------------------------
// IncrementalOperator
//------------------------------------------------------------------------------
class IncrementalOperator : public JoeLang::Parser::PostfixOperator
{
public:
    virtual
    ~IncrementalOperator();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<IncrementalOperator>& token );

protected:
    IncrementalOperator( Lexer::TerminalType terminal_type );

private:
    Lexer::TerminalType m_terminalType;
};

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

class PrimaryExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~PrimaryExpression();

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    PrimaryExpression( );

private:
};

//------------------------------------------------------------------------------
// IdentifierExpression
//------------------------------------------------------------------------------

class IdentifierExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~IdentifierExpression();

    virtual
    void Print( int depth ) const;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    IdentifierExpression( std::string identifier );

private:
    std::string m_identifier;
};

//------------------------------------------------------------------------------
// LiteralExpression
//------------------------------------------------------------------------------

class LiteralExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~LiteralExpression();

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    LiteralExpression();
};

//------------------------------------------------------------------------------
// ConstantValueExpression
//------------------------------------------------------------------------------

class ConstantValueExpression : public JoeLang::Parser::Expression
{
public:
    virtual
    ~ConstantValueExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    ConstantValueExpression( std::shared_ptr<LiteralExpression> value );

private:
    std::shared_ptr<LiteralExpression> m_value;
};

//------------------------------------------------------------------------------
// IntegralLiteralExpression
//------------------------------------------------------------------------------

class IntegralLiteralExpression : public JoeLang::Parser::LiteralExpression
{
public:
    virtual
    ~IntegralLiteralExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<IntegralLiteralExpression>& token );

//protected:
    IntegralLiteralExpression( long long value );

private:
    long long m_value;
};

//------------------------------------------------------------------------------
// FloatingLiteralExpression
//------------------------------------------------------------------------------

class FloatingLiteralExpression : public JoeLang::Parser::LiteralExpression
{
public:
    virtual
    ~FloatingLiteralExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<FloatingLiteralExpression>& token );

//protected:
    FloatingLiteralExpression( double value );

private:
    double m_value;
};

//------------------------------------------------------------------------------
// BooleanLiteralExpression
//------------------------------------------------------------------------------

class BooleanLiteralExpression : public JoeLang::Parser::LiteralExpression
{
public:
    virtual
    ~BooleanLiteralExpression();

    virtual
    void Print( int depth ) const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_generator ) const override;

    virtual
    Type GetReturnType() const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<BooleanLiteralExpression>& token );

//protected:
    BooleanLiteralExpression( bool value );

private:
    bool m_value;
};

} // namespace Parser
} // namespace JoeLang
