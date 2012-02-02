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

#include <parser/terminal_types.hpp>
#include <parser/tokens/token.hpp>

namespace JoeLang
{
namespace Parser
{

class Parser;

//class UnaryExpression;
class AssignmentOperator;
class UnaryOperator;

//------------------------------------------------------------------------------
// Expression
// All Classes deriving from Expression will fill a pointer to Expression when
// Parse is called, because they're used in a polymorphic way.
//------------------------------------------------------------------------------

class Expression : public JoeLang::Parser::Token
{
public:
    virtual ~Expression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    Expression( );
};

//------------------------------------------------------------------------------
// AssignmentExpression
//------------------------------------------------------------------------------

class AssignmentExpression : public JoeLang::Parser::Expression
{
public:
    virtual ~AssignmentExpression();

    virtual void Print( int depth ) const;

    static bool Parse(Parser& parser, std::unique_ptr<Expression> &token );

protected:
    AssignmentExpression( std::unique_ptr<Expression> unary_expression,
                          std::unique_ptr<AssignmentOperator> assignment_operator,
                          std::unique_ptr<Expression> assignment_expression );

private:
    std::unique_ptr<Expression> m_unaryExpression;
    std::unique_ptr<AssignmentOperator> m_assignmentOperator;
    std::unique_ptr<Expression> m_assignmentExpression;
};

//------------------------------------------------------------------------------
// AssignmentOperator
//------------------------------------------------------------------------------

class AssignmentOperator : public JoeLang::Parser::Token
{
public:
    virtual ~AssignmentOperator();

    virtual void Print( int depth ) const;

    static bool Parse(Parser& parser, std::unique_ptr<AssignmentOperator> &token );

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
    virtual ~ConditionalExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~BinaryOperatorExpression();

    virtual void Print( int depth ) const;

    template< typename ExpressionType, typename SubExpressionType >
    static bool ParseLeftAssociative( Parser& parser, std::unique_ptr<Expression>& token,
                                      const std::vector<Lexer::TerminalType>& operator_terminals );

    template< typename ExpressionType, typename SubExpressionType >
    static bool ParseRightAssociative( Parser& parser, std::unique_ptr<Expression>& token,
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
    virtual ~LogicalOrExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~LogicalAndExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~InclusiveOrExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~ExclusiveOrExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~AndExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~EqualityExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~RelationalExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~ShiftExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~AdditiveExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~MultiplicativeExpression();

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

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
    virtual ~CastExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    CastExpression( );
};

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

class UnaryExpression : public JoeLang::Parser::Expression
{
public:
    virtual ~UnaryExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    UnaryExpression( std::unique_ptr<UnaryOperator> unary_operator,
                     std::unique_ptr<Expression> unary_expression );

private:
    std::unique_ptr<UnaryOperator> m_unaryOperator;
    std::unique_ptr<Expression> m_unaryExpression;
};

//------------------------------------------------------------------------------
// UnaryOperator
//------------------------------------------------------------------------------

class UnaryOperator : public JoeLang::Parser::Token
{
public:
    virtual ~UnaryOperator();

    virtual void Print( int depth ) const;

    static bool Parse(Parser& parser, std::unique_ptr<UnaryOperator> &token );

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
    virtual ~PostfixExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    PostfixExpression( );
};

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

class PrimaryExpression : public JoeLang::Parser::Expression
{
public:
    virtual ~PrimaryExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    PrimaryExpression( std::string identifier );

private:
    std::string m_identifier;
};

} // namespace Parser
} // namespace JoeLang
