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
#include "terminal_types.hpp"
#include "token.hpp"

namespace JoeLang
{
namespace Parser
{

class Parser;

class UnaryExpression;
class AssignmentOperator;

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
// Assignment Expression
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
// Assignment Operator
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
// Conditional Expression
//------------------------------------------------------------------------------

class ConditionalExpression : public JoeLang::Parser::Expression
{
public:
    virtual ~ConditionalExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    ConditionalExpression( );
};

//------------------------------------------------------------------------------
// Unary Expression
//------------------------------------------------------------------------------

class UnaryExpression : public JoeLang::Parser::Expression
{
public:
    virtual ~UnaryExpression();

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<Expression>& token );

protected:
    UnaryExpression( );
};

} // namespace Parser
} // namespace JoeLang
