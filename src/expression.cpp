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

#include "expression.hpp"

#include <memory>
#include "parser.hpp"
#include "lexer.hpp"
#include "terminal.hpp"

namespace JoeLang
{
namespace Parser
{

//------------------------------------------------------------------------------
// Expression
//------------------------------------------------------------------------------

Expression::Expression()
{
}

Expression::~Expression()
{
}

bool Expression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    return AssignmentExpression::Parse( parser, token );
    //std::unique_ptr<AssignmentExpression> assignment_token;
    //if( !AssignmentExpression::Parse( parser, assignment_token ) )
        //return false;
    //token.reset( assignment_token.release() );
    //return true;
}

//------------------------------------------------------------------------------
// Assignment Expression
//------------------------------------------------------------------------------

AssignmentExpression::AssignmentExpression( std::unique_ptr<Expression> unary_expression,
                                            std::unique_ptr<AssignmentOperator> assignment_operator,
                                            std::unique_ptr<Expression> assignment_expression )
{
}

AssignmentExpression::~AssignmentExpression()
{
}

bool AssignmentExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( parser.Expect<ConditionalExpression, Expression>( token ) )
        return true;

    std::unique_ptr<Expression> unary_expression;
    if( !parser.Expect<UnaryExpression, Expression>( unary_expression ) )
        return false;

    std::unique_ptr<AssignmentOperator> assignment_operator;
    if( !parser.Expect<AssignmentOperator>( assignment_operator ) )
        return false;

    std::unique_ptr<Expression> assignment_expression;
    if( !parser.Expect<AssignmentExpression, Expression>( assignment_expression ) )
        return false;

    token.reset( new AssignmentExpression( std::move( unary_expression ),
                                           std::move( assignment_operator ),
                                           std::move( assignment_expression ) ) );
    return true;
}

AssignmentOperator::AssignmentOperator( Lexer::TokenType token_type )
    :m_tokenType( token_type )
{
}

AssignmentOperator::~AssignmentOperator()
{
}

bool AssignmentOperator::Parse( Parser& parser, std::unique_ptr<AssignmentOperator>& token )
{
    std::unique_ptr<Token> t;
    std::unique_ptr<TerminalBase> terminal_base;
    if( !parser.ExpectAnyOf< Terminal<Lexer::EQUALS>,
                             Terminal<Lexer::MULTIPLY_EQUALS>,
                             Terminal<Lexer::DIVIDE_EQUALS>,
                             Terminal<Lexer::MODULUS_EQUALS>,
                             Terminal<Lexer::ADD_EQUALS>,
                             Terminal<Lexer::SUBTRACT_EQUALS>,
                             Terminal<Lexer::LSHIFT_EQUALS>,
                             Terminal<Lexer::RSHIFT_EQUALS>,
                             Terminal<Lexer::AND_EQUALS>,
                             Terminal<Lexer::XOR_EQUALS>,
                             Terminal<Lexer::OR_EQUALS> >( t ) )
        return false;
    terminal_base.reset( dynamic_cast<TerminalBase*>( t.release() ) );
    if( !terminal_base )
        return false;

    token.reset( new AssignmentOperator( terminal_base->GetTokenType() ) );
    return true;
}

//------------------------------------------------------------------------------
// Conditional Expression
//------------------------------------------------------------------------------

ConditionalExpression::ConditionalExpression()
{
}

ConditionalExpression::~ConditionalExpression()
{
}

bool ConditionalExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( !parser.Expect< Terminal<Lexer::IDENTIFIER> >() )
        return false;
    token.reset( new ConditionalExpression() );
    return true;
}

//------------------------------------------------------------------------------
// Unary Expression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression()
{
}

UnaryExpression::~UnaryExpression()
{
}

bool UnaryExpression::Parse( Parser& parser, std::unique_ptr<Expression>& token )
{
    if( !parser.Expect< Terminal<Lexer::IDENTIFIER> >() )
        return false;
    token.reset( new UnaryExpression() );
    return true;
}


} // namespace Parser
} // namespace JoeLang
