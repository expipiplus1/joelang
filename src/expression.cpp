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

#include <iostream>
#include <ostream>
#include <string>

namespace JoeLang
{
    Expression::Expression()
    {
    }

    Expression::~Expression()
    {
    }

    std::string Expression::Indent( unsigned int l )
    {
        return std::string( l * 2, ' ' );
    }


    ConstantExpression::ConstantExpression( int value )
        : Expression()
        , m_value( value )
    {
    }

    ConstantExpression::~ConstantExpression()
    {
    }

    int ConstantExpression::Evaluate() const
    {
        return m_value;
    }

    void ConstantExpression::Print(std::ostream &out, unsigned int depth) const
    {
        out << Indent( depth ) << m_value << std::endl;
    }

    BinaryExpression::BinaryExpression( Expression* lhs, Expression* rhs, BinaryOperator binary_operator )
        : Expression()
        , m_lhs( lhs )
        , m_rhs( rhs )
        , m_binaryOperator( binary_operator )
    {
    }

    BinaryExpression::~BinaryExpression()
    {
        delete m_lhs;
        delete m_rhs;
    }

    int BinaryExpression::Evaluate() const
    {
        switch( m_binaryOperator )
        {
            case BinaryOperator::ADDITION:
                return m_lhs->Evaluate() + m_rhs->Evaluate();
            case BinaryOperator::SUBTRACTION:
                return m_lhs->Evaluate() - m_rhs->Evaluate();
            case BinaryOperator::MULTIPLICATION:
                return m_lhs->Evaluate() * m_rhs->Evaluate();
            case BinaryOperator::DIVISION:
                return m_lhs->Evaluate() / m_rhs->Evaluate();
            case BinaryOperator::MODULO:
                return m_lhs->Evaluate() % m_rhs->Evaluate();
            default:
                std::cerr << "ERROR: Unhandled binary operator." << std::endl;
                return 0;
        }
    }

    void    BinaryExpression::Print(std::ostream &out, unsigned int depth) const
    {
        switch( m_binaryOperator )
        {
            case BinaryOperator::ADDITION:
                out << Indent( depth ) << "+ add\n";
                break;
            case BinaryOperator::SUBTRACTION:
                out << Indent( depth ) << "- subtract\n";
                break;
            case BinaryOperator::MULTIPLICATION:
                out << Indent( depth ) << "* multiply\n";
                break;
            case BinaryOperator::DIVISION:
                out << Indent( depth ) << "/ divide\n";
                break;
            case BinaryOperator::MODULO:
                out << Indent( depth ) << "% modulo\n";
                break;
        }

        m_lhs->Print( out, depth + 1 );
        m_rhs->Print( out, depth + 1 );
    }

} // namespace JoeLang
