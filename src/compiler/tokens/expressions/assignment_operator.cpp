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

#include "assignment_operator.hpp"

#include <iostream>
#include <memory>
#include <utility>
#include <vector>

#include <compiler/parser.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// AssignmentOperator
//------------------------------------------------------------------------------

AssignmentOperator::AssignmentOperator( Op op )
    :Token( TokenTy::AssignmentOperator )
    ,m_Operator(op)
{
}

AssignmentOperator::~AssignmentOperator()
{
}

AssignmentOperator::Op AssignmentOperator::GetOp() const
{
    return m_Operator;
}

bool AssignmentOperator::Parse( Parser& parser,
                                std::unique_ptr<AssignmentOperator>& token )
{
    // A vector of terminals for assignment operators and their Op enumerants
    static const std::vector< std::pair<TerminalType, Op> >
           s_assignment_operator_terminals =
    {
        { TerminalType::EQUALS,             Op::EQUALS },
        { TerminalType::PLUS_EQUALS,        Op::PLUS_EQUALS },
        { TerminalType::MINUS_EQUALS,       Op::MINUS_EQUALS },
        { TerminalType::MULTIPLY_EQUALS,    Op::MULTIPLY_EQUALS },
        { TerminalType::DIVIDE_EQUALS,      Op::DIVIDE_EQUALS },
        { TerminalType::MODULO_EQUALS,      Op::MODULO_EQUALS },
        { TerminalType::LEFT_SHIFT_EQUALS,  Op::SHL_EQUALS },
        { TerminalType::RIGHT_SHIFT_EQUALS, Op::SHR_EQUALS },
        { TerminalType::AND_EQUALS,         Op::AND_EQUALS },
        { TerminalType::INCLUSIVE_OR_EQUALS, Op::OR_EQUALS },
        { TerminalType::EXCLUSIVE_OR_EQUALS, Op::XOR_EQUALS }
    };

    // Try and match any of these operators
    for( const auto& p : s_assignment_operator_terminals )
        if( parser.ExpectTerminal( p.first ) )
        {
            token.reset( new AssignmentOperator( p.second ) );
            return true;
        }
    return false;
}

} // namespace Compiler
} // namespace JoeLang
