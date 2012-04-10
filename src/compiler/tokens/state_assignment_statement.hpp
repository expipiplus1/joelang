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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

class Expression;
class Parser;

/**
  * \class StateAssignmentStatement
  * \brief Matches a state assignment statement
  *
  * StateAssignmentStatement = identifier '=' Expression ';'
  */
class StateAssignmentStatement : public JoeLang::Compiler::Token
{
public:
    /**
      * This constructor will assert on a null expression or empty identifier
      * \param identifier
      *   The identifier for the State to assign
      * \param expression
      *   The expression to assign to the State
      */
    StateAssignmentStatement( std::string identifier,
                              std::unique_ptr<Expression> expression );
    virtual
    ~StateAssignmentStatement();

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual void Print( int depth ) const;

    /**
      * Parses a state assignment statement
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static bool Parse( Parser& parser,
                       std::unique_ptr<StateAssignmentStatement>& token );

private:
    /** The identifier for the state to be assigned to **/
    std::string m_identifier;
    /** The expression to assign to the state **/
    std::unique_ptr<Expression> m_expression;
};

} // namespace Compiler
} // namespace JoeLang
