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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{

class Parser;

/** \enum AssignmentOperator
  *   An enum for any kind of assignment operator **/
enum class AssignmentOperator
{
    EQUALS,
    SHL_EQUALS,
    SHR_EQUALS,
    AND_EQUALS,
    XOR_EQUALS,
    OR_EQUALS,
    PLUS_EQUALS,
    MINUS_EQUALS,
    MULTIPLY_EQUALS,
    DIVIDE_EQUALS,
    MODULO_EQUALS
};

/**
  * \class AssignmentOperator
  * \ingroup Tokens
  * \brief matches an assignment operator
  *
  * AssignmentExpression = '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' |
  *                        '>>=' | '&=' | '^=' | '|='
  */
class AssignmentOperatorToken : public JoeLang::Compiler::Token
{
public:

    /**
      * \param assignment_operator
      *   The operator
      */
    AssignmentOperatorToken( AssignmentOperator assignment_operator );
    virtual
    ~AssignmentOperatorToken();

    AssignmentOperator GetOp() const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<AssignmentOperatorToken>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const AssignmentOperatorToken* d );
private:
    AssignmentOperator m_Operator;
};

} // namespace Compiler
} // namespace JoeLang
