/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include <compiler/tokens/statements/statement.hpp>

namespace JoeLang
{
namespace Compiler
{
class CodeGenerator;
class CompleteType;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class ExpressionStatement;
using ExpressionStatement_up = std::unique_ptr<ExpressionStatement>;
class Parser;
class SemaAnalyzer;

/**
  * \class ExpressionStatement
  * \ingroup Statements
  * \brief Matches a Expression Statement
  *
  * ExpressionStatement = Expression ';'
  */
class ExpressionStatement : public JoeLang::Compiler::Statement
{
public:
    explicit
    ExpressionStatement    ( Expression_up expression );
    virtual
    ~ExpressionStatement   ();

    void PerformSema( SemaAnalyzer& sema );

    static
    bool Parse ( Parser& parser, ExpressionStatement_up& token );

private:
    Expression_up m_Expression;
};


} // namespace Compiler
} // namespace JoeLang
