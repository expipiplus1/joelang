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
class ReturnStatement;
using ReturnStatement_up = std::unique_ptr<ReturnStatement>;
class Parser;
class SemaAnalyzer;

/**
  * \class ReturnStatement
  * \ingroup Statements
  * \brief Matches a ReturnStatement
  *
  * ReturnStatement = 'return' Expression ';'
  */
class ReturnStatement : public JoeLang::Compiler::Statement
{
public:
    explicit
    ReturnStatement    ( Expression_up expression );
    virtual
    ~ReturnStatement   ();

    virtual
    bool AlwaysReturns() const override;

    virtual
    std::set<Function_sp> GetCallees() const override;

    virtual
    std::set<Variable_sp> GetVariables() const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables() const override;

    virtual
    void PerformSema( SemaAnalyzer& sema,
                      const CompleteType& return_type ) override;

    virtual
    void CodeGen( CodeGenerator& code_gen ) override;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    static
    bool Parse ( Parser& parser, ReturnStatement_up& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const ReturnStatement* d );
private:
    Expression_up m_Expression;
};


} // namespace Compiler
} // namespace JoeLang
