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
#include <vector>

#include <compiler/tokens/statements/statement.hpp>

namespace JoeLang
{
namespace Compiler
{
class CodeGenerator;
class CompleteType;
class CompoundStatement;
typedef std::unique_ptr<CompoundStatement> CompoundStatement_up;
class Parser;
class SemaAnalyzer;

/**
  * \class CompoundStatement
  * \ingroup Statements
  * \brief Matches a CompoundStatement
  *
  * CompoundStatement = '{' Statement* '}'
  */
class CompoundStatement : public JoeLang::Compiler::Statement
{
public:
    CompoundStatement    ( std::vector<Statement_up> statements );
    virtual
    ~CompoundStatement   ();

    virtual
    bool AlwaysReturns() const override;

    virtual
    void PerformSema( SemaAnalyzer& sema, 
                      const CompleteType& return_type ) override;

    void PerformSemaAsFunction( SemaAnalyzer& sema,
                                const CompleteType& return_type );

    virtual
    void CodeGen( CodeGenerator& code_gen ) override;

    static
    bool Parse ( Parser& parser, CompoundStatement_up& token );

private:
    void PerformSemaCommon( SemaAnalyzer& sema,
                            const CompleteType& return_type,
                            bool must_return );

    std::vector<Statement_up> m_Statements;
};


} // namespace Compiler
} // namespace JoeLang
