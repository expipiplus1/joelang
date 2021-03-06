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
#include <set>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

class CodeGenerator;
class CompleteType;
class Function;
using Function_sp = std::shared_ptr<Function>;
class Parser;
class SemaAnalyzer;
class ShaderWriter;
class Statement;
using Statement_up = std::unique_ptr<Statement>;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

/**
  * \defgroup Statements
  * \ingroup Tokens
  */

/**
  * \class Statement
  * \ingroup Statements
  * \brief Matches any kind of statement
  *
  * Statement = CompoundStatement | ReturnStatement
  */
class Statement : public JoeLang::Compiler::Token
{
public:
    explicit
    Statement    ( TokenTy sub_class_id );
    virtual
    ~Statement   ();

    /**
      * \returns true if this statement will always return from the function
      */
    virtual
    bool AlwaysReturns() const = 0;

    /**
      * Returns all the functions called by this statement
      */
    virtual
    std::set<Function_sp> GetCallees() const = 0;

    /**
      * Returns all the variables referenced by this statement
      */
    virtual
    std::set<Variable_sp> GetVariables() const = 0;

    /**
      * Returns all the variables written to by this statement
      */
    virtual
    std::set<Variable_sp> GetWrittenToVariables() const = 0;

    virtual
    void PerformSema( SemaAnalyzer& sema, const CompleteType& return_type ) = 0;

    /**
      *
      */
    virtual
    void CodeGen( CodeGenerator& code_gen ) = 0;

    virtual
    void Write( ShaderWriter& shader_writer ) const = 0;

    static
    bool Parse ( Parser& parser, Statement_up& token );

    /** Used for casting **/
    static
    bool classof( const Token* t );
    static
    bool classof( const Statement* d );
};


} // namespace Compiler
} // namespace JoeLang
