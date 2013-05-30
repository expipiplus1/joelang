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

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/tokens/expressions/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

class CastExpression;
using CastExpression_up = std::unique_ptr<CastExpression>;

/**
  * \class CastExpression
  * \ingroup Expressions
  * \brief Matches a c-style cast expression
  *
  * This token is unimplemented and will just parse a UnaryExpression.
  * The class is used for representing casts though.
  *
  * CastExpression = '(' Type ')' UnaryExpression
  */
class CastExpression : public JoeLang::Compiler::Expression
{
public:
    /** This asserts that expression is not null **/
    CastExpression( CompleteType cast_type,
                    Expression_up expression,
                    bool is_explicit );
    virtual
    ~CastExpression();

    bool PerformSemaNoRecurse( SemaAnalyzer& sema );

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;
    
    virtual
    const Node& GenerateCodeDag( NodeManager& node_manager ) const override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    std::set<Function_sp> GetCallees() const override;

    virtual
    std::set<Variable_sp> GetVariables() const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables( bool is_assigned ) const override;

    virtual
    bool IsConst() const override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    bool IsExplicit() const; 
    
    static
    CastExpression_up Create( const CompleteType& cast_type,
                              Expression_up expression,
                              bool is_explicit );

    static
    CastExpression_up Create( Type cast_type,
                              Expression_up expression,
                              bool is_explicit );

    /**
      * Casts vectors to a different base type, preserving size
      */
    static
    CastExpression_up CreateBaseTypeCast( Type base_type,
                                          Expression_up cast_expression,
                                          bool is_explicit );

    static
    bool classof( const Expression* e );
    static
    bool classof( const CastExpression* e );

private:
    bool CanCastFromScalar( SemaAnalyzer& sema );
    bool CanCastFromVector( SemaAnalyzer& sema );
    bool CanCastFromMatrix( SemaAnalyzer& sema );
    bool CanCastFromStruct( SemaAnalyzer& sema );
    bool CanCastFromArray ( SemaAnalyzer& sema );
    bool CanCastFromString( SemaAnalyzer& sema );
    bool CanCastFromVoid  ( SemaAnalyzer& sema );

    CompleteType m_CastType;
    Expression_up m_Expression;
    bool m_IsExplicit;
};

} // namespace Compiler
} // namespace JoeLang
