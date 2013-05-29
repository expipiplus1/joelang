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

#include <compiler/tokens/expressions/assignment_operator.hpp>
#include <compiler/tokens/expressions/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

/**
  * \class AssignmentExpression
  * \ingroup Expressions
  * \brief matches an assignment expression
  *
  * AssignmentExpression =   ConditionalExpression
  *                        | UnaryExpression AssignmentOperator
  *                          AssignmentExpression
  */
class AssignmentExpression : public JoeLang::Compiler::Expression
{
public:
    /**
      * This constructor asserts on any null pointers
      * \param assignee
      *   The assignee
      * \param assignment_operator
      *   The assignment operator
      * \param assigned_expression
      *   The assigned Expression
      */
    AssignmentExpression( Expression_up assignee,
                          AssignmentOperator assignment_operator,
                          Expression_up assigned_expression );
    virtual
    ~AssignmentExpression();

    /**
      * For expressions such as 'a += b' this is inteerpreted as 'a = a+b', with
      * m_AssignedExpression being replaced with a+b and ownership of a being
      * given to m_AssignedExpression and a pointer being kept in m_AssigneePtr,
      * In the case of 'a = b' m_Assignee keeps the ownership;
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const override;

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

    virtual
    bool IsLValue() const override;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const AssignmentExpression* e );
private:
    Expression_up m_Assignee;
    AssignmentOperator m_AssignmentOperator;
    Expression_up m_AssignedExpression;
};

} // namespace Compiler
} // namespace JoeLang
