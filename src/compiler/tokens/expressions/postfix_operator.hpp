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

#include <array>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include <compiler/swizzle.hpp>
#include <compiler/tokens/token.hpp>

namespace llvm
{
    class Value;
}

namespace JoeLang
{
enum class Type;

namespace Compiler
{

typedef std::vector<unsigned> ArrayExtents;
class CodeGenerator;
class CompleteType;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class Function;
using Function_sp = std::shared_ptr<Function>;
class Parser;
class SemaAnalyzer;
class ShaderWriter;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

/**
  * \defgroup PostfixOperators
  * \ingroup Tokens
  */

/**
  * \class PostfixOperator
  * \ingroup PostfixOperators
  * \brief Matches any postfix operator
  *
  * PostfixOperator =   SubscriptOperator
  *                   | ArgumentListOperator
  *                   | MemberAccessOperator
  *                   | IncrementOrDecrementOperator
  */
class PostfixOperator : public JoeLang::Compiler::Token
{
public:
    PostfixOperator( TokenTy sub_class_id );
    virtual
    ~PostfixOperator();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema,
                             Expression_up& expression ) = 0;

    /** The postfix operator must performsema on the expression **/
    virtual
    bool PerformSema( SemaAnalyzer& sema, Expression_up& expression ) = 0;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression& expression ) = 0;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression& expression ) = 0;

    virtual
    void Write( ShaderWriter& shader_writer,
                const Expression& expression ) const = 0;

    virtual
    CompleteType GetType( const Expression& expression ) const = 0;

    /**
      * \returns the list of functions called by this operator
      */
    virtual
    std::set<Function_sp> GetCallees( const Expression& expression ) const = 0;

    /**
      * \returns the list of variables referenced by this operator
      */
    virtual
    std::set<Variable_sp> GetVariables(
                                       const Expression& expression ) const = 0;

    /**
      * \returns the list of variables written to by this operator
      */
    virtual
    std::set<Variable_sp> GetWrittenToVariables( const Expression& expression,
                                                 bool is_assigned ) const = 0;

    virtual
    bool IsConst( const Expression& expression ) const = 0;

    /**
      * Returns false by default
      */
    virtual
    bool IsLValue( const Expression& expression ) const;

    static
    bool Parse( Parser& parser, std::unique_ptr<PostfixOperator>& token );

    static
    bool classof( const Token* e );
    static
    bool classof( const PostfixOperator* e );
};

/**
  * \class SubscriptOperator
  * \ingroup PostfixOperators
  * \brief Matches an array subscript operator
  *
  * SubscriptOperator = '[' Expression ']'
  */
class SubscriptOperator : public JoeLang::Compiler::PostfixOperator
{

public:
    /**
      * This constructor asserts on a null expression
      * \param expression
      *   The index expression
      */
    SubscriptOperator( Expression_up index_expression );
    virtual
    ~SubscriptOperator();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema,
                             Expression_up& expression ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema, Expression_up& expression ) override;

    //TODO pass expression as reference instead of pointer
    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression& expression ) override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression& expression ) override;

    virtual
    void Write( ShaderWriter& shader_writer,
                const Expression& expression ) const override;

    virtual
    CompleteType GetType( const Expression& expression ) const override;

    virtual
    std::set<Function_sp> GetCallees(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetVariables(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                              const Expression& expression,
                                              bool is_assigned ) const override;

    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    bool IsLValue( const Expression& expression ) const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<SubscriptOperator>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const SubscriptOperator* d );
private:
    Expression_up  m_IndexExpression;
    ArrayExtents   m_ArrayExtents;
};

/**
  * \class ArgumentListOperator
  * \ingroup PostfixOperators
  * \brief Matches an argument list
  *
  * ArgumentListOperator = '(' ( AssignmentExpression
  *                                 (',' AssignmentExpression)* )? ')'
  */
class ArgumentListOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    using ArgumentExpressionVector = std::vector<Expression_up>;

    ArgumentListOperator( ArgumentExpressionVector arguments );
    virtual
    ~ArgumentListOperator();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema,
                             Expression_up& expression ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema,
                      Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression& expression ) override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression& expression ) override;

    virtual
    void Write( ShaderWriter& shader_writer,
                const Expression& expression ) const override;

    virtual
    CompleteType GetType( const Expression& expression ) const override;

    virtual
    std::set<Function_sp> GetCallees(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetVariables(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                              const Expression& expression,
                                              bool is_assigned ) const override;

    /** \returns false **/
    virtual
    bool IsConst( const Expression& expression ) const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<ArgumentListOperator>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const ArgumentListOperator* d );
private:
    ArgumentExpressionVector m_Arguments;

    Function_sp m_Function;
};

/**
  * \class MemberAccessOperator
  * \ingroup PostfixOperators
  * \brief Matches a member accessor
  *
  * MemberAccessOperator = '.' identifier
  */
class MemberAccessOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    /**
      * This constructor asserts on a null identifier
      * \param identifier
      *   The identifier for the member access
      */
    MemberAccessOperator( std::string identifier );
    virtual
    ~MemberAccessOperator();

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema,
                             Expression_up& expression ) override;

    virtual
    bool PerformSema( SemaAnalyzer& sema,
                      Expression_up& expression ) override;

    bool IsSwizzle() const;

    //
    // this asserts that IsSwizzle()
    //
    Swizzle GetSwizzle() const;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression& expression ) override;

    //
    // If this is a swizzle mask this returns the pointer to the vector
    // The code generator must apply the swizzle mask seperately
    //
    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression& expression ) override;

    virtual
    void Write( ShaderWriter& shader_writer,
                const Expression& expression ) const override;

    virtual
    CompleteType GetType( const Expression& expression ) const override;

    virtual
    std::set<Function_sp> GetCallees(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetVariables(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                              const Expression& expression,
                                              bool is_assigned ) const override;

    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    bool IsLValue( const Expression& expression ) const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<MemberAccessOperator>& token );

    static
    bool classof( const PostfixOperator* t );
    static
    bool classof( const MemberAccessOperator* d );
private:
    bool PerformSemaSwizzle( SemaAnalyzer& sema, Expression_up& expression );

    std::string m_Identifier;
    Swizzle m_Swizzle;
};

/**
  * \class IncrementOrDecrementOperator
  * \ingroup PostfixOperators
  * \brief Matches a post increment or post decrement operator
  *
  * IncrementOrDecrementOperator = '++' | '--'
  */
class IncrementOrDecrementOperator : public JoeLang::Compiler::PostfixOperator
{
public:
    enum class Op
    {
        INCREMENT,
        DECREMENT
    };
    IncrementOrDecrementOperator( Op terminal_type );
    virtual
    ~IncrementOrDecrementOperator();

    virtual
    bool PerformSema( SemaAnalyzer& sema,
                      Expression_up& expression ) override;

    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema,
                             Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression& expression ) override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression& expression ) override;

    virtual
    void Write( ShaderWriter& shader_writer,
                const Expression& expression ) const override;

    virtual
    CompleteType GetType( const Expression& expression ) const override;

    virtual
    std::set<Function_sp> GetCallees(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetVariables(
                                  const Expression& expression ) const override;

    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                              const Expression& expression,
                                              bool is_assigned ) const override;

    /** \returns false **/
    virtual
    bool IsConst( const Expression& expression ) const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IncrementOrDecrementOperator>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const IncrementOrDecrementOperator* d );
private:
    Op m_Operator;
};

} // namespace Compiler
} // namespace JoeLang
