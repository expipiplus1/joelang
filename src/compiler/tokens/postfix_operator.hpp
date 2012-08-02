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
#include <string>
#include <vector>

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

class CodeGenerator;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
typedef std::shared_ptr<Expression> Expression_sp;
class Parser;
class SemaAnalyzer;

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
    bool PerformSema( SemaAnalyzer& sema,
                      const Expression_up& expression ) = 0;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression_up& expression ) = 0;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression_up& expression );

    virtual
    Type GetReturnType( const Expression_up& expression ) const = 0;

    /// todo make these all take references
    virtual
    Type GetUnderlyingType( const Expression_up& expression ) const = 0;

    virtual
    const std::vector<unsigned>& GetArrayExtents(
                                    const Expression_up& expression) const = 0;

    virtual
    bool IsConst( const Expression& expression ) const = 0;

    /**
      * Returns false by default
      */
    virtual
    bool IsLValue( const Expression& expression ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<PostfixOperator>& token );

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
    bool PerformSema( SemaAnalyzer& sema,
                      const Expression_up& expression ) override;

    //TODO pass expression as reference instead of pointer
    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen,
                                   const Expression_up& expression ) override;

    virtual
    Type GetReturnType( const Expression_up& expression ) const override;

    virtual
    Type GetUnderlyingType( const Expression_up& expression ) const override;

    virtual
    const std::vector<unsigned>& GetArrayExtents(
                            const Expression_up& expression ) const override;

    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    bool IsLValue( const Expression& expression ) const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<SubscriptOperator>& token );
private:
    Expression_up           m_IndexExpression;
    std::vector<unsigned>   m_ArrayExtents;
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
    using ArgumentExpressionVector = std::vector< Expression_up >;

    ArgumentListOperator( ArgumentExpressionVector argument_expressions );
    virtual
    ~ArgumentListOperator();

    virtual
    bool PerformSema( SemaAnalyzer& sema,
                      const Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression_up& expression ) override;

    virtual
    Type GetReturnType( const Expression_up& expression ) const override;

    virtual
    Type GetUnderlyingType( const Expression_up& expression ) const override;

    virtual
    const std::vector<unsigned>& GetArrayExtents(
                            const Expression_up& expression ) const override;

    /** \returns false **/
    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<ArgumentListOperator>& token );
private:
    ArgumentExpressionVector m_ArgumentExpressions;
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
    bool PerformSema( SemaAnalyzer& sema,
                      const Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression_up& expression ) override;

    virtual
    Type GetReturnType( const Expression_up& expression ) const override;

    virtual
    Type GetUnderlyingType( const Expression_up& expression ) const override;

    virtual
    const std::vector<unsigned>& GetArrayExtents(
                            const Expression_up& expression ) const override;

    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<MemberAccessOperator>& token );
private:
    std::string m_Identifier;
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
                      const Expression_up& expression ) override;

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen,
                          const Expression_up& expression ) override;

    virtual
    Type GetReturnType( const Expression_up& expression ) const override;

    virtual
    Type GetUnderlyingType( const Expression_up& expression ) const override;

    virtual
    const std::vector<unsigned>& GetArrayExtents(
                            const Expression_up& expression ) const override;

    /** \returns false **/
    virtual
    bool IsConst( const Expression& expression ) const override;

    virtual
    void Print( int depth ) const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IncrementOrDecrementOperator>& token );
private:
    Op m_Operator;
};

} // namespace Compiler
} // namespace JoeLang