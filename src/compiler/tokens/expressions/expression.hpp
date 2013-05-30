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
#include <set>

#include <compiler/tokens/token.hpp>

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

namespace llvm
{
    class Value;
};

namespace JoeLang
{
    namespace Compiler
    {
        class Expression;
        using Expression_up = std::unique_ptr<Expression>;

        class CodeGenerator;
        class CompleteType;
        class Parser;
        class SemaAnalyzer;
        class ShaderWriter;

        class Variable;
        using Variable_sp = std::shared_ptr<Variable>;

        class Function;
        using Function_sp = std::shared_ptr<Function>;
        
        class Node;
        class NodeManager;

    } // namespace Compiler
} // namespace JoeLang

namespace JoeLang
{
namespace Compiler
{

/**
  * \defgroup Expressions
  * \ingroup Tokens
  * Expressions parse part of expressions in statements.
  * The static member function Expression::Parse must take a pointer to the
  * abstract base class expression rather than to the more specific base class
  */

/**
  * \class Expression
  * \ingroup Expressions
  * \brief Abstract base class for all expressions
  *
  * Expression = AssignmentExpression
  */
// todo split this file up
class Expression : public JoeLang::Compiler::Token
{
public:
    Expression( TokenTy sub_class_id );
    virtual
    ~Expression();

    /**
      * Performs type checking and things.
      * \param sema
      *   The semantic analyzer to use
      * \returns true if there were no errors parsing
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) = 0;
    
    virtual
    const Node& GenerateCodeDag( NodeManager& node_manager ) const;

    /**
      * Generates an llvm value for this expression
      * \param code_gen
      *   A reference to a CodeGenerator
      * \returns the llvm value representing this expression
      */
    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const = 0;

    /**
      * Generates an llvm value for a pointer to the value of this expression
      * \param code_gen
      *   A reference to a CodeGenerator
      * \returns the llvm value representing this expression's value's pointer
      */
    virtual
    llvm::Value* CodeGenPointerTo( CodeGenerator& code_gen ) const;

    /**
      * Writes this expression to the shader
      */
    virtual
    void Write( ShaderWriter& shader_writer ) const = 0;

    /**
      * \returns the CompleteType which this expression evaluates to
      */
    virtual
    CompleteType GetType() const = 0;

    /**
      * \returns the list of functions called by this expression
      */
    virtual
    std::set<Function_sp> GetCallees() const = 0;

    /**
      * \returns the list of variables referenced by this expression
      */
    virtual
    std::set<Variable_sp> GetVariables() const = 0;

    /**
      * \returns the list of variables written to by this expression
      * \param is_assigned
      *   This is true when the expression is on the left hand side of the
      *   nearest assignment operator
      */
    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                           bool is_assigned = false ) const = 0;

    /**
      * \returns true if the Expression represents a l-value
      * by default this returns false
      */
    virtual
    bool IsLValue() const;

    /**
      * \returns true if this is a constexpr
      */
    virtual
    bool IsConst() const = 0;

    /**
      * Parses any expression
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                Expression_up& token );

    /** Used for casting **/
    static
    bool classof( const Token* t );
    static
    bool classof( const Expression* e );
};

} // namespace Compiler
} // namespace JoeLang
