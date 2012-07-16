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

#include <map>
#include <string>
#include <memory>
#include <utility>
#include <vector>

namespace JoeLang
{

class Context;
class StateBase;
enum class Type;

namespace Compiler
{

class CodeGenerator;
class Expression;
typedef std::unique_ptr<Expression> Expression_up;
class GenericValue;
class PassDefinition;
class TechniqueDefinition;
class TranslationUnit;
class Variable;

/**
  * \class SemaAnalyzer
  * \brief A class to perform semantic analysis on the AST
  */
class SemaAnalyzer
{
public:
    using PassDefinitionRef = std::shared_ptr<std::unique_ptr<PassDefinition> >;

    SemaAnalyzer( const Context& context, CodeGenerator& code_gen );
    ~SemaAnalyzer();

    /**
      * Insert implicit casts and verify types and resolve identifiers and all
      * \param cst
      *   The parse tree
      * \returns
      *   true if there were no errors
      */
    bool BuildAst( TranslationUnit& cst );

    /**
      * Declare and optionally define a pass.
      * This will error if one tries to define an already defined pass
      * \param name
      *   The name of the pass to be declared
      * \param definition
      *   The definition of the pass if it has one, otherwise nullptr
      */
    void DeclarePass( std::string name,
                      std::unique_ptr<PassDefinition> definition );

    /**
      * Declare and define a technique.
      * This will error if one tries to declare an already declared technique
      * \param name
      *   The name of the technique to be declared
      */
    void DeclareTechnique( std::string name );

    /**
      * Check to see if a pass has been declared
      * \param name
      *   The name of the pass
      * \returns true if the pass has been declared
      */
    bool HasPass( const std::string& name ) const;

    /**
      * Gets the named pass
      * \param name
      *   The name of the pass
      * \returns the pass definition or nullptr if there is no pass by that name
      */
    PassDefinitionRef GetPass( const std::string& name ) const;

    /**
      * Check to see if a state has been given to context
      * \param name
      *   The name of the state
      * \returns true if the state has been declared
      */
    bool HasState( const std::string& name ) const;

    /**
      * Gets the named state or nullptr
      * \param name
      *   The name of the state to get
      * \returns the named state or nullptr if there is no state by that name
      */
    const StateBase* GetState( const std::string& name ) const;

    /**
      * Puts the state's enumerants into the scope as constant variables
      * \param state
      *   The state from which to load the enumerants
      */
    void LoadStateEnumerants( const StateBase& state );

    /**
      * Declares a variable
      * \param identifier
      *   The identifier for the variable
      * \param variable
      *   The variable object
      */
    void DeclareVariable( const std::string& identifier,
                          std::shared_ptr<Variable> variable );

    /**
      * \param identifier
      *   The name of the variable
      * \returns the Variable associated with the identifier, or nullptr if
      *   there is no variable with that name
      */
    std::shared_ptr<Variable> GetVariable( const std::string& identifier );

    /**
      * Creates a new scope on the scope stack
      */
    void EnterScope();

    /**
      * Pops the innermost scope from the stack
      */
    void LeaveScope();

    /**
      * \returns true if we are in the top scope
      */
    bool InGlobalScope() const;

    /**
      * Evaluates an expression using llvm
      * \param expression
      *   The expression to evaluate
      * \returns the llvm genericvalue containing the expression's result
      *
      * This function asserts that expression is const
      */
    GenericValue EvaluateExpression( const Expression& expression );

    /**
      * Resolves identifiers, folds constants and casts to the requested type
      * \param expression
      *   The expression to resolve
      * \param type
      *   The type to cast to
      * \returns true if the expression represents an identifier
      */
    bool TryResolveToLiteral( Expression_up& expression,
                              Type type );

    /**
      * Reports an error.
      * Sets m_Good to false
      * \param error_message
      *   The error message
      */
    void Error( const std::string& error_message );

    /**
      * Reports a warning
      * \param warning_message
      *   The warning message
      */
    void Warning( const std::string& warning_message );

    /**
      * Checks the error state
      * \returns true if there has been no call to Error
      */
    bool Good() const;

    /**
      * \returns the CodeGenerator object
      */
    CodeGenerator& GetCodeGenerator();
private:
    struct SymbolMaps
    {
        std::map<std::string, std::shared_ptr<Variable> > m_Variables;
    };

    using PassDefinitionMap = std::map< std::string, PassDefinitionRef >;

    PassDefinitionMap        m_PassDefinitions;
    std::vector<std::string> m_Techniques;

    std::vector<SymbolMaps>  m_SymbolStack;

    bool m_Good = true;

    const Context& m_Context;
    CodeGenerator& m_CodeGenerator;
};

} // namespace Compiler
} // namespace JoeLang
