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

#include <compiler/code_generator.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/semantic.hpp>

namespace JoeLang
{

class Context;
class StateBase;
enum class Type;

namespace Compiler
{

using ArrayExtents = std::vector<unsigned>;
class CodeGenerator;
class CompoundStatement;
using CompoundStatement_up = std::unique_ptr<CompoundStatement>;
class Expression;
using Expression_up = std::unique_ptr<Expression>;
class GenericValue;
class Function;
using Function_sp = std::shared_ptr<Function>;
class Initializer;
class PassDefinition;
class Runtime;
class TechniqueDefinition;
class TechniqueDeclaration;
using TechniqueDeclaration_up = std::unique_ptr<TechniqueDeclaration>;
class PassDeclaration;
using PassDeclaration_up = std::unique_ptr<PassDeclaration>;
class VariableDeclarationList;
using VariableDeclarationList_up = std::unique_ptr<VariableDeclarationList>;
class FunctionDefinition;
using FunctionDefinition_up = std::unique_ptr<FunctionDefinition>;
class TranslationUnit;
class Variable;
using Variable_sp = std::shared_ptr<Variable>;

/**
  * \class SemaAnalyzer
  * \brief A class to perform semantic analysis on the AST
  */
class SemaAnalyzer
{
public:
    using PassDefinitionRef = std::shared_ptr<std::unique_ptr<PassDefinition> >;

    SemaAnalyzer( const Context& context, Runtime& runtime );
    ~SemaAnalyzer();

    /**
      * Extract all the information from this translation unit.
      * Also put in implicit casts, verify typing, resolve identifiers, etc
      * \returns
      *   true if there were no errors
      */
    bool Analyze( TranslationUnit& tu );

    /**
      * Transfers ownership of a various declaration to the analyzer
      * Sema will be performed on these
      */
    void AddTechniqueDeclaration( std::unique_ptr<TechniqueDeclaration> t );

    void AddPassDeclaration( std::unique_ptr<PassDeclaration> p );

    void AddVariableDeclarations( std::unique_ptr<VariableDeclarationList> v );

    void AddFunctionDefinition( std::unique_ptr<FunctionDefinition> f );

    const std::vector<TechniqueDeclaration_up>& GetTechniqueDeclarations() const;

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
                          Variable_sp variable );

    /**
      * \param identifier
      *   The name of the variable
      * \returns the Variable associated with the identifier, or nullptr if
      *   there is no variable with that name
      */
    std::shared_ptr<Variable> GetVariable( const std::string& identifier );

    /**
      * Declares a function
      * \param identifier
      *   The Identifier for the function
      * \param return_type
      *   The function's return type
      * \param semantic
      *   The function's semantic
      */
    void DeclareFunction( std::string identifier,
                          CompleteType return_type,
                          Semantic semantic,
                          std::vector<CompleteType> parameter_types );

    /**
      * This asserts that it can find the required function
      * \param identifier
      *   The name of the function
      * \param parameters
      *   The parameters of the function, the types of these are used to find
      *   the function
      * \param definition
      *   The compound statement defining the function
      */
    void DefineFunction( const std::string& identifier,
                         const std::vector<Variable_sp>& parameters,
                         CompoundStatement_up definition );

    /**
      * \returns true if there is at least one function with a matching name
      */
    bool HasFunctionNamed( const std::string& identifier ) const;

    /**
      * \param identifier
      *   The name of the function
      * \param argument_types
      *   The types to try and match
      * \returns The best function overload if there is on, otherwise nullptr
      */
    Function_sp GetFunctionOverload(
                         const std::string& identifier,
                         const std::vector<CompleteType> argument_types );

    /**
      * \returns a big list of all the functions
      */
    std::vector<Function_sp> GetFunctions() const;

    /**
      * \returns true if we are in the top scope
      */
    bool InGlobalScope() const;

    /**
      * Evaluates an expression using llvm
      * \param expression
      *   The expression to evaluate
      * \returns the llvm genericvalue containing the expression's result
      */
    GenericValue EvaluateExpression( const Expression& expression );

    /**
      * Evaluates an initializer using llvm
      * \param initializer
      *   The initializer to evaluate
      * \returns the llvm genericvalue containing the initializer's result
      */
    GenericValue EvaluateInitializer( const Initializer& initializer );

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
      * \returns the vector of all errors
      */
    const std::vector<std::string>& GetErrors() const;

    /**
      * \returns the vector of all warnings
      */
    const std::vector<std::string>& GetWarnings() const;

    /**
      * \returns the CodeGenerator object
      */
    CodeGenerator& GetCodeGenerator();

    /**
      * \class ScopeHolder
      * A class to hold onto a scope and make sure it's released
      */
    class ScopeHolder
    {
    public:
        explicit
        ScopeHolder( SemaAnalyzer& sema );
        ScopeHolder(const ScopeHolder&) = delete;
        const ScopeHolder& operator=(const ScopeHolder&) = delete;
        ~ScopeHolder();

        void Enter();
        void Leave();
    private:
        SemaAnalyzer& m_Sema;
        bool          m_InScope;
    };
    friend ScopeHolder;
private:
    struct SymbolMaps
    {
        std::map<std::string, Variable_sp> m_Variables;
    };

    /**
      * Creates a new scope on the scope stack
      */
    void EnterScope();

    /**
      * Pops the innermost scope from the stack
      */
    void LeaveScope();

    using PassDefinitionMap = std::map< std::string, PassDefinitionRef >;
    /**
      * The functionoverloads store a vector to all functions with the same name
      */
    using FunctionOverloadsMap = std::map< std::string,
                                           std::vector<Function_sp> >;

    PassDefinitionMap        m_PassDefinitions;
    std::vector<std::string> m_Techniques;

    FunctionOverloadsMap     m_FunctionOverloads;

    std::vector<SymbolMaps>  m_SymbolStack;

    std::vector<TechniqueDeclaration_up>    m_TechniqueDeclarations;
    std::vector<PassDeclaration_up>         m_PassDeclarations;
    std::vector<VariableDeclarationList_up> m_VariableDeclarations;
    std::vector<FunctionDefinition_up>      m_FunctionDefinitions;

    bool m_Good = true;
    std::vector<std::string> m_Errors;
    std::vector<std::string> m_Warnings;

    const Context& m_Context;
    Runtime&       m_Runtime;
    CodeGenerator  m_CodeGenerator;
};

} // namespace Compiler
} // namespace JoeLang
