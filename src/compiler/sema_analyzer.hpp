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

namespace Compiler
{

class Expression;
class PassDefinition;
class TechniqueDefinition;
class TranslationUnit;

/**
  * \class SemaAnalyzer
  * \brief A class to perform semantic analysis on the AST
  */
class SemaAnalyzer
{
public:
    using PassDefinitionRef = std::shared_ptr<std::unique_ptr<PassDefinition> >;

    explicit
    SemaAnalyzer( const Context& context );
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
      * Declares a variable with an optional value
      * \param identifier
      *   The identifier for the variable
      * \param value
      *   The optional init value
      */
    void DeclareVariable( const std::string& identifier,
                          std::shared_ptr<Expression> value = nullptr );

    /**
      * \param identifier
      *   The name of the variable
      * \returns the Expression assiciated with the variable, or nullptr if
      *   there is no variable with that name
      */
    std::shared_ptr<Expression> GetVariable( const std::string& identifier );

    /**
      * Creates a new scope on the scope stack
      */
    void EnterScope();

    /**
      * Pops the innermost scope from the stack
      */
    void LeaveScope();

    /**
      * Reports an error.
      * Sets m_good to false
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

private:
    struct SymbolMaps
    {
        std::map<std::string, std::shared_ptr<Expression> > m_variables;
    };

    using PassDefinitionMap = std::map< std::string, PassDefinitionRef >;

    PassDefinitionMap        m_passDefinitions;
    std::vector<std::string> m_techniques;

    std::vector<SymbolMaps>  m_symbolStack;

    bool m_good = true;

    const Context& m_context;
};

} // namespace Compiler
} // namespace JoeLang
