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

namespace JoeLang
{

class Pass;

namespace Compiler
{

class CodeGenerator;
class CompileStatement;
using CompileStatement_up = std::unique_ptr<CompileStatement>;
class SemaAnalyzer;
class Parser;
class PassDeclaration;
class PassDeclarationOrIdentifier;
class PassStatement;
using PassStatement_up = std::unique_ptr<PassStatement>;
class StateAssignmentStatement;
using StateAssignmentStatement_up = std::unique_ptr<StateAssignmentStatement>;

/**
  * \class PassDefinition
  * \ingroup Tokens
  * \brief Matches the definition of a pass
  *
  * PassDefinition = '{' (StateAssignmentStatement)* '}'
  */
class PassDefinition : public JoeLang::Compiler::Token
{
public:
    using PassStatementVector = std::vector<PassStatement_up>;
    using StateAssignStmtVector = std::vector<StateAssignmentStatement_up>;
    using CompileStatementVector = std::vector<CompileStatement_up>;

    /**
      * \param statements
      *   A vector of the PassStatements belonging to this pass
      * This constructor asserts on any null StateAssignmentStatements or
      * null CompileStatements
      */
    PassDefinition  ( PassStatementVector statements );

    virtual
    ~PassDefinition ();

    /**
      * Performs semantic analysis on all the stateassignments
      */
    bool PerformSema( SemaAnalyzer& sema );

    const StateAssignStmtVector& GetStateAssignments() const;

    const CompileStatementVector& GetCompileStatements() const;

    /**
      * Parses a pass definition
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static bool             Parse   ( Parser&                          parser,
                                      std::unique_ptr<PassDefinition>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const PassDefinition* d );
private:
    StateAssignStmtVector   m_StateAssignments;
    CompileStatementVector  m_CompileStatements;
};

/**
  * \class PassDeclarationOrIdentifier
  * \ingroup Tokens
  * \brief A helper token for TechniqueDefinitions
  *
  * This token matches either an identifier or a pass declaration
  *
  * PassDeclarationOrIdentifier =   identifier ';'
  *                               | PassDeclaration
  */
class PassDeclarationOrIdentifier : public JoeLang::Compiler::Token
{
public:
    /**
      * This constructor will assert if the definition is not null and the name
      * is not of zero length
      * \param identifier
      *   The identifier for this pass
      * \param declaration
      *   This pass's declaration if it has one, otherwise nullptr
      */
    PassDeclarationOrIdentifier( std::string                      identifier,
                                 std::unique_ptr<PassDeclaration> declaration );

    virtual
    ~PassDeclarationOrIdentifier();

    /**
      * Generates the pass represented by the pass definition. This function
      * will assert if it doesn't have a definition reference.
      * \param code_gen
      *   The code generator
      * \returns the generated Pass
      */
    Pass GeneratePass( CodeGenerator& code_gen ) const;

    virtual
    bool PerformSema( SemaAnalyzer& sema );

    /** \returns if this token is an identifier for a pass **/
    bool                   IsIdentifier    () const;
    /** This funciton will assert if this token represents a declaration
      * \returns this token's identifier **/
    const std::string&     GetIdentifier   () const;
    /** This function will assert if this token represents an identifier
      * \returns this token's declaration **/
    const PassDeclaration& GetDeclaration  () const;
    PassDeclaration&       GetDeclaration  ();

    /**
      * Parses a pass declaration or identifier
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                std::unique_ptr<PassDeclarationOrIdentifier>& token );

    static
    bool classof( const Token* t );
    static
    bool classof( const PassDeclarationOrIdentifier* d );
private:
    /** This pass declaration's identifier if it has one, otherwise "" **/
    std::string                      m_Identifier;
    /** This token's declaration if it has one, otherwise nullptr **/
    std::unique_ptr<PassDeclaration> m_Declaration;

    /** The reference to the definition this token represents **/
    std::shared_ptr<std::unique_ptr<PassDefinition> > m_DefinitionRef = nullptr;
};

} // namespace Compiler
} // namespace JoeLang
