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
#include <vector>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

class Parser;
class PassDeclarationOrIdentifier;
class StateAssignmentStatement;

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
    using StateAssignStmtVector =
                    std::vector< std::unique_ptr<StateAssignmentStatement> >;

    /**
      * \param state_assignments
      *   A vector of the StateAssignmentStatements belonging to this pass
      * This constructor asserts on any null StateAssignmentStatements
      */
    PassDefinition  ( StateAssignStmtVector state_assignments );

    virtual
    ~PassDefinition ();

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual
    void                    Print   ( int depth ) const;

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

private:
    StateAssignStmtVector   m_stateAssignments;
};

/**
  * \class TechniqueDefinition
  * \ingroup Tokens
  * \brief Matches the definition of a technique
  *
  * TechniqueDefinition = '{' (PassDeclarationOrIdentifier)* '}'
  */
class TechniqueDefinition : public JoeLang::Compiler::Token
{
public:
    using PassDeclarationVector =
                    std::vector< std::unique_ptr<PassDeclarationOrIdentifier> >;
    /**
      * This constructor Asserts on null declarations or identifiers
      * \param passes
      *   A vector of pass declarations or identifiers
      */
    TechniqueDefinition( PassDeclarationVector passes );

    virtual
    ~TechniqueDefinition();

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual void Print( int depth ) const;

    /**
      * Parses a technique definition
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static bool Parse( Parser& parser,
                       std::unique_ptr<TechniqueDefinition>& token );

private:
    PassDeclarationVector m_passes;
};

} // namespace Compiler
} // namespace JoeLang
