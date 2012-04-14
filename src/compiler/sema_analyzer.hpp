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
namespace Compiler
{

class PassDefinition;
class TechniqueDefinition;
class TranslationUnit;

/**
  * \class AstBuilder
  * \brief A class to perform semantic analysis on the AST
  *   suitable for code generation
  */
class SemaAnalyzer
{
public:
    SemaAnalyzer();
    ~SemaAnalyzer();

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
    bool HasPass( const std::string& name );

    /**
      * Check to see if a state has been given to context
      * \param name
      *   The name of the state
      * \returns true if the state has been declared
      */
    bool HasState( const std::string& name );

    /**
      * Reports an error.
      * Sets m_good to false
      * \param error_message
      *   The error message
      */
    void Error( const std::string& error_message );

private:
    using PassDefinitionMap = std::map<std::string,
                                       std::unique_ptr<PassDefinition> >;

    PassDefinitionMap        m_passDefinitions;
    std::vector<std::string> m_techniques;

    bool m_good = true;
};

} // namespace Compiler
} // namespace JoeLang
