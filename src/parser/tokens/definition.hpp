/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#pragma once

#include <memory>
#include <vector>

#include <engine/pass.hpp>
#include <engine/technique.hpp>

#include <parser/tokens/token.hpp>

namespace JoeLang
{
namespace Parser
{

class Parser;
class PassDeclaration;
class StateAssignmentStatement;

//------------------------------------------------------------------------------
// PassDefinition
//------------------------------------------------------------------------------

class PassDefinition : public JoeLang::Parser::Token
{
public:
    virtual
    ~PassDefinition();

    Pass GetPass() const;

    virtual
    void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<PassDefinition>& token );

protected:
    PassDefinition( std::vector< std::unique_ptr<StateAssignmentStatement> > state_assignments  );

private:
    std::vector< std::unique_ptr<StateAssignmentStatement> > m_stateAssignments;
};

//------------------------------------------------------------------------------
// TechniqueDefinition
//------------------------------------------------------------------------------

class TechniqueDefinition : public JoeLang::Parser::Token
{
public:
    virtual ~TechniqueDefinition();

    Technique GetTechnique() const;

    virtual void Print( int depth ) const;

    static bool Parse( Parser& parser, std::unique_ptr<TechniqueDefinition>& token );

protected:
    TechniqueDefinition( std::vector< std::unique_ptr<PassDeclaration> > m_passes );

private:
    std::vector< std::unique_ptr<PassDeclaration> > m_passes;
};

} // namespace Parser
} // namespace JoeLang