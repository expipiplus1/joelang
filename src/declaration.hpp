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
#include <string>
#include <vector>
#include "token.hpp"

namespace JoeLang
{

namespace Lexer
{
    class Lexer;
} // namespace Lexer

namespace Parser
{

class Parser;

class DeclarationBase : public JoeLang::Parser::Token
{
public:
    virtual ~DeclarationBase();

    static bool Parse( Parser& parser, std::unique_ptr<DeclarationBase>& token );

protected:
    DeclarationBase() = default;
};

class EmptyDeclaration : public JoeLang::Parser::DeclarationBase
{
public:
    virtual ~EmptyDeclaration();

    static bool Parse( Parser& parser, std::unique_ptr<EmptyDeclaration>& token );

protected:
    EmptyDeclaration();
};

class PassDeclaration : public JoeLang::Parser::DeclarationBase
{
public:
    virtual ~PassDeclaration();

    static bool Parse( Parser& parser, std::unique_ptr<PassDeclaration>& token );

protected:
    PassDeclaration( std::string name );

private:
    std::string m_name;
};

class TechniqueDeclaration : public JoeLang::Parser::DeclarationBase
{
public:
    virtual ~TechniqueDeclaration();

    static bool Parse( Parser& parser, std::unique_ptr<TechniqueDeclaration>& token );

protected:
    TechniqueDeclaration( std::string name, std::vector< std::unique_ptr<PassDeclaration> > m_passes );

private:
    std::string m_name;
    std::vector< std::unique_ptr<PassDeclaration> > m_passes;
};

} // namespace Parser
} // namespace JoeLang
