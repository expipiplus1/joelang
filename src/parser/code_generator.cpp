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

#include "code_generator.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>

#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <parser/tokens/translation_unit.hpp>
#include <parser/tokens/declaration.hpp>
#include <parser/tokens/definition.hpp>

namespace JoeLang
{
namespace Parser
{

CodeGenerator::CodeGenerator( std::vector<Technique>& techniques )
    :m_techniques( techniques )
{
    m_module = new llvm::Module( "main", llvm::getGlobalContext() );
}

bool CodeGenerator::GenerateCode( const std::unique_ptr<TranslationUnit>& ast,
                                  std::vector<Technique>& techniques,
                                  llvm::Module*& llvm_module )
{
    for( const auto& declaration : ast->GetDeclarations() )
        declaration->Accept( *this );

    //techniques = std::move(m_techniques);
    llvm_module = nullptr;

    return m_good;
}

void CodeGenerator::Error( const std::string& message )
{
    m_good = false;
    std::cout << "Code Generation Error: " << message << std::endl;
}

void CodeGenerator::Visit( DeclarationBase& d )
{
}

void CodeGenerator::Visit( TechniqueDeclaration& t )
{
    //TODO Techniques shouldn't be declared without a definition
    const std::shared_ptr<TechniqueDefinition>& definition = t.GetDefinition();
    assert( definition && "TODO get rid of this" );
    m_techniques.push_back( definition->GetTechnique( *this ) );
}

StateAssignment CodeGenerator::GenerateStateAssignment(
        const State& state,
        const std::unique_ptr<Expression>& expression ) const
{
    return StateAssignment( state );
}

} // namespace Parser
} // namespace JoeLang
