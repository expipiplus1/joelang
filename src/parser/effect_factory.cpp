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

#include "effect_factory.hpp"

//#include <engine/context.hpp>
#include <engine/effect.hpp>
#include <parser/code_generator.hpp>
#include <parser/parser.hpp>
//#include <parser/tokens/declaration.hpp>
//#include <parser/tokens/definition.hpp>
//#include <parser/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Parser
{

EffectFactory::EffectFactory( const Context& context )
    :m_context( context )
{
}

std::unique_ptr<Effect> EffectFactory::CreateEffectFromString( const std::string& string )
{
    Parser parser( m_context );
    if( !parser.Parse( string ) )
        return nullptr;

    const std::unique_ptr<TranslationUnit>& ast = parser.GetTranslationUnit();

    std::vector<Technique> techniques;
    llvm::Module* llvm_module;

    CodeGenerator code_generator;
    if( !code_generator.GenerateCode( ast, techniques, llvm_module ) )
        return nullptr;

    return std::unique_ptr<Effect>( new Effect( std::move(techniques), llvm_module ) );

    //for( const auto& declaration : parser.GetTranslationUnit()->GetDeclarations() )
        //declaration->Accept( *this );
    //return std::unique_ptr<Effect>( new Effect( std::move( m_techniques ) ) );
}

} // namespace Parser
} // namespace JoeLang
