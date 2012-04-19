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

#include "effect_factory.hpp"

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include <engine/effect.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/parser.hpp>

#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Compiler
{

EffectFactory::EffectFactory( const Context& context )
    :m_context( context )
{
}

std::unique_ptr<Effect> EffectFactory::CreateEffectFromString(
                                                     const std::string& string )
{
    Parser parser;
    if( !parser.Parse( string ) )
        return nullptr;

    TranslationUnit& ast = *parser.GetTranslationUnit();
    ast.Print();

    SemaAnalyzer sema( m_context );
    if( !sema.BuildAst( ast ) )
        return nullptr;

    std::vector<Technique> techniques;
    std::unique_ptr<llvm::ExecutionEngine> llvm_execution_engine;

    CodeGenerator code_generator( m_context, techniques );
    code_generator.GenerateCode( ast, techniques, llvm_execution_engine );

    return std::unique_ptr<Effect>( new Effect(
                                           std::move(techniques),
                                           std::move(llvm_execution_engine) ) );
}

} // namespace Compiler
} // namespace JoeLang
