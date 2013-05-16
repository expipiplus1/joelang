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

#include <fstream>
#include <memory>

#include <joelang/context.hpp>
#include <joelang/effect.hpp>
#include <joelang/parameter.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/parser.hpp>

#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{

class ParameterBase;
using ParameterBase_up = std::unique_ptr<ParameterBase>;


namespace Compiler
{

EffectFactory::EffectFactory( const Context& context, Runtime& runtime )
    :m_Context( context )
    ,m_Runtime( runtime )
{
}

std::unique_ptr<Effect> EffectFactory::CreateEffectFromString(
                                                      const std::string& source,
                                                      const std::string& name )
{
    Lexer         token_stream( source, name );
    Parser        parser( token_stream );
    SemaAnalyzer  sema_analyzer( m_Context, m_Runtime );
    CodeGenerator code_generator( m_Runtime.CreateCodeGenerator() );

    parser.Parse();

    if( !parser.Good() )
    {
        for( const auto& s : parser.GetErrors() )
            m_Context.Error( s );
        return nullptr;
    }

    TranslationUnit& tu = *parser.GetTranslationUnit();

    //
    // This will resolve identifiers and insert implicit casts
    // It may also codegen variables used in initializers
    //
    sema_analyzer.Analyze( tu );

    if( !sema_analyzer.Good() )
    {
        for( const auto& s : sema_analyzer.GetErrors() )
            m_Context.Error( s );
        return nullptr;
    }

    std::vector<Technique> techniques = code_generator.GenerateTechniques(
                                     sema_analyzer.GetTechniqueDeclarations() );
    std::vector<ParameterBase_up> parameters =
       code_generator.GenerateParameters( sema_analyzer.GetUniformVariables() );

    return std::unique_ptr<Effect>( new Effect( std::move(techniques),
                                                std::move(parameters) ) );
}



} // namespace Compiler
} // namespace JoeLang
