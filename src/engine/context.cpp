/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include "context.hpp"

#include <algorithm>

#include <engine/effect.hpp>
#include <engine/internal/state.hpp>
#include <parser/effect_factory.hpp>
#include <parser/parser.hpp>

namespace JoeLang
{

Context::Context()
{
}

Context::~Context()
{
}

bool Context::CreateState( std::string state_name, Type type )
{
    if( std::find_if( m_states.begin(), m_states.end(),
                      [&state_name](const State& s)
                      {return s.GetName() == state_name;})
        != m_states.end() );
        return false;

    m_states.emplace_back( State( std::move(state_name), type ) );
    return true;
}

bool Context::CreateEffectFromString(const std::string& string)
{
    Parser::Parser parser;
    if( parser.Parse( string ) )
    {
        parser.Print();
        JoeLang::Parser::EffectFactory ef;
        JoeLang::Effect e = ef.CreateEffect( parser.GetTranslationUnit() );
        return true;
    }
    else
    {
        return false;
    }
}

} // namespace JoeLang
