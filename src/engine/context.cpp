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
#include <string>

#include <engine/effect.hpp>
#include <engine/state.hpp>
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

bool Context::AddState( State state )
{
    //TODO
    m_states.push_back( std::move(state) );
    return true;
}

bool Context::CreateEffectFromString(const std::string& string)
{
    Parser::Parser parser( *this );
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

bool Context::IsStateName(const std::string& name) const
{
    return std::find_if( m_states.begin(), m_states.end(),
                         [&name](const State& p){return name == p.GetName();} ) !=
           m_states.end();
}

} // namespace JoeLang
