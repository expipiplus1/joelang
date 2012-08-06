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

#include <joelang/context.hpp>

#include <algorithm>
#include <cassert>
#include <fstream>
#include <functional>
#include <memory>
#include <streambuf>
#include <string>
#include <utility>
#include <vector>

#include <engine/opengl_states.hpp>
#include <compiler/effect_factory.hpp>
#include <compiler/runtime.hpp>
#include <compiler/terminal_types.hpp>
#include <joelang/effect.hpp>
#include <joelang/state.hpp>

namespace JoeLang
{

Context::Context()
{
}

Context::~Context()
{
}

void Context::RegisterOpenGLStates()
{
    JoeLang::RegisterOpenGLStates( *this );
}

void Context::RegisterOpenGLActions()
{
    JoeLang::RegisterOpenGLActions( *this );
}

bool Context::AddState( StateBase* state )
{
    assert( state && "Context given a null state" );

    // todo report error here
    for( const auto s : m_States )
        if( s->GetName() == state->GetName() )
            return false;

    for( const std::string& name : state->GetEnumerantNames() )
        if( !Compiler::IsValidIdentifier( name ) )
            return false;

    m_States.push_back( state );
    return true;
}

Effect* Context::CreateEffectFromString( const std::string& string )
{
    if( !m_EffectFactory )
        m_EffectFactory.reset( new JoeLang::Compiler::EffectFactory( *this ) );
    assert( m_EffectFactory && "Couldn't create an effect factory" );

    std::unique_ptr<Effect> effect(
                            m_EffectFactory->CreateEffectFromString(string) );
    if( effect )
    {
        m_Effects.push_back( std::move(effect) );
        return m_Effects.rbegin()->get();
    }
    return nullptr;
}

Effect* Context::CreateEffectFromFile( const std::string& file_name )
{
   std::ifstream file( file_name );
   // todo error reporting here
   if( !file )
       return nullptr;

   std::string string;

   file.seekg( 0, std::ios::end );
   string.reserve( file.tellg() );
   file.seekg( 0, std::ios::beg );

   string.assign((std::istreambuf_iterator<char>(file)),
                  std::istreambuf_iterator<char>());

   return CreateEffectFromString( string );
}

const StateBase* Context::GetNamedState(const std::string& name) const
{
    auto s = std::find_if( m_States.begin(), m_States.end(),
                           [&name](StateBase* p)
                             {return name == p->GetName();} );
    if( s == m_States.end() )
        return nullptr;

    return *s;
}

const std::vector<StateBase*>& Context::GetStates() const
{
    return m_States;
}

void Context::SetErrorCallback(
                               std::function<void(std::string)> error_callback )
{
    m_ErrorCallback = std::move(error_callback);
}

const std::function<void(std::string)>& Context::GetErrorCallback() const
{
    return m_ErrorCallback;
}

} // namespace JoeLang
