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

#include "context.hpp"

#include <algorithm>
#include <cassert>
#include <fstream>
#include <functional>
#include <memory>
#include <streambuf>
#include <string>
#include <utility>
#include <vector>

#include <llvm/Support/TargetSelect.h>

#include <engine/effect.hpp>
#include <engine/state.hpp>
#include <compiler/effect_factory.hpp>

namespace JoeLang
{

Context::Context()
{
    llvm::InitializeNativeTarget();
}

Context::~Context()
{
}

bool Context::AddState( StateBase* state )
{
    for( const auto s : m_States )
        if( s->GetName() == state->GetName() )
            return false;

    //TODO Check for invalid state enumerants
    m_States.push_back( state );
    return true;
}

Effect* Context::CreateEffectFromString( const std::string& string )
{
    JoeLang::Compiler::EffectFactory ef( *this );
    std::unique_ptr<Effect> e( ef.CreateEffectFromString( string ) );
    if( e )
    {
        Effect* ret = e.get();
        m_Effects.push_back( std::move(e) );
        return ret;
    }
    return nullptr;
}

Effect* Context::CreateEffectFromFile( const std::string& file_name )
{
   std::ifstream file( file_name );
   assert( file && "File error" );

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
