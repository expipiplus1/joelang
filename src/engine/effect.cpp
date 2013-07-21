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

#include <joelang/effect.hpp>

#include <algorithm>
#include <memory>
#include <utility>
#include <vector>

#include <llvm/ExecutionEngine/ExecutionEngine.h>

#include <joelang/config.h>
#include <joelang/parameter.hpp>
#include <joelang/technique.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{

Effect::Effect( std::vector<Technique> techniques,
                std::vector<ParameterBase_up> parameters )
    :m_Techniques( std::move(techniques) )
    ,m_Parameters( std::move(parameters) )
{
}

Effect::~Effect()
{
}

const std::vector<Technique>& Effect::GetTechniques() const
{
    return m_Techniques;
}

const Technique* Effect::GetNamedTechnique( const std::string& name ) const
{
    const auto& technique = std::find_if( m_Techniques.begin(),
                                          m_Techniques.end(),
                                          [&name](const Technique& t)
                                            {return t.GetName() == name;} );
    return technique == m_Techniques.end() ? nullptr :  &*technique;
}

ParameterBase* Effect::GetNamedParameter( const std::string& name, Type type )
{
    auto it = std::find_if( m_Parameters.begin(),
                            m_Parameters.end(),
                            [&]( const ParameterBase_up& p )
                                { return p->GetName() == name; } );

    if( it == m_Parameters.end() )
        return nullptr;

    //
    // If a type was specified filter by that type
    //
    if( type != Type::UNKNOWN && (*it)->GetType() != type )
        return nullptr;

    //
    // Link the parameter to the programs if we're using opengl
    //
#ifdef JOELANG_WITH_OPENGL
    for( auto& t : m_Techniques )
        for( auto& p : t.GetPasses() )
            if( p.HasProgram() )
                p.GetProgram().NotifyParameter( **it );
#endif

    return (*it).get();
}

} // namespace JoeLang
