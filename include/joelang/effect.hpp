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

#pragma once

#include <memory>
#include <string>
#include <vector>

#include <joelang/technique.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{

class ParameterBase;
using ParameterBase_up = std::unique_ptr<ParameterBase>;
template<typename>
class Parameter;

class Effect
{
public:
    Effect() = default;
    Effect( std::vector<Technique> techniques,
            std::vector<ParameterBase_up> parameters );
    ~Effect();

    const std::vector<Technique>& GetTechniques() const;
    const Technique* GetNamedTechnique( const std::string& name ) const;

    ParameterBase* GetNamedParameter( const std::string& name,
                                      Type type = Type::UNKNOWN );
    
    template<typename T>
    Parameter<T>*  GetNamedParameter( const std::string& name );

private:
    std::vector<Technique>                  m_Techniques;
    std::vector<ParameterBase_up>           m_Parameters;
};

} // namespace JoeLang

#include "inl/effect-inl.hpp"
