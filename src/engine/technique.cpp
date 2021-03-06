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

#include <joelang/technique.hpp>

#include <string>
#include <utility>
#include <vector>

#include <joelang/pass.hpp>

namespace JoeLang
{

Technique::Technique( std::string name, std::vector<Pass> passes )
    :m_Name( std::move(name) )
    ,m_Passes( std::move( passes ) )
{
}

const std::vector<Pass>& Technique::GetPasses() const
{
    return m_Passes;
}

std::vector<Pass>& Technique::GetPasses()
{
    return m_Passes;
}

const std::string& Technique::GetName() const
{
    return m_Name;
}

} // namespace JoeLang
