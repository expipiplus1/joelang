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

#include <joelang/pass.hpp>

#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <joelang/program.hpp>
#include <joelang/state_assignment.hpp>

namespace JoeLang
{

Pass::Pass( std::string name,
            StateAssignmentVector state_assignments,
            Program program )
    :m_Name( std::move(name) )
    ,m_StateAssignments( std::move(state_assignments) )
    ,m_Program( std::move(program) )
{
#ifndef NDEBUG
    for( const auto& sa : m_StateAssignments )
        assert( sa && "null state assignment given to Pass" );
#endif
}

void Pass::SetState() const
{
    for( const auto& sa : m_StateAssignments )
        sa->SetState();

    m_Program.Bind();
}

void Pass::ResetState() const
{
    for( const auto& sa : m_StateAssignments )
        sa->ResetState();

    m_Program.Unbind();
}

bool Pass::Validate() const
{
    // validate program perhaps?
    for( const auto& sa : m_StateAssignments )
        if( !sa->ValidateState() )
            return false;
    return true;
}

const std::string& Pass::GetName() const
{
    return m_Name;
}

const Program& Pass::GetProgram() const
{
    return m_Program;
}

Program& Pass::GetProgram()
{
    return m_Program;
}

} // namespace JoeLang
