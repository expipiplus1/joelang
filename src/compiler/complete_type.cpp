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

#include "complete_type.hpp"

#include <utility>
#include <vector>

#include <engine/types.hpp>

namespace JoeLang
{
namespace Compiler
{

CompleteType::CompleteType()
    :m_BaseType( Type::UNKNOWN )
    ,m_IsConst( false )
{
}

CompleteType::CompleteType( Type base_type,
                            ArrayExtents array_extents,
                            bool is_const )
    :m_BaseType( base_type )
    ,m_ArrayExtents( std::move(array_extents) )
    ,m_IsConst( is_const )
{
}

Type CompleteType::GetType() const
{
    if( !m_ArrayExtents.empty() )
        return Type::ARRAY;
    return m_BaseType;
}

Type CompleteType::GetBaseType() const
{
    return m_BaseType;
}

const ArrayExtents& CompleteType::GetArrayExtents() const
{
    return m_ArrayExtents;
}

} // namespace Compiler
} // namespace JoeLang
