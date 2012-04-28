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

#include "variable.hpp"

#include <cassert>
#include <memory>
#include <utility>

#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/tokens/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

Variable::Variable( Type type,
                    bool is_const,
                    bool is_global,
                    std::unique_ptr<Expression> initializer )
    :m_type( type )
    ,m_isConst( is_const )
    ,m_isGlobal( is_global )
    ,m_initializer( std::move(initializer) )
{
    if( is_const )
    {
        assert( isa<LiteralExpression>(m_initializer) &&
                "Trying to initialize a const variable with a non-const "
                "expression" );
        assert( m_initializer->GetReturnType() == type &&
                "Trying to initialize a const variable with the wrong type" );
    }
}

void Variable::CodeGen( CodeGenerator& code_gen )
{
    // Don't generate storage for strings, they're determined at compile time
    if( m_type == Type::STRING )
        return;

    if( m_isGlobal )
    {
        m_globalVariable = code_gen.CreateGlobalVariable( m_type,
                                                          m_isConst,
                                                          m_initializer );
    }
    else
    {
        assert( false && "TODO: generate non global variables" );
    }
}

Type Variable::GetType() const
{
    return m_type;
}

const std::unique_ptr<Expression>& Variable::GetReadExpression() const
{
    if( m_isConst )
        return m_initializer;

    assert( false );
    return m_initializer;
}

} // namespace Compiler
} // namespace JoeLang
