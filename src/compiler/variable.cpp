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
#include <string>
#include <utility>
#include <vector>

#include <llvm/GlobalVariable.h>

#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/tokens/expressions/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

Variable::Variable( CompleteType type,
                    bool is_const,
                    bool is_global,
                    bool is_parameter,
                    GenericValue initializer,
                    std::string name )
    :m_Type( std::move(type) )
    ,m_IsConst( is_const )
    ,m_IsGlobal( is_global )
    ,m_IsParameter( is_parameter )
    ,m_Initializer( std::move(initializer) )
    ,m_Name( std::move(name) )
{
    // Assert that this has the correct initializer if this is const
    // Or that if it has an initializer it's the correct type
    assert( !m_Type.IsUnknown() && 
            "Trying to construct a variable of unknown type" );
    if( !m_IsParameter &&
        ( m_IsConst || !m_Initializer.GetType().IsUnknown() ) )
    {
        assert( !m_Initializer.GetType().IsUnknown() &&
                "No initializer on a const variable" );

        assert( m_Initializer.GetType() == m_Type &&
                "Trying to initialize a variable with the wrong type" );
    }
}

void Variable::CodeGen( CodeGenerator& code_gen )
{
    // Don't generate storage for strings, they're determined at compile time
    if( m_IsGlobal )
    {
        m_LLVMPointer = code_gen.CreateGlobalVariable( m_Type,
                                                       m_IsConst,
                                                       m_Initializer,
                                                       m_Name );
    }
    else
    {
        assert( false && "TODO: generate non global variables" );
    }
}

llvm::Value* Variable::GetLLVMPointer() const
{
    return m_LLVMPointer;
}

const CompleteType& Variable::GetType() const
{
    return m_Type;
}

Type Variable::GetUnderlyingType() const
{
    return m_Type.GetBaseType();
}

bool Variable::IsConst() const
{
    return m_IsConst;
}

} // namespace Compiler
} // namespace JoeLang
