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

#include "semantic.hpp"

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <utility>

#include <compiler/support/generic_value.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/writers/semantic_info.hpp>
#include <compiler/lexer/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

Semantic::Semantic  ()
    :m_Type( SemanticType::NO_SEMANTIC )
{
}

Semantic::Semantic  ( std::string string )
    :m_String( std::move(string) )
    ,m_HasIndex( false )
{
    assert( !m_String.empty() && "Semantic given empty string" );
    DetermineType();
}

Semantic::Semantic  ( std::string string, unsigned index )
    :m_String( std::move(string) )
    ,m_HasIndex( true )
    ,m_Index( index )
{
    assert( !m_String.empty() && "Semantic given empty string" );
    DetermineType();
}

bool Semantic::IsVarying() const
{
    auto s = g_SemanticInfoMap.find( m_Type );
    if( s != g_SemanticInfoMap.end() )
    {
        return s->second.m_IsVarying;
    }
    //
    // Semantics are not varying by default
    //
    return false;
}

bool Semantic::HasBuiltin( ShaderDomain domain, bool input ) const
{
    auto s = g_SemanticInfoMap.find( m_Type );
    if( s == g_SemanticInfoMap.end() )
        return false;
    if( s->second.m_GLSLBuiltin.empty() )
        return false;
    auto& d = input ? s->second.m_DomainInputs : s->second.m_DomainOutputs;
    if( d.find( domain ) == d.end() )
        return false;
    return true;
}

const std::string& Semantic::GetBuiltin( ShaderDomain domain, bool input ) const
{
    assert( HasBuiltin( domain, input ) && 
            "Trying to get the builtin name of a variable without one" );
    return g_SemanticInfoMap.at( m_Type ).m_GLSLBuiltin;
}

bool Semantic::HasIndex() const
{
    return m_HasIndex;
}

unsigned Semantic::GetIndex() const
{
    assert( HasIndex() && "Trying to get the index of a semantic without one" );
    return m_Index;
}

void Semantic::DetermineType()
{
    const static std::map<std::string, SemanticType> type_map =
    {
        { "POSITION", SemanticType::POSITION },
        { "VERTEXID", SemanticType::VERTEXID },
        { "DEPTH",    SemanticType::DEPTH    },
        { "COLOR",    SemanticType::COLOR    },
        { "WPOS",     SemanticType::WPOS     }
    };

    //
    // Try and find a built in semantic
    //
    const auto& s = type_map.find( m_String );
    if( s == type_map.end() )
        m_Type = SemanticType::CUSTOM;
    else
        m_Type = s->second;
}

} // namespace Compiler
} // namespace JoeLang
