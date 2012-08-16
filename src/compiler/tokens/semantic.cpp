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

#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Semantic
//------------------------------------------------------------------------------

Semantic::Semantic( std::string string, Expression_up index_expression )
    :Token( TokenTy::Semantic )
    ,m_String( std::move(string) )
    ,m_IndexExpression( std::move(index_expression) )
{
    assert( !m_String.empty() && "Semantic given an empty string" );
}

Semantic::~Semantic()
{
}

bool Semantic::HasIndex() const
{
    return static_cast<bool>( m_IndexExpression );
}

void Semantic::PerformSema( SemaAnalyzer& sema )
{
    const static std::map<std::string, SemanticType> semantic_type_map =
    {
        { "POSITION", SemanticType::POSITION },
        { "DEPTH",    SemanticType::DEPTH    },
        { "COLOR",    SemanticType::COLOR    },
    };

    //
    // If we don't have an index we have no work to do here
    //
    if( !m_IndexExpression )
        return;

    if( !m_IndexExpression->ResolveIdentifiers( sema ) )
        return;

    m_IndexExpression = CastExpression::Create( CompleteType( Type::I32 ),
                                                std::move(m_IndexExpression) );

    if( !m_IndexExpression->PerformSema( sema ) )
        return;

    if( !m_IndexExpression->IsConst() )
    {
        sema.Error( "Index in an semantic index must be const" );
        return;
    }

    int index = sema.EvaluateExpression( *m_IndexExpression ).GetI32();
    if( index < 0 )
    {
        sema.Error( "Semantic index must be non-negative" );
        return;
    }
    m_Index = index;

    //
    // Try and find a build in semantic
    //
    const auto& s = semantic_type_map.find( m_String );
    if( s == semantic_type_map.end() )
    {
        m_SemanticType = SemanticType::CUSTOM;
        //
        // Check that there is no index on this
        // todo index for color and attr
        if( HasIndex() )
            sema.Error( "Can't have an index with built in semantic " +
                        m_String );
    }
    else
        m_SemanticType = s->second;
}

bool Semantic::Parse( Parser& parser, Semantic_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::COLON ) )
        return false;

    std::string string;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, string ) )
    {
        parser.Error( "Expected semantic" );
        return false;
    }

    //
    // try and parse the optional index
    //
    Expression_up index_expression;
    if( parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
    {
        if( !parser.Expect<Expression>( index_expression ) )
            return false;
        if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        {
            parser.Error( "Expected closing square bracket on semantic index" );
            return false;
        }
    }

    token.reset( new Semantic( std::move(string),
                               std::move(index_expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
