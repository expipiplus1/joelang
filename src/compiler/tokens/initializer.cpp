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

#include "initializer.hpp"

#include <algorithm>
#include <cassert>
#include <memory>
#include <utility>
#include <vector>

#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Initializer
//------------------------------------------------------------------------------

Initializer::Initializer( std::vector<std::unique_ptr<Initializer> > sub_lists )
    :Token( TokenTy::Initializer )
    ,m_SubInitializers( std::move(sub_lists) )
{
#if !defined(NDEBUG)
    for( const auto& e : m_SubInitializers )
        assert( e && "Initializer given a null sub_initializer" );
#endif
}

Initializer::Initializer( Expression_up expression )
    :Token( TokenTy::Initializer )
    ,m_Expression( std::move(expression) )
{
    assert( m_Expression && "Initializer  given a null expression" );
}

Initializer::~Initializer()
{
}

bool Initializer::PerformSema( SemaAnalyzer& sema, Type desired_type )
{
    if( m_Expression )
    {
        // If this is a single expression try and cast it to the desired type
        m_Expression->ResolveIdentifiers( sema );
        m_Expression = CastExpression::Create( desired_type,
                                               std::move(m_Expression) );
        bool ret = m_Expression->PerformSema( sema );
        m_ArrayExtents = m_Expression->GetArrayExtents();
        return ret;
    }
    else
    {
        // If this is list of sub_initializers then check that they all have
        // the same extents and that there is at least one of them, then check
        // that they all pass sema

        if( m_SubInitializers.empty() )
        {
            sema.Error( "Empty Initializer list" );
            return false;
        }

        if( std::adjacent_find( m_SubInitializers.begin(),
                                m_SubInitializers.end(),
                                [](const std::unique_ptr<Initializer>& a,
                                   const std::unique_ptr<Initializer>& b)
                                { return a->GetArrayExtents() !=
                                         b->GetArrayExtents(); } ) !=
            m_SubInitializers.end() )
        {
            /// TODO use mismatched length from the result of adj find here
            sema.Error( "Mismatched array extents in initializer" );
            return false;
        }

        bool ret = true;

        for( const auto& sub_init : m_SubInitializers )
        {
            ret &= sub_init->PerformSema( sema, desired_type );
        }

        const ArrayExtents sub_array_extents =
                                        m_SubInitializers[0]->GetArrayExtents();
        m_ArrayExtents.reserve( 1 + sub_array_extents.size() );
        m_ArrayExtents = { static_cast<unsigned>( m_SubInitializers.size() ) };
        m_ArrayExtents.insert( m_ArrayExtents.end(),
                               sub_array_extents.begin(),
                               sub_array_extents.end() );
        return ret;
    }
}

const ArrayExtents& Initializer::GetArrayExtents() const
{
    return m_ArrayExtents;
}

bool Initializer::IsExpression() const
{
    return static_cast<bool>(m_Expression);
}

const Expression& Initializer::GetExpression() const
{
    assert( m_Expression &&
            "Trying to get the expression of a non-expression Initializer" );
    return *m_Expression;
}

const std::vector<std::unique_ptr<Initializer> >&
                                         Initializer::GetSubInitializers() const
{
    assert( !m_SubInitializers.empty() &&
            "Trying to get the sub initializers of an expression initializer" );
    return m_SubInitializers;
}

bool Initializer::CanReduceToExpression()
{
    return !IsExpression() &&
           m_SubInitializers.size() == 1 &&
           m_SubInitializers[0]->IsExpression();
}

void Initializer::ReduceToExpression()
{
    assert( CanReduceToExpression() &&
            "Trying to reduce an unreducable initializer" );
    m_Expression = std::move(m_SubInitializers[0]->m_Expression);
    m_SubInitializers.clear();
}

void Initializer::Print( int depth ) const
{
    assert( false && "complete me" );
}

bool Initializer::Parse( Parser& parser, std::unique_ptr<Initializer>& token )
{
    if( parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
    {
        // If we've seen an open brace, start reading sub initializer lists
        std::vector<std::unique_ptr<Initializer> > sub_initializers;
        parser.ExpectListOf<Initializer, TerminalType::COMMA>(
                                                             sub_initializers );
        CHECK_PARSER;
        if( !parser.ExpectTerminal( TerminalType::CLOSE_BRACE ) )
        {
            parser.Error( "No closing brace on initializer list" );
            return false;
        }
        token.reset( new Initializer( std::move(sub_initializers) ) );
        return true;
    }
    else
    {
        // Just parse an expression
        std::unique_ptr<Expression> expression;
        if( !parser.Expect<AssignmentExpression>( expression ) )
            return false;
        token.reset( new Initializer( std::move(expression) ) );
        return true;
    }
}

} // namespace Compiler
} // namespace JoeLang
