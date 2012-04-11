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

#include "parser.hpp"

#include <memory>
#include <utility>
#include <vector>

#include <compiler/lexer.hpp>

namespace JoeLang
{
namespace Compiler
{

template< typename T, typename U >
bool Parser::Expect( std::unique_ptr<U>& token )
{
    if( !Good() )
        return false;

    std::size_t p = m_lexer->GetPosition();

    if( T::Parse( *this, token ) )
    {
        return true;
    }
    else
    {
        if( m_lexer->GetPosition() != p )
            Error();

        return false;
    }
}

template< typename T >
bool Parser::Expect()
{
    if( !Good() )
        return false;

    std::size_t p = m_lexer->GetPosition();

    if( T::Parse( *this ) )
    {
        return true;
    }
    else
    {
        if( m_lexer->GetPosition() != p )
            Error();

        return false;
    }
}

template<typename T, typename U>
bool Parser::ExpectSequenceOf( std::vector< std::unique_ptr<U> >& token_sequence )
{
    std::unique_ptr<U> token;
    if( !Expect<T>( token ) )
        return false;

    do
    {
        token_sequence.push_back( std::move( token ) );
    }
    while( Expect<T>( token ) );

    if( !Good() )
        return false;

    return true;
}

template< typename T >
bool Parser::ExpectAnyOf( std::unique_ptr<Token>& token )
{
    std::unique_ptr<T> t;
    if( !Expect<T>( t ) )
        return false;
    token = std::move( t );
    return true;
}

template<typename T, typename T1, typename... Rest>
bool Parser::ExpectAnyOf( std::unique_ptr<Token>& token )
{
    std::unique_ptr<T> t;
    if( !Expect<T>( t ) )
    {
        if( !Good() )
            return false;

        return ExpectAnyOf<T1, Rest...>( token );
    }
    token = std::move( t );
    return true;
}

template< typename T >
bool Parser::ExpectAnyOf()
{
    return Expect<T>();
}

template<typename T, typename T1, typename... Rest>
bool Parser::ExpectAnyOf()
{
    if( !Expect<T>() )
    {
        if( !Good() )
            return false;

        return ExpectAnyOf<T1, Rest...>();
    }
    return true;
}

} // namespace Compiler
} // namespace JoeLang
