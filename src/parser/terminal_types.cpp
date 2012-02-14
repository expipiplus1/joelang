/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "terminal_types.hpp"

#include <algorithm>
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace JoeLang
{
namespace Lexer
{

//------------------------------------------------------------------------------
// LiteralTerminal
//------------------------------------------------------------------------------

int LiteralTerminal::Read( const std::string::const_iterator begin,
                           const std::string::const_iterator end ) const
{
    if( std::size_t(end - begin) < matched_string.size() ||
        !std::equal( matched_string.begin(), matched_string.end(), begin ) )
        return 0;

    return matched_string.size();
}

//------------------------------------------------------------------------------
// FunctionalTerminal
//------------------------------------------------------------------------------

int FunctionalTerminal::Read( const std::string::const_iterator begin,
                              const std::string::const_iterator end ) const
{
    return function( begin, end );
}

//------------------------------------------------------------------------------
// Reading Functions
//------------------------------------------------------------------------------

int ReadWhitespace( std::string::const_iterator begin,
                    std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    while( p < end )
    {
        char c = *p;
        if( c != ' '  &&
            c != '\n' &&
            c != '\r' &&
            c != '\t' &&
            c != '\v' )
            break;
        ++p;
    }

    return p - begin;
}

int ReadLineComment( const std::string::const_iterator begin,
                     const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( end - begin < 2 )
        return 0;

    if( *p++ == '/' &&
        *p++ == '/' )
    {
        while( p < end )
        {
            char c = *p;
            if( c == '\n' )
                break;
            ++p;
        }
        return p - begin;
    }

    return 0;
}

int ReadBlockComment(   const std::string::const_iterator begin,
                        const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( end - begin < 4 )
        return 0;

    if( *p++ == '/' &&
        *p++ == '*' )
    {
        ++p;
        while( p < end )
        {
            char c = *p;
            if( c == '/' &&
                *(p-1) == '*' )
                return p - begin + 1;

            ++p;
        }
        return 0;
    }

    return 0;
}

int ReadDigitSequence(      const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( ( c >= '0' ) && ( c <= '9' ) && ( p < end ) )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

int ReadHexDigitSequence(      const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( ( ( ( c >= '0' ) && ( c <= '9' ) ) ||
             ( ( c >= 'a' ) && ( c <= 'f' ) ) ||
             ( ( c >= 'A' ) && ( c <= 'F' ) ) ) && p < end )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

int ReadOctalDigitSequence(      const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    char c = *p;
    while( ( c >= '0' ) && ( c <= '7' ) && p < end )
    {
        ++p;
        c = *p;
    }
    return p - begin;
}

int ReadIntegerLiteral(     const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    if( *p == '0' )
    {
        ++p;
        if( p >= end )
            return 1;

        if( *p == 'x' ||
            *p == 'X' )
        {
            ++p;
            int n = ReadHexDigitSequence( p, end );
            if( n )
                return n + 2;
        }

        return 1 + ReadOctalDigitSequence( p, end );
    }
    else
    {
        return ReadDigitSequence( begin, end );
    }
}

int ReadExponent(           const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    if( end - begin < 2 )
        return 0;

    std::string::const_iterator p = begin;
    char c = *p;
    if( ( c != 'e' ) && ( c != 'E' ) )
        return 0;

    ++p;
    c = *p;

    if( ( c == '+' ) || ( c == '-' ) )
        ++p;

    int s = ReadDigitSequence( p, end );
    if( s == 0 )
        return 0;

    p += s;
    return p - begin;
}

int ReadFloatingLiteral(    const std::string::const_iterator begin,
                            const std::string::const_iterator end )
{
    std::string::const_iterator p = begin;
    int s = ReadDigitSequence( p, end );
    if( !s )
    {
        if( *p != '.' )
            return 0;

        ++p;
        s = ReadDigitSequence( p, end );
        if( !s )
            return 0;

        p += s;

        p += ReadExponent( p, end );
    }
    else
    {
        p += s;
        if( p >= end )
            return 0;

        if( *p != '.' )
        {
            s = ReadExponent( p, end );
            if( !s )
                return 0;
        }
        else
        {
            ++p;
            p += ReadDigitSequence( p, end );
            p += ReadExponent( p, end );
        }
    }

    if( p >= end )
        return p - begin;

    char c = *p;
    if( c == 'f' || c == 'F' )
        ++p;

    return p - begin;
}

/*
int ReadCharacterLiteral(   const std::string::const_iterator begin,
                            const std::string::const_iterator end );

int ReadStringLiteral(      const std::string::const_iterator begin,
                            const std::string::const_iterator end );

int ReadIdentifier( const std::string::const_iterator begin,
                    const std::string::const_iterator end );

*/

bool IsNonDigit( const char c )
{
    return ( c >= 'a' && c <= 'z' ) ||
           ( c >= 'A' && c <= 'Z' ) ||
           ( c == '_' );
}

bool IsDigitOrNonDigit( const char c )
{
    return IsNonDigit( c ) ||
           ( c >= '0' && c <= '9' );
}

} // namespace Lexer
} // namespace JoeLang
