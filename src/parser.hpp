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

#pragma once

#include <memory>
#include <string>
#include <vector>
#include "lexer.hpp"
#include "translation_unit.hpp"

namespace JoeLang
{
namespace Parser
{

class Parser
{
public:
    Parser() = default;
    ~Parser() = default;

    bool Parse( const std::string& string );

    template< typename T, typename U = T>
    bool Expect( std::unique_ptr<U>& token );

    template< typename T >
    bool Expect();


    template<typename T>
    bool ExpectSequenceOf( std::vector< std::unique_ptr<T> >& token_sequence );


    template<typename T>
    bool ExpectAnyOf( std::unique_ptr<Token>& token );

    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf( std::unique_ptr<Token>& token );

    template<typename T>
    bool ExpectAnyOf();

    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf();


    bool ExpectTerminal( Lexer::TokenType token_type, std::pair< Lexer::TokenType, std::string >& terminal );

    bool ExpectTerminal( Lexer::TokenType token_type );

private:
    Lexer::Lexer m_lexer;

    std::unique_ptr<TranslationUnit> m_translationUnit;
    std::vector< Lexer::TokenType > m_expectedTerminals;
};

} // namespace Parser
} // namespace JoeLang

#include "parser-inl.hpp"
