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

#include "translation_unit.hpp"

#include <iostream>
#include <memory>
#include <vector>

#include <compiler/parser.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// TranslationUnit
//------------------------------------------------------------------------------

TranslationUnit::TranslationUnit( DeclarationVector declarations )
    :m_declarations( std::move(declarations) )
{
}

TranslationUnit::~TranslationUnit()
{
}

void TranslationUnit::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i)
        std::cout << " ";
    std::cout << "TranslationUnit\n";
    for( const auto& declaration : m_declarations )
        declaration->Print( depth + 1 );
}

const TranslationUnit::DeclarationVector&
                                        TranslationUnit::GetDeclarations() const
{
    return m_declarations;
}

bool TranslationUnit::Parse( Parser& parser,
                             std::unique_ptr<TranslationUnit>& token )
{
    // Parse all of the top level declarations in the file
    DeclarationVector declarations;
    if( !parser.ExpectSequenceOf<DeclarationBase>( declarations ) )
        return false;

    // Make sure that there's nothing left
    if( !parser.ExpectTerminal( TerminalType::END_OF_INPUT ) )
        return false;

    token.reset( new TranslationUnit( std::move(declarations) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
