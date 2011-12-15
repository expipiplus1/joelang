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

#include "declaration.hpp"

#include <memory>
#include <string>
#include <utility>
#include <vector>
#include "lexer.hpp"
#include "parser.hpp"

namespace JoeLang
{
namespace Parser
{
namespace Declaration
{

std::unique_ptr< DeclarationBase > DeclarationBase::Parse( Lexer::Lexer& lexer )
{
    std::unique_ptr< DeclarationBase > declaration;
    lexer.PushRestorePoint();

    declaration = TechniqueDeclaration::Parse( lexer );
    if( declaration )
    {
        lexer.PopRestorePoint();
        return declaration;
    }

    lexer.Restore();
    return nullptr;
}

TechniqueDeclaration::~TechniqueDeclaration()
{
}

TechniqueDeclaration::TechniqueDeclaration( std::string name )
    :m_name( name )
{
}

std::unique_ptr< TechniqueDeclaration > TechniqueDeclaration::Parse( Lexer::Lexer& lexer )
{
    lexer.PushRestorePoint();
    std::string name;

    if( !lexer.ConsumeToken( Lexer::TECHNIQUE ) )
        return nullptr;

    if( lexer.PeekToken() == Lexer::IDENTIFIER )
    {
        name = lexer.PeekString();
        lexer.ConsumeNext();
    }

    if( !lexer.ConsumeToken( Lexer::OPEN_BRACE ) )
        return nullptr;

    if( !lexer.ConsumeToken( Lexer::CLOSE_BRACE ) )
        return nullptr;

    lexer.PopRestorePoint();
    return std::unique_ptr<TechniqueDeclaration>( new TechniqueDeclaration( name ) );
}

DeclarationSeq::~DeclarationSeq()
{
}

DeclarationSeq::DeclarationSeq( std::vector< std::unique_ptr<DeclarationBase> >&& declarations )
    :m_declarations( std::move( declarations ) )
{
}

std::unique_ptr<DeclarationSeq> DeclarationSeq::Parse(Lexer::Lexer &lexer)
{
    lexer.PushRestorePoint();
    std::vector< std::unique_ptr<DeclarationBase> > declarations;

    std::unique_ptr<DeclarationBase> declaration( DeclarationBase::Parse( lexer ) );
    if( !declaration )
    {
        lexer.Restore();
        return nullptr;
    }

    declarations.push_back( std::move(declaration) );

    while( ( declaration = DeclarationBase::Parse( lexer ) ) )
    {
        declarations.push_back( std::move( declaration ) );
    }

    lexer.PopRestorePoint();
    return std::unique_ptr<DeclarationSeq>( new DeclarationSeq( std::move( declarations ) ) );
}

} // namespace Declaration
} // namespace Parser
} // namespace JoeLang
