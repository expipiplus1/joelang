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
#include "terminal.hpp"
#include "token.hpp"

namespace JoeLang
{
namespace Parser
{

DeclarationBase::~DeclarationBase()
{
}

bool DeclarationBase::Parse( Parser& parser, std::unique_ptr<DeclarationBase>& token )
{
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<TechniqueDeclaration,
                            PassDeclaration,
                            EmptyDeclaration>( t ) )
        return false;
    token.reset( dynamic_cast<DeclarationBase*>( t.release() ) );
    return true;
}

EmptyDeclaration::EmptyDeclaration()
{
}

EmptyDeclaration::~EmptyDeclaration()
{
}

bool EmptyDeclaration::Parse(Parser& parser , std::unique_ptr<EmptyDeclaration>& token )
{
    if( !parser.Expect< Terminal<Lexer::SEMICOLON> >() )
        return false;
    token.reset( new EmptyDeclaration );
    return true;
}

PassDeclaration::PassDeclaration( std::string name )
    :m_name( name )
{
}

PassDeclaration::~PassDeclaration()
{
}


bool PassDeclaration::Parse( Parser& parser, std::unique_ptr<PassDeclaration>& token )
{
    if( !parser.Expect< Terminal<Lexer::PASS> >() )
        return false;

    std::unique_ptr< Terminal<Lexer::IDENTIFIER> > name_terminal;
    std::string name;
    if( parser.Expect< Terminal<Lexer::IDENTIFIER> >( name_terminal ) )
        name = name_terminal->GetString();

    if( !parser.Expect< Terminal<Lexer::OPEN_BRACE> >() )
        return false;

    if( !parser.Expect< Terminal<Lexer::CLOSE_BRACE> >() )
        return false;

    token.reset( new PassDeclaration( name ) );
    return true;
}


TechniqueDeclaration::TechniqueDeclaration( std::string name, std::vector< std::unique_ptr<PassDeclaration> > passes )
    :m_name( std::move( name ) )
    ,m_passes( std::move( passes ) )
{
}

TechniqueDeclaration::~TechniqueDeclaration()
{
}


bool TechniqueDeclaration::Parse( Parser& parser, std::unique_ptr<TechniqueDeclaration>& token )
{
    if( !parser.Expect< Terminal<Lexer::TECHNIQUE> >() )
        return false;

    //Oh what I'd give for some lambdas here
    std::unique_ptr< Terminal<Lexer::IDENTIFIER> > name_terminal;
    std::string name;
    if( parser.Expect< Terminal<Lexer::IDENTIFIER> >( name_terminal ) )
        name = name_terminal->GetString();

    if( !parser.Expect< Terminal<Lexer::OPEN_BRACE> >() )
        return false;

    std::vector< std::unique_ptr<PassDeclaration> > passes;
    parser.ExpectSequenceOf<PassDeclaration>( passes );

    if( !parser.Expect< Terminal<Lexer::CLOSE_BRACE> >() )
        return false;

    token.reset( new TechniqueDeclaration( name, std::move( passes ) ) );
    return true;
}


DeclarationSeq::DeclarationSeq( std::vector< std::unique_ptr<DeclarationBase> >&& declarations )
    :m_declarations( std::move( declarations ) )
{
}

DeclarationSeq::~DeclarationSeq()
{
}

bool DeclarationSeq::Parse( Parser& parser, std::unique_ptr<DeclarationSeq>& token )
{
    std::unique_ptr<DeclarationBase> declaration;
    if( !parser.Expect<DeclarationBase>( declaration ) )
        return false;

    std::vector< std::unique_ptr<DeclarationBase> > declarations;
    declarations.push_back( std::move( declaration ) );

    while( parser.Expect<DeclarationBase>( declaration ) )
        declarations.push_back( std::move( declaration ) );

    token.reset( new DeclarationSeq( std::move( declarations ) ) );
    return true;
}

} // namespace Parser
} // namespace JoeLang
