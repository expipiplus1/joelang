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

#include <memory>
#include <vector>

#include <compiler/casting.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
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
    :Token( TokenTy::TranslationUnit )
    ,m_Declarations( std::move(declarations) )
{
}

TranslationUnit::~TranslationUnit()
{
}

void TranslationUnit::PerformSema( SemaAnalyzer& sema )
{
    //
    // We want to give all of the declarations to sema
    //
    for( auto& d : m_Declarations )
    {
        DeclarationBase* r = d.release();
        if( std::unique_ptr<EmptyDeclaration> e{dyn_cast<EmptyDeclaration>(r)} )
        {
            // don't do anything for emptydeclarations
        }
        else if( std::unique_ptr<TechniqueDeclaration> t
                                           {dyn_cast<TechniqueDeclaration>(r)} )
        {
            sema.AddTechniqueDeclaration( std::move(t) );
        }
        else if( std::unique_ptr<PassDeclaration> p
                                           {dyn_cast<PassDeclaration>(r)} )
        {
            sema.AddPassDeclaration( std::move(p) );
        }
        else if( std::unique_ptr<VariableDeclarationList> v
                                        {dyn_cast<VariableDeclarationList>(r)} )
        {
            sema.AddVariableDeclarations( std::move(v) );
        }
        else if( std::unique_ptr<FunctionDefinition> f
                                             {dyn_cast<FunctionDefinition>(r)} )
        {
            sema.AddFunctionDefinition( std::move(f) );
        }
        else
        {
            assert( false && "Unhandled declaration type in PerformSema" );
        }
        assert( !d && "Unhandled declaraion" );
    }

    //
    // to remove the null pointers we now have
    //
    m_Declarations.clear();
}

const TranslationUnit::DeclarationVector&
                                        TranslationUnit::GetDeclarations() const
{
    return m_Declarations;
}

bool TranslationUnit::Parse( Parser& parser,
                             std::unique_ptr<TranslationUnit>& token )
{
    // Parse all of the top level declarations in the file
    DeclarationVector declarations;
    parser.ExpectSequenceOf<DeclarationBase>( declarations );
    CHECK_PARSER;

    // Make sure that there's nothing left
    if( !parser.ExpectTerminal( TerminalType::END_OF_INPUT ) )
    {
        parser.Error( "Extra input" );
        return false;
    }

    token.reset( new TranslationUnit( std::move(declarations) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
