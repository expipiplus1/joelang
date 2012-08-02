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

#include "parameter.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declarator_specifier.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Parameter
//------------------------------------------------------------------------------

Parameter::Parameter    ( DeclSpecsVector decl_specs,
                          std::string identifier,
                          ArraySpecifierVector array_specifiers,
                          Expression_up default_value )
:Token( TokenTy::Parameter )
,m_DeclarationSpecifiers( std::move(decl_specs) )
,m_Identifier( std::move(identifier) )
,m_ArraySpecifers( std::move(array_specifiers) )
,m_DefaultValue( std::move(default_value) )
{
    assert( !m_DeclarationSpecifiers.empty() &&
            "Parameter given no declaration specifiers" );
}

Parameter::~Parameter()
{
}

void Parameter::PerformSema( SemaAnalyzer& sema )
{
}

void Parameter::Print( int depth ) const
{
}

bool Parameter::Parse( Parser& parser, std::unique_ptr<Parameter>& token )
{
    DeclSpecsVector decl_specs;
    if( !parser.ExpectSequenceOf<DeclarationSpecifier>(decl_specs) )
        return false;

    std::string identifier;
    ArraySpecifierVector array_specifiers;
    Expression_up default_value;

    parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier );
    parser.ExpectSequenceOf<ArraySpecifier>( array_specifiers );
    CHECK_PARSER;

    // If we have an identifier we can look for an optional Expression (but only
    // if we also match an '='
    if( !identifier.empty() &&
        parser.ExpectTerminal( TerminalType::EQUALS ) &&
        !parser.Expect<AssignmentExpression>( default_value ) )
        return false;

    token.reset( new Parameter( std::move(decl_specs),
                                std::move(identifier),
                                std::move(array_specifiers),
                                std::move(default_value) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
