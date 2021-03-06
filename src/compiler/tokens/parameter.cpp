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

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/declarator_specifier.hpp>
#include <compiler/tokens/initializer.hpp>
#include <compiler/tokens/token.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Parameter
//------------------------------------------------------------------------------

Parameter::Parameter ( DeclSpecsVector decl_specs,
                       std::string identifier,
                       ArraySpecifierVector array_specifiers,
                       SemanticSpecifier_up semantic_specifier,
                       Initializer_up default_value )
:Token( TokenTy::Parameter )
,m_DeclarationSpecifiers( std::move(decl_specs) )
,m_Identifier( std::move(identifier) )
,m_ArraySpecifers( std::move(array_specifiers) )
,m_SemanticSpecifier( std::move(semantic_specifier) )
,m_DefaultValue( std::move(default_value) )
{
    assert( !m_DeclarationSpecifiers.empty() &&
            "Parameter given no declaration specifiers" );
}

Parameter::~Parameter()
{
}

bool Parameter::PerformSema( SemaAnalyzer& sema )
{
    DeclSpecs decl_specs;

    //
    // If there was a problem evaluating the declaration specifiers we can't
    // continue
    //
    if( !decl_specs.AnalyzeDeclSpecs( m_DeclarationSpecifiers, sema ) )
        return false;

    //
    // Check some other things about the specifiers
    //
    if( decl_specs.IsInline() )
        // Don't return here, this is a recoverable error
        sema.Error( "Can't have an 'inline' specifier on a parameter" );

    if( decl_specs.IsStatic() )
        sema.Error( "Can't have a 'static' specifier on a non-global "
                    "variable" );

    if( !decl_specs.IsUniform() )
        decl_specs.SetIsVarying( true );

    if( !(decl_specs.IsIn() || decl_specs.IsOut() ) )
        // parameters are in by default
        decl_specs.SetIsIn( true );

    Type base_type = decl_specs.GetType();
    if( base_type == Type::UNKNOWN )
    {
        sema.Error( "No type in declaration specifier" );
        return false;
    }
    else if( base_type == Type::VOID )
    {
        sema.Error( "Can't declare variables of void type" );
        return false;
    }
    ArrayExtents array_extents =ArraySpecifier::GetArrayExtents(
                                                               m_ArraySpecifers,
                                                               sema );

    if( array_extents.empty() &&
        m_DefaultValue &&
        m_DefaultValue->CanReduceToExpression() )
        m_DefaultValue->ReduceToExpression();

    if( m_DefaultValue )
        m_DefaultValue->PerformSema( sema, CompleteType( base_type,
                                                         array_extents ) );

    if( !decl_specs.IsConst() &&
        base_type == Type::STRING )
        sema.Error( "Variables of type string must be const" );

    if( m_DefaultValue &&
        m_DefaultValue->GetArrayExtents() != array_extents )
        sema.Error( "Default parameter value has mismatching array extents" );

    Semantic semantic = m_SemanticSpecifier ? m_SemanticSpecifier->GetSemantic()
                                            : Semantic();

    m_Variable = std::make_shared<Variable>( CompleteType( base_type,
                                                           array_extents ),
                                             std::move(semantic),
                                             decl_specs.IsConst(),
                                             decl_specs.IsUniform(),
                                             decl_specs.IsVarying(),
                                             decl_specs.IsIn(),
                                             decl_specs.IsOut(),
                                             false, //Isn't global
                                             true,  //Is a param
                                             GenericValue(),
                                             m_Identifier );

    m_Type = CompleteType( base_type,
                           std::move(array_extents) );
    return true;
}

void Parameter::Declare( SemaAnalyzer& sema ) const
{
    assert( m_Variable && "Trying to declare a parameter without a variable" );
    sema.DeclareVariable( m_Identifier, m_Variable );
}

const CompleteType& Parameter::GetType() const
{
    assert( !m_Type.IsUnknown() && "Trying to get uninitialized parameters" );
    return m_Type;
}

const Variable_sp& Parameter::GetVariable() const
{
    assert( m_Variable && "Trying to get an uninitialized variable" );
    return m_Variable;
}

bool Parameter::Parse( Parser& parser, std::unique_ptr<Parameter>& token )
{
    DeclSpecsVector decl_specs;
    if( !parser.ExpectSequenceOf<DeclarationSpecifier>(decl_specs) )
        return false;

    std::string identifier;
    ArraySpecifierVector array_specifiers;
    Initializer_up default_value;

    parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier );
    parser.ExpectSequenceOf<ArraySpecifier>( array_specifiers );
    CHECK_PARSER;

    SemanticSpecifier_up semantic_specifier;
    parser.Expect<SemanticSpecifier>( semantic_specifier );
    CHECK_PARSER;

    // If we have an identifier we can look for an optional Expression (but only
    // if we also match an '='
    if( !identifier.empty() &&
        parser.ExpectTerminal( TerminalType::EQUALS ) &&
        !parser.Expect<Initializer>( default_value ) )
        return false;

    token.reset( new Parameter( std::move(decl_specs),
                                std::move(identifier),
                                std::move(array_specifiers),
                                std::move(semantic_specifier),
                                std::move(default_value) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
