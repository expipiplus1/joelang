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

#include "declarator.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include <compiler/support/generic_value.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/lexer/terminal_types.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/declarator_specifier.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/initializer.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// InitDeclarator
//------------------------------------------------------------------------------

InitDeclarator::InitDeclarator( std::unique_ptr<Declarator> declarator,
                                std::unique_ptr<Initializer> initializer )
    :Token( TokenTy::InitDeclarator )
    ,m_Declarator( std::move(declarator) )
    ,m_Initializer( std::move(initializer) )
    ,m_Variable( nullptr )
{
    assert( m_Declarator && "InitDeclarator given a null declarator" );
}

InitDeclarator::~InitDeclarator()
{
}

void InitDeclarator::PerformSema( SemaAnalyzer& sema,
                                  DeclSpecs& decl_specs )
{
    // Resolve things in the declarator
    bool can_init = m_Declarator->PerformSema( sema, decl_specs );

    Type base_type = decl_specs.GetType();
    const ArrayExtents& array_extents = m_Declarator->GetArrayExtents();
    m_IsGlobal = sema.InGlobalScope();

    if( m_Declarator->IsFunctionDeclarator() )
    {
        // If this is a function declarator then handle everything in
        // m_Declarator
        return;
    }

    //
    // This isn't a function declarator
    //
    if( decl_specs.IsInline() )
        sema.Error( "Can only use the 'inline' specifier on functions" );

    //
    // Only initialization specific stuff below here
    //

    //
    // Allow initializing a variable with a single value in braces for
    // example: 'int a = {1};'
    //
    if( array_extents.empty() &&
        m_Initializer &&
        m_Initializer->CanReduceToExpression() )
        m_Initializer->ReduceToExpression();

    // Cast the initializer to the right type
    if( m_Initializer )
        can_init &= m_Initializer->PerformSema( sema,
                                                CompleteType( base_type,
                                                              array_extents ) );

    // If the variable is const and not a uniform, it must have an initializer
    if( decl_specs.IsConst() &&
        !decl_specs.IsUniform() &&
        !m_Initializer )
    {
        sema.Error( "Const variables must have an initializer" );
        can_init = false;
    }

    // If the variable is in it has to be varying
    if( decl_specs.IsIn() && !decl_specs.IsVarying() )
    {
        sema.Error(
                "'in' variables must be varying or a parameter to a function" );
        decl_specs.SetIsVarying( true );
        decl_specs.SetIsUniform( false );
    }

    // Evaluate the initializer
    GenericValue initializer;
    if( m_Initializer && can_init )
    {
        initializer = sema.EvaluateInitializer( *m_Initializer );
        assert( initializer.GetUnderlyingType() == base_type &&
                "Trying to initialize variable with wrong underlying type" );
        if( initializer.GetArrayExtents() != array_extents )
        {
            sema.Error( "Trying to initialize variable with wrong array "
                        "extents" );
            can_init = false;
        }
    }

    //
    // If we can't initialize this for whatever reason, sema will have been
    // notified, so just pretend that this is non-const for the sake of later
    // analysis
    //
    m_Variable = std::make_shared<Variable>( CompleteType( base_type,
                                                           array_extents ),
                                             m_Declarator->GetSemantic(),
                                             decl_specs.IsConst() && can_init,
                                             decl_specs.IsUniform(),
                                             decl_specs.IsVarying(),
                                             decl_specs.IsIn(),
                                             decl_specs.IsOut(),
                                             m_IsGlobal,
                                             false, // Isn't a parameter
                                             can_init ? std::move(initializer)
                                                      : GenericValue(),
                                             m_Declarator->GetIdentifier() );
    sema.DeclareVariable( m_Declarator->GetIdentifier(), m_Variable );

    //
    // CodeGen the variable because sema may depend on it later on
    //
    m_Variable->CodeGen( sema.GetCodeGenerator() );
}

Declarator_up InitDeclarator::TakeDeclarator()
{
    assert( !m_Initializer && "Taking a declarator away from its initializer" );
    return std::move(m_Declarator);
}

bool InitDeclarator::IsFunctionDeclarator() const
{
    return m_Declarator->IsFunctionDeclarator();
}

bool InitDeclarator::Parse( Parser& parser,
                            std::unique_ptr<InitDeclarator>& token )
{
    std::unique_ptr<Declarator> declarator;
    if( !parser.Expect<Declarator>( declarator ) )
        return false;

    if( declarator->IsFunctionDeclarator() )
    {
        // If this is a function declarator, don't try and parse an initializer
        token.reset( new InitDeclarator( std::move(declarator) ) );
        return true;
    }

    // If we don't see an equals sign, it may be a brace initializer, but we
    // want to leave the brace to be parsed by Initializer (a little dirty)
    if( !parser.ExpectTerminal( TerminalType::EQUALS ) &&
        parser.PeekTerminal() !=  TerminalType::OPEN_BRACE )
    {
        token.reset( new InitDeclarator( std::move(declarator) ) );
        return true;
    }

    // We've seen an equals sign so parse the initializer
    std::unique_ptr<Initializer> initializer;
    if( !parser.Expect<Initializer>( initializer ) )
        return false;

    token.reset( new InitDeclarator( std::move(declarator),
                                     std::move(initializer) ) );
    return true;
}

//------------------------------------------------------------------------------
// Declarator
//------------------------------------------------------------------------------

Declarator::Declarator( std::string identifier,
                        FunctionSpecifier_up function_specifier,
                        Declarator::ArraySpecifierVector array_specifiers,
                        SemanticSpecifier_up semantic_specifier )
    :Token( TokenTy::Declarator )
    ,m_Identifier( std::move(identifier) )
    ,m_FunctionSpecifier( std::move(function_specifier) )
    ,m_ArraySpecifiers( std::move(array_specifiers) )
    ,m_SemanticSpecifier( std::move(semantic_specifier) )
{
}

Declarator::~Declarator()
{
}

bool Declarator::PerformSema( SemaAnalyzer& sema, const DeclSpecs& decl_specs )
{
    bool ret = true;

    Type base_type = decl_specs.GetType();

    if( base_type == Type::UNKNOWN )
    {
        sema.Error( "No type in declaration specifier" );
        // No point in declaring things with no type
        ret = false;
    }
    else if( base_type == Type::VOID &&
             !IsFunctionDeclarator() )
    {
        // If this is of void type and isn't a function
        sema.Error( "Can't declare variables of void type" );
        ret = false;
    }

    // If this isn't const it can't be a string
    if( !decl_specs.IsConst() &&
        base_type == Type::STRING )
        sema.Error( "Variables of type string must be const" );

    m_ArrayExtents = ArraySpecifier::GetArrayExtents( m_ArraySpecifiers, sema );

    if( base_type == Type::VOID &&
        !m_ArrayExtents.empty() )
        sema.Error( "Can't have an array of void type" );

    if( m_SemanticSpecifier )
        m_SemanticSpecifier->PerformSema( sema );

    if( m_FunctionSpecifier )
    {
        // If we are a function specifier we have to declare
        ret &= m_FunctionSpecifier->PerformSema( sema );
        // If there was a problem parsing the function specifier then we can't
        // register the function with sema
        if( ret )
        {
            CompleteType return_type( base_type, m_ArrayExtents );
            sema.DeclareFunction( m_Identifier,
                                  std::move(return_type),
                                  GetSemantic(),
                                  GetFunctionParameterTypes() );
        }
    }

    return ret;
}

void Declarator::DeclareFunctionParameters( SemaAnalyzer& sema ) const
{
    assert( IsFunctionDeclarator() &&
            "Trying to declare function parameters for a non function "
            "declarator" );
    m_FunctionSpecifier->DeclareParameters( sema );
}

std::vector<CompleteType> Declarator::GetFunctionParameterTypes() const
{
    assert( IsFunctionDeclarator() &&
            "Trying to get function parameter types for a non-function "
            "declarator" );

    return m_FunctionSpecifier->GetParameterTypes();
}

std::vector<Variable_sp> Declarator::GetFunctionParameters() const
{
    return m_FunctionSpecifier->GetParameters();
}

const std::string& Declarator::GetIdentifier() const
{
    return m_Identifier;
}

const ArrayExtents& Declarator::GetArrayExtents() const
{
    return m_ArrayExtents;
}

bool Declarator::IsFunctionDeclarator() const
{
    return static_cast<bool>(m_FunctionSpecifier);
}

Semantic Declarator::GetSemantic() const
{
    if( m_SemanticSpecifier )
        return m_SemanticSpecifier->GetSemantic();
    else
        return Semantic();
}

bool Declarator::Parse( Parser& parser, std::unique_ptr<Declarator>& token )
{
    // Parse an identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    //
    // optional function specifier
    //
    std::unique_ptr<FunctionSpecifier> function_specifier;
    parser.Expect<FunctionSpecifier>( function_specifier );
    CHECK_PARSER;

    //
    // optional array specifier
    //
    ArraySpecifierVector array_specifiers;
    parser.ExpectSequenceOf<ArraySpecifier>( array_specifiers );
    CHECK_PARSER;

    //
    // optional semantic
    //
    SemanticSpecifier_up semantic_specifier;
    parser.Expect<SemanticSpecifier>( semantic_specifier );
    CHECK_PARSER;

    token.reset( new Declarator( std::move(identifier),
                                 std::move(function_specifier),
                                 std::move(array_specifiers),
                                 std::move(semantic_specifier) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
