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

#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/compound_statement.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/declarator_specifier.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/initializer.hpp>
#include <compiler/tokens/token.hpp>
#include <engine/internal/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Declarator
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
                                  const DeclSpecs& decl_specs )
{
    Type base_type = decl_specs.GetType();

    if( base_type == Type::UNKNOWN )
    {
        sema.Error( "No type in declaration specifier" );
        // No point in declaring things with no type
        return;
    }
    else if( base_type == Type::VOID )
    {
        sema.Error( "Can't declare variables of void type" );
        return;
    }

    // Resolve things in the declarator
    bool can_init = m_Declarator->PerformSema( sema );

    m_IsGlobal = sema.InGlobalScope();

    const ArrayExtents& array_extents = m_Declarator->GetArrayExtents();

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
        can_init &= m_Initializer->PerformSema( sema, base_type );

    // If the variable is const, it must have an initializer
    if( decl_specs.IsConst() &&
        !m_Initializer )
    {
        sema.Error( "Const variables must have an initializer" );
        can_init = false;
    }

    // If this isn't const it can't be a string
    if( !decl_specs.IsConst() &&
        base_type == Type::STRING )
        sema.Error( "Variables of type string must be const" );

    // Evaluate the initializer
    GenericValue initializer;
    if( m_Initializer && can_init )
    {
        initializer = sema.EvaluateInitializer( *m_Initializer );
        assert( initializer.GetUnderlyingType() == base_type &&
                "Trying to initialize variable with wrong underlying type" );
        if( initializer.GetArrayExtents() != array_extents )
        {
            sema.Error( "Trying to initialize variable with wrong array extents" );
            can_init = false;
        }
    }

    m_Variable = std::make_shared<Variable>( base_type,
                                             array_extents,
                                             decl_specs.IsConst() && can_init,
                                             m_IsGlobal,
                                             can_init ? std::move(initializer)
                                                      : GenericValue(),
                                             m_Declarator->GetIdentifier() );
    // If we can't initialize this for whatever reason, sema will have been
    // notified, so just pretend that this is non-const for the sake of later
    // analysis
    sema.DeclareVariable( m_Declarator->GetIdentifier(), m_Variable );

    //
    // CodeGen the variable because sema may depend on it later on
    //
    m_Variable->CodeGen( sema.GetCodeGenerator() );
}

void InitDeclarator::Print( int depth ) const
{
}

bool InitDeclarator::Parse( Parser& parser,
                            std::unique_ptr<InitDeclarator>& token )
{
    std::unique_ptr<Declarator> declarator;
    if( !parser.Expect<Declarator>( declarator ) )
        return false;

    // If we don't see an equals sign, it may be a brace initializer, but we
    // want to leave the brace to be parsed by Initializer (a little dirty)
    if( !parser.ExpectTerminal( TerminalType::EQUALS ) &&
        !parser.PeekTerminal( TerminalType::OPEN_BRACE ) )
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
                        CompoundStatement_up function_body )
    :Token( TokenTy::Declarator )
    ,m_Identifier( std::move(identifier) )
    ,m_FunctionSpecifier( std::move(function_specifier) )
    ,m_ArraySpecifiers( std::move(array_specifiers) )
    ,m_FunctionBody( std::move(function_body) )
{
    if( m_FunctionBody )
        assert( m_FunctionSpecifier &&
                "Declarator given a function definition without a function "
                "specifier" );
}

Declarator::~Declarator()
{
}

bool Declarator::PerformSema( SemaAnalyzer& sema )
{
    bool ret = true;
    if( m_FunctionSpecifier )
        ret &= m_FunctionSpecifier->PerformSema( sema );
    m_ArrayExtents = ArraySpecifier::GetArrayExtents( m_ArraySpecifiers, sema );
    return ret;
}

void Declarator::Print( int depth ) const
{
}

const std::string& Declarator::GetIdentifier() const
{
    return m_Identifier;
}

const ArrayExtents& Declarator::GetArrayExtents() const
{
    return m_ArrayExtents;
}

bool Declarator::Parse( Parser& parser, std::unique_ptr<Declarator>& token )
{
    // Parse an identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    std::unique_ptr<FunctionSpecifier> function_specifier;
    parser.Expect<FunctionSpecifier>( function_specifier );
    CHECK_PARSER;

    ArraySpecifierVector array_specifiers;
    parser.ExpectSequenceOf<ArraySpecifier>( array_specifiers );
    CHECK_PARSER;

    CompoundStatement_up compound_statement;

    if( function_specifier )
        // If we have a function specifier look for a compound statement
        parser.Expect<CompoundStatement>( compound_statement );

    CHECK_PARSER;

    token.reset( new Declarator( std::move(identifier),
                                 std::move(function_specifier),
                                 std::move(array_specifiers),
                                 std::move(compound_statement) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
