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

#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Declarator
//------------------------------------------------------------------------------

InitDeclarator::InitDeclarator( std::unique_ptr<Declarator> declarator,
                                std::unique_ptr<Expression> initializer )
    :Token( TokenTy::InitDeclarator )
    ,m_declarator( std::move(declarator) )
    ,m_initializer( std::move(initializer) )
    ,m_variable( nullptr )
{
    assert( m_declarator && "InitDeclarator given a null declarator" );
}

InitDeclarator::~InitDeclarator()
{
}

void InitDeclarator::PerformSema( SemaAnalyzer& sema,
                                  const DeclSpecs& decl_specs )
{
    // Resolve things in the declarator
    m_declarator->PerformSema( sema );

    m_isGlobal = sema.InGlobalScope();

    bool can_init = true;

    // Reduce the initializer as much as possible
    if( m_initializer )
    {
        //TODO move this somewhere else
        m_initializer->ResolveIdentifiers( sema );
        m_initializer = CastExpression::Create(
                                            decl_specs.GetType(),
                                            std::move( m_initializer ) );
        if( m_initializer->PerformSema( sema ) )
            m_initializer->FoldConstants( m_initializer );

        assert( m_initializer->GetReturnType() == decl_specs.GetType() &&
                "Trying to initialize a variable with mismatched types" );
    }

    // If the variable is const, it must have an initializer
    if( decl_specs.IsConst() )
    {
        if( !m_initializer )
        {
            sema.Error( "Const variables must have an initializer" );
            can_init = false;
        }
        else
        {
            if( !Expression::GetLiteral( m_initializer ) )
            {
                sema.Error( "trying to initialize a variable with a non-const "
                            "expression" );
                can_init = false;
            }
        }
    }
    else // If this isn't const it can't be a string
    {
        if( decl_specs.GetType() == Type::STRING )
            sema.Error( "Variables of type string must be const" );
    }

    Type base_type = decl_specs.GetType();
    std::vector<std::unique_ptr<Expression>> array_dimension_sizes =
                                        m_declarator->GetArrayDimensionSizes();

    m_variable = std::make_shared<Variable>( base_type,
                                             std::move(array_dimension_sizes),
                                             decl_specs.IsConst() && can_init,
                                             m_isGlobal,
                                             std::move(m_initializer) );
    // If we can't initialize this for whatever reason, sema will have been
    // notified, so just pretend that this is non-const for the sake of later
    // analysis
    sema.DeclareVariable( m_declarator->GetIdentifier(), m_variable );
}

void InitDeclarator::CodeGen( CodeGenerator& code_gen )
{
    m_variable->CodeGen( code_gen );
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

    if( !parser.ExpectTerminal( TerminalType::EQUALS ) )
    {
        token.reset( new InitDeclarator( std::move(declarator) ) );
        return true;
    }

    // We've seen an equals sign so parse the initializer
    std::unique_ptr<Expression> initializer;
    if( !parser.Expect<AssignmentExpression>( initializer ) )
        return false;

    token.reset( new InitDeclarator( std::move(declarator),
                                     std::move(initializer) ) );
    return true;
}

//------------------------------------------------------------------------------
// Declarator
//------------------------------------------------------------------------------

Declarator::Declarator( std::string identifier,
                        Declarator::ArraySpecifierVector array_specifiers )
    :Token( TokenTy::Declarator )
    ,m_identifier( std::move(identifier) )
    ,m_arraySpecifiers( std::move(array_specifiers) )
{
}

Declarator::~Declarator()
{
}

void Declarator::PerformSema( SemaAnalyzer& sema )
{
    for( auto& array_specifier : m_arraySpecifiers )
        array_specifier->PerformSema( sema );
}

void Declarator::Print( int depth ) const
{
}

const std::string& Declarator::GetIdentifier() const
{
    return m_identifier;
}

std::vector<std::unique_ptr<Expression> >
                                    Declarator::GetArrayDimensionSizes() const
{
    std::vector<std::unique_ptr<Expression>> ret;
    for( const auto& array_specifier : m_arraySpecifiers )
        ret.push_back( array_specifier->GetExpression() );
    return ret;
}

bool Declarator::Parse( Parser& parser, std::unique_ptr<Declarator>& token )
{
    // Parse an identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    // Is this a function declarator?
    if( parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
    {
        parser.Error( "Functions not implemented yet" );
    }

    ArraySpecifierVector array_specifiers;
    parser.ExpectSequenceOf<ArraySpecifier>( array_specifiers );
    CHECK_PARSER;

    token.reset( new Declarator( std::move(identifier),
                                 std::move(array_specifiers) ) );
    return true;
}

//------------------------------------------------------------------------------
// ArraySpecifier
//------------------------------------------------------------------------------

ArraySpecifier::ArraySpecifier( std::unique_ptr<Expression> expression )
    :Token( TokenTy::ArraySpecifier )
    ,m_expression( std::move( expression ) )
{
    assert( m_expression && "ArraySpecifier given a null expression" );
}

ArraySpecifier::~ArraySpecifier()
{
}

void ArraySpecifier::PerformSema( SemaAnalyzer& sema )
{
    if( !sema.TryResolveToLiteral( m_expression, Type::I64 ) )
        sema.Error( "Can't create array with non-const dimension" );
}

std::unique_ptr<Expression> ArraySpecifier::GetExpression()
{
    return std::move(m_expression);
}

void ArraySpecifier::Print( int depth ) const
{
}

bool ArraySpecifier::Parse( Parser& parser,
                            std::unique_ptr<ArraySpecifier>& token )
{
    // Opening square bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
        return false;

    std::unique_ptr<Expression> expression;
    if( !parser.Expect<Expression>( expression ) )
    {
        parser.Error( "No expression in array specifier" );
        return false;
    }

    if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
    {
        //TODO things like here non fatal error, assume the closing bracket
        parser.Error( "']' missing in array specifier" );
        return false;
    }

    token.reset( new ArraySpecifier( std::move(expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
