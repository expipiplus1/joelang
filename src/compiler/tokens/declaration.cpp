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

#include "declaration.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <compiler/casting.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/declarator.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/state_assignment_statement.hpp>
#include <compiler/tokens/token.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// DeclarationBase
//------------------------------------------------------------------------------

DeclarationBase::DeclarationBase( DeclarationTy sub_class_id )
    :m_subClassID( sub_class_id )
{
}

DeclarationBase::~DeclarationBase()
{
}

bool DeclarationBase::Parse( Parser& parser,
                             std::unique_ptr<DeclarationBase>& token )
{
    // Try and parse any of the top level declarations
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf< TechniqueDeclaration,
                             PassDeclaration,
                             VariableListOrFunctionDefinition,
                             EmptyDeclaration>( t ) )
        return false;

    //TODO set up casting for every token
    //assert( isa<DeclarationBase>( t ) &&
            //"DeclarationBase parsed a non-declaration" );
    // Cast it back to a DeclarationBase because ExpectAnyOf only returns Token*
    token.reset( static_cast<DeclarationBase*>( t.release() ) );
    return true;
}

DeclarationBase::DeclarationTy DeclarationBase::GetSubClassID() const
{
    return m_subClassID;
}

bool DeclarationBase::classof( const DeclarationBase* d )
{
    // A DeclarationBase is always a DeclarationBase
    return true;
}


//------------------------------------------------------------------------------
// EmptyDeclaration
//------------------------------------------------------------------------------

EmptyDeclaration::EmptyDeclaration()
    :DeclarationBase( DeclarationTy::EmptyDeclaration )
{
}

EmptyDeclaration::~EmptyDeclaration()
{
}

void EmptyDeclaration::PerformSema( SemaAnalyzer& sema )
{
}

void EmptyDeclaration::Print(int depth) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "Empty Declaration\n";
}

bool EmptyDeclaration::Parse( Parser& parser,
                              std::unique_ptr<EmptyDeclaration>& token )
{
    // Try to parse just a semicolon
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new EmptyDeclaration );
    return true;
}

bool EmptyDeclaration::classof( const DeclarationBase* d )
{
    return d->GetSubClassID() == DeclarationTy::EmptyDeclaration;
}

bool EmptyDeclaration::classof( const EmptyDeclaration* d )
{
    return true;
}

//------------------------------------------------------------------------------
// PassDeclaration
//------------------------------------------------------------------------------

PassDeclaration::PassDeclaration( std::string name,
                                  std::unique_ptr<PassDefinition> definition )
    :DeclarationBase( DeclarationTy::PassDeclaration )
    ,m_name         ( std::move(name) )
    ,m_definition   ( std::move(definition) )
{
}

PassDeclaration::~PassDeclaration()
{
}

void PassDeclaration::PerformSema( SemaAnalyzer& sema )
{
    sema.DeclarePass( m_name, std::move(m_definition) );
}

const std::string& PassDeclaration::GetName() const
{
    return m_name;
}

bool PassDeclaration::HasDefinition() const
{
    return bool(m_definition);
}

const std::unique_ptr<PassDefinition>& PassDeclaration::GetDefinition() const
{
    return m_definition;
}

void PassDeclaration::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Pass: " << ( m_name.size() ? m_name : "Unnamed" ) << "\n";
    if( m_definition )
    {
        m_definition->Print( depth + 1);
    }
    else
    {
        for( int i = 0; i < depth * 4; ++i )
            std::cout << " ";
        std::cout << "No definition\n";
    }
}

bool PassDeclaration::Parse( Parser& parser,
                             std::unique_ptr<PassDeclaration>& token )
{
    // Needs to start with 'pass'
    if( !parser.ExpectTerminal( TerminalType::PASS ) )
        return false;

    // TODO diagnostic about anonymous passes
    std::string name;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, name ) )
        return false;

    // If we have a semicolon here, this pass doesn't have a definition
    if( parser.ExpectTerminal( TerminalType::SEMICOLON ) )
    {
        token.reset( new PassDeclaration( name, nullptr ) );
        return true;
    }
    // We may have moved the lexer on trying to parse the semicolon
    CHECK_PARSER;

    // This declaration must have a definition
    std::unique_ptr< PassDefinition > definition;
    if( !parser.Expect<PassDefinition>( definition ) )
        return false;

    token.reset( new PassDeclaration( std::move(name),
                                      std::move(definition) ) );
    return true;
}

bool PassDeclaration::classof( const DeclarationBase* d )
{
    return d->GetSubClassID() == DeclarationTy::PassDeclaration;
}

bool PassDeclaration::classof( const PassDeclaration* d )
{
    return true;
}

//------------------------------------------------------------------------------
// TechniqueDeclaration
//------------------------------------------------------------------------------

TechniqueDeclaration::TechniqueDeclaration( std::string name,
                                            PassDeclarationVector passes )

    :DeclarationBase( DeclarationTy::TechniqueDeclaration )
    ,m_name( std::move(name) )
    ,m_passes( std::move(passes) )
{
#ifndef NDEBUG
    for( const auto& p : m_passes )
        assert( p && "TechniqueDeclaration given a null pass" );
#endif
}

TechniqueDeclaration::~TechniqueDeclaration()
{
}

void TechniqueDeclaration::PerformSema( SemaAnalyzer& sema )
{
    sema.DeclareTechnique( m_name );
    for( auto& p : m_passes )
        p->PerformSema( sema );
}

const std::string& TechniqueDeclaration::GetName() const
{
    return m_name;
}

Technique TechniqueDeclaration::GenerateTechnique(
                                                 CodeGenerator& code_gen ) const
{
    std::vector<Pass> passes;
    for( const auto& p : m_passes )
        passes.push_back( p->GeneratePass( code_gen ) );
    return Technique( m_name, std::move(passes) );
}

void TechniqueDeclaration::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";

    std::cout << "Technique: " << (m_name.size() ? m_name : "Unnamed") << "\n";
    for( const auto& p : m_passes )
        p->Print( depth + 1 );
}

bool TechniqueDeclaration::Parse( Parser& parser,
                                  std::unique_ptr<TechniqueDeclaration>& token )
{
    // Has to start with 'technique'
    if( !parser.ExpectTerminal( TerminalType::TECHNIQUE ) )
        return false;

    // Parse the technique's name into name
    std::string name;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, name ) )
        return false;

    // Technique Declarations always have a definition

    // Start with an open brace
    if( !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    // Try and parse the pass declarations
    PassDeclarationVector pass_declarations;
    parser.ExpectSequenceOf<PassDeclarationOrIdentifier>( pass_declarations );
    CHECK_PARSER;

    // End with a close brace
    if( !parser.ExpectTerminal( TerminalType::CLOSE_BRACE ) )
        return false;

    token.reset( new TechniqueDeclaration( std::move(name),
                                           std::move(pass_declarations) ) );
    return true;
}

bool TechniqueDeclaration::classof( const DeclarationBase* d )
{
    return d->GetSubClassID() == DeclarationTy::TechniqueDeclaration;
}

bool TechniqueDeclaration::classof( const TechniqueDeclaration* d )
{
    return true;
}

//------------------------------------------------------------------------------
// VariableListOrFunctionDefinition
//------------------------------------------------------------------------------

VariableListOrFunctionDefinition::VariableListOrFunctionDefinition(
                                                    DeclarationTy sub_class_id )
    :DeclarationBase( sub_class_id )
{
}

VariableListOrFunctionDefinition::~VariableListOrFunctionDefinition()
{
}

bool VariableListOrFunctionDefinition::Parse(
                      Parser& parser,
                      std::unique_ptr<VariableListOrFunctionDefinition>& token )
{
    DeclSpecsVector decl_specs;

    // We need at least one declaration specifier
    if( !parser.ExpectSequenceOf<DeclarationSpecifier>( decl_specs ) )
        return false;

    // If we see a semicolon after the declaration without a declarator
    if( parser.ExpectTerminal( TerminalType::SEMICOLON ) )
    {
        // This could also be a warning and return an empty
        // VariableDeclarationList
        parser.Error( "declaration without a declarator" );
        return false;
    }
    CHECK_PARSER;

    // Try and parse some declarators
    DeclaratorVector declarators;
    if( !parser.ExpectListOf<InitDeclarator, TerminalType::COMMA>( declarators ) )
        return false;

    // variable declarations must end in a semicolon
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new VariableDeclarationList( std::move(decl_specs),
                                              std::move(declarators) ) );
    return true;
}

//------------------------------------------------------------------------------
// VariableDeclarationList
//------------------------------------------------------------------------------

VariableDeclarationList::VariableDeclarationList( DeclSpecsVector decl_specs,
                                                  DeclaratorVector declarators)
    :VariableListOrFunctionDefinition( DeclarationTy::VariableDeclarationList )
    ,m_declSpecs( std::move(decl_specs) )
    ,m_declarators( std::move(declarators) )
{
    assert( !m_declSpecs.empty() &&
            "VariableDeclarationList given no declaration specifiers" );
}

VariableDeclarationList::~VariableDeclarationList()
{
}

void VariableDeclarationList::Print( int depth ) const
{
}

void VariableDeclarationList::PerformSema( SemaAnalyzer& sema )
{
}

//------------------------------------------------------------------------------
// FunctionDefinition
//------------------------------------------------------------------------------

FunctionDefinition::FunctionDefinition()
    :VariableListOrFunctionDefinition( DeclarationTy::FunctionDefinition )
{
}

FunctionDefinition::~FunctionDefinition()
{
}


} // namespace Compiler
} // namespace JoeLang
