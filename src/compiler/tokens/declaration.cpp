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
                             VariableOrFunctionDeclaration,
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
// VariableOrFunctionDeclaration
//------------------------------------------------------------------------------

VariableOrFunctionDeclaration::VariableOrFunctionDeclaration(
        std::unique_ptr<DeclarationSpecifiers> decl_specs )
    :DeclarationBase( DeclarationTy::VariableOrFunctionDeclaration )
    ,m_declSpecs( std::move(decl_specs) )
{
}

VariableOrFunctionDeclaration::~VariableOrFunctionDeclaration()
{
}

void VariableOrFunctionDeclaration::Print( int depth ) const
{
    for( int i = 0; i < depth; ++i )
        std::cout << "    ";
    std::cout << "declaration";
}

void VariableOrFunctionDeclaration::PerformSema( SemaAnalyzer& sema )
{
}

bool VariableOrFunctionDeclaration::Parse(
                          Parser& parser,
                          std::unique_ptr<VariableOrFunctionDeclaration>& token )
{
    std::unique_ptr<DeclarationSpecifiers> decl_specs;
    if( !parser.Expect<DeclarationSpecifiers>( decl_specs ) )
        return false;

    return false;
}

//------------------------------------------------------------------------------
// DeclarationSpecifiers
//------------------------------------------------------------------------------

DeclarationSpecifiers::DeclarationSpecifiers()
{
}

DeclarationSpecifiers::~DeclarationSpecifiers()
{
}

void DeclarationSpecifiers::Print( int depth ) const
{
}

bool DeclarationSpecifiers::Parse(
                                Parser& parser,
                                std::unique_ptr<DeclarationSpecifiers>& token )
{
    return false;
}

//------------------------------------------------------------------------------
// TypeSpecifier
//------------------------------------------------------------------------------

TypeSpecifier::TypeSpecifier( TypeSpec t )
    :m_typeSpec( t )
{
}

TypeSpecifier::~TypeSpecifier()
{
}

void TypeSpecifier::Print( int depth ) const
{
}

bool TypeSpecifier::Parse( Parser& parser,
                           std::unique_ptr<TypeSpecifier>& token )
{
    const static std::vector<std::pair<TerminalType, TypeSpec> > type_map =
    {
        { TerminalType::TYPE_VOID,     TypeSpec::VOID     },
        { TerminalType::TYPE_CHAR,     TypeSpec::CHAR     },
        { TerminalType::TYPE_SHORT,    TypeSpec::SHORT    },
        { TerminalType::TYPE_INT,      TypeSpec::INT      },
        { TerminalType::TYPE_LONG,     TypeSpec::LONG     },
        { TerminalType::TYPE_SIGNED,   TypeSpec::SIGNED   },
        { TerminalType::TYPE_UNSIGNED, TypeSpec::UNSIGNED },
        { TerminalType::TYPE_FLOAT,    TypeSpec::FLOAT    },
        { TerminalType::TYPE_DOUBLE,   TypeSpec::DOUBLE   },
        { TerminalType::TYPE_STRING,   TypeSpec::STRING   }
    };
    return false;
}

} // namespace Compiler
} // namespace JoeLang
