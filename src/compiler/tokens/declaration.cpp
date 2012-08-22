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

#include <algorithm>
#include <cassert>
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
#include <compiler/tokens/token.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <compiler/type_properties.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/technique.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// DeclarationBase
//------------------------------------------------------------------------------

DeclarationBase::DeclarationBase( TokenTy sub_class_id )
    :Token( sub_class_id )
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

    assert( isa<DeclarationBase>( t ) &&
            "DeclarationBase parsed a non-declaration" );
    // Cast it back to a DeclarationBase because ExpectAnyOf only returns Token*
    token.reset( static_cast<DeclarationBase*>( t.release() ) );
    return true;
}

bool DeclarationBase::classof( const Token* d )
{
    return d->GetSubClassID() >= TokenTy::Declaration_Start &&
           d->GetSubClassID() <= TokenTy::Declaration_End;
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
    :DeclarationBase( TokenTy::EmptyDeclaration )
{
}

EmptyDeclaration::~EmptyDeclaration()
{
}

void EmptyDeclaration::PerformSema( SemaAnalyzer& sema )
{
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
    return d->GetSubClassID() == TokenTy::EmptyDeclaration;
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
    :DeclarationBase( TokenTy::PassDeclaration )
    ,m_Name         ( std::move(name) )
    ,m_Definition   ( std::move(definition) )
{
}

PassDeclaration::~PassDeclaration()
{
}

void PassDeclaration::PerformSema( SemaAnalyzer& sema )
{
    // Only declare it if it's not an anonymous pass
    if( !m_Name.empty() )
        sema.DeclarePass( m_Name, std::move(m_Definition) );
    else
    {
        if( sema.InGlobalScope() )
            sema.Error( "Declaring an anonymous pass in global scope" );
        // We haven't given the pass to sema, so it must have sema performed on
        // it now
        m_Definition->PerformSema( sema );
    }
}

const std::string& PassDeclaration::GetName() const
{
    return m_Name;
}

bool PassDeclaration::HasDefinition() const
{
    return bool(m_Definition);
}

const std::unique_ptr<PassDefinition>& PassDeclaration::GetDefinition() const
{
    return m_Definition;
}

std::unique_ptr<PassDefinition> PassDeclaration::TakeDefinition()
{
    return std::move(m_Definition);
}

bool PassDeclaration::Parse( Parser& parser,
                             std::unique_ptr<PassDeclaration>& token )
{
    // Needs to start with 'pass'
    if( !parser.ExpectTerminal( TerminalType::PASS ) )
        return false;

    // Allow anonymous passes
    std::string name;
    parser.ExpectTerminal( TerminalType::IDENTIFIER, name );

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
    return d->GetSubClassID() == TokenTy::PassDeclaration;
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

    :DeclarationBase( TokenTy::TechniqueDeclaration )
    ,m_Name( std::move(name) )
    ,m_Passes( std::move(passes) )
{
#ifndef NDEBUG
    for( const auto& p : m_Passes )
        assert( p && "TechniqueDeclaration given a null pass" );
#endif
}

TechniqueDeclaration::~TechniqueDeclaration()
{
}

void TechniqueDeclaration::PerformSema( SemaAnalyzer& sema )
{
    sema.DeclareTechnique( m_Name );
    SemaAnalyzer::ScopeHolder scope( sema );
    scope.Enter();
    for( auto& p : m_Passes )
        p->PerformSema( sema );
    scope.Leave();
}

const std::string& TechniqueDeclaration::GetName() const
{
    return m_Name;
}

Technique TechniqueDeclaration::GenerateTechnique(
                                                 CodeGenerator& code_gen ) const
{
    std::vector<Pass> passes;
    for( const auto& p : m_Passes )
        passes.push_back( p->GeneratePass( code_gen ) );
    return Technique( m_Name, std::move(passes) );
}

bool TechniqueDeclaration::Parse( Parser& parser,
                                  std::unique_ptr<TechniqueDeclaration>& token )
{
    // Has to start with 'technique'
    if( !parser.ExpectTerminal( TerminalType::TECHNIQUE ) )
        return false;

    // Parse the technique's name into name
    // Don't worry if there isn't a name, techniques can be anonymous
    std::string name;
    parser.ExpectTerminal( TerminalType::IDENTIFIER, name );

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
    return d->GetSubClassID() == TokenTy::TechniqueDeclaration;
}

bool TechniqueDeclaration::classof( const TechniqueDeclaration* d )
{
    return true;
}

//------------------------------------------------------------------------------
// VariableListOrFunctionDefinition
//------------------------------------------------------------------------------

VariableListOrFunctionDefinition::VariableListOrFunctionDefinition(
                                                    TokenTy sub_class_id )
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

    // Try and parse some declarators
    DeclaratorVector declarators;
    if( !parser.ExpectListOf<InitDeclarator, TerminalType::COMMA>(
                                                                 declarators ) )
        return false;

    if( declarators.empty() )
    {
        parser.Error( "Declaration without a declarator" );
        return false;
    }

    CompoundStatement_up function_body;

    if( declarators.size() == 1 &&
        declarators[0]->IsFunctionDeclarator() &&
        parser.Expect<CompoundStatement>( function_body ) )
    {
        // If there was only one declarator and it's a function declarator and
        // there's a compound statement then we have a function definiton;
        token.reset( new FunctionDefinition( std::move(decl_specs),
                                             declarators[0]->TakeDeclarator(),
                                             std::move(function_body) ) );
        return true;
    }
    CHECK_PARSER;

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
    :VariableListOrFunctionDefinition( TokenTy::VariableDeclarationList )
    ,m_DeclSpecs( std::move(decl_specs) )
    ,m_Declarators( std::move(declarators) )
{
    assert( !m_DeclSpecs.empty() &&
            "VariableDeclarationList given no declaration specifiers" );
}

VariableDeclarationList::~VariableDeclarationList()
{
}

void VariableDeclarationList::PerformSema( SemaAnalyzer& sema )
{
    DeclSpecs decl_specs;

    //
    // If the decl specs were no good we can't continue
    //
    if( !decl_specs.AnalyzeDeclSpecs( m_DeclSpecs, sema ) )
        return;

    if( sema.InGlobalScope() &&
        !decl_specs.IsStatic() &&
        !decl_specs.IsVarying() )
        // Globals are uniform by default
        decl_specs.SetIsUniform( true );

    //
    // Check that
    //

    for( const InitDeclarator_up& declarator : m_Declarators )
        declarator->PerformSema( sema, decl_specs );
}

//------------------------------------------------------------------------------
// FunctionDefinition
//------------------------------------------------------------------------------

FunctionDefinition::FunctionDefinition( DeclSpecsVector decl_specs,
                                        Declarator_up declarator,
                                        CompoundStatement_up body )
    :VariableListOrFunctionDefinition( TokenTy::FunctionDefinition )
    ,m_DeclarationSpecifiers( std::move(decl_specs) )
    ,m_Declarator( std::move(declarator) )
    ,m_Body( std::move(body) )
{
#if !defined(NDEBUG)
    assert( !m_DeclarationSpecifiers.empty() &&
            "FunctionDefinition given no decl specs" );
    for( const auto& d : m_DeclarationSpecifiers )
        assert( d && "FunctionDefinition given a null declaration specifier" );
    assert( m_Declarator && "FunctionDefinition given a null declarator" );
    assert( m_Declarator->IsFunctionDeclarator() &&
            "FunctionDefinition given a non-function declarator" );
    assert( m_Body && "FunctionDefinition given a null body" );
#endif
}

FunctionDefinition::~FunctionDefinition()
{
}

void FunctionDefinition::PerformSema( SemaAnalyzer& sema )
{
    DeclSpecs decl_specs;
    decl_specs.AnalyzeDeclSpecs( m_DeclarationSpecifiers, sema );

    // This will register the function with sema and verify that it's all ok
    m_Declarator->PerformSema( sema, decl_specs );

    SemaAnalyzer::ScopeHolder scope( sema );
    scope.Enter();

    // Register the variables with sema
    m_Declarator->DeclareFunctionParameters( sema );

    // Pass the return type to sema for generating the return statements
    m_Body->PerformSemaAsFunction( sema,
                                   CompleteType(
                                            decl_specs.GetType(),
                                            m_Declarator->GetArrayExtents() ) );
    scope.Leave();

    sema.DefineFunction( m_Declarator->GetIdentifier(),
                         m_Declarator->GetFunctionParameters(),
                         std::move(m_Body) );
}

} // namespace Compiler
} // namespace JoeLang
