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

#include "sema_analyzer.hpp"

#include <algorithm>
#include <iostream>
#include <memory>
#include <map>
#include <string>
#include <utility>

#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <engine/context.hpp>
#include <engine/state.hpp>

namespace JoeLang
{
namespace Compiler
{

SemaAnalyzer::SemaAnalyzer( const Context& context, CodeGenerator& code_gen )
    :m_Context( context )
    ,m_CodeGenerator( code_gen )
{
}

SemaAnalyzer::~SemaAnalyzer()
{
    assert( m_SymbolStack.size() == 0 &&
            "The symbol table is still inside a scope" );
}

bool SemaAnalyzer::BuildAst( TranslationUnit& cst )
{
    EnterScope();
    // Perform sema on the tree
    cst.PerformSema( *this );
    LeaveScope();

    // Check for undefined things
    for( const auto& p : m_PassDefinitions )
    {
        // If this pass is referenced and not defined
        if( !p.second.unique() &&
            !(*p.second) )
            Error( "Use of undefined pass " + p.first );
    }

    // Return success or not
    return m_Good;
}

void SemaAnalyzer::DeclarePass( std::string name,
                                std::unique_ptr<PassDefinition> definition )
{
    if( definition )
        definition->PerformSema( *this );

    PassDefinitionMap::const_iterator d = m_PassDefinitions.find( name );

    if( d == m_PassDefinitions.end() )
        // If we haven't seen this name before: insert it
        m_PassDefinitions[name] =
                        std::make_shared<std::unique_ptr<PassDefinition> >(
                                                        std::move(definition) );
    else
    {
        if( definition )
        {
            if( *d->second  )
                // If it's being defined now and was defined before
                Error( "Multiple definition of pass " + name );
            (*d->second) = std::move(definition);
        }
    }
}

void SemaAnalyzer::DeclareTechnique( std::string name )
{
    // Check if this technique has already been defined
    const auto& i = std::find( m_Techniques.begin(), m_Techniques.end(), name );

    if( i != m_Techniques.end() )
        Error( "Multiple definitions of technique " + name );

    // Add the technique
    m_Techniques.push_back( std::move(name) );
}

bool SemaAnalyzer::HasPass( const std::string& name ) const
{
    return m_PassDefinitions.count( name ) > 0;
}

SemaAnalyzer::PassDefinitionRef SemaAnalyzer::GetPass(
                                                 const std::string& name ) const
{
    PassDefinitionMap::const_iterator p = m_PassDefinitions.find( name );
    if( p == m_PassDefinitions.end() )
        return nullptr;
    return p->second;
}

bool SemaAnalyzer::HasState( const std::string& name ) const
{
    return m_Context.GetNamedState( name ) != nullptr;
}

const StateBase* SemaAnalyzer::GetState( const std::string& name ) const
{
    return m_Context.GetNamedState( name );
}

std::map<std::string, GenericValue>
    GetGenericValueEnumerantMap( const StateBase& state_base )
{
    std::map<std::string, GenericValue> ret;
    switch( state_base.GetType() )
    {
    case Type::BOOL:
        for( const auto& e :
             static_cast<const State<jl_bool>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::FLOAT:
        for( const auto& e :
             static_cast<const State<jl_float>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::DOUBLE:
        for( const auto& e :
             static_cast<const State<jl_double>&>(state_base).GetEnumerations())
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::I8:
        for( const auto& e :
             static_cast<const State<jl_i8>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::I16:
        for( const auto& e :
             static_cast<const State<jl_i16>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::I32:
        for( const auto& e :
             static_cast<const State<jl_i32>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::I64:
        for( const auto& e :
             static_cast<const State<jl_i64>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::U8:
        for( const auto& e :
             static_cast<const State<jl_u8>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::U16:
        for( const auto& e :
             static_cast<const State<jl_u16>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
     break;
    case Type::U32:
        for( const auto& e :
             static_cast<const State<jl_u32>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::U64:
        for( const auto& e :
             static_cast<const State<jl_u64>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::STRING:
        for( const auto& e :
             static_cast<const State<std::string>&>(
                                                 state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    default:
        assert( false && "state_base is of an unhandled type" );
    }
    return ret;
}

void SemaAnalyzer::LoadStateEnumerants( const StateBase& state )
{
    /// TODO cache these results
    /// TODO support arrays here
    /// TODO something better than making these global
    for( auto& v : GetGenericValueEnumerantMap(state) )
    {
        std::shared_ptr<Variable> variable =  std::make_shared<Variable>(
                                                        v.second.GetType(),
                                                        std::vector<unsigned>(),
                                                        true,
                                                        true,
                                                        std::move(v.second),
                                                        v.first );
        variable->CodeGen( m_CodeGenerator );
        DeclareVariable( v.first, std::move(variable) );
    }
}

void SemaAnalyzer::DeclareVariable( const std::string& identifier,
                                    std::shared_ptr<Variable> value )
{
    bool inserted = m_SymbolStack.rbegin()->m_Variables.insert(
            std::make_pair( identifier, std::move(value) ) ).second;

    if( !inserted )
        Error( "Duplicate definition of variable: " + identifier );
    else
    {
        for( auto it = ++m_SymbolStack.rbegin();
             it != m_SymbolStack.rend();
             ++it )
        {
            if( it->m_Variables.find( identifier ) != it->m_Variables.end() )
                Warning( "Declaration of \'" + identifier +
                         "\' shadows previous declaration" );
        }
    }
}

std::shared_ptr<Variable> SemaAnalyzer::GetVariable(
                                                const std::string& identifier )
{
    // iterate in reverse because deeper scopes take priority
    for( auto it = m_SymbolStack.rbegin();
         it != m_SymbolStack.rend(); ++it )
    {
        const auto& v = it->m_Variables.find( identifier );
        if( v != it->m_Variables.end() )
            return v->second;
    }
    return nullptr;
}

void SemaAnalyzer::EnterScope()
{
    m_SymbolStack.resize( m_SymbolStack.size() + 1 );
}

void SemaAnalyzer::LeaveScope()
{
    m_SymbolStack.pop_back();
}

bool SemaAnalyzer::InGlobalScope() const
{
    return m_SymbolStack.size() == 1;
}

GenericValue SemaAnalyzer::EvaluateExpression( const Expression& expression )
{
    return m_CodeGenerator.EvaluateExpression( expression );
}

void SemaAnalyzer::Error( const std::string& error_message )
{
    m_Good = false;
    std::cout << "Error during semantic analysis: " << error_message <<
                 std::endl;
}

void SemaAnalyzer::Warning( const std::string& warning_message)
{
    std::cout << "Warning during semantic analysis: " << warning_message <<
                 std::endl;
}

bool SemaAnalyzer::Good() const
{
    return m_Good;
}

CodeGenerator& SemaAnalyzer::GetCodeGenerator()
{
    return m_CodeGenerator;
}

} // namespace Compiler
} // namespace JoeLang
