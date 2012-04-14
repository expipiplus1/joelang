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
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>

namespace JoeLang
{
namespace Compiler
{

SemaAnalyzer::SemaAnalyzer()
{
}

SemaAnalyzer::~SemaAnalyzer()
{
}

bool SemaAnalyzer::BuildAst( TranslationUnit& cst )
{
    cst.PerformSema( *this );
    return m_good;
}

void SemaAnalyzer::DeclarePass( std::string name,
                              std::unique_ptr<PassDefinition> definition )
{
    // TODO warning if name is funny

    if( definition )
        definition->PerformSema( *this );

    PassDefinitionMap::iterator d = m_passDefinitions.find( name );

    if( d == m_passDefinitions.end() )
        // If we haven't seen this name before: insert it
        m_passDefinitions.insert( std::make_pair(name, std::move(definition)) );
    else
    {
        if( d->second && definition )
            // If it's being defined now and was defined before
            Error( "Multiple definition of pass " + name );
    }
}

void SemaAnalyzer::DeclareTechnique( std::string name )
{
    // Check if this technique has already been defined
    const auto& i = std::find( m_techniques.begin(), m_techniques.end(), name );

    if( i != m_techniques.end() )
        Error( "Multiple definitions of technique " + name );

    // Add the technique
    m_techniques.push_back( std::move(name) );
}

bool SemaAnalyzer::HasPass( const std::string& name )
{
    return m_passDefinitions.count( name ) > 0;
}

bool SemaAnalyzer::HasState( const std::string& name )
{
    return false;
}

void SemaAnalyzer::Error( const std::string& error_message )
{
    m_good = false;
    std::cout << "Error during semantic analysis: " << error_message << "\n";
}

} // namespace Compiler
} // namespace JoeLang
