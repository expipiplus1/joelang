/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "symbol_table.hpp"

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>

#include <parser/tokens/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

SymbolTable::~SymbolTable()
{
    assert( m_symbolStack.size() == 0 && "The symbol table is still inside a scope" );
}

void SymbolTable::EnterScope()
{
    m_symbolStack.resize( m_symbolStack.size() + 1 );
}

void SymbolTable::LeaveScope()
{
    m_symbolStack.pop_back();
}

bool SymbolTable::GetConstant( std::string identifier,
                               std::shared_ptr<LiteralExpression>& constant )
{
    for( const auto& m : m_symbolStack )
    {
        auto e = m.m_constants.find( identifier );
        if( e != m.m_constants.end() )
        {
            constant = e->second;
            return true;
        }
    }
    return false;
}

bool SymbolTable::AddConstant( std::string identifier,
                               std::shared_ptr<LiteralExpression> constant )
{
    return m_symbolStack.rbegin()->m_constants.insert( std::make_pair( identifier, std::move(constant) ) ).second;
}

bool SymbolTable::HasTechniqueName( const std::string& name ) const
{
    return m_techniqueNames.find( name ) != m_techniqueNames.end();
}

bool SymbolTable::AddTechniqueName( const std::string& name )
{
    return m_techniqueNames.insert( name ).second;
}

} // namespace Compiler
} // namespace JoeLang
