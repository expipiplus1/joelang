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

#include "compile_statement.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include <compiler/complete_type.hpp>
#include <compiler/entry_function.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{
namespace Compiler
{

class Function;
using Function_sp = std::shared_ptr<Function>;

//------------------------------------------------------------------------------
// CompileStatement
//------------------------------------------------------------------------------
CompileStatement::CompileStatement( ShaderDomain domain,
                                    std::string identifier,
                                    std::vector<Expression_up> arguments )
    :PassStatement( TokenTy::CompileStatement )
    ,m_Domain( domain )
    ,m_Identifier( std::move(identifier) )
    ,m_Arguments( std::move(arguments) )
{
#if !defined(NDEBUG)
    assert( !m_Identifier.empty() &&
            "Trying to create a CompileStatement with an empty function name" );
    for( const auto& a : m_Arguments )
        assert( a && "CompileStatement given a null argument" );
#endif
}

CompileStatement::~CompileStatement()
{
}

ShaderDomain CompileStatement::GetDomain() const
{
    return m_Domain;
}

const EntryFunction_sp& CompileStatement::GetEntryFunction() const
{
    assert( m_EntryFunction && "Trying to get a null entryfunction" );
    return m_EntryFunction;
}

void CompileStatement::PerformSema( SemaAnalyzer& sema )
{
    assert( !m_EntryFunction && "About to overrite a non-null entryfunction" );

    //
    // Find out which function we're using as the entry
    //
    std::vector<CompleteType> argument_types;
    argument_types.reserve( m_Arguments.size() );
    for( const auto& a : m_Arguments )
        argument_types.push_back( a->GetType() );

    if( !sema.HasFunctionNamed( m_Identifier ) )
    {
        sema.Error( "Trying to compile an undeclared function: " +
                    m_Identifier );
        return;
    }

    Function_sp function = sema.GetFunctionOverload( m_Identifier,
                                                     argument_types );

    if( !function )
    {
        sema.Error( "Couldn't find compatible overload for " + m_Identifier );
        return;
    }

    m_EntryFunction.reset( new EntryFunction( m_Domain,
                                              std::move(function),
                                              std::move(m_Arguments) ) );

    return;
}

bool CompileStatement::Parse( Parser& parser, CompileStatement_up& token )
{
    const static std::map<TerminalType, ShaderDomain> s_TerminamDomainMap =
    {
        {TerminalType::PIXEL_SHADER, ShaderDomain::FRAGMENT},
        {TerminalType::VERTEX_SHADER, ShaderDomain::VERTEX}
    };

    //
    // Try and match any of the above terminals
    //
    bool found_domain = false;
    ShaderDomain domain;
    for( const auto& t : s_TerminamDomainMap )
        if( parser.ExpectTerminal( t.first ) )
        {
            domain = t.second;
            found_domain = true;
            break;
        }
    if( !found_domain )
        return false;

    // Parse the '='
    if( !parser.ExpectTerminal( TerminalType::EQUALS ) )
        return false;

    // Parse 'compile'
    if( !parser.ExpectTerminal( TerminalType::COMPILE ) )
        return false;

    std::string identifier;
    // Parse the function identifier
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    // Parse '('
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;

    std::vector<Expression_up> arguments;
    parser.ExpectListOf<AssignmentExpression, TerminalType::COMMA>( arguments );
    CHECK_PARSER;

    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
        return false;

    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new CompileStatement( domain,
                                       std::move(identifier),
                                       std::move(arguments) ) );
    return true;
}

bool CompileStatement::classof( const Token* t )
{
    return t->GetSubClassID() == TokenTy::CompileStatement;
}

bool CompileStatement::classof( const CompileStatement* t )
{
    return true;
}


} // namespace Compiler
} // namespace JoeLang
