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

#include "identifier_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/code_generator.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/variable.hpp>


namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// IdentifierExpression
//------------------------------------------------------------------------------

IdentifierExpression::IdentifierExpression( std::string identifier )
    :Expression( TokenTy::IdentifierExpression )
    ,m_Identifier( std::move( identifier ) )
{
    assert( !m_Identifier.empty() &&
            "IdentifierExpression given an empty identifier" );
}

IdentifierExpression::~IdentifierExpression()
{
}

bool IdentifierExpression::ResolveIdentifier( SemaAnalyzer& sema )
{
    m_Variable = sema.GetVariable( m_Identifier );
    if( !m_Variable )
    {
        if( sema.HasFunctionNamed( m_Identifier ) )
            sema.Error( "Using function as variable " + m_Identifier );
        else
            sema.Error( "Undeclared variable: " + m_Identifier );
        return false;
    }
    return true;
}

llvm::Value* IdentifierExpression::CodeGen( CodeGenerator& code_gen ) const
{
    assert( m_Variable &&
            "Trying to generate code for an unresolved variable" );
    return code_gen.CreateVariableRead( *m_Variable );
}

llvm::Value* IdentifierExpression::CodeGenPointerTo(
                                                 CodeGenerator& code_gen ) const
{
    assert( m_Variable &&
            "Trying to generate code for an unresolved variable" );
    return m_Variable->GetLLVMPointer();
}

void IdentifierExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer.WriteVariableName( m_Variable );
}

CompleteType IdentifierExpression::GetType() const
{
    // todo should this assert
    if( !m_Variable )
        return CompleteType();
    return m_Variable->GetType();
}

std::set<Function_sp> IdentifierExpression::GetCallees() const
{
    return std::set<Function_sp>{};
}

std::set<Variable_sp> IdentifierExpression::GetVariables() const
{
    assert( m_Variable &&
            "Trying to get the variables of an unresolved identifier" );
    return { m_Variable };
}

std::set<Variable_sp> IdentifierExpression::GetWrittenToVariables(
                                                     bool is_assigned ) const
{
    assert( m_Variable &&
            "Trying to get the variables of an unresolved identifier" );

    return is_assigned ? std::set<Variable_sp>{ m_Variable } :
                         std::set<Variable_sp>{};
}

const std::string& IdentifierExpression::GetIdentifier() const
{
    return m_Identifier;
}

bool IdentifierExpression::IsLValue() const
{
    return true;
}

bool IdentifierExpression::IsConst() const
{
    assert( m_Variable && "Trying to get constness of an unresolved variable" );
    return m_Variable->IsConst();
}

const std::shared_ptr<Variable>& IdentifierExpression::GetVariable() const
{
    assert( m_Variable && "Trying to get an unresolved variable" );
    return m_Variable;
}

bool IdentifierExpression::PerformSema( SemaAnalyzer& sema )
{
    return ResolveIdentifier( sema );
}

bool IdentifierExpression::Parse( Parser& parser,
                                  Expression_up& token )
{
    // Try and parse the identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    token.reset( new IdentifierExpression( identifier ) );
    return true;
}

bool IdentifierExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::IdentifierExpression;
}

bool IdentifierExpression::classof( const IdentifierExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
