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

#include "empty_statement.hpp"

#include <cassert>
#include <memory>

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/tokens/statements/statement.hpp>
#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// EmptyStatement
//------------------------------------------------------------------------------

EmptyStatement::EmptyStatement()
    :Statement( TokenTy::EmptyStatement )
{
}

EmptyStatement::~EmptyStatement()
{
}

const Node& EmptyStatement::GenerateCodeDag( NodeManager& node_manager ) const
{
    assert( false && "Trying to codegen an empty statement");
    return node_manager.MakeNode( NodeType::Unimplemented, {} );
}

bool EmptyStatement::AlwaysReturns() const
{
    return false;
}

std::set<Function_sp> EmptyStatement::GetCallees() const
{
    return std::set<Function_sp>{};
}

std::set<Variable_sp> EmptyStatement::GetVariables() const
{
    return std::set<Variable_sp>{};
}

std::set<Variable_sp> EmptyStatement::GetWrittenToVariables() const
{
    return std::set<Variable_sp>{};
}

void EmptyStatement::PerformSema( SemaAnalyzer& sema,
                                  const CompleteType& return_type )
{
}

void EmptyStatement::CodeGen( CodeGenerator& code_gen )
{
}

void EmptyStatement::Write( ShaderWriter& shader_writer ) const
{
    // glsl doesn't support empty statements
}

bool EmptyStatement::Parse( Parser& parser, EmptyStatement_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new EmptyStatement() );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
