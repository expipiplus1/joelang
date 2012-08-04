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

#include "statement.hpp"

#include <cassert>
#include <memory>

#include <compiler/casting.hpp>
#include <compiler/parser.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Statement
//------------------------------------------------------------------------------

Statement::Statement( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

Statement::~Statement()
{
}

void Statement::Print( int depth ) const
{
}

bool Statement::Parse( Parser& parser, Statement_up& token )
{
    // Try and parse any kind of statement
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf< CompoundStatement >( t ) )
        return false;

    assert( isa<Statement>( t ) && "Statement parsed a non-statement" );
    token.reset( static_cast<Statement*>( t.release() ) );
    return true;
}

bool Statement::classof( const Token* d )
{
    return d->GetSubClassID() >= TokenTy::Statement_Start &&
           d->GetSubClassID() <= TokenTy::Statement_End;
}

bool Statement::classof( const Statement* d )
{
    // A Statement is always a Statement
    return true;
}

} // namespace Compiler
} // namespace JoeLang
