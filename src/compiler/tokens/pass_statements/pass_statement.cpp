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

#include "pass_statement.hpp"

#include <cassert>
#include <memory>
#include <utility>

#include <compiler/support/casting.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/tokens/pass_statements/compile_statement.hpp>
#include <compiler/tokens/pass_statements/state_assignment_statement.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PassStatement
//------------------------------------------------------------------------------
PassStatement::PassStatement( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

PassStatement::~PassStatement()
{
}

bool PassStatement::Parse( Parser& parser, PassStatement_up& token )
{
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf< StateAssignmentStatement,
                             CompileStatement >( t ) )
        return false;

    assert( isa<PassStatement>( t ) && 
            "PassStatement parsed a non PassStatement" );
    token.reset( static_cast<PassStatement*>( t.release() ) );
    return true;
}

bool PassStatement::classof( const Token* t )
{
    return t->GetSubClassID() >= TokenTy::PassStatement_Start &&
           t->GetSubClassID() <= TokenTy::PassStatement_End;
}

bool PassStatement::classof( const PassStatement* p )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
