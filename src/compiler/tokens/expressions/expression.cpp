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

#include "expression.hpp"

#include <cassert>
#include <memory>
#include <set>

#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/tokens/expressions/assignment_expression.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// Expression
//------------------------------------------------------------------------------

Expression::Expression( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

Expression::~Expression()
{
}

const ExpressionNode& Expression::GenerateCodeDag( NodeManager& node_manager ) const
{
    return node_manager.MakeExpressionNode( NodeType::Unimplemented );
}

bool Expression::IsLValue() const
{
    return false;
}

llvm::Value* Expression::CodeGenPointerTo( CodeGenerator& code_gen ) const
{
    if( IsLValue() )
        assert( false && "Complete me" );
    assert( false && "Trying to write to an rvalue" );
    return nullptr;
}

bool Expression::Parse( Parser& parser, Expression_up& token )
{
    // TODO comma sep expressions
    return parser.Expect<AssignmentExpression>( token );
}

bool Expression::classof( const Token* t )
{
    return t->GetSubClassID() >= TokenTy::Expression_Start &&
           t->GetSubClassID() <= TokenTy::Expression_End;
}

bool Expression::classof( const Expression* e )
{
    // An Expression is always an Expression
    return true;
}

} // namespace Compiler
} // namespace JoeLang
