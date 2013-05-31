/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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

#include "llvm_writer.hpp"

#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

LLVMWriter::LLVMWriter( Runtime& runtime )
    : m_Runtime( runtime )
{
}

StateAssignmentBase_up LLVMWriter::GenerateStateAssignment(
    const StateAssignmentNode& state_assignment_node )
{
    //
    // Todo, checking node types
    //
    const StateBase& state = state_assignment_node.GetState();
    const ExpressionNode& assigned_node = state_assignment_node.GetAssignedExpression();

    assert( assigned_node.GetType().GetType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );

    switch( state.GetType() )
    {
    case Type::FLOAT:
        return StateAssignmentBase_up( new ConstStateAssignment<jl_float>(
            static_cast<const State<jl_float>&>( state ), 42.0 ) );
        break;
    default:
        assert( false && "Trying to generate code for an unhandled state type" );
    }
    /*

     llvm::Function* function = nullptr;

     std::unique_ptr<StateAssignmentBase> sa;

 #define CREATE_STATE_ASSIGNMENT( type )
     case JoeLangType<type>::value:
         sa.reset( new StateAssignment<type>(
             static_cast<const State<type>&>(state),
             WrapExpression<type>( expression, function ) ) );
         break;

 #define CREATE_STATE_ASSIGNMENT_N( type )
     CREATE_STATE_ASSIGNMENT( type )
     CREATE_STATE_ASSIGNMENT( type##2 )
     CREATE_STATE_ASSIGNMENT( type##3 )
     CREATE_STATE_ASSIGNMENT( type##4 )
     CREATE_STATE_ASSIGNMENT( type##2x2 )
     CREATE_STATE_ASSIGNMENT( type##2x3 )
     CREATE_STATE_ASSIGNMENT( type##2x4 )
     CREATE_STATE_ASSIGNMENT( type##3x2 )
     CREATE_STATE_ASSIGNMENT( type##3x3 )
     CREATE_STATE_ASSIGNMENT( type##3x4 )
     CREATE_STATE_ASSIGNMENT( type##4x2 )
     CREATE_STATE_ASSIGNMENT( type##4x3 )
     CREATE_STATE_ASSIGNMENT( type##4x4 )

     switch( state.GetType() )
     {
     CREATE_STATE_ASSIGNMENT_N( jl_bool )
     CREATE_STATE_ASSIGNMENT_N( jl_char )
     CREATE_STATE_ASSIGNMENT_N( jl_short )
     CREATE_STATE_ASSIGNMENT_N( jl_int )
     CREATE_STATE_ASSIGNMENT_N( jl_long )
     CREATE_STATE_ASSIGNMENT_N( jl_uchar )
     CREATE_STATE_ASSIGNMENT_N( jl_ushort )
     CREATE_STATE_ASSIGNMENT_N( jl_uint )
     CREATE_STATE_ASSIGNMENT_N( jl_ulong )
     CREATE_STATE_ASSIGNMENT_N( jl_float )
     CREATE_STATE_ASSIGNMENT_N( jl_double )
     case Type::STRING:
         sa.reset( new StateAssignment<std::string>(
             static_cast<const State<std::string>&>(state),
             WrapStringExpression( expression, function ) ) );
         break;
     default:
         assert( false && "Generating a stateassignment of unhandled type" );
     }

     function->setName( name );
     function->setLinkage( llvm::Function::ExternalLinkage );

     return sa;

 #undef CREATE_STATE_ASSIGNMENT_N
 #undef CREATE_STATE_ASSIGNMENT
 */
}

} // namespace Compiler
} // namespace JoeLang
