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

#include "state_assignment_statement.hpp"

#include <cassert>
#include <memory>
#include <string>
#include <utility>

#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// StateAssignmentStatement
//------------------------------------------------------------------------------
StateAssignmentStatement::StateAssignmentStatement(
                                    std::string identifier,
                                    Expression_up expression )
    :PassStatement( TokenTy::StateAssignmentStatement )
    ,m_Identifier( std::move(identifier) )
    ,m_Expression( std::move(expression) )
{
    assert( !m_Identifier.empty() &&
            "Trying to create a StateAssignmentStatement with an empty state "
            "name" );
    assert( m_Expression &&
            "Trying to create a StateAssignmentStatement with a null "
            "expression" );
}

StateAssignmentStatement::~StateAssignmentStatement()
{
}

bool StateAssignmentStatement::PerformSema( SemaAnalyzer& sema )
{
    // create a scope for the enumerants
    SemaAnalyzer::ScopeHolder scope( sema );
    scope.Enter();

    // Try and get the state to which we are assigning
    m_State = sema.GetState( m_Identifier );
    if( !m_State )
    {
        sema.Error( "Undeclared state: " + m_Identifier );
        return false;
    }
    sema.LoadStateEnumerants( *m_State );

    m_Expression = CastExpression::Create( m_State->GetType(),
                                           std::move(m_Expression),
                                           false );

    if( !m_Expression->PerformSema( sema ) )
        return false;

    // best to be explicit about these things
    scope.Leave();

    return true;
}

std::unique_ptr<StateAssignmentBase>
                    StateAssignmentStatement::GenerateStateAssignment(
                                                 CodeGenerator& code_gen,
                                                 const std::string& name ) const
{
    assert( m_State &&
            "Trying to generate a state assignment without a state" );

    Type t = m_State->GetType();

    assert( m_Expression->GetType().GetType() == t &&
            "Trying to create a state assignment with mismatched types" );

    // If this is just a constant return a ConstStateAssignment
    if( m_Expression->IsConst() )
    {
        GenericValue v = code_gen.EvaluateExpression( *m_Expression );
        switch( t )
        {
        case Type::BOOL:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_bool>(
                                 static_cast<const State<jl_bool>&>(*m_State),
                                 v.GetBool() ) );
        case Type::CHAR:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_char>(
                                 static_cast<const State<jl_char>&>(*m_State),
                                 v.GetI8() ) );
        case Type::SHORT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_short>(
                                 static_cast<const State<jl_short>&>(*m_State),
                                 v.GetI16() ) );
        case Type::INT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_int>(
                                 static_cast<const State<jl_int>&>(*m_State),
                                 v.GetI32() ) );
        case Type::LONG:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_long>(
                                 static_cast<const State<jl_long>&>(*m_State),
                                 v.GetI64() ) );
        case Type::UCHAR:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_uchar>(
                                 static_cast<const State<jl_uchar>&>(*m_State),
                                 v.GetU8() ) );
        case Type::USHORT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_ushort>(
                                 static_cast<const State<jl_ushort>&>(*m_State),
                                 v.GetU16() ) );
        case Type::UINT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_uint>(
                                 static_cast<const State<jl_uint>&>(*m_State),
                                 v.GetU32() ) );
        case Type::ULONG:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_ulong>(
                                 static_cast<const State<jl_ulong>&>(*m_State),
                                 v.GetU64() ) );
        case Type::FLOAT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float>(
                                 static_cast<const State<jl_float>&>(*m_State),
                                 v.GetFloat() ) );
        case Type::FLOAT2:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float2>(
                                 static_cast<const State<jl_float2>&>(*m_State),
                                 v.GetFloat2() ) );
        case Type::FLOAT3:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float3>(
                                 static_cast<const State<jl_float3>&>(*m_State),
                                 v.GetFloat3() ) );
        case Type::FLOAT4:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float4>(
                                 static_cast<const State<jl_float4>&>(*m_State),
                                 v.GetFloat4() ) );
        case Type::DOUBLE:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_double>(
                                 static_cast<const State<jl_double>&>(*m_State),
                                 v.GetDouble() ) );
        case Type::STRING:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<std::string>(
                               static_cast<const State<std::string>&>(*m_State),
                               v.GetString() ) );
        default:
            assert( false &&
             "Trying to create a ConstStateAssignment with an unhandled type" );
            return nullptr;
        }
    }

    return code_gen.GenerateStateAssignment( *m_State,
                                             *m_Expression,
                                             name + "_" + m_Identifier );
}

bool StateAssignmentStatement::Parse(
                              Parser& parser,
                              std::unique_ptr<StateAssignmentStatement>& token )
{
    // Read the state identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    // The assignment operator
    if( !parser.ExpectTerminal( TerminalType::EQUALS ) )
        return false;

    // And the expression
    Expression_up expression;
    if( !parser.Expect< Expression >( expression ) )
        return false;

    // Closing semicolon
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new StateAssignmentStatement( std::move(identifier),
                                               std::move(expression) ) );
    return true;
}

bool StateAssignmentStatement::classof( const Token* t )
{
    return t->GetSubClassID() == TokenTy::StateAssignmentStatement;
}

bool StateAssignmentStatement::classof( const StateAssignmentStatement* t )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
