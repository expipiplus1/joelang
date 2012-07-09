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
#include <iostream>
#include <memory>
#include <string>
#include <utility>

#include <compiler/code_generator.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expression.hpp>
#include <engine/state.hpp>
#include <engine/state_assignment.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// StateAssignmentStatement
//------------------------------------------------------------------------------
StateAssignmentStatement::StateAssignmentStatement(
                                    std::string identifier,
                                    std::unique_ptr< Expression > expression )
    :Token( TokenTy::StateAssignmentStatement )
    ,m_identifier( std::move(identifier) )
    ,m_expression( std::move(expression) )
{
    assert( !m_identifier.empty() &&
            "Trying to create a StateAssignmentStatement with an empty state "
            "name" );
    assert( m_expression &&
            "Trying to create a StateAssignmentStatement with a null "
            "expression" );
}

StateAssignmentStatement::~StateAssignmentStatement()
{
}

void StateAssignmentStatement::PerformSema( SemaAnalyzer& sema )
{
    // create a scope for the enumerants
    sema.EnterScope();

    // Try and get the state to which we are assigning
    m_state = sema.GetState( m_identifier );
    if( !m_state )
        sema.Error( "Undeclared state: " + m_identifier );
    else
        sema.LoadStateEnumerants( *m_state );

    m_expression->ResolveIdentifiers( sema );

    // only create the cast if we have a type to cast it to
    if( m_state )
        m_expression = CastExpression::Create( m_state->GetType(),
                                               std::move(m_expression) );

    if( m_expression->PerformSema( sema ) )
        m_expression->FoldConstants( m_expression );

    sema.LeaveScope();
}

std::unique_ptr<StateAssignmentBase>
                    StateAssignmentStatement::GenerateStateAssignment(
                                                 CodeGenerator& code_gen ) const
{
    Type t = m_state->GetType();
    assert( m_expression->GetReturnType() == t &&
            "Trying to create a state assignment with mismatched types" );

    LiteralExpression* l = Expression::GetLiteral( m_expression );

    // If this is just a constant return a ConstStateAssignment
    if( l )
    {
        GenericValue v( l->GetValue() );
        switch( t )
        {
        case Type::BOOL:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_bool>(
                                 static_cast<const State<jl_bool>&>(*m_state),
                                 v.GetBool() ) );
        case Type::I8:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i8>(
                                 static_cast<const State<jl_i8>&>(*m_state),
                                 v.GetI8() ) );
        case Type::I16:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i16>(
                                 static_cast<const State<jl_i16>&>(*m_state),
                                 v.GetI16() ) );
        case Type::I32:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i32>(
                                 static_cast<const State<jl_i32>&>(*m_state),
                                 v.GetI32() ) );
        case Type::I64:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i64>(
                                 static_cast<const State<jl_i64>&>(*m_state),
                                 v.GetI64() ) );
        case Type::U8:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u8>(
                                 static_cast<const State<jl_u8>&>(*m_state),
                                 v.GetU8() ) );
        case Type::U16:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u16>(
                                 static_cast<const State<jl_u16>&>(*m_state),
                                 v.GetU16() ) );
        case Type::U32:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u32>(
                                 static_cast<const State<jl_u32>&>(*m_state),
                                 v.GetU32() ) );
        case Type::U64:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u64>(
                                 static_cast<const State<jl_u64>&>(*m_state),
                                 v.GetU64() ) );
        case Type::FLOAT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float>(
                                 static_cast<const State<jl_float>&>(*m_state),
                                 v.GetFloat() ) );
        case Type::DOUBLE:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_double>(
                                 static_cast<const State<jl_double>&>(*m_state),
                                 v.GetDouble() ) );
        case Type::STRING:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_string>(
                                 static_cast<const State<jl_string>&>(*m_state),
                                 v.GetString() ) );
        default:
            assert( false &&
             "Trying to create a ConstStateAssignment with an unhandled type" );
            return nullptr;
        }
    }

    return code_gen.GenerateStateAssignment( *m_state, *m_expression );
}

void StateAssignmentStatement::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "State Assignment to " << m_identifier << "\n";
    m_expression->Print( depth + 1 );
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
    std::unique_ptr< Expression > expression;
    if( !parser.Expect< Expression >( expression ) )
        return false;

    // Closing semicolon
    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
        return false;

    token.reset( new StateAssignmentStatement( std::move(identifier),
                                               std::move(expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
