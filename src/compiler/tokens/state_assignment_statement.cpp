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
                                    Expression_up expression )
    :Token( TokenTy::StateAssignmentStatement )
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

void StateAssignmentStatement::PerformSema( SemaAnalyzer& sema )
{
    // create a scope for the enumerants
    sema.EnterScope();

    // Try and get the state to which we are assigning
    m_State = sema.GetState( m_Identifier );
    if( !m_State )
        sema.Error( "Undeclared state: " + m_Identifier );
    else
        sema.LoadStateEnumerants( *m_State );

    m_Expression->ResolveIdentifiers( sema );

    // only create the cast if we have a type to cast it to
    if( m_State )
        m_Expression = CastExpression::Create( m_State->GetType(),
                                               std::move(m_Expression) );

    if( m_Expression->PerformSema( sema ) )
        m_Expression->FoldConstants( m_Expression );

    sema.LeaveScope();
}

std::unique_ptr<StateAssignmentBase>
                    StateAssignmentStatement::GenerateStateAssignment(
                                                 CodeGenerator& code_gen ) const
{
    Type t = m_State->GetType();
    assert( m_Expression->GetReturnType() == t &&
            "Trying to create a state assignment with mismatched types" );

    LiteralExpression* l = Expression::GetLiteral( m_Expression );

    // If this is just a constant return a ConstStateAssignment
    if( l )
    {
        GenericValue v( l->GetValue() );
        switch( t )
        {
        case Type::BOOL:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_bool>(
                                 static_cast<const State<jl_bool>&>(*m_State),
                                 v.GetBool() ) );
        case Type::I8:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i8>(
                                 static_cast<const State<jl_i8>&>(*m_State),
                                 v.GetI8() ) );
        case Type::I16:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i16>(
                                 static_cast<const State<jl_i16>&>(*m_State),
                                 v.GetI16() ) );
        case Type::I32:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i32>(
                                 static_cast<const State<jl_i32>&>(*m_State),
                                 v.GetI32() ) );
        case Type::I64:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_i64>(
                                 static_cast<const State<jl_i64>&>(*m_State),
                                 v.GetI64() ) );
        case Type::U8:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u8>(
                                 static_cast<const State<jl_u8>&>(*m_State),
                                 v.GetU8() ) );
        case Type::U16:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u16>(
                                 static_cast<const State<jl_u16>&>(*m_State),
                                 v.GetU16() ) );
        case Type::U32:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u32>(
                                 static_cast<const State<jl_u32>&>(*m_State),
                                 v.GetU32() ) );
        case Type::U64:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_u64>(
                                 static_cast<const State<jl_u64>&>(*m_State),
                                 v.GetU64() ) );
        case Type::FLOAT:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_float>(
                                 static_cast<const State<jl_float>&>(*m_State),
                                 v.GetFloat() ) );
        case Type::DOUBLE:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_double>(
                                 static_cast<const State<jl_double>&>(*m_State),
                                 v.GetDouble() ) );
        case Type::STRING:
            return std::unique_ptr<StateAssignmentBase>(
                          new ConstStateAssignment<jl_string>(
                                 static_cast<const State<jl_string>&>(*m_State),
                                 v.GetString() ) );
        default:
            assert( false &&
             "Trying to create a ConstStateAssignment with an unhandled type" );
            return nullptr;
        }
    }

    return code_gen.GenerateStateAssignment( *m_State, *m_Expression );
}

void StateAssignmentStatement::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "State Assignment to " << m_Identifier << "\n";
    m_Expression->Print( depth + 1 );
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

} // namespace Compiler
} // namespace JoeLang
