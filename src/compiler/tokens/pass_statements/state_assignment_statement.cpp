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

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/types.hpp>

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

#define CREATE_CONST_STATE_ASSIGNMENT( type, Type ) \
    case JoeLangType<type>::value: \
        return std::unique_ptr<StateAssignmentBase>( \
                          new ConstStateAssignment<type>( \
                                    static_cast<const State<type>&>(*m_State), \
                                    v.Get##Type() ) );

#define CREATE_CONST_STATE_ASSIGNMENT_N( type, Type ) \
    CREATE_CONST_STATE_ASSIGNMENT( type, Type ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##2, Type##2 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##3, Type##3 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##4, Type##4 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##2x2, Type##2x2 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##2x3, Type##2x3 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##2x4, Type##2x4 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##3x2, Type##3x2 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##3x3, Type##3x3 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##3x4, Type##3x4 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##4x2, Type##4x2 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##4x3, Type##4x3 ) \
    CREATE_CONST_STATE_ASSIGNMENT( type##4x4, Type##4x4 )

    // If this is just a constant return a ConstStateAssignment
    if( m_Expression->IsConst() )
    {
        GenericValue v = code_gen.EvaluateExpression( *m_Expression );
        switch( t )
        {
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_bool, Bool )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_char, Char )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_short, Short )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_int, Int )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_long, Long )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_uchar, UChar )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_ushort, UShort )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_uint, UInt )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_ulong, ULong )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_float, Float )
        CREATE_CONST_STATE_ASSIGNMENT_N( jl_double, Double )
        CREATE_CONST_STATE_ASSIGNMENT( std::string, String )

        default:
            assert( false &&
             "Trying to create a ConstStateAssignment with an unhandled type" );
            return nullptr;
        }
    }

#undef CREATE_CONST_STATE_ASSIGNMENT_N
#undef CREATE_CONST_STATE_ASSIGNMENT

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
