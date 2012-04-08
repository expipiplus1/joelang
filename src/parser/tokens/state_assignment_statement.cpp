/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#include "state_assignment_statement.hpp"

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <utility>

#include <engine/types.hpp>
#include <engine/state.hpp>
#include <engine/state_assignment.hpp>
#include <parser/code_generator.hpp>
#include <parser/parser.hpp>
#include <parser/terminal_types.hpp>
#include <parser/tokens/expression.hpp>

namespace JoeLang
{
namespace Compiler
{

StateAssignmentStatement::StateAssignmentStatement( const StateBase& state,
                                                    std::unique_ptr< Expression > expression )
    :m_state( state )
    ,m_expression( std::move( expression ) )
{
}

StateAssignmentStatement::~StateAssignmentStatement()
{
}

std::unique_ptr<StateAssignmentBase> StateAssignmentStatement::GetStateAssignment( CodeGenerator& code_generator ) const
{
    //TODO
    return code_generator.GenerateStateAssignment( m_state, *m_expression );
}

void StateAssignmentStatement::Print( int depth ) const
{
    for( int i = 0; i < depth * 4; ++i )
        std::cout << " ";
    std::cout << "State Assignment to " << m_state.GetName() << "\n";
    m_expression->Print( depth + 1 );
}

//TODO move this from here, and store these somewhere
std::map<std::string, std::shared_ptr<LiteralExpression> >
    GetLiteralValueEnumerantMap( const StateBase* state_base )
{
    std::map<std::string, std::shared_ptr<LiteralExpression> > ret;
    switch( state_base->GetType() )
    {
    case Type::BOOL:
        for( const auto& e : reinterpret_cast<const State<jl_bool>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<BooleanLiteralExpression>( e.second );
        break;
    case Type::FLOAT:
        for( const auto& e : reinterpret_cast<const State<jl_float>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<FloatingLiteralExpression>( e.second, false );
        break;
    case Type::DOUBLE:
        for( const auto& e : reinterpret_cast<const State<jl_double>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<FloatingLiteralExpression>( e.second, true );
        break;
    case Type::I8:
        for( const auto& e : reinterpret_cast<const State<jl_i8>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::I16:
        for( const auto& e : reinterpret_cast<const State<jl_i16>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::I32:
        for( const auto& e : reinterpret_cast<const State<jl_i32>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::I64:
        for( const auto& e : reinterpret_cast<const State<jl_i64>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::U8:
        for( const auto& e : reinterpret_cast<const State<jl_u8>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::U16:
        for( const auto& e : reinterpret_cast<const State<jl_u16>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::U32:
        for( const auto& e : reinterpret_cast<const State<jl_u32>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::U64:
        for( const auto& e : reinterpret_cast<const State<jl_u64>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<IntegralLiteralExpression>( e.second );
        break;
    case Type::STRING:
        for( const auto& e : reinterpret_cast<const State<jl_string>*>(state_base)->GetEnumerations() )
            ret[e.first] = std::make_shared<StringLiteralExpression>( e.second );
        break;
    default:
        assert( false && "state_base is of an unhandled type" );
    }
    return ret;
}

bool StateAssignmentStatement::Parse( Parser& parser, std::unique_ptr<StateAssignmentStatement>& token )
{
    std::string state_name;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, state_name ) )
        return false;

    //Check if the state name is a valid state name
    const StateBase* state = parser.GetNamedState( state_name );
    if( !state )
    {
        parser.Error( "\'" + state_name + "\' is not a valid state name" );
        return false;
    }

    if( !parser.ExpectTerminal( TerminalType::EQUALS ) )
        return false;

    // TODO some kind of raii for the symbol table scopes
    //
    // Add the state enumerants to the symbol table
    //
    SymbolTable& symbol_table = parser.GetSymbolTable();
    symbol_table.EnterScope();
    for( const auto& s : GetLiteralValueEnumerantMap( state ) )
        symbol_table.AddConstant( s.first, s.second );

    std::unique_ptr< Expression > expression;
    if( !parser.Expect< Expression >( expression ) )
    {
        symbol_table.LeaveScope();
        return false;
    }

    if( !parser.ExpectTerminal( TerminalType::SEMICOLON ) )
    {
        symbol_table.LeaveScope();
        return false;
    }

    //
    // Reset the symbol table
    //
    symbol_table.LeaveScope();

    token.reset( new StateAssignmentStatement( *state,
                                               std::move( expression ) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
