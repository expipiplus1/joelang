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

#include "type_constructor_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/code_generator.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/expressions/assignment_expression.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/identifier_expression.hpp>
#include <compiler/tokens/expressions/literal_expression.hpp>
#include <compiler/tokens/expressions/primary_expression.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// TypeConstructorExpression
//------------------------------------------------------------------------------

TypeConstructorExpression::TypeConstructorExpression(
                                         Type type,
                                         std::vector<Expression_up> arguments )
    :Expression( TokenTy::TypeConstructorExpression )
    ,m_Type( type )
    ,m_Arguments( std::move(arguments) )
{
#if !defined(NDEBUG)
    assert( type != Type::UNKNOWN &&
            "TypeConstructorExpression given an unknown type" );
    for( const auto& argument : m_Arguments )
        assert( argument && "TypeConstructorExpression given a null argument" );
#endif
}

TypeConstructorExpression::~TypeConstructorExpression()
{
}

bool TypeConstructorExpression::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;

    for( const auto& argument : m_Arguments )
        good &= argument->PerformSema( sema );

    if( !good )
        return false;

    //
    // Verify that we have the correct number of parameters for this type
    // Or just one parameter
    //
    unsigned num_desired_elements = GetNumElementsInType( m_Type );
    unsigned num_elements = 0;
    for( const auto& argument : m_Arguments )
        num_elements += argument->GetType().GetVectorSize();

    if( num_elements != num_desired_elements && num_elements != 1 )
    {
        sema.Error( "Wrong number of elements in constructor" );
        good = false;
    }

    //
    // Verify that all the parameters can be converted into the correct base
    // type, if there's only one argument, then this is an explicit cast
    // todo matrices
    //
    for( auto& argument : m_Arguments )
    {
        CastExpression_up c;
        c =  CastExpression::CreateBaseTypeCast( GetScalarType( m_Type ),
                                                 std::move(argument),
                                                 m_Arguments.size() == 1 );
        good &= c->PerformSemaNoRecurse( sema );
        argument = std::move(c);
    }

    return good;
}

llvm::Value* TypeConstructorExpression::CodeGen( CodeGenerator& code_gen ) const
{
    if( IsVectorType( m_Type ) )
    {
        if( m_Arguments.size() == 1 )
            return code_gen.CreateCast( *m_Arguments.front(),
                                        CompleteType( m_Type ) );
        else
            return code_gen.CreateVectorConstructor( m_Type, m_Arguments );
    }
    else
    {
        assert( IsScalarType( m_Type ) &&
                "Trying to construct an unhandled type" );
        assert( m_Arguments.size() == 1 &&
                "Trying to construct scalar type with wrong number of "
                "arguments" );
        return code_gen.CreateScalarConstructor( m_Type, *m_Arguments[0] );
    }
}

void TypeConstructorExpression::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << CompleteType(m_Type) << "(";
    bool first = true;
    for( const auto& argument : m_Arguments )
    {
        if( !first )
            shader_writer << ", ";
        else
            first = false;
        shader_writer << *argument;
    }
    shader_writer << ")";
}

CompleteType TypeConstructorExpression::GetType() const
{
    /// todo constructing arrays
    return CompleteType( m_Type );
}

std::set<Function_sp> TypeConstructorExpression::GetCallees() const
{
    std::set<Function_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetCallees();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

std::set<Variable_sp> TypeConstructorExpression::GetVariables() const
{
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetVariables();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

std::set<Variable_sp> TypeConstructorExpression::GetWrittenToVariables(
                                                        bool is_assigned) const
{
    assert( !is_assigned && "Trying to assign to a constructor expression");
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetWrittenToVariables( is_assigned );
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

bool TypeConstructorExpression::IsConst() const
{
    for( const auto& argument : m_Arguments )
        if( !argument->IsConst() )
            return false;
    return true;
}

bool TypeConstructorExpression::IsLValue() const
{
    return false;
}

bool TypeConstructorExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try and parse the primary expression
    if( parser.Expect<PrimaryExpression>( token ) )
        return true;
    CHECK_PARSER;

    std::unique_ptr<TypeSpecifier> type_specifier;
    if( !parser.Expect<TypeSpecifier>(type_specifier) )
        return false;

    //
    // match opening bracket or brace
    //
    bool round_brackets = parser.ExpectTerminal( TerminalType::OPEN_ROUND );
    if( !round_brackets &&
        !parser.ExpectTerminal( TerminalType::OPEN_BRACE ) )
        return false;

    std::vector<Expression_up> arguments;
    parser.ExpectListOf<AssignmentExpression, TerminalType::COMMA>( arguments );
    CHECK_PARSER;

    //
    // match closing bracket or brace
    //
    if( !parser.ExpectTerminal( round_brackets ? TerminalType::CLOSE_ROUND
                                               : TerminalType::CLOSE_BRACE ) )
        return false;

    Type type = type_specifier->GetType();
    token.reset( new TypeConstructorExpression( type, std::move(arguments) ) );
    return true;
}

bool TypeConstructorExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::TypeConstructorExpression;
}

bool TypeConstructorExpression::classof( const TypeConstructorExpression* e )
{
    return true;
}

//------------------------------------------------------------------------------
// PrimaryExpression
//------------------------------------------------------------------------------

bool PrimaryExpression::Parse( Parser& parser,
                               Expression_up& token )
{
    // Try and parse and identifier
    if( parser.Expect<IdentifierExpression>( token ) )
        return true;
    CHECK_PARSER;

    // Try and parse a literal
    if( parser.Expect<LiteralExpression>( token ) )
        return true;
    CHECK_PARSER;

    // Try and parse a bracketed expression
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;
    if( !parser.Expect<Expression>( token ) )
        return false;
    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
    {
        parser.Error( "Expected closing ')' in primary expression" );
        return false;
    }

    return true;
}

} // namespace Compiler
} // namespace JoeLang
