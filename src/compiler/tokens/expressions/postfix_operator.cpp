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

#include "postfix_operator.hpp"

#include <cassert>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <utility>

#include <joelang/types.hpp>
#include <compiler/casting.hpp>
#include <compiler/code_generator.hpp>
#include <compiler/function.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/parser.hpp>
#include <compiler/sema_analyzer.hpp>
#include <compiler/shader_writer.hpp>
#include <compiler/terminal_types.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// PostfixOperator
//------------------------------------------------------------------------------

PostfixOperator::PostfixOperator( TokenTy sub_class_id )
    :Token( sub_class_id )
{
}

PostfixOperator::~PostfixOperator()
{
}

bool PostfixOperator::Parse( Parser& parser,
                             std::unique_ptr<PostfixOperator>& token )
{
    // Try and parse any of the operators
    std::unique_ptr<Token> t;
    if( !parser.ExpectAnyOf<SubscriptOperator,
                            ArgumentListOperator,
                            MemberAccessOperator,
                            IncrementOrDecrementOperator>( t ) )
        return false;

    // Cast the result to a PostfixOperator
    assert( isa<PostfixOperator>(t) );
    token.reset( static_cast<PostfixOperator*>( t.release() ) );
    return true;
}

bool PostfixOperator::IsLValue( const Expression& expression ) const
{
    return false;
}

bool PostfixOperator::classof( const Token* e )
{
    return e->GetSubClassID() >= TokenTy::PostfixOperator_Start &&
           e->GetSubClassID() <= TokenTy::PostfixOperator_End;
}

bool PostfixOperator::classof( const PostfixOperator* e )
{
    return true;
}

//------------------------------------------------------------------------------
// SubscriptOperator
//------------------------------------------------------------------------------

SubscriptOperator::SubscriptOperator( Expression_up index_expression )
    :PostfixOperator( TokenTy::SubscriptOperator )
    ,m_IndexExpression( std::move(index_expression) )
{
    assert( m_IndexExpression &&
            "SubscriptOperator given a null index expression" );
}

SubscriptOperator::~SubscriptOperator()
{
}

bool SubscriptOperator::ResolveIdentifiers( SemaAnalyzer& sema,
                                            Expression& expression )
{
    bool good = expression.ResolveIdentifiers( sema );
    good &= m_IndexExpression->ResolveIdentifiers( sema );
    return good;
}

bool SubscriptOperator::PerformSema( SemaAnalyzer& sema,
                                     Expression& expression )
{
    bool good = true;
    good &= expression.PerformSema( sema );
    m_IndexExpression = CastExpression::Create( Type::I64,
                                                std::move(m_IndexExpression) );
    good &= m_IndexExpression->PerformSema( sema );

    if( !expression.GetType().IsArrayType() )
    {
        sema.Error( "Trying to index into a non-array" );
        return false;
    }
    const ArrayExtents extents = expression.GetType().GetArrayExtents();
    assert( !extents.empty() && "Indexing into a non array" );
    if( m_IndexExpression->IsConst() )
    {
        unsigned index = sema.EvaluateExpression( *m_IndexExpression ).GetI64();
        if( index >= extents[0] )
            sema.Error( "Indexing beyond array bounds" );
    }
    m_ArrayExtents.assign( ++extents.begin(), extents.end() );
    return good;
}

llvm::Value* SubscriptOperator::CodeGen( CodeGenerator& code_gen,
                                         const Expression& expression )
{
    return code_gen.CreateArrayIndex( expression, *m_IndexExpression );
}

llvm::Value* SubscriptOperator::CodeGenPointerTo(
                                            CodeGenerator& code_gen,
                                            const Expression& expression )
{
    return code_gen.CreateArrayIndexPointerTo( expression,
                                               *m_IndexExpression );
}

void SubscriptOperator::Write( ShaderWriter& shader_writer ) const
{
    assert( false && "complete me" );
}

CompleteType SubscriptOperator::GetType( const Expression& expression ) const
{
    ArrayExtents array_extents = expression.GetType().GetArrayExtents();
    if( !array_extents.empty() )
        array_extents.resize( array_extents.size() - 1 );
    return CompleteType( expression.GetType().GetBaseType(),
                         std::move( array_extents ) );
}

std::set<Function_sp> SubscriptOperator::GetCallees(
                                            const Expression& expression ) const
{
    return expression.GetCallees();
}

std::set<Variable_sp> SubscriptOperator::GetVariables(
                                            const Expression& expression ) const
{
    return expression.GetVariables();
}

bool SubscriptOperator::IsConst( const Expression& expression ) const
{
    return expression.IsConst() &&
           m_IndexExpression->IsConst();
}

bool SubscriptOperator::IsLValue( const Expression& expression ) const
{
    return true;
}

bool SubscriptOperator::Parse( Parser& parser,
                               std::unique_ptr<SubscriptOperator>& token )
{
    // open bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
        return false;

    // parse the index expression
    Expression_up index_expression;
    if( !parser.Expect<Expression>( index_expression ) )
        return false;

    // close bracket
    if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        return false;

    token.reset( new SubscriptOperator( std::move(index_expression) ) );
    return true;
}

//------------------------------------------------------------------------------
// ArgumentListOperator
//------------------------------------------------------------------------------

ArgumentListOperator::ArgumentListOperator( ArgumentExpressionVector arguments )
    :PostfixOperator( TokenTy::ArgumentListOperator )
    ,m_Arguments( std::move(arguments) )
{
#ifndef NDEBUG
    for( const auto& e : m_Arguments )
        assert( e && "ArgumentListOperator given a null argument expression" );
#endif
}

ArgumentListOperator::~ArgumentListOperator()
{
}

bool ArgumentListOperator::ResolveIdentifiers( SemaAnalyzer& sema,
                                               Expression& expression )
{
    // This doesn't resolve the identifier in expression because that will only
    // find variables
    bool good = true;
    for( auto& a : m_Arguments )
        good &= a->ResolveIdentifiers( sema );

    //
    // Find the function to call
    //
    std::vector<CompleteType> argument_types;
    for( auto& a : m_Arguments )
        argument_types.push_back( a->GetType() );

    //
    // Make sure we're being called on an IdentifierExpression
    //
    if( !isa<IdentifierExpression>( expression ) )
    {
        sema.Error( "Trying to call a non-function" );
        // Can't really proceed without a function
        return false;
    }

    const IdentifierExpression& identifier_expression =
                           static_cast<const IdentifierExpression&>(expression);
    const std::string& name = identifier_expression.GetIdentifier();

    if( !sema.HasFunctionNamed( name ) )
    {
        sema.Error( "Calling undeclared function " + name );
        return false;
    }

    m_Function = sema.GetFunctionOverload( name, argument_types );
    if( !m_Function )
    {
        sema.Error( "Couldn't find compatible overload for " + name );
        return false;
    }

    return good;
}

bool ArgumentListOperator::PerformSema( SemaAnalyzer& sema,
                                        Expression& expression )
{
    bool good = true;

    for( auto& a : m_Arguments )
        good &= a->PerformSema( sema );

    return good;
}

llvm::Value* ArgumentListOperator::CodeGen( CodeGenerator& code_gen,
                                            const Expression& expression )
{
    return code_gen.CreateFunctionCall( m_Function, m_Arguments );
}

llvm::Value* ArgumentListOperator::CodeGenPointerTo( CodeGenerator& code_gen,
                                                  const Expression& expression )
{
    assert( false && "Can't get the pointer of a function call" );
    return nullptr;
}

void ArgumentListOperator::Write( ShaderWriter& shader_writer ) const
{
    shader_writer << "(";
    bool first = true;
    for( const auto& a : m_Arguments )
    {
        if( !first )
            shader_writer << ", ";
        else
            first = false;

        shader_writer << *a;
    }
    shader_writer << ")";
}

CompleteType ArgumentListOperator::GetType( const Expression& expression ) const
{
    if( !m_Function )
        return CompleteType();
    return m_Function->GetReturnType();
}

std::set<Function_sp> ArgumentListOperator::GetCallees(
                                            const Expression& expression ) const
{
    // The expression here is actually an identifier, which we don't want to dip
    // into because it thinks it's a variable
    assert( m_Function &&
            "Trying to get the Callees of an unresolved ArgumentListOperator" );
    std::set<Function_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetCallees();
        ret.insert( f.begin(), f.end() );
    }
    ret.insert( m_Function );
    return ret;
}

std::set<Variable_sp> ArgumentListOperator::GetVariables(
                                            const Expression& expression ) const
{
    // The expression here is actually an identifier, which we don't want to dip
    // into because it thinks it's a variable
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetVariables();
        ret.insert( f.begin(), f.end() );
    }
    return ret;
}

bool ArgumentListOperator::IsConst( const Expression& expression ) const
{
    return false;
}

bool ArgumentListOperator::Parse( Parser& parser,
                                  std::unique_ptr<ArgumentListOperator>& token )
{
    // parse (
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;

    // The vector to hold the expressions
    ArgumentExpressionVector arguments;

    parser.ExpectListOf<AssignmentExpression, TerminalType::COMMA>( arguments );
    CHECK_PARSER;

    // parse closing )
    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
    {
        parser.Error( "Expected closing ')' after argument list" );
        return false;
    }

    token.reset( new ArgumentListOperator( std::move(arguments) ) );
    return true;
}

//------------------------------------------------------------------------------
// MemberAccessOperator
//------------------------------------------------------------------------------

MemberAccessOperator::MemberAccessOperator( std::string identifier )
    :PostfixOperator( TokenTy::MemberAccessOperator )
    ,m_Identifier( std::move( identifier ) )
{
}

MemberAccessOperator::~MemberAccessOperator()
{
}

bool MemberAccessOperator::ResolveIdentifiers( SemaAnalyzer& sema,
                                               Expression& expression )
{
    assert( false && "Complete me" );
    return false;
}

bool MemberAccessOperator::PerformSema(
                                SemaAnalyzer& sema,
                                Expression& expression )
{
    assert( false && "Complete me" );
    return false;
}

llvm::Value* MemberAccessOperator::CodeGen( CodeGenerator& code_gen,
                                            const Expression& expression )
{
    assert( false && "complete me" );
    return nullptr;
}

llvm::Value* MemberAccessOperator::CodeGenPointerTo(
                                                  CodeGenerator& code_gen,
                                                  const Expression& expression )
{
    assert( false && "Complete me" );
    return nullptr;
}

void MemberAccessOperator::Write( ShaderWriter& shader_writer ) const
{
    assert( false && "complete me" );
}

CompleteType MemberAccessOperator::GetType( const Expression& expression ) const
{
    assert( false && "Complete me" );
    return CompleteType();
}

std::set<Function_sp> MemberAccessOperator::GetCallees(
                                            const Expression& expression ) const
{
    assert( false && "Complete me" );
    return {};
}

std::set<Variable_sp> MemberAccessOperator::GetVariables(
                                            const Expression& expression ) const
{
    assert( false && "Complete me" );
    return {};
}

bool MemberAccessOperator::IsConst( const Expression& expression ) const
{
    return expression.IsConst();
}

bool MemberAccessOperator::Parse( Parser& parser,
                                  std::unique_ptr<MemberAccessOperator>& token )
{
    // Parse the member access operator
    if( !parser.ExpectTerminal( TerminalType::PERIOD ) )
        return false;

    // Store the member identifier in identifier
    std::string identifier;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, identifier ) )
        return false;

    token.reset( new MemberAccessOperator( std::move(identifier) ) );
    return true;
}

//------------------------------------------------------------------------------
// IncrementOrDecrementOperator
//------------------------------------------------------------------------------

IncrementOrDecrementOperator::IncrementOrDecrementOperator( Op op )
    :PostfixOperator( TokenTy::IncrementOrDecrementOperator )
    ,m_Operator( op )
{
}

IncrementOrDecrementOperator::~IncrementOrDecrementOperator()
{
}

bool IncrementOrDecrementOperator::ResolveIdentifiers( SemaAnalyzer& sema,
                                                       Expression& expression )
{
    assert( false && "Complete me" );
    return false;
}

bool IncrementOrDecrementOperator::PerformSema( SemaAnalyzer& sema,
                                                Expression& expression )
{
    assert( false && "Complete me" );
    return false;
}

llvm::Value* IncrementOrDecrementOperator::CodeGen(
                                               CodeGenerator& code_gen,
                                               const Expression& expression )
{
    assert( false && "complete me" );
    return nullptr;
}

llvm::Value* IncrementOrDecrementOperator::CodeGenPointerTo(
                                                  CodeGenerator& code_gen,
                                                  const Expression& expression )
{
    assert( false && "Complete me" );
    return nullptr;
}

void IncrementOrDecrementOperator::Write( ShaderWriter& shader_writer ) const
{
    assert( false && "complete me" );
}

CompleteType IncrementOrDecrementOperator::GetType(
                                        const Expression& expression ) const
{
    assert( false && "Complete me" );
    return CompleteType();
}

std::set<Function_sp> IncrementOrDecrementOperator::GetCallees(
                                            const Expression& expression ) const
{
    assert( false && "Complete me" );
    return {};
}

std::set<Variable_sp> IncrementOrDecrementOperator::GetVariables(
                                            const Expression& expression ) const
{
    assert( false && "Complete me" );
    return {};
}

bool IncrementOrDecrementOperator::IsConst( const Expression& expression ) const
{
    return false;
}

bool IncrementOrDecrementOperator::Parse(
                          Parser& parser,
                          std::unique_ptr<IncrementOrDecrementOperator>& token )
{
    // Try to parse ++
    if( parser.ExpectTerminal( TerminalType::INCREMENT ) )
    {
        token.reset( new IncrementOrDecrementOperator( Op::INCREMENT ) );
        return true;
    }

    // Try to parse --
    if( parser.ExpectTerminal( TerminalType::DECREMENT ) )
    {
        token.reset( new IncrementOrDecrementOperator( Op::DECREMENT ) );
        return true;
    }

    return false;
}

} // namespace Compiler
} // namespace JoeLang
