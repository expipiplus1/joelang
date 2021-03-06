/*
    Copyright 2012 Joe Hermaszewski. All rights reserved.

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

#include "declarator_specifier.hpp"

#include <cassert>
#include <memory>
#include <utility>
#include <vector>

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/semantic.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/parameter.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// ArraySpecifier
//------------------------------------------------------------------------------

ArraySpecifier::ArraySpecifier( Expression_up expression )
    :Token( TokenTy::ArraySpecifier )
    ,m_Expression( std::move( expression ) )
{
    assert( m_Expression && "ArraySpecifier given a null expression" );
}

ArraySpecifier::~ArraySpecifier()
{
}

void ArraySpecifier::PerformSema( SemaAnalyzer& sema )
{
    m_Expression = CastExpression::Create( Type::LONG,
                                           std::move(m_Expression),
                                           false );
    m_Expression->PerformSema( sema );
    if( !m_Expression->IsConst() )
        sema.Error( "Can't create array with non-const dimension" );
}

Expression_up ArraySpecifier::GetExpression()
{
    return std::move(m_Expression);
}

ArrayExtents ArraySpecifier::GetArrayExtents(
            std::vector<std::unique_ptr<ArraySpecifier> >& specifiers,
            SemaAnalyzer& sema )
{
    ArrayExtents ret;
    for( auto& array_specifier : specifiers )
    {
        // This ensures it's const
        array_specifier->PerformSema( sema );
        GenericValue g = sema.EvaluateExpression(
                                            *array_specifier->GetExpression() );
        jl_long size = g.GetLong();
        if( size <= 0 )
            sema.Error( "Can't create an array with a non-positive dimension" );
        ret.push_back( size );
    }
    return ret;
}

bool ArraySpecifier::Parse( Parser& parser,
                            std::unique_ptr<ArraySpecifier>& token )
{
    // Opening square bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
        return false;

    Expression_up expression;
    if( !parser.Expect<Expression>( expression ) )
    {
        parser.Error( "No expression in array specifier" );
        return false;
    }

    if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
    {
        //TODO things like here non fatal error, assume the closing bracket
        parser.Error( "']' missing in array specifier" );
        return false;
    }

    token.reset( new ArraySpecifier( std::move(expression) ) );
    return true;
}

//------------------------------------------------------------------------------
// FunctionSpecifier
//------------------------------------------------------------------------------

FunctionSpecifier::FunctionSpecifier(
                           std::vector<std::unique_ptr<Parameter> > parameters )
    :Token( TokenTy::FunctionSpecifier )
    ,m_Parameters( std::move(parameters) )
{
#if !defined(NDEBUG)
    for( const auto& p : parameters )
        assert( p && "FunctionSpecifier given null parameter" );
#endif
}

FunctionSpecifier::~FunctionSpecifier()
{
}

bool FunctionSpecifier::PerformSema( SemaAnalyzer& sema )
{
    bool good = true;
    for( auto& p : m_Parameters )
        good &= p->PerformSema( sema );
    return good;
}

void FunctionSpecifier::DeclareParameters( SemaAnalyzer& sema )
{
    for( const auto& p : m_Parameters )
        p->Declare( sema );
}

std::vector<CompleteType> FunctionSpecifier::GetParameterTypes() const
{
    std::vector<CompleteType> ret;
    for( const auto& p : m_Parameters )
        ret.push_back( p->GetType() );
    return ret;
}

std::vector<Variable_sp> FunctionSpecifier::GetParameters() const
{
    std::vector<Variable_sp> ret;
    for( const auto& p : m_Parameters )
        ret.push_back( p->GetVariable() );
    return ret;
}

bool FunctionSpecifier::Parse( Parser& parser,
                            std::unique_ptr<FunctionSpecifier>& token )
{
    // Opening round bracket
    if( !parser.ExpectTerminal( TerminalType::OPEN_ROUND ) )
        return false;

    std::vector<std::unique_ptr<Parameter> > parameters;
    parser.ExpectListOf<Parameter, TerminalType::COMMA>( parameters );
    CHECK_PARSER;

    if( !parser.ExpectTerminal( TerminalType::CLOSE_ROUND ) )
        return false;

    token.reset( new FunctionSpecifier( std::move(parameters) ) );
    return true;
}

//------------------------------------------------------------------------------
// SemanticSpecifier
//------------------------------------------------------------------------------

SemanticSpecifier::SemanticSpecifier( std::string string,
                                      Expression_up index_expression )
    :Token( TokenTy::SemanticSpecifier )
    ,m_String( std::move(string) )
    ,m_IndexExpression( std::move(index_expression) )
{
    assert( !m_String.empty() && "SemanticSpecifier given an empty string" );
}

SemanticSpecifier::~SemanticSpecifier()
{
}

bool SemanticSpecifier::HasIndex() const
{
    return static_cast<bool>( m_IndexExpression );
}

Semantic SemanticSpecifier::GetSemantic() const
{
    if( HasIndex() )
        return Semantic( m_String );
    else
        return Semantic( m_String, m_Index );
}

void SemanticSpecifier::PerformSema( SemaAnalyzer& sema )
{
    //
    // If we don't have an index we have no work to do here
    //
    if( !m_IndexExpression )
        return;

    m_IndexExpression = CastExpression::Create( CompleteType( Type::INT ),
                                                std::move(m_IndexExpression),
                                                false );

    if( !m_IndexExpression->PerformSema( sema ) )
        return;

    if( !m_IndexExpression->IsConst() )
    {
        sema.Error( "Index in an semantic index must be const" );
        return;
    }

    int index = sema.EvaluateExpression( *m_IndexExpression ).GetInt();
    if( index < 0 )
    {
        sema.Error( "SemanticSpecifier index must be non-negative" );
        return;
    }
    m_Index = index;
}

bool SemanticSpecifier::Parse( Parser& parser, SemanticSpecifier_up& token )
{
    if( !parser.ExpectTerminal( TerminalType::COLON ) )
        return false;

    std::string string;
    if( !parser.ExpectTerminal( TerminalType::IDENTIFIER, string ) )
    {
        parser.Error( "Expected semantic" );
        return false;
    }

    //
    // try and parse the optional index
    //
    Expression_up index_expression;
    if( parser.ExpectTerminal( TerminalType::OPEN_SQUARE ) )
    {
        if( !parser.Expect<Expression>( index_expression ) )
            return false;
        if( !parser.ExpectTerminal( TerminalType::CLOSE_SQUARE ) )
        {
            parser.Error( "Expected closing square bracket on semantic index" );
            return false;
        }
    }

    token.reset( new SemanticSpecifier( std::move(string),
                                        std::move(index_expression) ) );
    return true;
}

} // namespace Compiler
} // namespace JoeLang
