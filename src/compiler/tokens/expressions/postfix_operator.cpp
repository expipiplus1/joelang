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
#include <memory>
#include <set>
#include <string>
#include <utility>

#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/tokens/expressions/assignment_expression.hpp>
#include <compiler/tokens/expressions/cast_expression.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/expressions/identifier_expression.hpp>
#include <compiler/tokens/expressions/postfix_expression.hpp>
#include <compiler/tokens/token.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/runtime.hpp>
#include <compiler/writers/shader_writer.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

class FunctionNode;

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

bool SubscriptOperator::PerformSema( SemaAnalyzer& sema,
                                     Expression_up& expression )
{
    // todo, subscripting into vectors
    assert( expression && "PerformSema given null expression" );

    bool good = true;
    good &= expression->PerformSema( sema );
    m_IndexExpression = CastExpression::Create( Type::LONG,
                                                std::move(m_IndexExpression),
                                                false );
    good &= m_IndexExpression->PerformSema( sema );

    if( !expression->GetType().IsArrayType() )
    {
        sema.Error( "Trying to index into a non-array" );
        return false;
    }
    const ArrayExtents extents = expression->GetType().GetArrayExtents();
    assert( !extents.empty() && "Indexing into a non array" );
    if( m_IndexExpression->IsConst() )
    {
        unsigned index = sema.EvaluateExpression( *m_IndexExpression ).GetLong();
        if( index >= extents[0] )
            sema.Error( "Indexing beyond array bounds" );
    }
    m_ArrayExtents.assign( ++extents.begin(), extents.end() );
    return good;
}

const PointerExpressionNode& SubscriptOperator::GenerateCodeDag( NodeManager& node_manager, Expression& expression ) const
{
    const Node& array = expression.GenerateCodeDag( node_manager );
    const Node& index = m_IndexExpression->GenerateCodeDag( node_manager );
    return node_manager.MakePointerExpressionNode( NodeType::ArrayIndex, {array, index} );
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

void SubscriptOperator::Write( ShaderWriter& shader_writer,
                               const Expression& expression ) const
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
    auto ret = expression.GetCallees();
    auto f   = m_IndexExpression->GetCallees();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> SubscriptOperator::GetVariables(
                                            const Expression& expression ) const
{
    auto ret = expression.GetVariables();
    auto f   = m_IndexExpression->GetVariables();
    ret.insert( f.begin(), f.end() );
    return ret;
}

std::set<Variable_sp> SubscriptOperator::GetWrittenToVariables(
                                                   const Expression& expression,
                                                   bool is_assigned ) const
{
    auto ret = expression.GetWrittenToVariables( is_assigned );
    auto f   = m_IndexExpression->GetWrittenToVariables( false );
    ret.insert( f.begin(), f.end() );
    return ret;
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

bool ArgumentListOperator::ResolveFunctionIdentifier(
                                                    SemaAnalyzer& sema,
                                                    Expression& expression )
{
    // This doesn't resolve the identifier in expression because that will only
    // find variables and not functions
    bool good = true;

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
                                        Expression_up& expression )
{
    bool good = true;

    for( auto& a : m_Arguments )
        good &= a->PerformSema( sema );

    if( !good )
        return false;

    // Resolve the function overload
    //
    if( !ResolveFunctionIdentifier( sema, *expression ) )
        return false;

    assert( m_Function && "Performing Sema on a null function" );

    std::vector<CompleteType> types = m_Function->GetParameterTypes();

    assert( types.size() >= m_Arguments.size() && "Too many arguments" );

    //
    // Cast all the arguments
    //
    for( unsigned i = 0; i < m_Arguments.size(); ++i )
    {
        CastExpression_up a = CastExpression::Create( types[i],
                                                      std::move(m_Arguments[i]),
                                                      false );
        good &= a->PerformSemaNoRecurse( sema );
        m_Arguments[i] = std::move(a);
    }

    return good;
}

const ExpressionNode& ArgumentListOperator::GenerateCodeDag( NodeManager& node_manager, 
                                                             Expression& expression ) const
{
    const FunctionNode& function_node = node_manager.MakeFunctionNode( m_Function );
    // put the function in here, to avoid having to concatenate the arguments later
    std::vector<Node_ref> argument_nodes;
    argument_nodes.reserve( 1 + m_Arguments.size() );
    
    // todo, default arguments
    for( const Expression_up& argument : m_Arguments )
        argument_nodes.push_back( argument->GenerateCodeDag( node_manager ) );
        
    argument_nodes.push_back( function_node );
    return node_manager.MakeExpressionNode( NodeType::Call, argument_nodes );
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

void ArgumentListOperator::Write( ShaderWriter& shader_writer,
                                  const Expression& expression ) const
{
    //
    // If this is a runtime function, defer writing to the runtime
    //
    if( m_Function->IsRuntimeFunction() )
    {
        shader_writer.WriteRuntimeFunctionCall(
                                               m_Function->GetRuntimeFunction(),
                                               m_Arguments );
        return;
    }

    shader_writer << ShaderWriter::Mangle( m_Function->GetIdentifier(),
                                           IdentifierType::FUNCTION ) << "(";
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

std::set<Variable_sp> ArgumentListOperator::GetWrittenToVariables(
                                                   const Expression& expression,
                                                   bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a function call" );

    // The expression here is actually an identifier, which we don't want to dip
    // into because it thinks it's a variable
    std::set<Variable_sp> ret;
    for( const auto& a : m_Arguments )
    {
        auto f = a->GetWrittenToVariables( is_assigned );
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
    assert( !m_Identifier.empty() &&
            "MemberAccessOpertor given empty identifier" );
}

MemberAccessOperator::~MemberAccessOperator()
{
}

bool MemberAccessOperator::PerformSema(
                                SemaAnalyzer& sema,
                                Expression_up& expression )
{
    assert( expression && "PerformSema passed a null expression" );

    if( !expression->PerformSema( sema ) )
        return false;

    //
    // We're a swizzle if we're being applied to a vector or a scalar
    //
    if( expression->GetType().IsScalarType() ||
        expression->GetType().IsVectorType() )
    {
        return PerformSemaSwizzle( sema, expression );
    }

    assert( false && "Complete me" );
    return false;
}

bool MemberAccessOperator::PerformSemaSwizzle( SemaAnalyzer& sema,
                                               Expression_up& expression )
{
    assert( expression && "PerformSemaSwizzle passed a null expression" );

    //
    // A swizzle can be at most four characters
    // Although this will be detected anyway, we can give a different error here
    //
    if( m_Identifier.size() > 4 )
    {
        sema.Error( "Swizzle too large: " + m_Identifier );
        return false;
    }
    unsigned num_swizzle_elements = m_Identifier.size();

    //
    // A swizzle can only be from one of the swizzle sets
    //

    static
    const std::array<std::string, 3> swizzle_sets =
    {{
        "xyzw",
        "rgba",
        "stpq"
    }};

    int swizzle_set_index = -1;

    std::array<unsigned char, 4> swizzle_indices = {{ 0xff, 0xff, 0xff, 0xff }};

    for( unsigned h = 0; h < num_swizzle_elements; ++h )
    {
        char c = m_Identifier[h];
        int j = -1;
        for( unsigned i = 0; i < swizzle_sets.size() && j == -1; ++i )
        {
            for( unsigned char k = 0; k < swizzle_sets[i].size(); ++k )
            {
                char s = swizzle_sets[i][k];
                if( c == s )
                {
                    j = i;
                    swizzle_indices[h] = k;
                    break;
                }
            }
        }
        if( j == -1 )
        {
            sema.Error( "Unknown element in swizzle: " + m_Identifier );
            return false;
        }
        if( swizzle_set_index == -1 )
            swizzle_set_index = j;
        else if( swizzle_set_index != j )
        {
            sema.Error( "Swizzle of different swizzle sets: "
                        + m_Identifier );
            return false;
        }
    }

    //
    // Check if any of the indices are out of bounds
    //
    unsigned max_index = expression->GetType().GetNumElements() - 1;
    for( unsigned i = 0; swizzle_indices[i] != 0xff && i < 4; ++i )
    {
        if( swizzle_indices[i] > max_index )
        {
            sema.Error( "Swizzle index out of bounds" );
            return false;
        }
    }

    //
    // We have our swizzle, now check if we can fold this swizzle with the one
    // below ours if there is one
    //
    if( PostfixExpression* p = dyn_cast<PostfixExpression>( expression.get() ) )
    if( MemberAccessOperator* m = dyn_cast<MemberAccessOperator>(
                                                           &p->GetOperator() ) )
    if( m->IsSwizzle() )
    {
        //
        // If this is a swizzle then we can steal it and fold the two together
        //
        Swizzle other_swizzle = m->GetSwizzle();

        for( unsigned i = 0; i < num_swizzle_elements; ++i )
            swizzle_indices[i] = other_swizzle.GetIndex( swizzle_indices[i] );

        expression = std::move( p->TakeExpression() );
    }

    m_Swizzle = Swizzle( swizzle_indices[0],
                         swizzle_indices[1],
                         swizzle_indices[2],
                         swizzle_indices[3] );

    return true;
}

const ExpressionNode& MemberAccessOperator::GenerateCodeDag( NodeManager& node_manager, Expression& expression ) const
{
    if( IsSwizzle() )
    {
        const ExpressionNode& swizzled_node = expression.GenerateCodeDag( node_manager );
        return node_manager.MakeSwizzleNode( swizzled_node, m_Swizzle );
    }
        
    assert( false && "Complete me" );
    return node_manager.MakeExpressionNode( NodeType::Unimplemented, {} );
}

bool MemberAccessOperator::IsSwizzle() const
{
    return m_Swizzle.IsValid();
}

Swizzle MemberAccessOperator::GetSwizzle() const
{
    assert( IsSwizzle() &&
            "Trying to get a swizzle from a non swizzle operator" );
    return m_Swizzle;
}

llvm::Value* MemberAccessOperator::CodeGen( CodeGenerator& code_gen,
                                            const Expression& expression )
{
    if( IsSwizzle() )
        return code_gen.CreateSwizzle( expression, m_Swizzle );

    assert( false && "complete me" );
    return nullptr;
}

llvm::Value* MemberAccessOperator::CodeGenPointerTo(
                                                  CodeGenerator& code_gen,
                                                  const Expression& expression )
{
    if( IsSwizzle() )
    {
        return expression.CodeGenPointerTo( code_gen );
    }

    assert( false && "Complete me" );
    return nullptr;
}

void MemberAccessOperator::Write( ShaderWriter& shader_writer,
                                  const Expression& expression ) const
{
    if( IsSwizzle() )
    {
        //
        // If this is a scalar, we have to make a vector constructor for it
        // because glsl doesn't support swizzle operators before version 420
        //
        if( IsScalarType( expression.GetType().GetBaseType() ) )
        {
            //
            // If this is a scalar to scalar swizzle just output the expression
            //
            if( m_Swizzle.GetSize() == 1 )
                shader_writer << expression;
            else
                shader_writer << GetType( expression )
                              << "(" << expression << ")";
        }
        else
        {
            static
            const std::string swizzle_set = "xyzw";
            shader_writer << expression << ".";
            for( unsigned i = 0; i < m_Swizzle.GetSize(); ++i )
                shader_writer << swizzle_set[m_Swizzle.GetIndex(i)];
        }
    }
    else
        assert( false && "complete me" );
}

CompleteType MemberAccessOperator::GetType( const Expression& expression ) const
{
    if( IsSwizzle() )
    {
        Type base_type = GetScalarType( expression.GetType().GetBaseType() );
        return CompleteType( GetVectorType( base_type, m_Swizzle.GetSize() ) );
    }

    assert( false && "Complete me" );
    return CompleteType();
}

std::set<Function_sp> MemberAccessOperator::GetCallees(
                                            const Expression& expression ) const
{
    if( IsSwizzle() )
        return expression.GetCallees();

    assert( false && "Complete me" );
    return std::set<Function_sp>{};
}

std::set<Variable_sp> MemberAccessOperator::GetVariables(
                                            const Expression& expression ) const
{
    if( IsSwizzle() )
        return expression.GetVariables();

    assert( false && "Complete me" );
    return std::set<Variable_sp>{};
}

std::set<Variable_sp> MemberAccessOperator::GetWrittenToVariables(
                                                   const Expression& expression,
                                                   bool is_assigned ) const
{
    if( IsSwizzle() )
        return expression.GetWrittenToVariables( is_assigned );

    assert( false && "Complete me" );
    return std::set<Variable_sp>{};
}

bool MemberAccessOperator::IsConst( const Expression& expression ) const
{
    return expression.IsConst();
}

bool MemberAccessOperator::IsLValue( const Expression& expression ) const
{
    if( IsSwizzle() )
    {
        return expression.IsLValue() && !m_Swizzle.HasDuplicates();
    }

    assert( false && "complete me" );
    return false;
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

bool MemberAccessOperator::classof( const PostfixOperator* e )
{
    return e->GetSubClassID() == TokenTy::MemberAccessOperator;
}

bool MemberAccessOperator::classof( const MemberAccessOperator* e )
{
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

bool IncrementOrDecrementOperator::PerformSema( SemaAnalyzer& sema,
                                                Expression_up& expression )
{
    assert( false && "Complete me" );
    return false;
}

const ExpressionNode& IncrementOrDecrementOperator::GenerateCodeDag( NodeManager& node_manager, 
                                                                     Expression& expression ) const
{
#if 0
    const ExpressionNode& expression_node = expression->GenerateCodeDag( node_manager );
    if( IsIncrement() )
        return node_manager.MakeExpressionNode( NodeType::PostIncrement, {expression_node} );
    else
    return node_manager.MakeExpressionNode( NodeType::PostDecrement, {expression_node} );
#endif
    assert( false && "Complete me" );
    return node_manager.MakeExpressionNode( NodeType::Unimplemented, {} );
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

void IncrementOrDecrementOperator::Write( ShaderWriter& shader_writer,
                                          const Expression& expression ) const
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
    return std::set<Function_sp>{};
}

std::set<Variable_sp> IncrementOrDecrementOperator::GetVariables(
                                            const Expression& expression ) const
{
    assert( false && "Complete me" );
    return std::set<Variable_sp>{};
}

std::set<Variable_sp> IncrementOrDecrementOperator::GetWrittenToVariables(
                                                   const Expression& expression,
                                                   bool is_assigned ) const
{
    assert( false && "Complete me" );
    return std::set<Variable_sp>{};
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
