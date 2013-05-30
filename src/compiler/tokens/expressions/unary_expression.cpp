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

#include "unary_expression.hpp"

#include <cassert>
#include <memory>
#include <set>
#include <utility>

#include <compiler/lexer/terminal_types.hpp>
#include <compiler/parser/parser.hpp>
#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/tokens/expressions/postfix_expression.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/shader_writer.hpp>

#include <compiler/code_dag/node_manager.hpp>
#include <compiler/code_dag/node.hpp>

namespace JoeLang
{
namespace Compiler
{

//------------------------------------------------------------------------------
// UnaryExpression
//------------------------------------------------------------------------------

UnaryExpression::UnaryExpression( Op op,
                                  Expression_up expression )
    :Expression( TokenTy::UnaryExpression )
    ,m_Operator( op )
    ,m_Expression( std::move(expression) )
{
}

UnaryExpression::~UnaryExpression()
{
}

bool UnaryExpression::PerformSema( SemaAnalyzer& sema )
{
    /// TODO check that expression isn't an array
    bool good = true;

    good &= m_Expression->PerformSema( sema );

    const CompleteType& t = m_Expression->GetType();
    if( t.GetType() == Type::STRING )
    {
        good = false;
        sema.Error( "Invalid type to unary operator: string" );
    }
    if( !t.IsIntegral() &&
        m_Operator == Op::BITWISE_NOT )
    {
        good = false;
        sema.Error( "Invalid type to bitwise unary not operator" );
    }

    return good;
}

const Node& UnaryExpression::GenerateCodeDag( NodeManager& node_manager ) const
{
    const Node& expression = m_Expression->GenerateCodeDag( node_manager );
    switch( m_Operator )
    {       
    case Op::PLUS:
        return expression; // A unary plus is a no-op
    case Op::MINUS:
        return node_manager.MakeNode( NodeType::Negate, { expression } );
    case Op::BITWISE_NOT:
        return node_manager.MakeNode( NodeType::BitwiseNot, { expression } );
    case Op::LOGICAL_NOT:
        return node_manager.MakeNode( NodeType::LogicalNot, { expression } );
    case Op::INCREMENT:
        return node_manager.MakeNode( NodeType::PreIncrement, { expression } );
    case Op::DECREMENT:
        return node_manager.MakeNode( NodeType::PreDecrement, { expression } );
    }
}

llvm::Value* UnaryExpression::CodeGen( CodeGenerator& code_gen ) const
{
    switch( m_Operator )
    {
    case Op::PLUS:
        return m_Expression->CodeGen( code_gen );
    case Op::MINUS:
        return code_gen.CreateNeg( *m_Expression );
    case Op::INCREMENT:
    case Op::DECREMENT:
        assert( false && "Complete me" );
        return nullptr;
    case Op::BITWISE_NOT:
        return code_gen.CreateNot( *m_Expression );
    case Op::LOGICAL_NOT:
        return code_gen.CreateLNot( *m_Expression );
    default:
        assert( false &&
                "Trying to generate code for unhandled unary operator" );
        return nullptr;
    }
}

void UnaryExpression::Write( ShaderWriter& shader_writer ) const
{
    static const std::map<Op, std::string> op_string_map =
    {
        { Op::PLUS,        "+" },
        { Op::MINUS,       "-" },
        { Op::INCREMENT,   "++" },
        { Op::DECREMENT,   "--" },
        { Op::BITWISE_NOT, "~" },
        { Op::LOGICAL_NOT, "!" }
    };
    shader_writer << "(" << op_string_map.at(m_Operator) << *m_Expression << ")";
}

CompleteType UnaryExpression::GetType() const
{
    /// Todo vector and matrix bool things

    CompleteType t = m_Expression->GetType();

    switch( m_Operator )
    {
    case Op::PLUS:
    case Op::MINUS:
    case Op::INCREMENT:
    case Op::DECREMENT:
    case Op::BITWISE_NOT:
        return t;
    case Op::LOGICAL_NOT:
        return CompleteType( Type::BOOL );
    }
    assert( false && "unreachable" );
    return CompleteType();
}

std::set<Function_sp> UnaryExpression::GetCallees() const
{
    return m_Expression->GetCallees();
}

std::set<Variable_sp> UnaryExpression::GetVariables() const
{
    return m_Expression->GetVariables();
}

std::set<Variable_sp> UnaryExpression::GetWrittenToVariables(
                                                    bool is_assigned ) const
{
    assert( !is_assigned && "Trying to assign to a unaryexpression" );
    return m_Expression->GetWrittenToVariables( is_assigned );
}

bool UnaryExpression::IsConst() const
{
    return m_Expression->IsConst() &&
           m_Operator != Op::INCREMENT &&
           m_Operator != Op::DECREMENT;
}

bool UnaryExpression::Parse( Parser& parser,
                             Expression_up& token )
{
    static const std::vector< std::pair<TerminalType, Op> >
            operator_terminal_map =
    {
        { TerminalType::PLUS,        Op::PLUS },
        { TerminalType::MINUS,       Op::MINUS },
        { TerminalType::BITWISE_NOT, Op::BITWISE_NOT },
        { TerminalType::LOGICAL_NOT, Op::LOGICAL_NOT },
        { TerminalType::INCREMENT,   Op::INCREMENT },
        { TerminalType::DECREMENT,   Op::DECREMENT }
    };

    bool found = false;
    Op op;
    for( const auto& p : operator_terminal_map )
        if( parser.ExpectTerminal( p.first ) )
        {
            op = p.second;
            found = true;
            break;
        }

    if( found )
    {
        // Parse the next unary expression
        Expression_up unary_expression;
        // we had a unary operator, there should be an expression
        if( !parser.Expect<UnaryExpression>( unary_expression ) )
            return false;

        token.reset( new UnaryExpression( op, std::move(unary_expression) ) );
        return true;
    }

    // if there was no operator just forward the parse to a postfix expression
    return parser.Expect<PostfixExpression>( token );
}

bool UnaryExpression::classof( const Expression* e )
{
    return e->GetSubClassID() == TokenTy::UnaryExpression;
}

bool UnaryExpression::classof( const UnaryExpression* e )
{
    return true;
}

} // namespace Compiler
} // namespace JoeLang
