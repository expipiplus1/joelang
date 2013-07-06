/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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

#include "glsl_writer.hpp"

#include <algorithm>
#include <string>

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/compile_statement_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/glsl_builtin_node.hpp>
#include <compiler/code_dag/node.hpp>
#include <compiler/code_dag/pointer_expression_node.hpp>
#include <compiler/code_dag/statement_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/swizzle_store_node.hpp>
#include <compiler/code_dag/temporary_assignment_node.hpp>
#include <compiler/code_dag/temporary_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/semantic.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/casting.hpp>
#include <joelang/context.hpp>
#include <joelang/shader.hpp>
#include <joelang/types.hpp>

using string_ref = std::reference_wrapper<const std::string>;

namespace JoeLang
{
namespace Compiler
{

enum class GLSLWriter::IdentifierType{ Function, Variable, };

const std::string GLSLWriter::s_GLSLVersion = "150";

std::string GLSLWriter::GenerateGLSL( const Context& context,
                                      const CompileStatementNode& compile_statement )
{
    GLSLWriter writer( context );
    return writer.Generate( compile_statement );
}

GLSLWriter::GLSLWriter( const Context& context )
    : m_Context( context )
{
}

GLSLWriter::~GLSLWriter()
{
}

std::string GLSLWriter::Generate( const CompileStatementNode& compile_statement )
{
    m_Source << "#version " << s_GLSLVersion;

    NewLine( 2 );

    const Function& entry_function = compile_statement.GetEntryFunction();

    //
    // Find all the functions we need to generate
    //
    bool recursion;
    std::set<const Function*> function_dependencies =
        entry_function.GetFunctionDependencies( recursion );
    function_dependencies.insert( &entry_function );

    if( recursion )
    {
        Error( "Can't have recursion in glsl" );
        return "";
    }

    //
    // Declare all the input, output and global variables
    //
    std::set<const Variable*> input_variables;
    std::set<const Variable*> output_variables;
    std::set<const Variable*> uniform_variables;
    for( const Function* function : function_dependencies )
    {
        const Node& function_node = function->GetCodeDag();
        const std::set<const Node*> variable_nodes =
            function_node.GetDescendantsWithNodeType( NodeType::VariableIdentifier );
        for( const Node* variable_node : variable_nodes )
        {
            const Variable& v = cast<VariableNode>( *variable_node ).GetVariable();
            if( v.IsGlobal() || ( function == &entry_function && v.IsParameter() ) )
            {
                if( v.IsIn() )
                    input_variables.insert( &v );
                if( v.IsOut() )
                    output_variables.insert( &v );
                if( v.IsUniform() )
                    uniform_variables.insert( &v );
            }
        }
    }

    WriteInputVariables( input_variables );
    NewLine();

    WriteOutputVariables( output_variables );
    NewLine();

    WriteUniformVariables( uniform_variables );
    NewLine();

    //
    // Generate all the functions this shader needs
    //
    WriteFunctionDeclarations( function_dependencies );
    NewLine();

    WriteFunctionDefinitions( function_dependencies );
    NewLine();


    //
    // Generate the wrapper, calling the JoeLang main function
    //
    WriteMainFunction( compile_statement );

    return m_Source.str();
}

//
// Writing Variables
//
void GLSLWriter::WriteInputVariables( std::set<const Variable*> input_variables )
{
    for( const Variable* variable : input_variables )
    {
        assert( variable && "WriteInputVariables given null variable" );
        assert( variable->IsIn() && "WriteInputVariables given non-in variable" );

        unsigned attribute_number = GetVariableAttributeNumber( *variable );
        m_VariableNames.insert(
            std::make_pair( variable, "i_" + std::to_string( attribute_number ) ) );
        m_Source << GetVariableTypeString( *variable ) << " i_" << attribute_number << ";";
        //if( !variable->IsConst() )
        NewLine();
    }
}

void GLSLWriter::WriteOutputVariables( std::set<const Variable*> output_variables )
{
    for( const Variable* variable : output_variables )
    {
        assert( variable && "WriteOutputVariables given null variable" );
        assert( variable->IsOut() && "WriteOutputVariables given non-out variable" );

        unsigned attribute_number = GetVariableAttributeNumber( *variable );
        m_VariableNames.insert(
            std::make_pair( variable, "o_" + std::to_string( attribute_number ) ) );
        m_Source << GetVariableTypeString( *variable ) << " o_" << attribute_number << ";";
        //if( !variable->IsConst() )
        NewLine();
    }
}

void GLSLWriter::WriteUniformVariables( std::set<const Variable*> uniform_variables )
{
    for( const Variable* variable : uniform_variables )
    {
        assert( variable && "WriteUniformVariables given null variable" );
        assert( variable->IsUniform() && "WriteUniformVariables given non-uniform variable" );

        m_VariableNames.insert( std::make_pair( variable, "u_" + variable->GetName() ) );
        m_Source << "uniform " << GetTypeString( variable->GetType() ) << " u_"
                 << variable->GetName() << ";";
        NewLine();
    }
}

unsigned GLSLWriter::GetVariableAttributeNumber( const Variable& variable )
{
    return 0; // todo, complete me
}

//
// Writing functions
//

void GLSLWriter::WriteFunctionDeclarations( std::set<const Function*> functions )
{
#if !defined( NDEBUG )
    for( const auto& f : functions )
        assert( f && "Null function in WriteFunctionDeclarations" );
#endif

    for( const Function* function : functions )
    {
        WriteFunctionHeader( *function );
        m_Source << ";";
        NewLine();
    }
}

void GLSLWriter::WriteFunctionDefinitions( std::set<const Function*> functions )
{
#if !defined( NDEBUG )
    for( const auto& f : functions )
        assert( f && "Null function in WriteFunctionDeclarations" );
#endif

    for( const Function* function : functions )
    {
        WriteFunctionHeader( *function );
        NewLine();
        m_Source << "{";
        NewLine();
        GenerateCompoundStatement( function->GetCodeDag() );
        m_Source << "}";
        NewLine();
    }
}

void GLSLWriter::WriteFunctionHeader( const Function& function )
{
    m_Source << GetTypeString( function.GetReturnType() ) << " "
             << MangleIdentifier( function.GetIdentifier(), IdentifierType::Function ) << "(";

    bool first = true;
    for( const auto& parameter : function.GetParameters() )
    {
        if( !first )
            m_Source << ", ";
        else
            first = false;

        m_Source << GetVariableTypeString( *parameter ) << " "
                 << MangleIdentifier( parameter->GetName(), IdentifierType::Variable );
    }

    m_Source << ")";
}

void GLSLWriter::WriteMainFunction( const CompileStatementNode& compile_statement )
{
    std::vector<Node_ref> statements;

    const Function& main_function = compile_statement.GetEntryFunction();

    //
    // Initialize all the global input variables
    //

    //
    // Generate the main function call
    //
    const ExpressionNode& main_function_call =
        m_NodeManager.MakeExpressionNode( NodeType::Call, compile_statement.GetChildren() );
    if( main_function.GetReturnType().IsVoid() || main_function.GetSemantic().IsVoid() )
    {
        // If it's void or doesn't have a semantic, no need to do anything with the return value
        statements.push_back( m_NodeManager.MakeStatementNode( NodeType::ExpressionStatement,
                                                               { main_function_call } ) );
    }
    else
    {
        //
        // Get the variable we'll be assigning to
        //
        Semantic semantic = main_function.GetSemantic();
        const PointerExpressionNode& assigned = m_NodeManager.MakeGLSLBuiltinNode(
            semantic.HasBuiltin( compile_statement.GetDomain(), /* is_input = */ false )
                ? semantic.GetBuiltin( compile_statement.GetDomain(), /* is_input = */ false )
                : "o_" + std::to_string( semantic.GetIndex() ) );
        const ExpressionNode& main_assignment =
            m_NodeManager.MakeExpressionNode( NodeType::Store, { assigned, main_function_call } );
        statements.push_back(
            m_NodeManager.MakeStatementNode( NodeType::ExpressionStatement, { main_assignment } ) );
    }

/*
    Semantic main_semantic = main_function.GetSemantic();

    const PointerExpressionNode* main_result_variable = nullptr;

    if( main_semantic.HasBuiltin( compile_statement.GetDomain(), false ) )
    {
        std::string main_semantic_builtin_name =
            main_semantic.GetBuiltin( compile_statement.GetDomain(), false );

        main_result_variable = m_NodeManager.MakeGLSLBuiltinNode( main_semantic_builtin_name );
    }
    else


        std::vector<Node_ref> main_call_nodes = compile_statement.GetChildren();
    const ExpressionNode main_call =
        m_NodeManager.MakeExpressionNode( NodeType::Call, std::move( main_call_nodes ) );

    const ExpressionNode& main_store_node =
        m_NodeManager.MakeExpressionNode( NodeType::Store, { main_result_variable, main_call } );

    statements.push_back(
        m_NodeManager.MakeStatementNode( NodeType::ExpressionStatement, { main_store_node } ) );

        */
    const StatementNode& main_sequence =
        m_NodeManager.MakeStatementNode( NodeType::Sequence, std::move( statements ) );

    m_Source << "void main()";
    NewLine();
    m_Source << "{";
    NewLine();
    GenerateCompoundStatement( main_sequence );
    m_Source << "}";
    NewLine();
}

//
// Typoes
//

std::string GLSLWriter::GetTypeString( const CompleteType& type )
{
    const static
    std::map<Type, std::string> type_string_map = {
        { Type::FLOAT, "float" }, { Type::FLOAT2, "vec2" }, { Type::FLOAT3, "vec3" },
        { Type::FLOAT4, "vec4" }, { Type::FLOAT2x2, "mat2x2" }, { Type::FLOAT2x3, "mat2x3" },
        { Type::FLOAT2x4, "mat2x4" }, { Type::FLOAT3x2, "mat3x2" }, { Type::FLOAT3x3, "mat3x3" },
        { Type::FLOAT3x4, "mat3x4" }, { Type::FLOAT4x2, "mat4x2" }, { Type::FLOAT4x3, "mat4x3" },
        { Type::FLOAT4x4, "mat4x4" }, { Type::UINT, "uint" }, { Type::UINT2, "uvec2" },
        { Type::UINT3, "uvec3" }, { Type::UINT4, "uvec4" }, { Type::INT, "int" },
        { Type::INT2, "ivec2" }, { Type::INT3, "ivec3" }, { Type::INT4, "ivec4" },
        { Type::BOOL, "bool" }, { Type::BOOL2, "bvec2" }, { Type::BOOL3, "bvec3" },
        { Type::BOOL4, "bvec4" }, { Type::VOID, "void" },
    };

    //
    // Find the nearest opengl type to this one
    //
    Type base_type = type.GetBaseType();
    Type element_type = GetScalarType( base_type );

    if( IsFloatingPoint( element_type ) || IsMatrixType( base_type ) )
        element_type = Type::FLOAT;
    else if( IsSigned( element_type ) )
        element_type = Type::INT;
    else if( IsIntegral( element_type ) && !( element_type == Type::BOOL ) )
        element_type = Type::UINT;
    else
    {
        assert( base_type == Type::VOID && "Unexpected type" );
        element_type = Type::VOID;
    }

    Type new_base_type =
        IsMatrixType( base_type )
            ? GetMatrixType(
                  element_type, GetNumColumnsInType( base_type ), GetNumRowsInType( base_type ) )
            : IsVectorType( base_type )
            ? GetVectorType( element_type, GetNumElementsInType( base_type ) )
            : element_type == Type::VOID
            ? Type::VOID
            : element_type;

    std::string type_string = type_string_map.at( new_base_type );

    if( new_base_type != base_type )
        Warning( "GLSL doesn't have type: " + GetTypeString( base_type ) + ". Using " +
                 type_string );


    for( unsigned extent : type.GetArrayExtents() )
        type_string += "[ " + std::to_string( extent ) + "]";

    return type_string;
}

std::string GLSLWriter::GetVariableTypeString( const Variable& variable )
{
    std::string ret;
    if( variable.IsConst() )
        ret += "const ";
    if( variable.IsIn() )
        ret += "in ";
    if( variable.IsOut() )
        ret += "out ";

    ret += GetTypeString( variable.GetType() );
    return ret;
}


//
// Misc
//

void GLSLWriter::NewLine( unsigned n )
{
    while( n-- )
        m_Source << "\n";
}

std::string GLSLWriter::MangleIdentifier( std::string identifier, IdentifierType identifier_type )
{
    switch( identifier_type )
    {
    case IdentifierType::Function:
        return "f_" + identifier;
    case IdentifierType::Variable:
        return "v_" + identifier;
    }
}


void GLSLWriter::Error( const std::string& message )
{
    m_Context.Error( "Error generating shader: " + message );
}

void GLSLWriter::Warning( const std::string& message )
{
    m_Context.Error( "Warning generating shader: " + message );
}

//--------------------------------------------------------------------------------------------------
// Generating statements
//--------------------------------------------------------------------------------------------------

void GLSLWriter::GenerateStatement( const StatementNode& statement_node )
{
    switch( statement_node.GetNodeType() )
    {
    case NodeType::Sequence:
        GenerateCompoundStatement( statement_node );
        break;
    case NodeType::Return:
        if( statement_node.GetNumChildren() == 1 )
            GenerateReturn( cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        else
            GenerateVoidReturn();
        break;
    case NodeType::ExpressionStatement:
        GenerateExpressionStatement( cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        break;
    case NodeType::Conditional:
        GenerateConditional( cast<ExpressionNode>( statement_node.GetChild( 0 ) ),
                             cast<StatementNode>( statement_node.GetChild( 1 ) ),
                             statement_node.GetNumChildren() == 3
                                 ? &cast<StatementNode>( statement_node.GetChild( 2 ) )
                                 : nullptr );
        break;
    case NodeType::TemporaryAssignment:
        GenerateTemporaryAssignment(
            cast<TemporaryAssignmentNode>( statement_node ).GetTemporaryNumber(),
            cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        break;
    default:
        assert( false && "Trying to generate an unhandled statement type" );
    }
    NewLine();
}

void GLSLWriter::GenerateCompoundStatement( const StatementNode& sequence_node )
{
    assert( sequence_node.GetNodeType() == NodeType::Sequence );

    for( const Node& n : sequence_node.GetChildren() )
        GenerateStatement( cast<StatementNode>( n ) );
}

void GLSLWriter::GenerateExpressionStatement( const ExpressionNode& expression )
{
    m_Source << GenerateValue( expression ) << ";";
}

void GLSLWriter::GenerateConditional( const ExpressionNode& condition,
                                      const StatementNode& true_statement,
                                      const StatementNode* else_statement )
{
    assert( false && "complete me" );
}

void GLSLWriter::GenerateVoidReturn()
{
    m_Source << "return;";
}

void GLSLWriter::GenerateReturn( const ExpressionNode& returned )
{
    m_Source << "return " << GenerateValue( returned ) << ";";
}

void GLSLWriter::GenerateTemporaryAssignment( unsigned temporary_number,
                                              const ExpressionNode& expression )
{
    // Why can't these be const glsl?
    m_Source << GetTypeString( expression.GetType() ) << " "
             << GetTemporaryIdentifier( temporary_number ) << " = " << GenerateValue( expression )
             << ";";
}

//
// Value generation
//

std::string GLSLWriter::GenerateValue( const ExpressionNode& expression )
{
    if( isa<PointerExpressionNode>( expression ) )
        return GenerateAddress( cast<PointerExpressionNode>( expression ) );
    else
    {
        switch( expression.GetNodeType() )
        {
        case NodeType::Constant:
            return GenerateConstant( cast<ConstantNodeBase>( expression ) );
        case NodeType::Zero:
            return GenerateZero( cast<ZeroNode>( expression ).GetType() );
        case NodeType::Cast:
            return GenerateCast( cast<CastNode>( expression ) );
        case NodeType::Call:
            return GenerateCall( expression );
        case NodeType::ExtractElement:
            return GenerateExtractElement( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::InsertElement:
            return GenerateInsertElement( /* vec = */ expression.GetOperand( 0 ),
                                          /* element = */ expression.GetOperand( 1 ),
                                          /* index = */ expression.GetOperand( 2 ) );
        case NodeType::Swizzle:
            return GenerateSwizzle( expression.GetOperand( 0 ),
                                    cast<SwizzleNode>( expression ).GetSwizzle() );
        case NodeType::Select:
            return GenerateSelect( expression.GetOperand( 0 ),
                                   expression.GetOperand( 1 ),
                                   /* condition = */ expression.GetOperand( 2 ) );
        case NodeType::VectorConstructor:
            return GenerateVectorConstructor( expression );
        case NodeType::MatrixConstructor:
            return GenerateMatrixConstructor( expression );
        case NodeType::PostIncrement:
            return GeneratePostIncrement(
                cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
        case NodeType::PostDecrement:
            return GeneratePostDecrement(
                cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );

        //
        // Binary Operators
        //
        case NodeType::LogicalOr:
        case NodeType::BitwiseOr:
            return GenerateOr( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::LogicalAnd:
        case NodeType::BitwiseAnd:
            return GenerateAnd( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::BitwiseExclusiveOr:
            return GenerateExclusiveOr( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::CompareEqual:
            return GenerateCompareEqual( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::CompareNotEqual:
            return GenerateCompareNotEqual( expression.GetOperand( 0 ),
                                            expression.GetOperand( 1 ) );
        case NodeType::CompareLessThan:
            return GenerateCompareLessThan( expression.GetOperand( 0 ),
                                            expression.GetOperand( 1 ) );
        case NodeType::CompareGreaterThan:
            return GenerateCompareGreaterThan( expression.GetOperand( 0 ),
                                               expression.GetOperand( 1 ) );
        case NodeType::CompareLessThanEquals:
            return GenerateCompareLessThanEquals( expression.GetOperand( 0 ),
                                                  expression.GetOperand( 1 ) );
        case NodeType::CompareGreaterThanEquals:
            return GenerateCompareGreaterThanEquals( expression.GetOperand( 0 ),
                                                     expression.GetOperand( 1 ) );
        case NodeType::LeftShift:
            return GenerateLeftShift( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::RightShift:
            return GenerateRightShift( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::Add:
            return GenerateAdd( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::Subtract:
            return GenerateSubtract( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::Multiply:
            return GenerateMultiply( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::Divide:
            return GenerateDivide( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        case NodeType::Modulo:
            return GenerateModulo( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );

        //
        // Unary operators
        //
        case NodeType::Negate:
            return GenerateNegate( expression.GetOperand( 0 ) );
        case NodeType::BitwiseNot:
            return GenerateBitwiseNot( expression.GetOperand( 0 ) );
        case NodeType::LogicalNot:
            return GenerateLogicalNot( expression.GetOperand( 0 ) );
        case NodeType::Temporary:
            return GenerateTemporaryRead( cast<TemporaryNode>( expression ).GetTemporaryNumber() );

        default:
            assert( false && "Trying to generate an unhandled expression type" );
            std::abort();
        }
    }
}

std::string GLSLWriter::GenerateAddress( const PointerExpressionNode& expression )
{
    //
    // All of these needs to return an expression which can be evaluated without side effects
    //

    switch( expression.GetNodeType() )
    {
    case NodeType::Store:
        return GenerateStore( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                              expression.GetOperand( 1 ) );
    case NodeType::SwizzleStore:
        return GenerateSwizzleStore( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                                     expression.GetOperand( 1 ),
                                     cast<SwizzleStoreNode>( expression ).GetSwizzle() );
    case NodeType::VariableIdentifier:
        return GenerateVariableAddress( cast<VariableNode>( expression ) );
    case NodeType::ArrayIndex:
        return GenerateArrayIndex( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                                   expression.GetOperand( 1 ) );
    case NodeType::PreIncrement:
        return GeneratePreIncrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
    case NodeType::PreDecrement:
        return GeneratePreDecrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
    case NodeType::GLSLBuiltinVariable:
        return std::string( cast<GLSLBuiltinNode>( expression ).GetIdentifier() );
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        std::abort();
    }
}

std::string GLSLWriter::GetTemporaryIdentifier( unsigned temporary_number )
{
    return "t_" + std::to_string( temporary_number );
}

//
// Addresses
//

std::string GLSLWriter::GenerateStore( const PointerExpressionNode& address,
                                       const ExpressionNode& assigned )
{
    return GenerateAddress( address ) + " = " + GenerateValue( assigned );
}

std::string GLSLWriter::GenerateSwizzleStore( const PointerExpressionNode& address,
                                              const ExpressionNode& assigned,
                                              const Swizzle& swizzle )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateVariableAddress( const VariableNode& variable_node )
{
    return m_VariableNames.at( &variable_node.GetVariable() );
}

std::string GLSLWriter::GenerateArrayIndex( const PointerExpressionNode& address,
                                            const ExpressionNode& index )
{
    assert( false && "complete me" );
    std::abort();
}

std::string GLSLWriter::GeneratePreIncrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

std::string GLSLWriter::GeneratePreDecrement( const PointerExpressionNode& address )
{
    assert( false && "Complete me" );
    std::abort();
}

//
// Values
//

std::string GLSLWriter::GenerateTemporaryRead( unsigned temporary_number )
{
    return "t_" + std::to_string( temporary_number );
}

std::string GLSLWriter::GenerateCast( const CastNode& expression )
{
    return GetTypeString( expression.GetType() ) + "(" +
           GenerateValue( expression.GetOperand( 0 ) ) + ")";
}

std::string GLSLWriter::GenerateConstant( const ConstantNodeBase& expression )
{
    //
    // todo, change this to use glsl types
    //

    Type joelang_type = expression.GetType();
    switch( joelang_type )
    {
    case Type::BOOL:
        return std::to_string( static_cast<const ConstantNode<jl_bool>&>( expression )
                                   .GetConstant() );
    case Type::CHAR:
        return std::to_string( static_cast<const ConstantNode<jl_char>&>( expression )
                                   .GetConstant() );
    case Type::SHORT:
        return std::to_string( static_cast<const ConstantNode<jl_short>&>( expression )
                                   .GetConstant() );
    case Type::INT:
        return std::to_string( static_cast<const ConstantNode<jl_int>&>( expression )
                                   .GetConstant() );
    case Type::LONG:
        return std::to_string( static_cast<const ConstantNode<jl_long>&>( expression )
                                   .GetConstant() );
    case Type::UCHAR:
        return std::to_string( static_cast<const ConstantNode<jl_uchar>&>( expression )
                                   .GetConstant() );
    case Type::USHORT:
        return std::to_string( static_cast<const ConstantNode<jl_ushort>&>( expression )
                                   .GetConstant() );
    case Type::UINT:
        return std::to_string( static_cast<const ConstantNode<jl_uint>&>( expression )
                                   .GetConstant() );
    case Type::ULONG:
        return std::to_string( static_cast<const ConstantNode<jl_ulong>&>( expression )
                                   .GetConstant() );
    case Type::FLOAT:
        return std::to_string( static_cast<const ConstantNode<jl_float>&>( expression )
                                   .GetConstant() );
    case Type::DOUBLE:
        return std::to_string( static_cast<const ConstantNode<jl_double>&>( expression )
                                   .GetConstant() );
    default:
        assert( false && "Trying to generate a constant of unhandled type" );
    }
}

std::string GLSLWriter::GenerateZero( Type type )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCall( const ExpressionNode& expression )
{
    const Function& called =
        cast<FunctionNode>( expression.GetChild( expression.GetNumChildren() - 1 ) ).GetFunction();

    std::string ret = MangleIdentifier( called.GetIdentifier(), IdentifierType::Function );
    ret += "(";
    for( unsigned i = 0; i < expression.GetNumChildren() - 1; ++i )
    {
        if( i != 0 )
            ret += ", ";
        ret += GenerateValue( expression.GetOperand( i ) );
    }

    ret += ")";
    return ret;
}

std::string GLSLWriter::GenerateExtractElement( const ExpressionNode& vector,
                                                const ExpressionNode& index )
{
    //
    // Extract things with index notation
    //
    return GenerateValue( vector ) + "[" + GenerateValue( index ) + "]";
}

std::string GLSLWriter::GenerateInsertElement( const ExpressionNode& vector,
                                               const ExpressionNode& element,
                                               const ExpressionNode& index )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateSwizzle( const ExpressionNode& expression, const Swizzle& swizzle )
{
    return GenerateValue( expression ) + "." + swizzle.GetString();
}

std::string GLSLWriter::GenerateSelect( const ExpressionNode& true_expression,
                                        const ExpressionNode& false_expression,
                                        const ExpressionNode& condition )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateVectorConstructor( const ExpressionNode& constructor )
{
    const CompleteType& type = constructor.GetType();

    std::string ret = GetTypeString( type );

    ret += "(";
    for( unsigned i = 0; i < type.GetNumElements(); ++i )
    {
        if( i != 0 )
            ret += ", ";
        ret += GenerateValue( constructor.GetOperand( i ) );
    }
    ret += ")";
    return ret;
}

std::string GLSLWriter::GenerateMatrixConstructor( const ExpressionNode& constructor )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GeneratePostIncrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

std::string GLSLWriter::GeneratePostDecrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

//
// Binary Operators
//

std::string GLSLWriter::GenerateOr( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateAnd( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateExclusiveOr( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareEqual( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareNotEqual( const ExpressionNode& lhs,
                                                 const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareLessThan( const ExpressionNode& lhs,
                                                 const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareGreaterThan( const ExpressionNode& lhs,
                                                    const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                                       const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                          const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateLeftShift( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateRightShift( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateAdd( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateSubtract( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateMultiply( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateDivide( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return "(" + GenerateValue( lhs ) + " / " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateModulo( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateNegate( const ExpressionNode& expression )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateBitwiseNot( const ExpressionNode& expression )
{
    assert( false && "Complete me" );
    std::abort();
}
std::string GLSLWriter::GenerateLogicalNot( const ExpressionNode& expression )
{
    assert( false && "Complete me" );
    std::abort();
}

} // namespace Compiler
} // namespace JoeLang
