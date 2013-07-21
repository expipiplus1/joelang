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
#include <compiler/writers/runtime.hpp>
#include <compiler/semantic_analysis/semantic.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/writers/shader_compilation_context.hpp>
#include <joelang/shader.hpp>
#include <joelang/types.hpp>

using string_ref = std::reference_wrapper<const std::string>;

namespace JoeLang
{
namespace Compiler
{

enum class GLSLWriter::IdentifierType{ Function, Variable, };

// todo, remove this
const std::string GLSLWriter::s_GLSLVersion = "150";

std::string GLSLWriter::GenerateGLSL( const ShaderCompilationContext& compilation_context,
                                      const CompileStatementNode& compile_statement )
{
    GLSLWriter writer( compilation_context, compile_statement );
    return writer.Generate();
}

GLSLWriter::GLSLWriter( const ShaderCompilationContext& compilation_context,
                        const CompileStatementNode& compile_statement )
    : m_CompilationContext( compilation_context )
    , m_CompileStatement( compile_statement )
{
}

GLSLWriter::~GLSLWriter()
{
}

std::string GLSLWriter::Generate()
{
    WriteHeaderComment();

    WriteVersion();

    WriteRequiredExtensions();

    NewLine( 2 );

    const Function& entry_function = m_CompileStatement.GetEntryFunction();

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
    // Inputs and uniforms are immutable in glsl so if we write to them we need to create a global
    // copy and initialize that at the start of main.
    // TODO, don't do this for variables which are never written to
    //
    std::set<const Variable*> input_variables;
    std::set<const Variable*> output_variables;
    std::set<const Variable*> uniform_variables;
    std::set<const Variable*> global_variables;
    for( const Function* function : function_dependencies )
    {
        const Node& function_node = function->GetCodeDag();
        const std::set<const Node*> variable_nodes =
            function_node.GetDescendantsWithNodeType( NodeType::VariableIdentifier );
        for( const Node* variable_node : variable_nodes )
        {
            const Variable& v = cast<VariableNode>( *variable_node ).GetVariable();
            if( v.IsGlobal() )//|| ( function == &entry_function && v.IsParameter() ) )
            {
                // TODO, allow this, as long as it's only used as an input in one shader and an
                // output in another earlier in the pipeline
                assert( !(v.IsIn() && v.IsOut()) && "Global Variable is both in and out" );

                // TODO: Insert some checking code to make sure we don't have 'in uniforms' or
                // 'in out' variables in global scope

                const Semantic& semantic = v.GetSemantic();
                if( semantic.HasBuiltin( m_CompileStatement.GetDomain(), v.IsIn() ) )
                    m_VariableNames.insert(
                        std::make_pair( &v, semantic.GetBuiltin( m_CompileStatement.GetDomain(), v.IsIn() ) ) );
                else if( v.IsIn() )
                    input_variables.insert( &v );
                else if( v.IsOut() )
                    output_variables.insert( &v );
                else if( v.IsUniform() )
                    uniform_variables.insert( &v );
                else
                    global_variables.insert( &v );
            }
        }
    }

    //
    // Declare all these variables
    //

    if( !global_variables.empty() )
    {
        m_Source << "// Global variables";
        NewLine();

        WriteGlobalVariables( global_variables );
        NewLine();
    }

    if( !input_variables.empty() )
    {
        m_Source << "// Input variables";
        NewLine();

        WriteInputVariables( input_variables );
        NewLine();
    }

    if( !output_variables.empty() )
    {
        m_Source << "// Output variables";
        NewLine();

        WriteOutputVariables( output_variables );
        NewLine();
    }

    //
    // Write the main return value
    //
    const Semantic& entry_semantic = entry_function.GetSemantic();
    if( !entry_semantic.IsVoid() )
    {
        if( entry_semantic.HasBuiltin( m_CompileStatement.GetDomain(), /* input = */ false ) )
            m_EntryReturnValue = entry_semantic.GetBuiltin( m_CompileStatement.GetDomain(), /* input = */ false );
        else
        {
            // Write an output declaration
            m_Source << "// Entry function return value";
            NewLine();

            m_Source << "out " << GetTypeString( entry_function.GetReturnType() ) << " r_EntryReturnValue;";
            NewLine();
            m_EntryReturnValue = "r_EntryReturnValue";
        }
    }

    if( !uniform_variables.empty() )
    {
        m_Source << "// Uniform variables";
        NewLine();

        WriteUniformVariables( uniform_variables );
        NewLine();
    }

    //
    // Generate all the functions this shader needs
    //

    InitFunctionVariableNames( function_dependencies );

    m_Source << "// Function declarations";
    NewLine();
    WriteFunctionDeclarations( function_dependencies );
    NewLine();

    m_Source << "// Function definitions";
    NewLine();
    WriteFunctionDefinitions( function_dependencies );
    NewLine();


    //
    // Generate the wrapper, calling the JoeLang main function
    //
    WriteMainFunction( m_CompileStatement );

    return m_Source.str();
}

void GLSLWriter::WriteHeaderComment()
{
    m_Source << "//";
    NewLine();
    m_Source << "// " << GetDomainString( m_CompileStatement.GetDomain() ) << " shader generated by JoeLang";
    NewLine();
    m_Source << "//";
    NewLine();
}

void GLSLWriter::WriteVersion()
{
    m_Source << "#version 150";
    NewLine();

}


void GLSLWriter::WriteRequiredExtensions()
{
    m_Source << "#extension GL_ARB_explicit_attrib_location : require";
    NewLine();
}

std::string GLSLWriter::GetDomainString( ShaderDomain domain )
{
    switch( domain )
    {
    case ShaderDomain::FRAGMENT:
        return "fragment";
    case ShaderDomain::VERTEX:
        return "vertex";
    default:
        return "unknown";
    }
}

//
// Writing Variables
//
void GLSLWriter::WriteGlobalVariables( std::set<const Variable*> global_variables )
{
    for( const Variable* variable : global_variables )
    {
        assert( variable && "WriteGlobalVariables given null variable" );
        assert( variable->IsGlobal() && "WriteGlobalVariables given non-global variable" );
        assert( !variable->IsIn() && "WriteGlobalVariables given an in variable" );
        assert( !variable->IsOut() && "WriteGlobalVariables given an out variable" );
        assert( !variable->IsUniform() && "WriteGlobalVariables given a uniform variable" );

        std::string name = "g_" + variable->GetName();
        m_VariableNames.insert( std::make_pair( variable, name ) );
        m_Source << GetVariableTypeString( *variable ) << " " << name;
        const GenericValue& initializer = variable->GetInitializer();
        if( variable->GetInitializer().GetUnderlyingType() != Type::UNKNOWN )
            m_Source << " = " << GenerateGenericValue( initializer );
        m_Source << ";";
        NewLine();
    }
}

void GLSLWriter::WriteInputVariables( std::set<const Variable*> input_variables )
{
    for( const Variable* variable : input_variables )
    {
        assert( variable && "WriteInputVariables given null variable" );
        assert( variable->IsIn() && "WriteInputVariables given non-in variable" );

        unsigned index = GetVariableAttributeIndex( *variable );
        std::string input_name =
            m_CompilationContext.GetInputPrefix( m_CompileStatement.GetDomain() ) +
            std::to_string( index );
        std::string mutable_name = "v_" + variable->GetName();

        // Todo, make the mutable name optional if the variable is never written to
        m_VariableNames.insert( std::make_pair( variable, mutable_name ) );

        // Todo, don't hardcode this
        std::string layout_location_string = m_CompileStatement.GetDomain() == ShaderDomain::VERTEX ?
        "layout( location = " + std::to_string( index ) + " )" :
        "/* layout( location = " + std::to_string( index ) + " ) */";

        m_Source << layout_location_string << " " <<  "in " << GetTypeString( variable->GetType() ) << " " << input_name << ";";
        NewLine();
        m_Source << GetTypeString( variable->GetType() ) << " " << mutable_name << " = " << input_name << ";";
        NewLine();
    }
}

void GLSLWriter::WriteOutputVariables( std::set<const Variable*> output_variables )
{
    for( const Variable* variable : output_variables )
    {
        assert( variable && "WriteOutputVariables given null variable" );
        assert( variable->IsOut() && "WriteOutputVariables given non-out variable" );

        // todo use some kind of link_context for this
        unsigned index = GetVariableAttributeIndex( *variable );
        std::string output_name =
            m_CompilationContext.GetOutputPrefix( m_CompileStatement.GetDomain() ) +
            std::to_string( index );
        m_VariableNames.insert( std::make_pair( variable, output_name ) );

        // Todo, don't hardcode this
        std::string layout_location_string = m_CompileStatement.GetDomain() == ShaderDomain::FRAGMENT ?
        "layout( location = " + std::to_string( index ) + " )" :
        "/* layout( location = " + std::to_string( index ) + " ) */";

        m_Source << layout_location_string << " " << GetVariableTypeString( *variable ) << " " << output_name << ";";
        NewLine();
    }
}

void GLSLWriter::WriteUniformVariables( std::set<const Variable*> uniform_variables )
{
    for( const Variable* variable : uniform_variables )
    {
        assert( variable && "WriteUniformVariables given null variable" );
        assert( variable->IsUniform() && "WriteUniformVariables given non-uniform variable" );

        std::string uniform_name = "u_" + variable->GetName();
        std::string mutable_name = "v_" + uniform_name;

        // Todo, make the mutable name optional if the variable is never written to
        m_VariableNames.insert( std::make_pair( variable, mutable_name ) );
        m_Source << "uniform " << GetTypeString( variable->GetType() ) << " " << uniform_name << ";";
        NewLine();
        m_Source << GetTypeString( variable->GetType() ) << " " << mutable_name << " = " << uniform_name << ";";
        NewLine();
    }

    // TODO, add test checking for error on 'uniform in' etc...
}

unsigned GLSLWriter::GetVariableAttributeIndex( const Variable& variable )
{
    // todo, complete me, for varyings without explicit attributes etc...
    assert( variable.GetSemantic().GetSemanticType() == SemanticType::ATTR &&
            "Trying to get the index of a semantic without one" );
    return variable.GetSemantic().GetIndex();
}

//
// Writing functions
//

void GLSLWriter::InitFunctionVariableNames( std::set<const Function*> functions )
{
    for( const Function* function : functions )
        for( const auto& parameter : function->GetParameters() )
            m_VariableNames.insert( std::make_pair( parameter.get(), "p_" + parameter->GetName() ) );
    // TODO locals
}

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
        m_Indentation += 1;
        NewLine();
        WriteCompoundStatement( function->GetCodeDag() );
        m_Indentation -= 1;
        NewLine();
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

        m_Source << GetVariableTypeString( *parameter ) << " " << m_VariableNames.at( parameter.get() );
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
        const PointerExpressionNode& assigned = m_NodeManager.MakeGLSLBuiltinNode( m_EntryReturnValue );
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
    m_Indentation += 1;
    NewLine();
    WriteCompoundStatement( main_sequence );
    m_Indentation -= 1;
    NewLine();
    m_Source << "}";
    NewLine();
}

//
// Types
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
    else if( IsIntegral( element_type ) && element_type != Type::BOOL )
        element_type = Type::UINT;
    else if( element_type != Type::BOOL )
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
        Warning( "GLSL doesn't have type: " + Compiler::GetTypeString( base_type ) + ". Using " +
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
    assert( n != 0 && "Inserting zero newlines" );
    while( n-- )
        m_Source << "\n";
    for( unsigned i = 0; i < m_Indentation; ++i )
        m_Source << "    ";
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


void GLSLWriter::Error( const std::string& message ) const
{
    m_CompilationContext.Error( "Error generating shader: " + message );
}

void GLSLWriter::Warning( const std::string& message ) const
{
    m_CompilationContext.Error( "Warning generating shader: " + message );
}

//--------------------------------------------------------------------------------------------------
// Writing statements
//--------------------------------------------------------------------------------------------------

void GLSLWriter::WriteStatement( const StatementNode& statement_node )
{
    switch( statement_node.GetNodeType() )
    {
    case NodeType::Sequence:
        WriteCompoundStatement( statement_node );
        break;
    case NodeType::Return:
        if( statement_node.GetNumChildren() == 1 )
            WriteReturn( cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        else
            WriteVoidReturn();
        break;
    case NodeType::ExpressionStatement:
        WriteExpressionStatement( cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        break;
    case NodeType::Conditional:
        WriteConditional( cast<ExpressionNode>( statement_node.GetChild( 0 ) ),
                             cast<StatementNode>( statement_node.GetChild( 1 ) ),
                             statement_node.GetNumChildren() == 3
                                 ? &cast<StatementNode>( statement_node.GetChild( 2 ) )
                                 : nullptr );
        break;
    case NodeType::TemporaryAssignment:
        WriteTemporaryAssignment(
            cast<TemporaryAssignmentNode>( statement_node ).GetTemporaryNumber(),
            cast<ExpressionNode>( statement_node.GetChild( 0 ) ) );
        break;
    default:
        assert( false && "Trying to generate an unhandled statement type" );
    }
    NewLine();
}

void GLSLWriter::WriteCompoundStatement( const StatementNode& sequence_node )
{
    assert( sequence_node.GetNodeType() == NodeType::Sequence );

    for( const Node& n : sequence_node.GetChildren() )
        WriteStatement( cast<StatementNode>( n ) );
}

void GLSLWriter::WriteExpressionStatement( const ExpressionNode& expression )
{
    m_Source << GenerateValue( expression ) << ";";
}

void GLSLWriter::WriteConditional( const ExpressionNode& condition,
                                      const StatementNode& true_statement,
                                      const StatementNode* else_statement )
{
    m_Source << "if(" << GenerateValue( condition ) << ")";
    NewLine();
    m_Source << "{";
    m_Indentation += 1;
    NewLine();
    WriteStatement( true_statement );
    m_Indentation -= 1;
    NewLine();
    m_Source << "}";
    NewLine();
    if( else_statement )
    {
        m_Source << "else";
        NewLine();
        m_Source << "{";
        m_Indentation += 1;
        NewLine();
        WriteStatement( *else_statement );
        m_Indentation -= 1;
        NewLine();
        m_Source << "}";
        NewLine();
    }
}

void GLSLWriter::WriteVoidReturn()
{
    m_Source << "return;";
}

void GLSLWriter::WriteReturn( const ExpressionNode& returned )
{
    m_Source << "return " << GenerateValue( returned ) << ";";
}

void GLSLWriter::WriteTemporaryAssignment( unsigned temporary_number,
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
    case NodeType::Temporary:
        return GenerateTemporaryRead( cast<TemporaryNode>( expression ).GetTemporaryNumber() );
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        std::abort();
    }
}

std::string GLSLWriter::GetTemporaryIdentifier( unsigned temporary_number )
{
    return "t_" + std::to_string( temporary_number );
}

template<typename T>
std::string GLSLWriter::GenerateLiteral( T t )
{
    //std::string suffix = GetGLSLTypeSuffix( JoeLangType<T>::value );
    return GetTypeString( JoeLangType<T>::value ) + "(" + std::to_string( t ) + ")"; // + suffix
}

template<>
std::string GLSLWriter::GenerateLiteral( bool b )
{
    //std::string suffix = GetGLSLTypeSuffix( JoeLangType<T>::value );
    return b ? "true" : "false";
}

template<typename Scalar, JoeMath::u32 Rows, JoeMath::u32 Columns>
std::string GLSLWriter::GenerateLiteral( const JoeMath::Matrix<Scalar, Rows, Columns>& m )
{
    std::string ret = GetTypeString( JoeLangType<JoeMath::Matrix<Scalar, Rows, Columns>>::value );
    ret += "(";

    bool first = true;
    for( unsigned i = 0; i < Columns; ++i )
        for( unsigned j = 0; j < Rows; ++j )
        {
            if( !first )
                ret += ", ";
            else
                first = false;

            ret += GenerateLiteral( m.GetColumn(i)[j] );
        }
    ret += ")";
    return ret;
}

std::string GLSLWriter::GenerateGenericValue( const JoeLang::Compiler::GenericValue& value )
{
    #define WRITE( Type, TYPE ) \
    case TYPE: \
        return GenerateLiteral( value.Get##Type() );

    #define WRITE_N( Type, TYPE ) \
    WRITE( Type, TYPE ); \
    WRITE( Type##2, TYPE##2 ); \
    WRITE( Type##3, TYPE##3 ); \
    WRITE( Type##4, TYPE##4 ); \
    WRITE( Type##2x2, TYPE##2x2 ); \
    WRITE( Type##2x3, TYPE##2x3 ); \
    WRITE( Type##2x4, TYPE##2x4 ); \
    WRITE( Type##3x2, TYPE##3x2 ); \
    WRITE( Type##3x3, TYPE##3x3 ); \
    WRITE( Type##3x4, TYPE##3x4 ); \
    WRITE( Type##4x2, TYPE##4x2 ); \
    WRITE( Type##4x3, TYPE##4x3 ); \
    WRITE( Type##4x4, TYPE##4x4 );

    switch( value.GetUnderlyingType() )
    {
        WRITE_N( Bool,   Type::BOOL )
        WRITE_N( Char,   Type::CHAR )
        WRITE_N( Short,  Type::SHORT )
        WRITE_N( Int,    Type::INT )
        WRITE_N( Long,   Type::LONG )
        WRITE_N( UChar,  Type::UCHAR )
        WRITE_N( UShort, Type::USHORT )
        WRITE_N( UInt,   Type::UINT )
        WRITE_N( ULong,  Type::ULONG )
        WRITE_N( Float,  Type::FLOAT )
        WRITE_N( Double, Type::DOUBLE )

    case Type::ARRAY:
    /*{
        shader_writer << GetGLSLTypeString(GetUnderlyingType()) << "[]" << "(";
        bool first = true;
        for( const GenericValue& g : m_ArrayValue )
        {
            if( !first )
                shader_writer << ", ";
            else
                first = false;
            shader_writer << g;
        }
        shader_writer << ")";
        break;
    }*/
    case Type::STRING:
    default:
        assert( false && "Trying to write an unhandled type" );
        std::abort();
    }
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
    return GenerateAddress( address ) + "." + swizzle.GetString() + " = " + GenerateValue( assigned );
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
        return GenerateLiteral( static_cast<const ConstantNode<jl_bool>&>( expression )
                                   .GetConstant() );
    case Type::CHAR:
        return GenerateLiteral( static_cast<const ConstantNode<jl_char>&>( expression )
                                   .GetConstant() );
    case Type::SHORT:
        return GenerateLiteral( static_cast<const ConstantNode<jl_short>&>( expression )
                                   .GetConstant() );
    case Type::INT:
        return GenerateLiteral( static_cast<const ConstantNode<jl_int>&>( expression )
                                   .GetConstant() );
    case Type::LONG:
        return GenerateLiteral( static_cast<const ConstantNode<jl_long>&>( expression )
                                   .GetConstant() );
    case Type::UCHAR:
        return GenerateLiteral( static_cast<const ConstantNode<jl_uchar>&>( expression )
                                   .GetConstant() );
    case Type::USHORT:
        return GenerateLiteral( static_cast<const ConstantNode<jl_ushort>&>( expression )
                                   .GetConstant() );
    case Type::UINT:
        return GenerateLiteral( static_cast<const ConstantNode<jl_uint>&>( expression )
                                   .GetConstant() );
    case Type::ULONG:
        return GenerateLiteral( static_cast<const ConstantNode<jl_ulong>&>( expression )
                                   .GetConstant() );
    case Type::FLOAT:
        return GenerateLiteral( static_cast<const ConstantNode<jl_float>&>( expression )
                                   .GetConstant() );
    case Type::DOUBLE:
        return GenerateLiteral( static_cast<const ConstantNode<jl_double>&>( expression )
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

    if( called.GetRuntimeFunction() != RuntimeFunction::NONE )
        return GenerateRuntimeCall( expression );

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

std::string GLSLWriter::GenerateRuntimeCall( const ExpressionNode& expression )
{
    const Function& called =
        cast<FunctionNode>( expression.GetChild( expression.GetNumChildren() - 1 ) ).GetFunction();

    RuntimeFunction runtime_function = called.GetRuntimeFunction();
    assert( runtime_function != RuntimeFunction::NONE && "Trying to generate a runtime call to a non-runtime function" );

    std::string glsl_func;

    switch( runtime_function )
    {
    case RuntimeFunction::FLOAT2x2_FLOAT2_MUL:
    case RuntimeFunction::FLOAT2x2_FLOAT2x2_MUL:
    case RuntimeFunction::FLOAT2x2_FLOAT3x2_MUL:
    case RuntimeFunction::FLOAT2x2_FLOAT4x2_MUL:
    case RuntimeFunction::FLOAT2x3_FLOAT2_MUL:
    case RuntimeFunction::FLOAT2x3_FLOAT2x2_MUL:
    case RuntimeFunction::FLOAT2x3_FLOAT3x2_MUL:
    case RuntimeFunction::FLOAT2x3_FLOAT4x2_MUL:
    case RuntimeFunction::FLOAT2x4_FLOAT2_MUL:
    case RuntimeFunction::FLOAT2x4_FLOAT2x2_MUL:
    case RuntimeFunction::FLOAT2x4_FLOAT3x2_MUL:
    case RuntimeFunction::FLOAT2x4_FLOAT4x2_MUL:
    case RuntimeFunction::FLOAT3x2_FLOAT3_MUL:
    case RuntimeFunction::FLOAT3x2_FLOAT2x3_MUL:
    case RuntimeFunction::FLOAT3x2_FLOAT3x3_MUL:
    case RuntimeFunction::FLOAT3x2_FLOAT4x3_MUL:
    case RuntimeFunction::FLOAT3x3_FLOAT3_MUL:
    case RuntimeFunction::FLOAT3x3_FLOAT2x3_MUL:
    case RuntimeFunction::FLOAT3x3_FLOAT3x3_MUL:
    case RuntimeFunction::FLOAT3x3_FLOAT4x3_MUL:
    case RuntimeFunction::FLOAT3x4_FLOAT3_MUL:
    case RuntimeFunction::FLOAT3x4_FLOAT2x3_MUL:
    case RuntimeFunction::FLOAT3x4_FLOAT3x3_MUL:
    case RuntimeFunction::FLOAT3x4_FLOAT4x3_MUL:
    case RuntimeFunction::FLOAT4x2_FLOAT4_MUL:
    case RuntimeFunction::FLOAT4x2_FLOAT2x4_MUL:
    case RuntimeFunction::FLOAT4x2_FLOAT3x4_MUL:
    case RuntimeFunction::FLOAT4x2_FLOAT4x4_MUL:
    case RuntimeFunction::FLOAT4x3_FLOAT4_MUL:
    case RuntimeFunction::FLOAT4x3_FLOAT2x4_MUL:
    case RuntimeFunction::FLOAT4x3_FLOAT3x4_MUL:
    case RuntimeFunction::FLOAT4x3_FLOAT4x4_MUL:
    case RuntimeFunction::FLOAT4x4_FLOAT4_MUL:
    case RuntimeFunction::FLOAT4x4_FLOAT2x4_MUL:
    case RuntimeFunction::FLOAT4x4_FLOAT3x4_MUL:
    case RuntimeFunction::FLOAT4x4_FLOAT4x4_MUL:
    case RuntimeFunction::FLOAT2_FLOAT2x2_MUL:
    case RuntimeFunction::FLOAT2_FLOAT3x2_MUL:
    case RuntimeFunction::FLOAT2_FLOAT4x2_MUL:
    case RuntimeFunction::FLOAT3_FLOAT2x3_MUL:
    case RuntimeFunction::FLOAT3_FLOAT3x3_MUL:
    case RuntimeFunction::FLOAT3_FLOAT4x3_MUL:
    case RuntimeFunction::FLOAT4_FLOAT2x4_MUL:
    case RuntimeFunction::FLOAT4_FLOAT3x4_MUL:
    case RuntimeFunction::FLOAT4_FLOAT4x4_MUL:
        // Todo, check matrix ordering
        return "(" + GenerateValue( expression.GetOperand( 0 ) ) + " * " + GenerateValue( expression.GetOperand( 1 ) ) + ")";

    case RuntimeFunction::FLOAT_DOT:
    case RuntimeFunction::FLOAT2_DOT:
    case RuntimeFunction::FLOAT3_DOT:
    case RuntimeFunction::FLOAT4_DOT:
        glsl_func = "dot";
        break;

    case RuntimeFunction::FLOAT_NORMALIZE:
    case RuntimeFunction::FLOAT2_NORMALIZE:
    case RuntimeFunction::FLOAT3_NORMALIZE:
    case RuntimeFunction::FLOAT4_NORMALIZE:
        glsl_func = "normalize";
        break;

    default:
        assert( false && "Trying to generate an unhandled runtime function" );
    }

    if( !glsl_func.empty() )
    {
        glsl_func += "(";
        for( unsigned i = 0; i < expression.GetNumChildren() - 1; ++i )
        {
            if( i != 0 )
                glsl_func += ", ";
            glsl_func += GenerateValue( expression.GetOperand( i ) );
        }

        glsl_func += ")";
        return glsl_func;
    }

    assert( false && "Trying to generate an unhandled runtime function" );
    std::abort();
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
    // GLSL only evaluates one side of the ternary operator, the joelang ternary operator evaluates both sides

    if( condition.GetType().GetType() == Type::BOOL )
    {
        // Todo, implement dag pass to move these into temporaries which will always be evaluated
        return "(" + GenerateValue( condition ) + " ? " + GenerateValue( true_expression ) + " : " + GenerateValue( false_expression ) + ")";
    }
    else
    {
        return "mix(" + GenerateValue( false_expression ) + ", " + GenerateValue( true_expression ) + ", " + GenerateValue( condition ) + ")";
    }
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
    return "(" + GenerateValue( lhs ) + " & " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateExclusiveOr( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return "(" + GenerateValue( lhs ) + " ^ " + GenerateValue( rhs ) + ")";
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
    return "(" + GenerateValue(lhs) + " < " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateCompareGreaterThan( const ExpressionNode& lhs,
                                                    const ExpressionNode& rhs )
{
    return "(" + GenerateValue(lhs) + " > " + GenerateValue( rhs ) + ")";
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
    // Todo, look at this, not all glsl supports this
    return "(" + GenerateValue( lhs ) + " << " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateRightShift( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    assert( false && "Complete me" );
    std::abort();
}

std::string GLSLWriter::GenerateAdd( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return "(" + GenerateValue( lhs ) + " + " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateSubtract( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return "(" + GenerateValue( lhs ) + " - " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateMultiply( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    // Todo, look into this regarding matrices ( * can be a matrix multiplication in glsl )
    return "(" + GenerateValue( lhs ) + " * " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateDivide( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return "(" + GenerateValue( lhs ) + " / " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateModulo( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    // Todo, check glsl support for this
    return "(" + GenerateValue( lhs ) + " % " + GenerateValue( rhs ) + ")";
}

std::string GLSLWriter::GenerateNegate( const ExpressionNode& expression )
{
    return "(-" + GenerateValue( expression ) + ")";
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
