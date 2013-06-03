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

#include "shader_writer.hpp"

#include <algorithm>
#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stack>
#include <string>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/entry_function.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <compiler/writers/runtime.hpp>
#include <compiler/writers/semantic_info.hpp>
#include <joelang/context.hpp>
#include <joelang/shader.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

const std::string ShaderWriter::s_GLSLVersion = " 150";

ShaderWriter::ShaderWriter( const Context& context )
    :m_Context( context )
{
    m_Shader.setf( std::ios_base::scientific );
    m_Shader.setf( std::ios_base::boolalpha );
    m_Shader.setf( std::ios_base::showpoint );
}

std::string ShaderWriter::GenerateGLSL( const EntryFunction& entry_function )
{
    GenerateShader( entry_function );
        
    std::string ret = m_Shader.str();

    // reset things
    m_Shader.str("");
    m_WrittenToVariables.clear();

    assert( m_Indentation == 0 && "Unmatched indentations" );

    // return an empty string if we've had an error
    if( m_Good )
        return ret;
    else
        return "";
}

void ShaderWriter::PushIndentation()
{
    ++m_Indentation;
}

void ShaderWriter::PopIndentation()
{
    assert( m_Indentation > 0 && "Trying to pop 0 indentation" );
    --m_Indentation;
}

std::string ShaderWriter::Mangle( const std::string& identifier,
                                  IdentifierType identifier_type )
{
    const static std::map<IdentifierType, std::string> prefix_map
    {
        { IdentifierType::VARIABLE,    "_"  },
        { IdentifierType::IN_VARYING,  "i_"  },
        { IdentifierType::OUT_VARYING, "o_"  },
        { IdentifierType::UNIFORM,     "u_"  },
        { IdentifierType::FUNCTION,    "f_" }
    };
    return prefix_map.at(identifier_type) + identifier;
}

std::string ShaderWriter::MangleVariable( const Variable& v )
{
    return Mangle( v.GetName(),
        v.IsVarying() ?
            ( v.IsIn() ? IdentifierType::IN_VARYING :
                         IdentifierType::OUT_VARYING ) :
        v.IsUniform() ? IdentifierType::UNIFORM :
        IdentifierType::VARIABLE );
}

void ShaderWriter::WriteVariableName( const Variable_sp v )
{
    //
    // If we don't write to this variable and it's global then use the u_ prefix
    //
    if( v->IsGlobal() && v->IsUniform() &&
        m_WrittenToVariables.find(v) == m_WrittenToVariables.end() )
        *this << Mangle( v->GetName(), IdentifierType::UNIFORM );
    else
        *this << Mangle( v->GetName(), IdentifierType::VARIABLE );
}

ShaderWriter& ShaderWriter::operator << ( const CompleteType& value )
{
    value.Write( *this );
    return *this;
}

ShaderWriter& ShaderWriter::operator << ( const Expression& value )
{
    value.Write( *this );
    return *this;
}

ShaderWriter& ShaderWriter::operator << ( const Statement& value )
{
    value.Write( *this );
    return *this;
}

ShaderWriter& ShaderWriter::operator << ( const GenericValue& value )
{
    value.Write( *this );
    return *this;
}

void ShaderWriter::GenerateShader( const EntryFunction& entry_function )
{
    assert( false && "removeme");
    std::abort();
    
    /*
    WriteGLSLVersion();

    //
    // Get all the functions used by this entry function
    //
    bool recursion;
    std::set<const Function*> functions =
      entry_function.GetFunction().GetFunctionDependencies( recursion );
    functions.insert( entry_function.GetFunction() );

    if( recursion )
    {
        m_Context.Error( "Creating a shader with recursion" );
        return;
    }

    //
    // Find out which variables are ever written to
    //
    m_WrittenToVariables = GatherWrittenToVariables( functions );

    //
    // Get all the variables used by the functions. We're interested in the
    // inputs and outputs
    //
    std::set<Variable_sp> variables = GatherVariables( functions,
                                                       entry_function );
    std::set<Variable_sp> input_variables;
    std::set<Variable_sp> output_variables;
    std::set<Variable_sp> global_variables;

    input_variables = WriteInputVaryings( entry_function, variables );
    NewLine();
    output_variables = WriteOutputVaryings( entry_function, variables );
    NewLine();
    global_variables = WriteGlobalVariables( variables );
    NewLine();

    WriteFunctionDeclarations( functions );
    NewLine();

    WriteFunctionDefinitions( functions );
    NewLine();

    WriteMainFunction( entry_function,
                       input_variables,
                       output_variables,
                       global_variables );
                       */
}

void ShaderWriter::WriteGLSLVersion()
{
    m_Shader << "#version " << s_GLSLVersion;
    NewLine(2);
}

std::set<Variable_sp> ShaderWriter::WriteInputVaryings(
                                        const EntryFunction& entry_function,
                                        const std::set<Variable_sp>& variables )
{
    std::set<Variable_sp> input_varyings;
    std::set<Variable_sp> input_parameters = GatherParameterVariables(
                                                               entry_function );

    for( const auto& v : variables )
        // Ignore the varying specifier if it's on a parameter to a
        // non-top-level function
        if( v->IsVarying() && v->IsIn() && !v->IsParameter() )
            input_varyings.insert( v );

    for( const auto& v : input_parameters )
        if( v->IsVarying() && v->IsIn() &&
            input_varyings.find( v ) == input_varyings.end() )
        {
            // We only care about input varyings here,
            // and only about ones which are not used elsewhere
            
            //
            // If this has a builtin don't write the 'in'
            //
            if( !v->GetSemantic().HasBuiltin( entry_function.GetDomain(), 
                                              true ) )
                *this << "in ";  
            
            // todo layout information here
            *this << v->GetType() << " " <<
                     Mangle( v->GetName(), IdentifierType::IN_VARYING ) << ";";
            NewLine();
        }

    for( const auto& v : input_varyings )
    {
        //
        // if this has a builtin don't write the layout information
        //
        if( !v->GetSemantic().HasBuiltin( entry_function.GetDomain(), true ) )
            *this << "in ";
        
        // todo layout information here
        *this << v->GetType() << " " <<
                 Mangle( v->GetName(), IdentifierType::IN_VARYING ) << ";";
        NewLine();
    }

    return input_varyings;
}

std::set<Variable_sp> ShaderWriter::WriteOutputVaryings(
                                        const EntryFunction& entry_function,
                                        const std::set<Variable_sp>& variables )
{
    std::set<Variable_sp> output_varyings;

    //
    // The entry function may have a varying semantic
    //
    if( entry_function.GetFunction().GetSemantic().IsVarying() )
    {
        //
        // If this has a glsl builtin don't create the variable as an 'out'
        //
        if( !entry_function.GetFunction().GetSemantic().HasBuiltin( 
                                                     entry_function.GetDomain(),
                                                     false ) )
            *this << "out "; 
        // This isn't actually a variable, so just output it now
        *this << entry_function.GetFunction().GetReturnType() <<  " " <<
                 Mangle( entry_function.GetFunction().GetIdentifier(),
                         IdentifierType::OUT_VARYING ) <<  ";";
        NewLine();
    }

    //
    // Add the outputs in the form of function parameters
    //
    for( const auto& v : entry_function.GetFunction().GetParameters() )
        // we only care about varying parameters
        if( v->IsVarying() && v->IsOut() )
            output_varyings.insert( v );

    for( const auto& v : variables )
        // Ignore the varying specifier if it's on a parameter to a
        // non-top-level function
        if( v->IsVarying() && v->IsOut() && !v->IsParameter() )
            output_varyings.insert( v );

    for( const auto& v : output_varyings )
    {
        if( !v->GetSemantic().HasBuiltin( entry_function.GetDomain(), false ) )
            *this << "out ";
        // todo layout information here
        *this << v->GetType() << " " <<
                 Mangle( v->GetName(), IdentifierType::OUT_VARYING ) << ";";
        NewLine();
    }

    return output_varyings;
}

std::set<Variable_sp> ShaderWriter::WriteGlobalVariables(
                                        const std::set<Variable_sp>& variables )
{
    std::set<Variable_sp> global_variables;
    for( const auto& v : variables )
        if( v->IsGlobal() )
        {
            WriteVariableDeclaration( v );
            NewLine();

            global_variables.insert( v );
        }
    return global_variables;
}

void ShaderWriter::WriteVariableDeclaration( Variable_sp variable )
{

    if( variable->IsUniform() )
    {
        //
        // If we write to this variable we need to create a mutable alias for it
        //
        if( m_WrittenToVariables.find(variable) != m_WrittenToVariables.end() )
        {
            assert( !variable->IsConst() &&
                    "We've written to a const variable?" );
            *this << variable->GetType() << " " <<
                     Mangle( variable->GetName(), IdentifierType::VARIABLE ) <<
                     ";";
            NewLine();
        }

        //
        // Write out the uniform version
        //
        *this << "uniform " << variable->GetType() << " " <<
                         ShaderWriter::Mangle( variable->GetName(),
                                               IdentifierType::UNIFORM );
    }
    else
    {
        if( variable->IsConst() )
            *this << "const ";

        //
        // Write out the variable
        //
        *this << variable->GetType() << " " <<
                 ShaderWriter::Mangle( variable->GetName(),
                                       IdentifierType::VARIABLE );
    }

    //
    // If we have an initializer write the assignment
    //
    if( variable->GetInitializer().GetUnderlyingType() != Type::UNKNOWN )
        *this << " = " << variable->GetInitializer();

    *this << ";";
}

void ShaderWriter::WriteFunctionDeclarations(
                                         const std::set<Function_sp> functions )
{
#if !defined(NDEBUG)
    for( const auto& f : functions )
        assert( f && "Null function in WriteFunctionDeclarations" );
#endif

    for( const auto& f : functions )
    {
        //
        // Don't write runtime functions
        //
        if( !f->IsRuntimeFunction() )
            f->WriteDeclaration( *this );
    }
}

void ShaderWriter::WriteFunctionDefinitions(
                                         const std::set<Function_sp> functions )
{
#if !defined(NDEBUG)
    for( const auto& f : functions )
        assert( f && "Null function in WriteFunctionDefinitions" );
#endif

    for( const auto& f : functions )
        //
        // Don't write runtime functions
        //
        if( !f->IsRuntimeFunction() )
            f->WriteDefinition( *this );
}

void ShaderWriter::WriteMainFunction(
                                 const EntryFunction& entry_function,
                                 const std::set<Variable_sp>& input_variables,
                                 const std::set<Variable_sp>& output_variables,
                                 const std::set<Variable_sp>& global_variables )
{
    m_Shader << "void main()";
    NewLine();
    m_Shader << "{";
    PushIndentation();
    NewLine();

    //
    // Gather all the input parameters
    //
    std::set<Variable_sp> input_parameters = GatherParameterVariables(
                                                               entry_function );

    //
    // Copy all the in varyings to the global variables except if they came from
    // a parameter
    //
    for( const auto& v : input_variables )
        // Dont allow input parameters, they have already been assigned to a
        // local variable
        if( v->IsGlobal() )
        {
            *this << Mangle( v->GetName(), IdentifierType::VARIABLE ) <<
                     " = ";
            if( !v->GetSemantic().HasBuiltin( entry_function.GetDomain(), 
                                              true ) )
                *this << Mangle( v->GetName(), IdentifierType::IN_VARYING );
            else
                *this << v->GetSemantic().GetBuiltin( 
                                                     entry_function.GetDomain(),
                                                     true );
            *this << ";";
            NewLine();
        }

    NewLine();

    //
    // Copy all the non-const uniforms to their aliases
    //
    for( const auto& v : global_variables )
    {
        assert( v->IsGlobal() && "Non global variable in global variables" );

        //
        // If this is a uniform that is written to we must initialize the alias
        // with the value of the uniform
        //
        if( v->IsUniform() &&
            m_WrittenToVariables.find(v) != m_WrittenToVariables.end() )
        {
            *this << Mangle( v->GetName(), IdentifierType::VARIABLE ) << " = "
                  << Mangle( v->GetName(), IdentifierType::UNIFORM ) << ";";
            NewLine();
        }
    }

    //
    // Write all the input parameters unless they are global
    //
    for( const auto& v : input_parameters )
    {
        if( !v->IsGlobal() )
            *this << "const " << v->GetType() << " "
                  << Mangle( v->GetName(), IdentifierType::VARIABLE ) << " = "
                  << MangleVariable( *v ) << ";";

        NewLine();
    }

    NewLine();

    //
    // Write a call the the entry function and sort out all the output variables
    //
    if( entry_function.GetFunction().GetSemantic().IsVarying() )
        // If we have a varying semantic on this function write to the output
        *this << Mangle( entry_function.GetFunction().GetIdentifier(),
                         IdentifierType::OUT_VARYING ) << " = ";
    *this << Mangle( entry_function.GetFunction().GetIdentifier(),
                     IdentifierType::FUNCTION ) << "(";
    bool first = true;
    for( const auto& argument : entry_function.GetParameters() )
    {
        if( !first )
            *this << ", ";
        else
            first = false;
        *this << *argument;
    }
    *this << ");";

    NewLine();

    //
    // Copy the function's return variable to the builtin if it has one
    //
    if( entry_function.GetFunction().GetSemantic().HasBuiltin( 
                                                     entry_function.GetDomain(), 
                                                     false ) )
    {
        NewLine();
        *this << entry_function.GetFunction().GetSemantic().GetBuiltin( 
                                                     entry_function.GetDomain(),
                                                     false ) <<
                 " = " << Mangle( entry_function.GetFunction().GetIdentifier(),
                                  IdentifierType::OUT_VARYING ) << ";";
    }
    
    //
    // Copy all the global variables to their out variables or builtins
    //
    for( const auto& v : output_variables )
        if( v->IsGlobal() )
        {
            if( v->GetSemantic().HasBuiltin( entry_function.GetDomain(), 
                                             false ) )
                *this << v->GetSemantic().GetBuiltin( 
                                                     entry_function.GetDomain(),
                                                     false );
            else
                *this << Mangle( v->GetName(), IdentifierType::OUT_VARYING );
            
            *this << " = " <<
                     Mangle( v->GetName(), IdentifierType::VARIABLE ) << ";";
            NewLine();
        }

    PopIndentation();
    NewLine();
    m_Shader << "}";
    NewLine(2);
}

std::set<Variable_sp> ShaderWriter::GatherWrittenToVariables(
                                        const std::set<Function_sp>& functions )
{
    std::set<Variable_sp> ret;
    for( const auto& f : functions )
    {
        std::set<Variable_sp> v = f->GetWrittenToVariables();
        ret.insert( v.begin(), v.end() );
    }
    return ret;
}

std::set<Variable_sp> ShaderWriter::GatherVariables(
                                        const std::set<Function_sp>& functions,
                                        const EntryFunction& entry_function )
{
    std::set<Variable_sp> ret;
    for( const auto& f : functions )
    {
        std::set<Variable_sp> v = f->GetVariables();
        ret.insert( v.begin(), v.end() );
    }
    for( const auto& v : GatherParameterVariables( entry_function ) )
        ret.insert( v );
    return ret;
}

std::set<Variable_sp> ShaderWriter::GatherParameterVariables(
                                        const EntryFunction& entry_function )
{
    std::set<Variable_sp> ret;
    for( const auto& p : entry_function.GetParameters() )
    {
        std::set<Variable_sp> v = p->GetVariables();
        ret.insert( v.begin(), v.end() );
    }
    return ret;
}

void ShaderWriter::NewLine( unsigned num_lines )
{
    const std::string indent = "    ";
    for( unsigned i = 0; i < num_lines; ++i )
        m_Shader << '\n';
    for( unsigned i = 0; i < m_Indentation; ++i )
        m_Shader << indent;
}

void ShaderWriter::WriteRuntimeFunctionCall(
                                   RuntimeFunction function,
                                   const std::vector<Expression_up>& arguments )
{
    std::string function_name;

    switch( function )
    {
    case RuntimeFunction::FLOAT_DOT:
    case RuntimeFunction::FLOAT2_DOT:
    case RuntimeFunction::FLOAT3_DOT:
    case RuntimeFunction::FLOAT4_DOT:
        function_name = "dot";
        break;
    case RuntimeFunction::FLOAT_NORMALIZE:
    case RuntimeFunction::FLOAT2_NORMALIZE:
    case RuntimeFunction::FLOAT3_NORMALIZE:
    case RuntimeFunction::FLOAT4_NORMALIZE:
        function_name = "normalize";
        break;
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
    {
        assert( arguments.size() == 2 &&
                "Trying to multiply wrong number of arguments" );
        *this << "(" << *arguments[0] << " * " << *arguments[1] << ")";
        return;
    }
    default:
        assert( false && "Trying to write an unhandled runtime function" );
    }

    *this << function_name << "(";

    bool first = true;
    for( const auto& a : arguments )
    {
        if( !first )
            *this << ", ";
        else
            first = false;

        *this << *a;
    }
    *this << ")";
}

void ShaderWriter::Error( const std::string& error_string )
{
    m_Good = false;
    m_Context.Error( error_string );
}

void ShaderWriter::Warning( const std::string& warning_string )
{
    // todo Context::Warning
    m_Context.Error( "Warning: " + warning_string );
}

} // namespace Compiler
} // namespace JoeLang
