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

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <sstream>

#include <compiler/complete_type.hpp>
#include <compiler/entry_function.hpp>
#include <compiler/function.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <joelang/context.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{
namespace Compiler
{

const std::string ShaderWriter::s_GLSLVersion = "150";

ShaderWriter::ShaderWriter( const Context& context )
    :m_Context( context )
{
}

std::string ShaderWriter::GenerateGLSL( const EntryFunction& entry_function )
{
    // todo, I'm sure that there's something missing here...

    if( entry_function.GetDomain() == ShaderDomain::FRAGMENT )
        GenerateFragmentShader( entry_function );
    else
        return  "#version 150\n"
                "in vec4 position;\n"
                "void main()\n"
                "{\n"
                "   gl_Position = position;\n"
                "}\n";

    std::string ret = m_Shader.str();

    // reset things
    m_FunctionSignatures.clear();
    m_Shader.str("");

    assert( m_Indentation == 0 && "Unmatched indentations" );

    // return an empty string if we've had an error
    if( m_Good )
        return ret;
    else
        return "";
}

void ShaderWriter::WriteFunction( const Function& function )
{
    const std::string& signature = function.GetSignatureString();
    if( m_FunctionSignatures.find( signature ) != m_FunctionSignatures.end() )
        return;
    m_FunctionSignatures.insert( function.GetSignatureString() );
    function.Write( *this );
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
        { IdentifierType::VARIABLE, "_"  },
        { IdentifierType::FUNCTION, "_f" }
    };
    return prefix_map.at(identifier_type) + identifier;
}

ShaderWriter& ShaderWriter::operator << ( const CompleteType& value )
{
    value.Write( *this );
    return *this;
}

ShaderWriter& ShaderWriter::operator << ( const PostfixOperator& value )
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

void ShaderWriter::GenerateFragmentShader( const EntryFunction& entry_function )
{
    WriteGLSLVersion();

    WriteOutputVariables( entry_function );

    WriteFunction( entry_function.GetFunction() );

    WriteMainFunction( entry_function );
}

void ShaderWriter::WriteGLSLVersion()
{
    m_Shader << "#version " << s_GLSLVersion;
    NewLine(2);
}

void ShaderWriter::WriteOutputVariables( const EntryFunction& entry_function )
{
    // todo
    m_Shader << "out vec4 output_color;";
    NewLine(2);
}

void ShaderWriter::WriteMainFunction( const EntryFunction& entry_function )
{
    m_Shader << "void main()";
    NewLine();
    m_Shader << "{";
    PushIndentation();
    NewLine();

    //
    // Write a call the the entry function and sort out all the output variables
    //
    // todo hidden varyings
    *this << "output_color = " <<
             Mangle( entry_function.GetFunction().GetIdentifier(),
                     IdentifierType::FUNCTION ) <<
             "(";
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

    PopIndentation();
    NewLine();
    m_Shader << "}";
    NewLine(2);
}

void ShaderWriter::NewLine( unsigned num_lines )
{
    const std::string indent = "    ";
    for( unsigned i = 0; i < num_lines; ++i )
        m_Shader << '\n';
    for( unsigned i = 0; i < m_Indentation; ++i )
        m_Shader << indent;
}

void ShaderWriter::Error( const std::string& error_string )
{
    m_Good = false;
    m_Context.Error( error_string );
}

} // namespace Compiler
} // namespace JoeLang
