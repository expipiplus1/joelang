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

#include <joelang/shader.hpp>

#include <joelang/config.h>
#ifdef JOELANG_WITH_OPENGL

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <utility>

#include <iostream>

#include <GL/GLee.h>

#include <compiler/semantic_analysis/entry_function.hpp>
#include <compiler/writers/shader_writer.hpp>

namespace JoeLang
{

Shader::Shader( const Context& context, ShaderDomain domain, std::string source )
    :m_Context( context )
    ,m_Source( std::move(source) )
    ,m_Object( 0 )
    ,m_Domain( domain )
{
}

Shader::Shader( const Context& context,
                std::shared_ptr<Compiler::EntryFunction> entry_function )
    :m_Context( context )
    ,m_EntryFunction( std::move(entry_function) )
    ,m_Object( 0 )
{
    assert( m_EntryFunction && "Null EntryFunction given to Shader" );
    m_Domain = m_EntryFunction->GetDomain();
}


Shader::Shader( Shader&& other )
    :m_Context( other.m_Context )
    ,m_Object( 0 )
{
    Swap( other );
}

Shader& Shader::operator=( Shader&& other )
{
    assert( &m_Context == &other.m_Context &&
            "Trying to assign a shader across contexts" );
    Swap( other );
    return *this;
}

Shader::~Shader()
{
    glDeleteShader(m_Object);
}

void Shader::Swap( Shader& other )
{
    if( &other == this )
        return;
    std::swap( m_EntryFunction, other.m_EntryFunction );
    std::swap( m_Source, other.m_Source );
    std::swap( m_Domain, other.m_Domain );
    std::swap( m_Object, other.m_Object );
}

void Shader::Compile()
{
    const static std::map<ShaderDomain, GLenum> domain_map =
    {
        { ShaderDomain::VERTEX,   GL_VERTEX_SHADER   },
        { ShaderDomain::FRAGMENT, GL_FRAGMENT_SHADER }
    };

    //
    // Don't recompile if we already have an object
    //
    if( m_Object )
        return;

    //
    // Generate the glsl
    //
    Compiler::ShaderWriter shader_writer( m_Context );
    assert( m_EntryFunction && "Generating glsl for a null entryfunction" );
    m_Source = shader_writer.GenerateGLSL( *m_EntryFunction );

    std::cout << m_Source << std::endl;

    //
    // Compile the glsl
    //
    glDeleteShader(m_Object);
    m_Object = glCreateShader( domain_map.at(m_Domain) );
    assert( m_Object && "Couldn't create shader object" );
    const char* c = m_Source.c_str();
    glShaderSource( m_Object, 1, &c, NULL );
    glCompileShader( m_Object );

    GLint status;
    glGetShaderiv( m_Object, GL_COMPILE_STATUS, &status );
    // todo report error with context

    if (status == GL_FALSE)
    {
        GLint info_log_length;
        glGetShaderiv( m_Object, GL_INFO_LOG_LENGTH, &info_log_length );

        GLchar *info_log = new GLchar[info_log_length + 1];
        glGetShaderInfoLog( m_Object, info_log_length, NULL, info_log );

        std::cerr << "Compile Error in shader: " << info_log << std::endl;
        delete[] info_log;
    }
}

bool Shader::IsCompiled() const
{
    return m_Object;
}

const std::string& Shader::GetString() const
{
    return m_Source;
}

} // namespace JoeLang

#endif
