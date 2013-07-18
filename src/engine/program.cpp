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

#include <joelang/program.hpp>

#include <joelang/config.h>
#ifdef JOELANG_WITH_OPENGL

#include <cassert>
#include <iostream>
#include <utility>
#include <vector>

#include <GL/GLee.h>

#include <compiler/writers/shader_compilation_context.hpp>
#include <engine/parameter_watcher.hpp>
#include <joelang/shader.hpp>


namespace JoeLang
{

Program::Program( std::vector<Shader> shaders, Compiler::ShaderCompilationContext_up compilation_context )
    :m_Shaders( std::move(shaders) )
    ,m_CompilationContext( std::move( compilation_context ) )
    ,m_Object( 0 )
{
}

Program::Program( Program&& other )
    :m_Object( 0 )
{
    Swap( other );
}

Program& Program::operator=( Program&& other )
{
    Swap( other );
    return *this;
}

Program::~Program()
{
    glDeleteProgram( m_Object );
}

void Program::Swap( Program& other )
{
    std::swap( m_Shaders, other.m_Shaders );
    std::swap( m_Object, other.m_Object );
}

void Program::Bind() const
{
    glUseProgram( m_Object );
    for( const auto& p: m_ParameterWatchers )
        p->Update();
}

void Program::Unbind() const
{
    glUseProgram( 0 );
}

void Program::NotifyParameter( const ParameterBase& parameter )
{
    ParameterWatcherBase_up watcher = ParameterWatcherBase::Create( *this,
                                                                    parameter );
    if( watcher )
        m_ParameterWatchers.push_back( std::move( watcher ) );
}

unsigned Program::GetGLUniformLocation( const std::string& uniform_name ) const
{
    return glGetUniformLocation( m_Object, uniform_name.c_str() );
}

void Program::Compile()
{
    //
    // Don't recompile if we have an object
    //
    if( m_Object )
        return;
    
    //
    // Don't compile if we don't have any shaders
    //
    if( m_Shaders.empty() )
        return; 
        
    for( Shader& shader : m_Shaders )
        shader.Compile();

    m_Object = glCreateProgram();
    assert( m_Object && "Error creating OpenGL program" );

    for( const Shader& s : m_Shaders )
        glAttachShader( m_Object, s.m_Object);

    glLinkProgram( m_Object );

    GLint status;
    // todo report context error here
    glGetProgramiv( m_Object, GL_LINK_STATUS, &status );

    if (status == GL_FALSE)
    {
        GLint info_log_length;
        glGetProgramiv( m_Object, GL_INFO_LOG_LENGTH, &info_log_length );

        GLchar *info_log = new GLchar[info_log_length + 1];
        glGetProgramInfoLog( m_Object, info_log_length, NULL, info_log );

        std::cerr << "Link Error in shader: " << info_log << std::endl;
        delete[] info_log;
    }

    for( const Shader& s : m_Shaders )
        glDetachShader( m_Object, s.m_Object );
}

bool Program::IsCompiled() const
{
    return m_Object;
}

} // namespace JoeLang

#endif
