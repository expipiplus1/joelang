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

#include <engine/parameter_watcher.hpp>

#include <joelang/config.h>
#ifdef JOELANG_WITH_OPENGL

#include <joelang/parameter.hpp>
#include <joelang/program.hpp>
#include <compiler/shader_writer.hpp>

namespace JoeLang
{

ParameterWatcherBase::ParameterWatcherBase( int uniform_location )
    :m_UniformLocation( uniform_location )
{
    assert( uniform_location >= 0 &&
            "Creating a parameter watcher with an invalid uniform location" );
}

ParameterWatcherBase::~ParameterWatcherBase()
{
}

ParameterWatcherBase_up ParameterWatcherBase::Create(
                                                const Program& program,
                                                const ParameterBase& parameter )
{
    int uniform_location = program.GetGLUniformLocation(
          Compiler::ShaderWriter::Mangle( parameter.GetName(),
                                          Compiler::IdentifierType::UNIFORM ) );

    if( uniform_location == -1 )
        return nullptr;

#define CREATE_TYPED_PARAMETER_WATCHER(type) \
    case JoeLangType<type>::value: \
        return ParameterWatcherBase_up( \
            new ParameterWatcher<type>( \
                        uniform_location, \
                        reinterpret_cast<const Parameter<type>&>( parameter) ) )

#define CREATE_TYPED_PARAMETER_WATCHER_N(type) \
        CREATE_TYPED_PARAMETER_WATCHER(type); \
        CREATE_TYPED_PARAMETER_WATCHER(type##2); \
        CREATE_TYPED_PARAMETER_WATCHER(type##3); \
        CREATE_TYPED_PARAMETER_WATCHER(type##4)

    switch( parameter.GetType() )
    {
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_bool);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_char);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_short);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_int);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_long);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_uchar);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_ushort);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_uint);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_ulong);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_float);
    CREATE_TYPED_PARAMETER_WATCHER_N(jl_double);
    default:
        assert( false &&
                "Trying to create a parameter watcher for an unhandled type" );
        break;
    }

#undef CREATE_TYPED_PARAMETER_WATCHER

    return nullptr;
}

#define CREATE_SCALAR_SPECIALIZATION(type, gl_function) \
template<> \
void ParameterWatcher<type>::Update() \
{ \
    if( m_CurrentValue != m_Parameter.GetParameter() ) \
    { \
        m_CurrentValue = m_Parameter.GetParameter(); \
        (gl_function)( m_UniformLocation, m_CurrentValue ); \
    } \
}

#define CREATE_VEC1_SPECIALIZATION(type, gl_function) \
template<> \
void ParameterWatcher<type>::Update() \
{ \
    if( m_CurrentValue != m_Parameter.GetParameter() ) \
    { \
        m_CurrentValue = m_Parameter.GetParameter(); \
        (gl_function)( m_UniformLocation, m_CurrentValue.x() ); \
    } \
}

#define CREATE_VEC2_SPECIALIZATION(type, gl_function) \
template<> \
void ParameterWatcher<type>::Update() \
{ \
    if( m_CurrentValue != m_Parameter.GetParameter() ) \
    { \
        m_CurrentValue = m_Parameter.GetParameter(); \
        (gl_function)( m_UniformLocation, m_CurrentValue.x(), \
                                          m_CurrentValue.y() ); \
    } \
}

#define CREATE_VEC3_SPECIALIZATION(type, gl_function) \
template<> \
void ParameterWatcher<type>::Update() \
{ \
    if( m_CurrentValue != m_Parameter.GetParameter() ) \
    { \
        m_CurrentValue = m_Parameter.GetParameter(); \
        (gl_function)( m_UniformLocation, m_CurrentValue.x(), \
                                          m_CurrentValue.y(), \
                                          m_CurrentValue.z() ); \
    } \
}

#define CREATE_VEC4_SPECIALIZATION(type, gl_function) \
template<> \
void ParameterWatcher<type>::Update() \
{ \
    if( m_CurrentValue != m_Parameter.GetParameter() ) \
    { \
        m_CurrentValue = m_Parameter.GetParameter(); \
        (gl_function)( m_UniformLocation, m_CurrentValue.x(), \
                                          m_CurrentValue.y(), \
                                          m_CurrentValue.z(),\
                                          m_CurrentValue.w() ); \
    } \
}


#define CREATE_SPECIALIZATION(type, gl_function_suffix) \
    CREATE_SCALAR_SPECIALIZATION(type, glUniform1##gl_function_suffix) \
    CREATE_VEC1_SPECIALIZATION(type##1,   glUniform1##gl_function_suffix) \
    CREATE_VEC2_SPECIALIZATION(type##2,   glUniform2##gl_function_suffix) \
    CREATE_VEC3_SPECIALIZATION(type##3,   glUniform3##gl_function_suffix) \
    CREATE_VEC4_SPECIALIZATION(type##4,   glUniform4##gl_function_suffix)

CREATE_SPECIALIZATION(jl_bool, ui)
CREATE_SPECIALIZATION(jl_char, i);
CREATE_SPECIALIZATION(jl_short, i);
CREATE_SPECIALIZATION(jl_int, i);
CREATE_SPECIALIZATION(jl_long, i); // TODO make this use 64 but uniforms if pos.
CREATE_SPECIALIZATION(jl_uchar, ui);
CREATE_SPECIALIZATION(jl_ushort, ui);
CREATE_SPECIALIZATION(jl_uint, ui);
CREATE_SPECIALIZATION(jl_ulong, ui);
CREATE_SPECIALIZATION(jl_float, f);
CREATE_SPECIALIZATION(jl_double, d);


#undef CREATE_VEC1_SPECIALIZATION

} // namespace JoeLang

#endif
