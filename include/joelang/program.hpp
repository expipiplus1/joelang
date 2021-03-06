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

#pragma once

#include <joelang/config.h>
#ifdef JOELANG_WITH_OPENGL

#include <memory>
#include <vector>

#include <joelang/shader.hpp>

namespace JoeLang
{

class ParameterBase;
class ParameterWatcherBase;
using ParameterWatcherBase_up = std::unique_ptr<ParameterWatcherBase>;

class Program
{
public:
    explicit
    Program( std::vector<Shader> shaders = {} );
    Program( const Program& ) = delete;
    Program( Program&& other );
    Program& operator=( const Program& ) = delete;
    Program& operator=( Program&& other );
    ~Program();

    void Swap( Program& other );

    /**
      * Bind the program with OpenGL
      */
    void Bind() const;

    /**
      * Unbind the program from OpenGL
      */
    void Unbind() const;

    void NotifyParameter( const ParameterBase& parameter );

    unsigned GetGLUniformLocation( const std::string& name ) const;

    void Compile();

    bool IsCompiled() const;
private:
    std::vector<Shader>                  m_Shaders;
    std::vector<ParameterWatcherBase_up> m_ParameterWatchers;

    unsigned m_Object;
};

} // namespace JoeLang

#else

namespace JoeLang
{
    class Program
    {
    public:
        void Bind() const {};
        void Unbind() const {};
    };
}

#endif
