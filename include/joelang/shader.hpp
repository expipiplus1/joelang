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
#include <string>
#include <vector>

namespace JoeLang
{

class Context;

namespace Compiler
{
    class EntryFunction;
};

enum class ShaderDomain
{
    VERTEX,
    FRAGMENT
};

class Shader
{
public:
    Shader( const Context& context, ShaderDomain domain, std::string source );
    Shader( const Shader& ) = delete;
    Shader( Shader&& other );
    Shader& operator=( const Shader& ) = delete;
    Shader& operator=( Shader&& other );
    ~Shader();
    void Swap( Shader& other );

    void Compile();

    bool IsCompiled() const;

    const std::string& GetString() const;

    /// Used internally by joelang
    /// This asserts on a null pointer
    Shader( const Context& context,
            std::shared_ptr<Compiler::EntryFunction> entry_function );

    friend class Program;
private:
    /// The context that this shader belongs to
    const Context& m_Context;

    /// The compile statement as seen in the joelang source
    std::shared_ptr<Compiler::EntryFunction> m_EntryFunction;

    /// The glsl source of the shader
    std::string m_Source;

    /// The OpenGL shader object
    unsigned m_Object = 0;

    /// The shader domain
    ShaderDomain m_Domain;
};

} // namespace JoeLang

#else
namespace JoeLang
{
    enum class ShaderDomain
    {
        VERTEX,
        FRAGMENT
    };
}
#endif
