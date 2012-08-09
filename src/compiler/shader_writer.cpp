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

#include <memory>
#include <string>

#include <compiler/entry_function.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{
namespace Compiler
{

std::string ShaderWriter::GenerateGLSL( const EntryFunction& entry_function )
{
    // todo, I'm sure that there's something missing here...

    if( entry_function.GetDomain() == ShaderDomain::FRAGMENT )
        return  "#version 150\n"
                "out vec4 output_color;\n"
                "void main()\n"
                "{\n"
                "   output_color = vec4(0.44, 0.85, 0.29, 1.0);\n"
                "}\n";
    else
        return  "#version 150\n"
                "in vec4 position;\n"
                "void main()\n"
                "{\n"
                "   gl_Position = position;\n"
                "}\n";
}

} // namespace Compiler
} // namespace JoeLang
