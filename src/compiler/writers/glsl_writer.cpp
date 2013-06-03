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

#include <string>

#include <compiler/code_dag/compile_statement_node.hpp>
#include <joelang/shader.hpp>

namespace JoeLang
{
namespace Compiler
{

std::string GLSLWriter::GenerateGLSL( const CompileStatementNode& compile_statement )
{
    switch( compile_statement.GetDomain() )
    {
    case ShaderDomain::FRAGMENT:
        return "#version 150\n"
               "void main()\n"
               "{ gl_FragColor = vec4(1,0,0,1); }\n";
    case ShaderDomain::VERTEX:
        return "#version 150\n"
               "in vec4 position;\n"
               "void main()\n"
               "{ gl_Position = position; }\n";
    }
}

GLSLWriter::GLSLWriter()
{
}

GLSLWriter::~GLSLWriter()
{
}

} // namespace Compiler
} // namespace JoeLang
