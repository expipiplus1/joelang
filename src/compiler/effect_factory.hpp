/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

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

#include <functional>
#include <memory>
#include <vector>

#include <compiler/semantic_analysis/sema_analyzer.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/llvm_writer.hpp>
#include <compiler/writers/runtime.hpp>

namespace JoeLang
{

class Context;
class Effect;
class Pass;
class StateAssignmentBase;
class Technique;

namespace Compiler
{

class PassNode;
class StateAssignmentNode;
class TechniqueNode;
using TechniqueNode_ref = std::reference_wrapper<const TechniqueNode>;

class EffectFactory
{
public:
    explicit
    EffectFactory( const Context& context, Runtime& runtime );

    std::unique_ptr<Effect> CreateEffectFromString( const std::string& source,
                                                    const std::string& name = "" );

private:
    std::vector<Technique> GenerateTechniques(
        const std::vector<TechniqueNode_ref>& technique_nodes );

    Technique GenerateTechnique( const TechniqueNode& technique_node );

    Pass GeneratePass( const PassNode& pass_node );

    std::vector<ParameterBase_up> GenerateParameters( const std::vector<Variable_sp>& uniforms );

    const Context& m_Context;
    Runtime& m_Runtime;
    LLVMWriter m_LLVMWriter;
};

} // namespace Compiler
} // namespace JoeLang
