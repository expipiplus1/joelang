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

#include "function.hpp"

#include <algorithm>
#include <cassert>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <compiler/code_generator.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>

namespace JoeLang
{
namespace Compiler
{

Function::Function( std::string identifier,
                    CompleteType return_type,
                    std::vector<CompleteType> parameter_types )
    :m_Identifier( std::move(identifier) )
    ,m_ReturnType( std::move(return_type) )
    ,m_ParameterTypes( std::move(parameter_types) )
    ,m_LLVMFunction( nullptr )
{
}

const std::string& Function::GetIdentifier() const
{
    return m_Identifier;
}

const CompleteType& Function::GetReturnType() const
{
    return m_ReturnType;
}

const std::vector<CompleteType>& Function::GetParameterTypes() const
{
    return m_ParameterTypes;
}

std::size_t Function::GetNumParams() const
{
    return m_ParameterTypes.size();
}

llvm::Function* Function::GetLLVMFunction() const
{
    assert( m_LLVMFunction && "Trying to get an undeclared function" );
    return m_LLVMFunction;
}

void Function::SetParameters( std::vector<Variable_sp> parameters )
{
    m_Parameters = std::move(parameters);
    assert( m_Parameters.size() == m_ParameterTypes.size() );
#if !defined(NDEBUG)
    for( unsigned i = 0; i < m_Parameters.size(); ++i )
        assert( m_Parameters[i]->GetType() == m_ParameterTypes[i] &&
                "Mismatching parameter types" );
#endif
}

void Function::SetDefinition( CompoundStatement_up definition )
{
    assert( !HasDefinition() && "Definining a function twice" );
    m_Definition = std::move(definition);
}

bool Function::HasDefinition() const
{
    return static_cast<bool>(m_Definition);
}

std::string Function::GetSignatureString() const
{
    /// TODO do this properly
    return m_Identifier;
}

bool Function::HasSameParameterTypes(
                            const std::vector<CompleteType>& other_types ) const
{
    return other_types == m_ParameterTypes;
}

void Function::CodeGenDeclaration( CodeGenerator& code_gen )
{
    m_LLVMFunction = code_gen.CreateFunctionDeclaration( m_Identifier,
                                                         m_ReturnType,
                                                         m_ParameterTypes );
}

void Function::CodeGenDefinition( CodeGenerator& code_gen )
{
    assert( m_LLVMFunction &&
            "Trying to generate a definition without a declaration" );
    assert( m_Parameters.size() == m_ParameterTypes.size() &&
            "Parameter type vector size mismatch" );
    //
    // codegen the parameters
    //
    code_gen.BindFunctionParameters( m_LLVMFunction, m_Parameters );


    //
    // codegen the body
    //
    code_gen.CreateFunctionDefinition( m_LLVMFunction,
                                       m_Parameters,
                                       m_Definition );
}

} // namespace Compiler
} // namespace JoeLang
