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
#include <set>
#include <string>
#include <utility>
#include <vector>

#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/writers/runtime.hpp>
#include <compiler/writers/shader_writer.hpp>

#include <compiler/code_dag/node.hpp>

namespace JoeLang
{
namespace Compiler
{

Function::Function( std::string identifier,
                    CompleteType return_type,
                    Semantic semantic,
                    std::vector<CompleteType> parameter_types )
    :m_Identifier( std::move(identifier) )
    ,m_ReturnType( std::move(return_type) )
    ,m_Semantic( std::move(semantic) )
    ,m_ParameterTypes( std::move(parameter_types) )
    ,m_LLVMFunction( nullptr )
    ,m_RuntimeFunction( RuntimeFunction::NONE )
{
}

Function::Function( std::string identifier,
                    CompleteType return_type,
                    std::vector<CompleteType> parameter_types,
                    llvm::Function* llvm_function,
                    RuntimeFunction runtime_function )
    :m_Identifier( std::move(identifier) )
    ,m_ReturnType( std::move(return_type) )
    ,m_ParameterTypes( std::move(parameter_types) )
    ,m_LLVMFunction( llvm_function )
    ,m_RuntimeFunction( runtime_function )
{
    assert( llvm_function && "Function given a null llvm_function" );
}

Function::~Function()
{
}

void Function::GenerateCodeDag( NodeManager& node_manager )
{
    m_CodeDag = &m_Definition->GenerateCodeDag( node_manager );
}

const Node& Function::GetCodeDag() const
{
    assert( m_CodeDag && "Trying to get the code dag of a function without one" );
    return *m_CodeDag;
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

const std::vector<Variable_sp>& Function::GetParameters() const
{
    return m_Parameters;
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
    assert( !IsRuntimeFunction() && "Trying to define a runtime function" );
    assert( !HasDefinition() && "Definining a function twice" );
    m_Definition = std::move(definition);
}

bool Function::HasDefinition() const
{
    return static_cast<bool>(m_Definition);
}

bool Function::HasLLVMFunction() const
{
    return m_LLVMFunction;
}

bool Function::IsRuntimeFunction() const
{
    return m_RuntimeFunction != RuntimeFunction::NONE;
}

RuntimeFunction Function::GetRuntimeFunction() const
{
    return m_RuntimeFunction;
}

std::set<Function_sp> Function::GetCallees() const
{
    if( IsRuntimeFunction() )
        return std::set<Function_sp>{};
    assert( HasDefinition() &&
            "Trying to get the callees of a function without a definition" );
    return m_Definition->GetCallees();
}

std::set<Function_sp> Function::GetFunctionDependencies( bool& recursion ) const
{
    std::set<Function_sp> ret;
    std::set<const Function*> stack = { this };
    recursion = false;

    std::function<void(Function_sp)> visit;
    visit = [&ret, &stack, &recursion, &visit]( Function_sp f ) -> void
    {
        //
        // If this is in the temp list we have recursion
        // go no further
        //
        if( stack.find( f.get() ) != stack.end() )
        {
            recursion = true;
            return;
        }

        //
        // Otherwise it's in ret or needs to go in temp
        //
        stack.insert( f.get() );
        for( auto c : f->GetCallees() )
            visit( c );
        stack.erase( f.get() );
        ret.insert( f );
    };

    for( auto c : GetCallees() )
        visit( c );

    return ret;
}

std::set<Variable_sp> Function::GetVariables() const
{
    if( IsRuntimeFunction() )
        return std::set<Variable_sp>{};

    assert( m_Definition &&
            "Trying to get the variables of a function without a definition" );
    return m_Definition->GetVariables();
}

std::set<Variable_sp> Function::GetWrittenToVariables() const
{
    if( IsRuntimeFunction() )
        return std::set<Variable_sp>{};

    assert( m_Definition &&
            "Trying to get the written to variables of a function without a "
            "definition" );
    return m_Definition->GetWrittenToVariables();
}


const Semantic& Function::GetSemantic() const
{
    return m_Semantic;
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
    // codegen the body
    //
    code_gen.CreateFunctionDefinition( m_LLVMFunction,
                                       m_Parameters,
                                       m_Definition );
}

void Function::WriteDeclaration( ShaderWriter& shader_writer ) const
{
    assert( !IsRuntimeFunction() &&
            "Trying to write the decalaration for a runtime function" );
    WriteHeader( shader_writer );
    shader_writer << ";";
    shader_writer.NewLine();
}

void Function::WriteDefinition( ShaderWriter& shader_writer ) const
{
    assert( !IsRuntimeFunction() &&
            "Trying to write the definition for a runtime function" );

    WriteHeader( shader_writer );

    shader_writer.NewLine();

    shader_writer << static_cast<Statement&>(*m_Definition);

    shader_writer.NewLine(2);
}

void Function::WriteHeader( ShaderWriter& shader_writer ) const
{
    shader_writer << m_ReturnType << " " <<
                     ShaderWriter::Mangle( m_Identifier,
                                           IdentifierType::FUNCTION ) <<
                     "(";

    bool first = true;
    for( const auto& parameter : m_Parameters )
    {
        if( !first )
            shader_writer << ", ";
        else
            first = false;
        if( parameter->IsConst() )
            shader_writer << "const ";
        if( parameter->IsIn() )
            shader_writer << "in ";
        if( parameter->IsOut() )
            shader_writer << "out ";
        // todo more attributes
        shader_writer << parameter->GetType() << " " <<
                         ShaderWriter::Mangle( parameter->GetName(),
                                               IdentifierType::VARIABLE );
    }

    shader_writer << ")";
}


} // namespace Compiler
} // namespace JoeLang
