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

#include "sema_analyzer.hpp"

#include <algorithm>
#include <functional>
#include <iostream>
#include <memory>
#include <map>
#include <string>
#include <utility>

#include <compiler/support/casting.hpp>
#include <compiler/writers/code_generator.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/support/generic_value.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/writers/runtime.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/initializer.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <joelang/context.hpp>
#include <joelang/state.hpp>

namespace JoeLang
{
namespace Compiler
{

SemaAnalyzer::SemaAnalyzer( const Context& context, Runtime& runtime )
    :m_Context( context )
    ,m_Runtime( runtime )
    ,m_CodeGenerator( std::move(m_Runtime.CreateCodeGenerator()) )
{
    for( Function_sp f : m_Runtime.GetRuntimeFunctions() )
        AddFunction( std::move(f) );
}

SemaAnalyzer::~SemaAnalyzer()
{
    assert( m_SymbolStack.size() == 0 &&
            "The symbol table is still inside a scope" );
}

bool SemaAnalyzer::Analyze( TranslationUnit& tu )
{
    assert( m_SymbolStack.size() == 0 &&
            "Analyzing a translation unit while already in a scope" );

    SemaAnalyzer::ScopeHolder scope( *this );
    scope.Enter();

    //
    // This will cause this analyzer to be filled up with all that it needs
    //
    tu.PerformSema( *this );

    scope.Leave();

    //
    // Check for undefined passes
    //
    for( const auto& p : m_PassDefinitions )
    {
        //
        // If this pass is referenced and not defined
        //
        if( !p.second.unique() &&
            !(*p.second) )
            Error( "Use of undefined pass: " + p.first );
    }

    for( auto& fo : m_FunctionOverloads )
    {
        // error if the function is referenced and not defined
        for( const auto& f : fo.second )
            if( !f.unique() &&
                !f->HasDefinition() &&
                !f->HasLLVMFunction() )
                Error( "Use of undefined function: " +
                       f->GetSignatureString() );

        //
        // remove all functions without definitions
        //
        fo.second.erase( std::remove_if( fo.second.begin(), fo.second.end(),
                                         [](const Function_sp& f)
                                         {return !f->HasDefinition();} ),
                         fo.second.end() );
    }

    // Todo, reset state here
    // Return success or not
    return Good();
}

void SemaAnalyzer::AddTechniqueDeclaration(
                                       std::unique_ptr<TechniqueDeclaration> t )
{
    t->PerformSema( *this );
    m_TechniqueDeclarations.push_back( std::move(t) );
}

void SemaAnalyzer::AddPassDeclaration( std::unique_ptr<PassDeclaration> p )
{
    p->PerformSema( *this );
    m_PassDeclarations.push_back( std::move(p) );
}

void SemaAnalyzer::AddVariableDeclarations(
                                    std::unique_ptr<VariableDeclarationList> v )
{
    v->PerformSema( *this );
    m_VariableDeclarations.push_back( std::move(v) );
}

void SemaAnalyzer::AddFunctionDefinition(
                                         std::unique_ptr<FunctionDefinition> f )
{
    f->PerformSema( *this );
    m_FunctionDefinitions.push_back( std::move(f) );
}

const std::vector<TechniqueDeclaration_up>&
                                  SemaAnalyzer::GetTechniqueDeclarations() const
{
    return m_TechniqueDeclarations;
}

const std::vector<Variable_sp>& SemaAnalyzer::GetUniformVariables() const
{
    return m_UniformVariables;
}

void SemaAnalyzer::DeclarePass( std::string name,
                                std::unique_ptr<PassDefinition> definition )
{
    if( definition )
        definition->PerformSema( *this );

    PassDefinitionMap::const_iterator d = m_PassDefinitions.find( name );

    if( d == m_PassDefinitions.end() )
        // If we haven't seen this name before: insert it
        m_PassDefinitions[name] =
                        std::make_shared<std::unique_ptr<PassDefinition> >(
                                                        std::move(definition) );
    else
    {
        if( definition )
        {
            if( *d->second  )
                // If it's being defined now and was defined before
                Error( "Multiple definition of pass " + name );
            (*d->second) = std::move(definition);
        }
    }
}

void SemaAnalyzer::DeclareTechnique( std::string name )
{
    // Check if this technique has already been defined
    const auto& i = std::find( m_Techniques.begin(), m_Techniques.end(), name );

    if( i != m_Techniques.end() )
        Error( "Multiple definitions of technique " + name );

    // Add the technique
    m_Techniques.push_back( std::move(name) );
}

bool SemaAnalyzer::HasPass( const std::string& name ) const
{
    return m_PassDefinitions.count( name ) > 0;
}

SemaAnalyzer::PassDefinitionRef SemaAnalyzer::GetPass(
                                                 const std::string& name ) const
{
    PassDefinitionMap::const_iterator p = m_PassDefinitions.find( name );
    if( p == m_PassDefinitions.end() )
        return nullptr;
    return p->second;
}

bool SemaAnalyzer::HasState( const std::string& name ) const
{
    return m_Context.GetNamedState( name ) != nullptr;
}

const StateBase* SemaAnalyzer::GetState( const std::string& name ) const
{
    return m_Context.GetNamedState( name );
}

std::map<std::string, GenericValue>
    GetGenericValueEnumerantMap( const StateBase& state_base )
{
    std::map<std::string, GenericValue> ret;
    switch( state_base.GetType() )
    {
    case Type::BOOL:
        for( const auto& e :
             static_cast<const State<jl_bool>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::FLOAT:
        for( const auto& e :
             static_cast<const State<jl_float>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::FLOAT2:
        for( const auto& e :
             static_cast<const State<jl_float2>&>(state_base).GetEnumerations())
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::FLOAT3:
        for( const auto& e :
             static_cast<const State<jl_float3>&>(state_base).GetEnumerations())
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::FLOAT4:
        for( const auto& e :
             static_cast<const State<jl_float4>&>(state_base).GetEnumerations())
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::DOUBLE:
        for( const auto& e :
             static_cast<const State<jl_double>&>(state_base).GetEnumerations())
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::CHAR:
        for( const auto& e :
             static_cast<const State<jl_char>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    case Type::SHORT:
        for( const auto& e :
             static_cast<const State<jl_short>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::INT:
        for( const auto& e :
             static_cast<const State<jl_int>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::LONG:
        for( const auto& e :
             static_cast<const State<jl_long>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::UCHAR:
        for( const auto& e :
             static_cast<const State<jl_uchar>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::USHORT:
        for( const auto& e :
             static_cast<const State<jl_ushort>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
     break;
    case Type::UINT:
        for( const auto& e :
             static_cast<const State<jl_uint>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::ULONG:
        for( const auto& e :
             static_cast<const State<jl_ulong>&>(state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
         break;
    case Type::STRING:
        for( const auto& e :
             static_cast<const State<std::string>&>(
                                                 state_base).GetEnumerations() )
            ret[e.first] = GenericValue( e.second );
        break;
    default:
        assert( false && "state_base is of an unhandled type" );
    }
    return ret;
}

void SemaAnalyzer::LoadStateEnumerants( const StateBase& state )
{
    m_StateEnumerants = &state.GetEnumerations();

    /*
    /// TODO cache these results
    /// TODO support arrays here
    /// TODO something better than making these global
    for( auto& v : GetGenericValueEnumerantMap(state) )
    {
        std::shared_ptr<Variable> variable =  std::make_shared<Variable>(
                                                        v.second.GetType(),
                                                        Semantic(),
                                                        true, //Is const
                                                        false, //Isn't uniform
                                                        false, //Isn't varying
                                                        false, //Isn't in
                                                        false, //Isn't out
                                                        true, //Is global
                                                        false,//Isn't a param
                                                        std::move(v.second),
                                                        v.first );
        variable->CodeGen( m_CodeGenerator );
        DeclareVariable( v.first, std::move(variable) );
    }
    */
}

void SemaAnalyzer::DeclareVariable( const std::string& identifier,
                                    std::shared_ptr<Variable> variable )
{
    bool inserted = m_SymbolStack.rbegin()->m_Variables.insert(
                        std::make_pair( identifier, variable ) ).second;

    if( !inserted )
        Error( "Duplicate definition of variable: " + identifier );
    else
    {
        //
        // Remember this if it's a global uniform
        //
        if( variable->IsUniform() && variable->IsGlobal() )
        {
            m_UniformVariables.push_back( std::move(variable) );
        }

        //
        // Give a warning if we are shadowing another variable
        //
        for( auto it = ++m_SymbolStack.rbegin();
             it != m_SymbolStack.rend();
             ++it )
        {
            if( it->m_Variables.find( identifier ) != it->m_Variables.end() )
                Warning( "Declaration of '" + identifier +
                         "' shadows previous declaration" );
        }
    }
}

std::shared_ptr<Variable> SemaAnalyzer::GetVariable(
                                                const std::string& identifier )
{
    //
    // If we have any state enumerants, try and match those first
    //
    if( m_StateEnumerants )
    {
        auto s = m_StateEnumerants->find( identifier );
        if( s != m_StateEnumerants->end() )
        {
            //
            // if we've found it, declare it and return it, no need to hang onto
            // it
            //
            std::shared_ptr<Variable> variable =  std::make_shared<Variable>(
                                                        Type::INT,
                                                        Semantic(),
                                                        true, //Is const
                                                        false, //Isn't uniform
                                                        false, //Isn't varying
                                                        false, //Isn't in
                                                        false, //Isn't out
                                                        true, //Is global
                                                        false,//Isn't a param
                                                        GenericValue(s->second),
                                                        s->first );
            variable->CodeGen( m_CodeGenerator );
            return std::move(variable);
        }
    }

    // iterate in reverse because deeper scopes take priority
    for( auto it = m_SymbolStack.rbegin();
         it != m_SymbolStack.rend(); ++it )
    {
        const auto& v = it->m_Variables.find( identifier );
        if( v != it->m_Variables.end() )
            return v->second;
    }
    return nullptr;
}

void SemaAnalyzer::AddFunction( Function_sp function )
{
    std::vector<Function_sp>& function_overloads =
                                 m_FunctionOverloads[function->GetIdentifier()];

    for( const auto& f : function_overloads )
        if( f->HasSameParameterTypes( function->GetParameterTypes() ) )
        {
            // If we already have this funciton, make sure that we have the same
            // return type
            if( f->GetReturnType().GetBaseType() !=
                                      function->GetReturnType().GetBaseType() ||
                f->GetReturnType().GetArrayExtents() !=
                                   function->GetReturnType().GetArrayExtents() )
                assert( false && "Trying to add a function which differs only "
                        "by return type" );
            return;
        }

    function_overloads.emplace_back( function );
    return;
}

void SemaAnalyzer::DeclareFunction( std::string identifier,
                                    CompleteType return_type,
                                    Semantic semantic,
                                    std::vector<CompleteType> parameter_types )
{
    std::vector<Function_sp>& function_overloads =
                                                m_FunctionOverloads[identifier];

    for( const auto& f : function_overloads )
        if( f->HasSameParameterTypes( parameter_types ) )
        {
            // If we already have this funciton, make sure that we have the same
            // return type
            if( f->GetReturnType().GetBaseType() != return_type.GetBaseType() ||
                f->GetReturnType().GetArrayExtents() !=
                                                 return_type.GetArrayExtents() )
                Error( "Functions differ only by return type " + identifier );
            return;
        }

    function_overloads.push_back( std::make_shared<Function>(
                                                 identifier,
                                                 std::move(return_type),
                                                 std::move(semantic),
                                                 std::move(parameter_types) ) );
    return;
}

void SemaAnalyzer::DefineFunction(
                         const std::string& identifier,
                         const std::vector<Variable_sp>& parameters,
                         CompoundStatement_up definition )
{
    const auto& i = m_FunctionOverloads.find( identifier );
    assert( i != m_FunctionOverloads.end() &&
            "Couldn't find function to define" );
    std::vector<CompleteType> parameter_types( parameters.size() );
    std::transform( parameters.begin(),
                    parameters.end(),
                    parameter_types.begin(),
                    [](const Variable_sp& v){return v->GetType();} );
    Function_sp function;
    for( const auto& f : i->second )
        if( f->HasSameParameterTypes( parameter_types ) )
        {
            function = f;
            break;
        }
    assert( function && "Couldn't find function overload to define" );
    if( function->HasDefinition() )
        Error( "Redefinition of function " + function->GetSignatureString() );
    else
    {
        function->SetDefinition( std::move(definition ) );
        function->SetParameters( parameters );
    }
}

bool SemaAnalyzer::HasFunctionNamed( const std::string& identifier ) const
{
    return m_FunctionOverloads.find( identifier ) != m_FunctionOverloads.end();
}

namespace
{
/**
  * If none match, this will keep them all
  */
void KeepOnlyMatching( std::vector<Function_sp>& functions,
                       std::function<bool(const Function_sp&)> filter )
{
    bool any_match = false;
    for( const auto& f : functions )
        if( filter( f ) )
        {
            any_match = true;
            break;
        }

    if( !any_match )
        return;

    auto doesnt_match = [&filter](const Function_sp& f)
                        {return !filter(f);};
    functions.erase( std::remove_if( functions.begin(),
                                     functions.end(),
                                     doesnt_match ),
                     functions.end() );
}
}

Function_sp SemaAnalyzer::GetFunctionOverload(
                          const std::string& identifier,
                          const std::vector<CompleteType> argument_types )
{
    const auto& f = m_FunctionOverloads.find( identifier );
    if( f == m_FunctionOverloads.end() )
        return nullptr;

    std::vector<Function_sp> potential_functions = f->second;
    assert( !potential_functions.empty() &&
            "No overloads for a function name" );

    //
    // Remove all functions that have the wrong number of parameters (except for
    // default parameters)
    //
    auto has_different_num_args =
        [&argument_types]( const Function_sp& f )
        {
            // todo default arguments
            return f->GetParameterTypes().size() != argument_types.size();
        };
    potential_functions.erase( std::remove_if( potential_functions.begin(),
                                               potential_functions.end(),
                                               has_different_num_args ),
                               potential_functions.end() );

    if( potential_functions.empty() )
        return nullptr;

    for( unsigned i = 0; i < argument_types.size(); ++i )
    {
        //
        // for each parameter
        //

        auto matches_exactly =
                [i,&argument_types]( const Function_sp& f )
                {
                    return f->GetParameterTypes()[i] == argument_types[i];
                };

        KeepOnlyMatching( potential_functions, matches_exactly );

        //
        // Todo unsized arrays here
        //

        auto has_promotion =
                [i,&argument_types]( const Function_sp& f )
                {
                    return GetCommonType( argument_types[i],
                                          f->GetParameterTypes()[i] )
                                                   == f->GetParameterTypes()[i];
                };

        KeepOnlyMatching( potential_functions, has_promotion );

        //
        // Todo implicit conversion here
        //
    }

    if( potential_functions.size() > 1 )
    {
        /// todo print potential matches
        Error( "Ambiguous function call" );
        return nullptr;
    }

    if( potential_functions.empty() )
        return nullptr;

    return potential_functions[0];
}

std::vector<Function_sp> SemaAnalyzer::GetFunctions() const
{
    std::vector<Function_sp> ret;
    for( const auto& i : m_FunctionOverloads )
        ret.insert( ret.end(), i.second.begin(), i.second.end() );
    return ret;
}

void SemaAnalyzer::EnterScope()
{
    m_SymbolStack.resize( m_SymbolStack.size() + 1 );
}

void SemaAnalyzer::LeaveScope()
{
    m_SymbolStack.pop_back();

    //
    // If we had any state enumerants here remove them
    //
    m_StateEnumerants = nullptr;
}

bool SemaAnalyzer::InGlobalScope() const
{
    return m_SymbolStack.size() == 1;
}

GenericValue SemaAnalyzer::EvaluateExpression( const Expression& expression )
{
    return m_CodeGenerator.EvaluateExpression( expression );
}

GenericValue SemaAnalyzer::EvaluateInitializer( const Initializer& initializer )
{
    if( initializer.IsExpression() )
        return EvaluateExpression( initializer.GetExpression() );
    else
    {
        const std::vector<std::unique_ptr<Initializer> >& sub_initializers =
                                               initializer.GetSubInitializers();
        std::vector<GenericValue> sub_values;
        sub_values.reserve( sub_initializers.size() );
        for( const auto& sub_initializer : sub_initializers )
            sub_values.push_back( EvaluateInitializer( *sub_initializer ) );
        return GenericValue( sub_values );
    }
}

void SemaAnalyzer::Error( const std::string& error_message )
{
    m_Good = false;
    m_Errors.push_back( "Error during semantic analysis: " + error_message );
}

void SemaAnalyzer::Warning( const std::string& warning_message)
{
    m_Warnings.push_back( "Warning during semantic analysis: "
                        + warning_message );
}

bool SemaAnalyzer::Good() const
{
    return m_Good;
}

const std::vector<std::string>& SemaAnalyzer::GetErrors() const
{
    return m_Errors;
}

const std::vector<std::string>& SemaAnalyzer::GetWarnings() const
{
    return m_Warnings;
}

CodeGenerator& SemaAnalyzer::GetCodeGenerator()
{
    return m_CodeGenerator;
}

SemaAnalyzer::ScopeHolder::ScopeHolder( SemaAnalyzer& sema )
    :m_Sema( sema )
    ,m_InScope( false )
{
}

SemaAnalyzer::ScopeHolder::~ScopeHolder()
{
    if( m_InScope )
        Leave();
}

void SemaAnalyzer::ScopeHolder::Enter()
{
    assert( !m_InScope && "Entering a scope twice" );
    m_Sema.EnterScope();
    m_InScope = true;
}

void SemaAnalyzer::ScopeHolder::Leave()
{
    m_Sema.LeaveScope();
    m_InScope = false;
}

} // namespace Compiler
} // namespace JoeLang
