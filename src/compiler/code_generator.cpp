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

#include "code_generator.hpp"

#include <cassert>
#include <memory>
#include <stack>
#include <string>
#include <utility>
#include <vector>

#include <x86intrin.h>

#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <joelang/context.hpp>
#include <joelang/parameter.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/technique.hpp>
#include <compiler/casting.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/function.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/runtime.hpp>
#include <compiler/swizzle.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/expressions/assignment_expression.hpp>
#include <compiler/tokens/expressions/postfix_operator.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <runtime/types.hpp>

namespace JoeLang
{
namespace Compiler
{

CodeGenerator::CodeGenerator( Runtime& runtime )
    :m_Runtime( runtime )
    ,m_Builder( m_Runtime.GetLLVMContext() )
    ,m_ExecutionEngine( runtime.GetExecutionEngine() )
    ,m_Module( runtime.GetModule() )
{
}

CodeGenerator::~CodeGenerator()
{
}

const Runtime& CodeGenerator::GetRuntime() const
{
    return m_Runtime;
}

void CodeGenerator::GenerateFunctions( std::set<Function_sp>& functions )
{
    //
    // remove any functions which have already been codegened
    //
    for( auto it = functions.begin(); it != functions.end(); )
        if( (*it)->HasLLVMFunction() )
            functions.erase( it++ );
        else
            ++it;

    //
    // First, declare all the functions
    //
    for( auto& f : functions )
        f->CodeGenDeclaration( *this );

    //
    // Then define them
    //
    for( auto& f : functions )
        f->CodeGenDefinition( *this );
}

std::vector<Technique> CodeGenerator::GenerateTechniques(
            const std::vector<TechniqueDeclaration_up>& technique_declarations )
{
    std::vector<Technique> techniques;
    for( const auto& technique_declaration : technique_declarations )
    {
        techniques.push_back(
                            technique_declaration->GenerateTechnique( *this ) );
    }

    return std::move(techniques);
}

std::vector<ParameterBase_up> CodeGenerator::GenerateParameters(
                             const std::vector<Variable_sp>& uniform_variables )
{
    std::vector<ParameterBase_up> parameters;

    for( const auto& uniform : uniform_variables )
    {
        assert( uniform->IsUniform() && "Trying to generate a parameter for a "
                "non-uniform variable" );
        assert( uniform->GetType().GetArrayExtents().empty() &&
                "Trying to create a parameter for an array" );
        assert( uniform->GetLLVMPointer() &&
                "Uniform doesn't have a llvm value" );
        assert( llvm::isa<llvm::GlobalValue>(uniform->GetLLVMPointer()) &&
                "Uniform doesn't have a llvm global value" );

        Type type = uniform->GetType().GetBaseType();
        llvm::GlobalValue* gv = cast<llvm::GlobalValue>(
                                                    uniform->GetLLVMPointer() );
        assert( gv->getType()->isValidElementType(
                                m_Runtime.GetLLVMType( uniform->GetType() ) ) &&
                "Trying to create a parameter with the wrong llvm type" );

#define CREATE_TYPED_PARAM(type) \
        case JoeLangType<type>::value: \
           parameters.emplace_back( new Parameter<type>( \
                    uniform->GetName(), \
                    *(static_cast<type*>( \
                           m_ExecutionEngine.getPointerToGlobal( gv ) ) ) ) ); \
            break

#define CREATE_TYPED_PARAM_N(type) \
        CREATE_TYPED_PARAM(type); \
        CREATE_TYPED_PARAM(type##2); \
        CREATE_TYPED_PARAM(type##3); \
        CREATE_TYPED_PARAM(type##4); \
        CREATE_TYPED_PARAM(type##2x2); \
        CREATE_TYPED_PARAM(type##2x3); \
        CREATE_TYPED_PARAM(type##2x4); \
        CREATE_TYPED_PARAM(type##3x2); \
        CREATE_TYPED_PARAM(type##3x3); \
        CREATE_TYPED_PARAM(type##3x4); \
        CREATE_TYPED_PARAM(type##4x2); \
        CREATE_TYPED_PARAM(type##4x3); \
        CREATE_TYPED_PARAM(type##4x4);

        switch( type )
        {
        CREATE_TYPED_PARAM_N(jl_bool)
        CREATE_TYPED_PARAM_N(jl_char)
        CREATE_TYPED_PARAM_N(jl_short)
        CREATE_TYPED_PARAM_N(jl_int)
        CREATE_TYPED_PARAM_N(jl_long)
        CREATE_TYPED_PARAM_N(jl_uchar)
        CREATE_TYPED_PARAM_N(jl_ushort)
        CREATE_TYPED_PARAM_N(jl_uint)
        CREATE_TYPED_PARAM_N(jl_ulong)
        CREATE_TYPED_PARAM_N(jl_float)
        CREATE_TYPED_PARAM_N(jl_double)
        default:
            assert( false &&
                    "Trying to create a parameter for an unhandled type" );
            break;
        }
    }

#undef CREATE_TYPED_PARAM_N
#undef CREATE_TYPED_PARAM

    return parameters;
}

llvm::Function* CodeGenerator::WrapExpressionCommon(
                                                   const Expression& expression,
                                                   bool has_simple_return )
{
    /// TODO assigning arrays

    //
    // Generate all the functions this may rely on first
    //
    std::set<Function_sp> function_dependencies;
    for( const auto& f : expression.GetCallees() )
    {
        bool recursion;
        auto d = f->GetFunctionDependencies( recursion );
        function_dependencies.insert( d.begin(), d.end() );
        function_dependencies.insert( f );
    }
    GenerateFunctions( function_dependencies );


    llvm::Type* return_type = m_Runtime.GetLLVMType( expression.GetType() );

    llvm::FunctionType* prototype;

    if( has_simple_return )
    {
        //
        // create a function prototype which takes no arguments!
        //
        prototype = llvm::FunctionType::get( return_type,
                                             {},
                                             false );
    }
    else
    {
        //
        // The function takes a pointer to some memory in which to put the
        // return value
        //
        prototype = llvm::FunctionType::get(
                            llvm::Type::getVoidTy( m_Runtime.GetLLVMContext() ),
                            { return_type->getPointerTo() },
                            false );
    }
    assert( prototype && "Error generating empty function prototype" );

    //
    // Create an anonymous function
    //
    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                llvm::Function::ExternalLinkage,
                                                "",
                                                &m_Module );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create(
                                                    m_Runtime.GetLLVMContext(),
                                                    "entry",
                                                    function );

    auto old_insert_point = m_Builder.saveAndClearIP();

    m_Builder.SetInsertPoint( body );

    //
    // Generate the code from the expression
    //
    llvm::Value* v = expression.CodeGen( *this );
    /// TODO arrays of strings
    if( expression.GetType().GetType() == Type::STRING )
    {
        assert( !expression.GetType().IsArrayType() &&
                "Todo arrays of strings" );
        v = m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_COPY,
                                         {v},
                                         m_Builder );
    }
    CreateDestroyTemporaryCalls();

    assert( v && "Invalid expression llvm::Value*" );
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    if( has_simple_return )
    {
        //
        // Set it as the return value for the function
        //
        m_Builder.CreateRet( v );
    }
    else
    {
        assert( function->arg_size() == 1 &&
                "This isn't a pointer return function" );

        m_Builder.CreateStore( v, function->arg_begin() );
        m_Builder.CreateRetVoid();
    }

    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function not valid" );

    m_Builder.restoreIP( old_insert_point );

    return function;
}

std::unique_ptr<StateAssignmentBase> CodeGenerator::GenerateStateAssignment(
        const StateBase& state,
        const Expression& expression,
        const std::string& name )
{
    /// TODO assigning arrays
    assert( expression.GetType().GetType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );

    llvm::Function* function = nullptr;

    std::unique_ptr<StateAssignmentBase> sa;

#define CREATE_STATE_ASSIGNMENT( type ) \
    case JoeLangType<type>::value: \
        sa.reset( new StateAssignment<type>( \
            static_cast<const State<type>&>(state), \
            WrapExpression<type>( expression, function ) ) ); \
        break;

#define CREATE_STATE_ASSIGNMENT_N( type ) \
    CREATE_STATE_ASSIGNMENT( type ) \
    CREATE_STATE_ASSIGNMENT( type##2 ) \
    CREATE_STATE_ASSIGNMENT( type##3 ) \
    CREATE_STATE_ASSIGNMENT( type##4 ) \
    CREATE_STATE_ASSIGNMENT( type##2x2 ) \
    CREATE_STATE_ASSIGNMENT( type##2x3 ) \
    CREATE_STATE_ASSIGNMENT( type##2x4 ) \
    CREATE_STATE_ASSIGNMENT( type##3x2 ) \
    CREATE_STATE_ASSIGNMENT( type##3x3 ) \
    CREATE_STATE_ASSIGNMENT( type##3x4 ) \
    CREATE_STATE_ASSIGNMENT( type##4x2 ) \
    CREATE_STATE_ASSIGNMENT( type##4x3 ) \
    CREATE_STATE_ASSIGNMENT( type##4x4 )

    switch( state.GetType() )
    {
    CREATE_STATE_ASSIGNMENT_N( jl_bool )
    CREATE_STATE_ASSIGNMENT_N( jl_char )
    CREATE_STATE_ASSIGNMENT_N( jl_short )
    CREATE_STATE_ASSIGNMENT_N( jl_int )
    CREATE_STATE_ASSIGNMENT_N( jl_long )
    CREATE_STATE_ASSIGNMENT_N( jl_uchar )
    CREATE_STATE_ASSIGNMENT_N( jl_ushort )
    CREATE_STATE_ASSIGNMENT_N( jl_uint )
    CREATE_STATE_ASSIGNMENT_N( jl_ulong )
    CREATE_STATE_ASSIGNMENT_N( jl_float )
    CREATE_STATE_ASSIGNMENT_N( jl_double )
    case Type::STRING:
        sa.reset( new StateAssignment<std::string>(
            static_cast<const State<std::string>&>(state),
            WrapStringExpression( expression, function ) ) );
        break;
    default:
        assert( false && "Generating a stateassignment of unhandled type" );
    }

    function->setName( name );
    function->setLinkage( llvm::Function::ExternalLinkage );

    return sa;

#undef CREATE_STATE_ASSIGNMENT_N
#undef CREATE_STATE_ASSIGNMENT
}

GenericValue CodeGenerator::EvaluateExpression( const Expression& expression )
{
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    llvm::Function* function;

#define GET_VALUE( type ) \
    case JoeLangType<type>::value: \
        ret = GenericValue( WrapExpression<type>( expression, function )() ); \
        break;

#define GET_VALUE_N( type ) \
    GET_VALUE( type ) \
    GET_VALUE( type##2 ) \
    GET_VALUE( type##3 ) \
    GET_VALUE( type##4 ) \
    GET_VALUE( type##2x2 ) \
    GET_VALUE( type##2x3 ) \
    GET_VALUE( type##2x4 ) \
    GET_VALUE( type##3x2 ) \
    GET_VALUE( type##3x3 ) \
    GET_VALUE( type##3x4 ) \
    GET_VALUE( type##4x2 ) \
    GET_VALUE( type##4x3 ) \
    GET_VALUE( type##4x4 )

    //
    // Extract the result
    //
    GenericValue ret;
    switch( expression.GetType().GetType() )
    {
    GET_VALUE_N( jl_bool )
    GET_VALUE_N( jl_char )
    GET_VALUE_N( jl_short )
    GET_VALUE_N( jl_int )
    GET_VALUE_N( jl_long )
    GET_VALUE_N( jl_uchar )
    GET_VALUE_N( jl_ushort )
    GET_VALUE_N( jl_uint )
    GET_VALUE_N( jl_ulong )
    GET_VALUE_N( jl_float )
    GET_VALUE_N( jl_double )
    case Type::STRING:
        ret = GenericValue( WrapStringExpression( expression, function )() );
        break;
    default:
        assert( false &&
                "Trying to run a function returning an unhandled type" );
    }

#undef GET_VALUE_N
#undef GET_VALUE

    m_ExecutionEngine.freeMachineCodeForFunction( function );
    function->eraseFromParent();

    return ret;
}

//
// Type Construction
//

llvm::Value* CodeGenerator::CreateMatrixConstructor(
                                   Type type,
                                   const std::vector<Expression_up>& arguments )
{
#if !defined(NDEBUG)
    for( const auto& argument : arguments )
        assert( argument && "CreateMatrixTypeConstructor given null argument" );
    assert( IsMatrixType( type ) &&
            "CreateMatrixTypeConstructor given non-vector type" );
#endif

    std::vector<llvm::Value*> values;
    values.reserve( arguments.size() );
    for( const Expression_up& a : arguments )
        values.push_back( a->CodeGen(*this) );

    return m_Runtime.CreateDeepCopy( values,
                                     m_Runtime.GetLLVMType( type ),
                                     m_Builder );
}

llvm::Value* CodeGenerator::CreateVectorConstructor(
                                   Type type,
                                   const std::vector<Expression_up>& arguments )
{
#if !defined(NDEBUG)
    for( const auto& argument : arguments )
        assert( argument && "CreateVectorTypeConstructor given null argument" );
    assert( IsVectorType( type ) &&
            "CreateVectorTypeConstructor given non-vector type" );
#endif

    std::vector<llvm::Value*> values;
    values.reserve( arguments.size() );
    for( const Expression_up& a : arguments )
        values.push_back( a->CodeGen(*this) );

    return m_Runtime.CreateDeepCopy( values,
                                     m_Runtime.GetLLVMType( type ),
                                     m_Builder );
}

llvm::Value* CodeGenerator::CreateScalarConstructor(
                                                    Type type,
                                                    const Expression& argument )
{
    return argument.CodeGen( *this );
}

//
// Swizzle operators
//

llvm::Value* CodeGenerator::CreateSwizzle( const Expression& e,
                                           Swizzle swizzle )
{
#ifndef NDEBUG
    unsigned e_vector_size = e.GetType().GetVectorSize();
    for( unsigned i = 0; i < swizzle.GetSize(); ++i )
        assert( swizzle.GetIndex(i) >= 0 && swizzle.GetIndex(i) < e_vector_size
                && "swizzle index out of bounds" );
#endif
    //
    //
    //

    llvm::Value* e_value = e.CodeGen( *this );

    return SwizzleValue( e.GetType(), e_value, swizzle );
}

llvm::Value* CodeGenerator::SwizzleValue( const CompleteType& type,
                                          llvm::Value* value,
                                          Swizzle swizzle )
{
    assert( value->getType() == m_Runtime.GetLLVMType(type) && "Type mismatch");
    //
    // If we only have one swizzle index we want to extract an element
    //
    if( swizzle.GetSize() == 1 )
    {
        //
        // if e is already a scalar type (the expression was myscalar.x)
        // just return the scalar
        //
        if( type.IsScalarType() )
            return value;

        //
        // Otherwise extract the element
        //
        return m_Builder.CreateExtractElement(
                    value,
                    llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ),
                                            swizzle.GetIndex(0) ) );
    }

    //
    // We are swizzling and returning a vector
    //

    //
    // If this isn't a vector, package it into one
    //
    if( type.IsScalarType() )
        value = m_Builder.CreateInsertElement(
            llvm::UndefValue::get( llvm::VectorType::get( value->getType(),
                                                          1 ) ),
            value,
            CreateInteger( 0, Type::INT ) );

    std::vector<unsigned> indices( swizzle.begin(), swizzle.end() );

    return m_Builder.CreateShuffleVector(
                    value,
                    llvm::UndefValue::get( value->getType() ),
                    llvm::ConstantDataVector::get( m_Runtime.GetLLVMContext(),
                                                   indices ) );

}

//
// Cast Operators
//

llvm::Value* CodeGenerator::CreateCast( const Expression& e,
                                        const CompleteType& to_type )
{
    const CompleteType& from_type = e.GetType();
    assert( !from_type.IsUnknown() && !to_type.IsUnknown() &&
            "Can't cast an unknown type" );

    if( to_type == from_type )
        return e.CodeGen( *this );

    if( to_type.IsScalarType() )
        return CreateCastToScalar( e, to_type );
    if( to_type.IsVectorType() )
        return CreateCastToVector( e, to_type );

    assert( false && "Trying to cast to and unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateScalarOrVectorCast(
                                                llvm::Value* e_value,
                                                const CompleteType& from_type,
                                                const CompleteType& to_type )
{
    assert( to_type.GetVectorSize() == to_type.GetVectorSize() &&
            "Trying to cast different sized types" );

    //
    // for a cast to bool, compare to zero
    //
    if( GetScalarType( to_type.GetType() ) == Type::BOOL )
    {
        if( from_type.IsFloatingPoint() )
            return m_Builder.CreateFCmpOEQ(
                     e_value,
                     llvm::ConstantFP::getNullValue( e_value->getType() ) );
        return m_Builder.CreateIsNotNull( e_value );
    }

    if( to_type.IsFloatingPoint() )
    {
        if( from_type.IsFloatingPoint() )
            return m_Builder.CreateFPCast( e_value,
                                           m_Runtime.GetLLVMType( to_type ) );
        else if( from_type.IsSigned() )
            return m_Builder.CreateSIToFP( e_value,
                                           m_Runtime.GetLLVMType( to_type ) );
        else
        {
            assert( from_type.IsIntegral() && !from_type.IsSigned() &&
                    "we've let a bad type through" );
            return m_Builder.CreateUIToFP( e_value,
                                           m_Runtime.GetLLVMType( to_type ) );
        }
    }

    assert( to_type.IsIntegral() && "Type should be integral" );
    if( from_type.IsIntegral() )
    {
        return m_Builder.CreateIntCast( e_value,
                                        m_Runtime.GetLLVMType( to_type ),
                                        from_type.IsSigned() );
    }

    assert( from_type.IsFloatingPoint() &&
            "from_type should be floating point" );
    if( to_type.IsSigned() )
        return m_Builder.CreateFPToSI( e_value,
                                       m_Runtime.GetLLVMType( to_type ) );
    else
        return m_Builder.CreateFPToUI( e_value,
                                       m_Runtime.GetLLVMType( to_type ) );
}

llvm::Value* CodeGenerator::CreateCastToScalar( const Expression& expression,
                                                const CompleteType& to_type )
{
    CompleteType from_type = expression.GetType();
    llvm::Value* e_value = expression.CodeGen( *this );
    assert( e_value->getType() == m_Runtime.GetLLVMType( from_type ) &&
            "Type mismatch" );

    //
    // If we're casting from a vector type extract the first element and then
    // cast as if we're casting from a scalar
    //
    if( from_type.IsVectorType() )
    {
        e_value = m_Builder.CreateExtractElement(
                                                e_value,
                                                CreateInteger( 0, Type::INT ) );
        from_type = CompleteType( from_type.GetVectorElementType() );
    }

    if( from_type.IsScalarType() )
    {
        return CreateScalarOrVectorCast( e_value, from_type, to_type );
    }

    assert( false && "Trying to cast from an unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateCastToVector( const Expression& expression,
                                                const CompleteType& to_type )
{
    assert( to_type.IsVectorType() &&
            "Trying to create a vector cast to a non-vector type" );

    CompleteType from_type = expression.GetType();
    //llvm::Value* e_value = expression.CodeGen( *this );

    if( from_type.IsScalarType() )
    {
        //
        // If we have a scalar type then cast it to the right element type and
        //  splat it
        //
        llvm::Value* e_value = CreateCastToScalar(
                               expression,
                               CompleteType( to_type.GetVectorElementType() ) );
        return m_Builder.CreateVectorSplat( to_type.GetVectorSize(), e_value );
    }

    if( from_type.IsVectorType() )
    {
        llvm::Value* e_value = expression.CodeGen( *this );

        if( from_type.GetVectorSize() != to_type.GetVectorSize() )
        {
            //
            // They are not the same size
            //
            assert( from_type.GetVectorSize() > to_type.GetVectorSize() &&
                    "Trying to cast a smaller vector into a bigger one" );

            //
            // we can do this with a shuffle
            //

            std::vector<unsigned> indices( to_type.GetVectorSize() );

            for( unsigned i = 0; i < to_type.GetVectorSize(); ++i )
                indices[i] = i;

            e_value = m_Builder.CreateShuffleVector(
                      e_value,
                      llvm::UndefValue::get( e_value->getType() ),
                      llvm::ConstantDataVector::get( m_Runtime.GetLLVMContext(),
                                                     indices ) );
        }

        return CreateScalarOrVectorCast( e_value,
                                         from_type,
                                         to_type );
    }

    assert( false && "Trying to cas from an unhandled type" );
    return nullptr;
}

//
// Unary Operators
//


llvm::Value* CodeGenerator::CreateNeg( const Expression& e )
{
    if( e.GetType().IsFloatingPoint() )
        return m_Builder.CreateFNeg( e.CodeGen( *this ) );
    else
        return m_Builder.CreateNeg( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNot( const Expression& e )
{
    return m_Builder.CreateNot( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLNot( const Expression& e )
{
    return m_Builder.CreateIsNotNull( e.CodeGen( *this ) );
}

//
// Binary Operators
// Todo typing for all of these
//

llvm::Value* CodeGenerator::CreateLOr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateOr( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l,
                                        const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateAnd( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateOr( l.CodeGen( *this ),
                               r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsIntegral() )
        return m_Builder.CreateICmpEQ( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFCmpOEQ( l.CodeGen( *this ),
                                            r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_EQUAL,
                                            {l.CodeGen( *this ),
                                             r.CodeGen( *this )},
                                            m_Builder );
    assert( false && "Trying to compare unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateNeq( const Expression& l,
                                       const Expression& r )
{
    if( l.GetType().IsIntegral() )
        return m_Builder.CreateICmpNE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFCmpONE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_NOTEQUAL,
                                            {l.CodeGen( *this ),
                                             r.CodeGen( *this )},
                                            m_Builder );
    assert( false && "Trying to compare unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateLT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_Builder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFAdd( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else if( l.GetType().IsIntegral() )
        return m_Builder.CreateAdd( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
    {
        llvm::Value* ret = m_Runtime.CreateRuntimeCall(
                                                RuntimeFunction::STRING_CONCAT,
                                                {l.CodeGen( *this ),
                                                 r.CodeGen( *this )},
                                                m_Builder );
        m_Temporaries.push( ret );
        return ret;
    }

    assert( false && "Trying to Add unhandled types" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateSub( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFSub( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_Builder.CreateSub( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFMul( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_Builder.CreateMul( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_Builder.CreateFDiv( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        if( l.GetType().IsSigned() )
            return m_Builder.CreateSDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
        else
            return m_Builder.CreateUDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsSigned() )
        return m_Builder.CreateSRem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_Builder.CreateURem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
}

//
// Ternary Operators
//

llvm::Value* CodeGenerator::CreateSelect( const Expression& condition,
                                          const Expression& true_expression,
                                          const Expression& false_expression )
{
    return m_Builder.CreateSelect( condition.CodeGen( *this ),
                                       true_expression.CodeGen( *this ),
                                       false_expression.CodeGen( *this ) );

}

//
// Other things
//

llvm::Value* CodeGenerator::CreateArrayIndex( const Expression& array,
                                              const Expression& index )
{
    llvm::Value* ptr = CreateArrayIndexPointerTo( array, index );
    return m_Builder.CreateLoad( ptr );
}

llvm::Value* CodeGenerator::CreateArrayIndexPointerTo( const Expression& array,
                                                       const Expression& index )
{
    llvm::Value* array_ptr = m_Builder.CreateConstGEP2_32(
                                    array.CodeGenPointerTo( *this ),
                                    0, 0 );
    return m_Builder.CreateGEP( array_ptr, index.CodeGen( *this ) );
}

//
// Statements
//

void CodeGenerator::CreateReturnStatement( const Expression_up& expression )
{
    if( expression )
    {
        llvm::Value* v = expression->CodeGen( *this );
        if( expression->GetType().IsVoid() )
            m_Builder.CreateRetVoid();
        else
            m_Builder.CreateRet( v );
    }
    else
    {
        m_Builder.CreateRetVoid();
    }
}

//
// Constants
//
llvm::Constant* CodeGenerator::CreateInteger( jl_ulong value,
                                              Type type )
{
    assert( IsIntegral( type ) &&
            "Trying to create an integer constant of non-integer type" );
    return llvm::ConstantInt::get( m_Runtime.GetLLVMType( type ),
                                   value,
                                   IsSigned( type ) );
}

llvm::Constant* CodeGenerator::CreateIntegerVector(
                                             const std::vector<jl_ulong>& value,
                                             Type type )
{
    assert( IsIntegral( type ) &&
            "Trying to create a integer constant of non-integer type" );
    assert( IsVectorType( type ) &&
            "Trying to create a vector constant of non-vector type" );
    assert( value.size() == GetNumElementsInType( type ) &&
            "Wrong number of values to construct vector constant" );
    std::vector<llvm::Constant*> data;
    data.reserve( value.size() );
    for( double d : value )
        data.push_back( CreateInteger( d, GetScalarType( type ) ) );
    auto ret = llvm::ConstantVector::get( data );
    assert( ret->getType() == m_Runtime.GetLLVMType( type ) &&
            "Created a wrong type" ) ;
    return ret;
}

llvm::Constant* CodeGenerator::CreateFloating( double value, Type type )
{
    assert( IsFloatingPoint( type ) &&
            "Trying to create a floating constant of non-floating type" );
    return llvm::ConstantFP::get( m_Runtime.GetLLVMType( type ),
                                  value );
}

llvm::Constant* CodeGenerator::CreateFloatingVector(
                                               const std::vector<double>& value,
                                               Type type )
{
    assert( IsFloatingPoint( type ) &&
            "Trying to create a floating constant of non-floating type" );
    assert( IsVectorType( type ) &&
            "Trying to create a vector constant of non-vector type" );
    assert( value.size() == GetNumElementsInType( type ) &&
            "Wrong number of values to construct vector constant" );
    std::vector<llvm::Constant*> data;
    data.reserve( value.size() );
    for( double d : value )
        data.push_back( CreateFloating( d, GetScalarType( type ) ) );
    auto ret = llvm::ConstantVector::get( data );
    assert( ret->getType() == m_Runtime.GetLLVMType( type ) &&
            "Created a wrong type" ) ;
    return ret;
}

llvm::Constant* CodeGenerator::CreateString( const std::string& value )
{
    /// TODO use CreateGlobalString here
    //
    // Set up the globalvariable holding the characters in an array
    //

    // u32 here because that's what's used in string for size
    llvm::Constant* size_constant = CreateInteger( value.size(), Type::UINT );

    llvm::ArrayType* array_type = llvm::ArrayType::get(
                                         m_Runtime.GetLLVMType( Type::UCHAR ),
                                         value.size() );

    std::vector<llvm::Constant*> characters;
    for( char c : value )
        characters.push_back( CreateInteger( c, Type::UCHAR ) );

    llvm::Constant* data_constant = llvm::ConstantArray::get( array_type,
                                                              characters );

    llvm::GlobalVariable* data_array = new llvm::GlobalVariable(
                                           m_Module,
                                           array_type,
                                           true,
                                           llvm::GlobalVariable::PrivateLinkage,
                                           data_constant,
                                           "string_data" );

    llvm::Constant* zero = CreateInteger( 0, Type::UINT );
    llvm::Constant* args[] = { zero, zero };
    llvm::Constant* data_ptr =
                    llvm::ConstantExpr::getInBoundsGetElementPtr( data_array,
                                                                  args );

    llvm::Constant* string = llvm::ConstantStruct::get(
          llvm::cast<llvm::StructType>(m_Runtime.GetLLVMType(Type::STRING)),
          std::vector<llvm::Constant*> {size_constant, data_ptr} );
    return string;
}

llvm::Constant* CodeGenerator::CreateArray(
                                        const std::vector<GenericValue>& value )
{
    assert( !value.empty() &&
            "Trying to create an array constant of zero size" );
    std::vector<llvm::Constant*> array_data;
    array_data.reserve( value.size() );
    for( const auto& g : value )
        array_data.push_back( g.CodeGen( *this ) );

    llvm::ArrayType* type = llvm::ArrayType::get( array_data[0]->getType(),
                                                  value.size() );

    return llvm::ConstantArray::get( type, array_data );
}

//
// Variables
//

llvm::GlobalVariable* CodeGenerator::CreateGlobalVariable(
                                const CompleteType& type,
                                bool is_const,
                                bool is_uniform,
                                const GenericValue& initializer,
                                const std::string& name )
{
    assert( ( initializer.GetType().IsUnknown() ||
              initializer.GetType() == type ) &&
            "Initializer type mismatch" );

    llvm::Type* t = m_Runtime.GetLLVMType( type );
    llvm::Constant* init;
    if( !initializer.GetType().IsUnknown() )
        init = initializer.CodeGen( *this );
    else
        init = llvm::Constant::getNullValue( t );

    auto linkage = is_uniform ? llvm::GlobalVariable::ExternalLinkage :
                                llvm::GlobalVariable::PrivateLinkage;

    llvm::GlobalVariable* ret = new llvm::GlobalVariable(
                                     m_Module,
                                     t,
                                     is_const && !is_uniform, //If something is
                                                              // uniform, it may
                                                              // be changed from
                                                              // outside
                                     linkage,
                                     init,
                                     name );
    //
    // The adderss of non const variables is important because we can modify
    // them from outside
    //
    ret->setUnnamedAddr( is_const );
    return ret;
}

llvm::Value* CodeGenerator::CreateVariableRead( const Variable& variable )
{
    // If this is a const param, we can use the value it was passed with,
    // otherwise it will have been alloc
    return m_Builder.CreateLoad( variable.GetLLVMPointer() );
}

//
// todo class for swizzle
//
llvm::Value* CodeGenerator::CreateAssignment( const Expression& variable,
                                              const Expression& e,
                                              Swizzle swizzle,
                                              AssignmentOperator op )
{
    assert( variable.IsLValue() &&
            "Trying to codegen an assignment to a RValue" );
    assert( !variable.IsConst() &&
            "Trying to codegen an assignment to a const variable" );
    assert( variable.GetType() == e.GetType() &&
            "Trying to assign a variable with a different type" );

    llvm::Value* assigned_value = e.CodeGen( *this );
    llvm::Value* pointer        = variable.CodeGenPointerTo( *this );
    llvm::Value* ret            = assigned_value;

    if( op != AssignmentOperator::EQUALS )
    {
        //
        // If we're not just assigning, create a new assigned value from
        // the old one
        //
        llvm::Value* assignee_value = m_Builder.CreateLoad( pointer );
        if( swizzle.IsValid() )
            assignee_value = SwizzleValue( variable.GetType(),
                                           assignee_value,
                                           swizzle );

        switch( op )
        {
        case AssignmentOperator::PLUS_EQUALS:
            assigned_value = m_Builder.CreateAdd( assignee_value,
                                                  assigned_value );
            break;
        default:
            assert( false &&
                    "Trying to codegen an unhandled assignment operator" );
        }
    }

    if( swizzle.IsValid() )
    {
        //
        // We are storing with a swizzle mask
        //
        llvm::Value* assignee_value = m_Builder.CreateLoad( pointer );

        assert( e.GetType().GetVectorSize() == swizzle.GetSize() &&
                "Assigning to a different sized vector type" );
        for( unsigned i = 0; i < e.GetType().GetVectorSize(); ++i )
        {
            llvm::Value* element;
            if( e.GetType().IsScalarType() )
                element = assigned_value;
            else
                element = m_Builder.CreateExtractElement(
                                                 assigned_value,
                                                 CreateInteger( i, Type::INT) );
            assignee_value = m_Builder.CreateInsertElement(
                               assignee_value,
                               element,
                               CreateInteger( swizzle.GetIndex(i), Type::INT) );
        }

        assigned_value = assignee_value;
    }

    m_Builder.CreateStore( assigned_value, pointer );

    return ret;
}

llvm::Function* CodeGenerator::CreateFunctionDeclaration(
                              const std::string& identifier,
                              const CompleteType& return_type,
                              const std::vector<CompleteType>& parameter_types )
{
    llvm::Type* llvm_return_type = m_Runtime.GetLLVMType( return_type );
    std::vector<llvm::Type*> llvm_parameter_types;
    llvm_parameter_types.reserve( parameter_types.size() );
    for( const auto& p : parameter_types )
        llvm_parameter_types.push_back( m_Runtime.GetLLVMType( p ) );

#if !defined(NDEBUG)
    assert( llvm_return_type && "Couldn't get llvm return type" );
    for( auto& t : llvm_parameter_types )
        assert( t && "Couldn't get llvm parameter type" );
#endif

    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                                         llvm_return_type,
                                                         llvm_parameter_types,
                                                         false );
    assert( prototype && "Error generating function prototype" );

    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                llvm::Function::ExternalLinkage,
                                                identifier,
                                                &m_Module );
    assert( function && "Error generating llvm function" );
    return function;
}

void CodeGenerator::CreateFunctionDefinition(
                                     llvm::Function* function,
                                     const std::vector<Variable_sp>& parameters,
                                     const CompoundStatement_up& body )
{
    assert( function && "Trying to define a null function" );
    assert( body && "Trying to define function with a null body" );
    assert( parameters.size() == function->arg_size() &&
            "Function parameter size mismatch" );

    llvm::BasicBlock* llvm_body = llvm::BasicBlock::Create(
                                                     m_Runtime.GetLLVMContext(),
                                                     "entry",
                                                     function );

    m_Builder.SetInsertPoint( llvm_body );

    auto arg_iterator = function->arg_begin();
    for( unsigned i = 0; i < parameters.size(); ++i, ++arg_iterator )
    {
        // todo in out and inout things
        const Variable_sp& p = parameters[i];
        assert( arg_iterator != function->arg_end() &&
                "llvm arg iterator overrun" );
        assert( arg_iterator->getType() == m_Runtime.GetLLVMType(
                                                   p->GetType() ) &&
                "Type mismatch in function parameters" );
        assert( p->IsParameter() &&
                "non-parameter in function parameter list" );

        llvm::Value* v = m_Builder.CreateAlloca( m_Runtime.GetLLVMType(
                                                           p->GetType() ) );
        m_Builder.CreateStore( arg_iterator, v );
        p->SetParameterPointer( v );
    }
    assert( arg_iterator == function->arg_end() &&
            "llvm arg iterator underrun" );

    assert( body->AlwaysReturns() &&
            "Generating code for a statement which doesn't always return" );

    body->CodeGen( *this );
}

llvm::Value* CodeGenerator::CreateFunctionCall(
                                   const Function_sp& function,
                                   const std::vector<Expression_up>& arguments )
{
#if !defined(NDEBUG)
    assert( function && "Trying to codegen a null function" );
    for( const auto& a : arguments )
        assert( a && "Trying to codegen a null argument" );
#endif
    std::vector<llvm::Value*> llvm_arguments;
    llvm_arguments.reserve( function->GetNumParams() );
    for( unsigned i = 0; i < function->GetNumParams(); ++i )
        if( i < arguments.size() )
            llvm_arguments.push_back( arguments[i]->CodeGen( *this ) );
        // TODO default arguments
        //else
            //llvm_argumens.push_back(
                             //function->GetDefaultArgument->CodeGen( *this ) );
    return m_Builder.CreateCall( function->GetLLVMFunction(),
                                     std::move(llvm_arguments) );
}

void CodeGenerator::CreateDestroyTemporaryCalls()
{
    while( !m_Temporaries.empty() )
    {
        llvm::Value* v = m_Temporaries.top();
        m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_DESTROY,
                                     {v},
                                     m_Builder );
        m_Temporaries.pop();
    }
}

llvm::Function* CodeGenerator::CreateFunctionFromExpression(
                                                const Expression& expression,
                                                std::string name )
{
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    llvm::Type* return_type = m_Runtime.GetLLVMType( expression.GetType() );
    assert( return_type &&
            "Trying to get the type of an unhandled JoeLang::Type" );

    //
    // create a function prototype which takes no arguments!
    //
    llvm::FunctionType* prototype = llvm::FunctionType::get(
                                        return_type,
                                        std::vector<llvm::Type*>(),
                                        false );
    assert( prototype && "Error generating empty function prototype" );

    //
    // create an anonymous function
    //
    llvm::Function* function = llvm::Function::Create(
                                                prototype,
                                                llvm::Function::ExternalLinkage,
                                                std::move(name),
                                                &m_Module );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create(
                                                    m_Runtime.GetLLVMContext(),
                                                    "entry",
                                                    function );
    m_Builder.SetInsertPoint( body );

    //
    // Generate the code from the expression
    //
    llvm::Value* v = expression.CodeGen( *this );
    /// TODO arrays of strings
    if( expression.GetType().GetType() == Type::STRING )
    {
        assert( !expression.GetType().IsArrayType() &&
                "Todo arrays of strings" );
        v = m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_COPY,
                                         {v},
                                         m_Builder );
    }
    CreateDestroyTemporaryCalls();
    assert( v && "Invalid expression llvm::Value*" );
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    //
    // Set it as the return value for the function
    //
    m_Builder.CreateRet( v );
    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function not valid" );

    return function;
}

} // namespace Compiler
} // namespace JoeLang

