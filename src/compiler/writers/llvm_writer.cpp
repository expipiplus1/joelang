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

#include "llvm_writer.hpp"

#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

#include <compiler/code_dag/constant_node.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/writers/runtime.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/types.hpp>

namespace JoeLang
{
namespace Compiler
{

LLVMWriter::LLVMWriter( Runtime& runtime )
    : m_Runtime( runtime ),
      m_Module( m_Runtime.GetModule() ),
      m_ExecutionEngine( m_Runtime.GetExecutionEngine() )
    
{
}

StateAssignmentBase_up LLVMWriter::GenerateStateAssignment(
    const StateAssignmentNode& state_assignment_node )
{
    const StateBase& state = state_assignment_node.GetState();
    const ExpressionNode& assigned_node = state_assignment_node.GetAssignedExpression();

    assert( assigned_node.GetType().GetType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );
    
#define CREATE_STATE_ASSIGNMENT( type ) \
    case JoeLangType<type>::value: \
        return StateAssignmentBase_up( new StateAssignment<type>( \
            static_cast<const State<type>&>( state ), WrapExpression<type>( assigned_node ) ) ); \
        break

#define CREATE_STATE_ASSIGNMENT_N( type ) \
    CREATE_STATE_ASSIGNMENT( type ); \
    CREATE_STATE_ASSIGNMENT( type##2 ); \
    CREATE_STATE_ASSIGNMENT( type##3 ); \
    CREATE_STATE_ASSIGNMENT( type##4 ); \
    CREATE_STATE_ASSIGNMENT( type##2x2 ); \
    CREATE_STATE_ASSIGNMENT( type##2x3 ); \
    CREATE_STATE_ASSIGNMENT( type##2x4 ); \
    CREATE_STATE_ASSIGNMENT( type##3x2 ); \
    CREATE_STATE_ASSIGNMENT( type##3x3 ); \
    CREATE_STATE_ASSIGNMENT( type##3x4 ); \
    CREATE_STATE_ASSIGNMENT( type##4x2 ); \
    CREATE_STATE_ASSIGNMENT( type##4x3 ); \
    CREATE_STATE_ASSIGNMENT( type##4x4 )
            
    switch( state.GetType() )
    {
     CREATE_STATE_ASSIGNMENT_N( jl_bool );
     CREATE_STATE_ASSIGNMENT_N( jl_char );
     CREATE_STATE_ASSIGNMENT_N( jl_short );
     CREATE_STATE_ASSIGNMENT_N( jl_int );
     CREATE_STATE_ASSIGNMENT_N( jl_long );
     CREATE_STATE_ASSIGNMENT_N( jl_uchar );
     CREATE_STATE_ASSIGNMENT_N( jl_ushort );
     CREATE_STATE_ASSIGNMENT_N( jl_uint );
     CREATE_STATE_ASSIGNMENT_N( jl_ulong );
     CREATE_STATE_ASSIGNMENT_N( jl_float );
     CREATE_STATE_ASSIGNMENT_N( jl_double );
        
    default:
        assert( false && "Trying to generate code for an unhandled state type" );
    }
    
 #undef CREATE_STATE_ASSIGNMENT_N
 #undef CREATE_STATE_ASSIGNMENT
}

void* LLVMWriter::WrapExpressionCommon( const ExpressionNode& expression )
{
    assert( !expression.GetType().IsArrayType() && "Can't wrap an array type" );

    //
    // Create a new builder for this temporary evaluation
    //
    IRBuilder builder( m_Runtime.GetLLVMContext() );

//
// Gather all the functions this depends on
//
#if 0
    std::set<Function_ref> function_dependencies = expression.GetFunctionDependencies();
    GenerateFunctions( function_dependencies );
#endif

    llvm::Type* return_type = m_Runtime.GetWrapperLLVMType( expression.GetType() );
    llvm::FunctionType* prototype = llvm::FunctionType::get(
        m_Runtime.GetLLVMType( Type::VOID ), { return_type->getPointerTo() }, false );
    llvm::Function* function = llvm::Function::Create(
        prototype, llvm::Function::ExternalLinkage, "expression_evaluation", &m_Module );

    llvm::BasicBlock* body =
        llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "entry", function );

    builder.SetInsertPoint( body );
    
    // The code for the expression
    llvm::Value* v = GenerateValue( expression, builder );
    
#if 0
    if( expression.GetType().IsString() )
        v = m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_COPY, { v }, builder );
    CreateDestroyTemporaryCalls( builder );
#endif

    v = m_Runtime.CreateDeepCopy( v, return_type, builder );

#if 0
    assert( m_Temporaries.empty() && "Leftover temporaries" );
#endif

    builder.CreateStore( v, function->arg_begin() );
    builder.CreateRetVoid();

    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) && "Function not valid" );

    void* function_ptr = m_ExecutionEngine.getPointerToFunction( function );
    
    return function_ptr;
}
    
template <typename T>
std::function<T()> LLVMWriter::WrapExpression( const ExpressionNode& expression )
{
    void* function_ptr = WrapExpressionCommon( expression );

    return[function_ptr]()->T{ T t[1];
        reinterpret_cast<void ( * )( T* )>( function_ptr )( t );
        return t[0];
    }
    ;
}

//--------------------------------------------------------------------------------------------------
// All the functions dealing with generating llvm values
//--------------------------------------------------------------------------------------------------

llvm::Value* LLVMWriter::GenerateValue( const ExpressionNode& expression, IRBuilder& builder )
{
    switch( expression.GetNodeType() )
    {
    case NodeType::Constant:
        return GenerateConstant( cast<ConstantNodeBase>( expression ), builder );
    case NodeType::Cast:
        return GenerateCast( cast<CastNode>( expression ), builder );
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        return nullptr;
    }
}

llvm::Value* LLVMWriter::GenerateCast( const CastNode& expression, IRBuilder& builder )
{
    assert( expression.GetNodeType() == NodeType::Cast &&
            "Trying to generate a cast from a non-cast node" );
    
    Type from_type = cast<ExpressionNode>( expression.GetChild( 0 ) ).GetType().GetType();
    Type to_type = expression.GetType().GetType();
    
    llvm::Value* from_value =
        GenerateValue( cast<ExpressionNode>( expression.GetChild( 0 ) ), builder );
    
    if( IsScalarType( from_type ) || IsVectorType( from_type ) )
        return GenerateScalarOrVectorCast( from_value, from_type, to_type, builder );
    else if( IsMatrixType( from_type ) )
        return GenerateMatrixCast( from_value, from_type, to_type, builder );
    
    assert( false && "Trying to cast an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateConstant( const ConstantNodeBase& expression, IRBuilder& builder )
{
    Type joelang_type = expression.GetType();
    llvm::Type* llvm_type = m_Runtime.GetLLVMType( joelang_type );
    switch( joelang_type )
    {
    case Type::CHAR:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_char>&>( expression ).GetConstant() );
    case Type::SHORT:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_short>&>( expression ).GetConstant() );
    case Type::INT:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_int>&>( expression ).GetConstant() );
    case Type::LONG:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_long>&>( expression ).GetConstant() );
    case Type::UCHAR:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_uchar>&>( expression ).GetConstant() );
    case Type::USHORT:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_ushort>&>( expression ).GetConstant() );
    case Type::UINT:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_uint>&>( expression ).GetConstant() );
    case Type::ULONG:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_ulong>&>( expression ).GetConstant() );
    case Type::FLOAT:
        return llvm::ConstantFP::get(
            llvm_type, static_cast<const ConstantNode<jl_float>&>( expression ).GetConstant() );
    case Type::DOUBLE:
        return llvm::ConstantFP::get(
            llvm_type, static_cast<const ConstantNode<jl_double>&>( expression ).GetConstant() );
    default:
        assert( false && "Trying to generate a constant of unhandled type" );
    }
}

//
// Helpers
//
llvm::Value* LLVMWriter::GenerateScalarOrVectorCast( llvm::Value* from_value,
                                                     Type from_type,
                                                     Type to_type,
                                                     IRBuilder& builder )
{
    assert( ( IsVectorType( from_type ) || IsScalarType( from_type ) ) &&
            "Trying to create a scalar or vector cast from a non-scalar and non-vector type" );
    assert( ( IsVectorType( to_type ) || IsScalarType( to_type ) ) &&
            "Trying to create a scalar or vector cast to a non-scalar and non-vector type" );
    assert( GetNumElementsInType( to_type ) == GetNumElementsInType( from_type ) &&
            "Trying to create a scalar or vector cast between different sized types" );

    //
    // for a cast to bool, compare to zero
    //
    if( GetScalarType( to_type ) == Type::BOOL )
    {
        if( IsFloatingPoint( from_type ) )
            return builder.CreateFCmpOEQ(
                from_value, llvm::ConstantFP::getNullValue( m_Runtime.GetLLVMType( from_type ) ) );
        return builder.CreateIsNotNull( from_value );
    }

    if( IsFloatingPoint( to_type ) )
    {
        if( IsFloatingPoint( from_type ) )
            return builder.CreateFPCast( from_value, m_Runtime.GetLLVMType( to_type ) );
        else if( IsSigned( from_type ) )
            return builder.CreateSIToFP( from_value, m_Runtime.GetLLVMType( to_type ) );
        else
        {
            assert( IsIntegral( from_type ) && !IsSigned( from_type ) &&
                    "we've let a bad type through" );
            return builder.CreateUIToFP( from_value, m_Runtime.GetLLVMType( to_type ) );
        }
    }

    assert( IsIntegral( to_type ) && "Type should be integral" );
    if( IsIntegral( from_type ) )
    {
        return builder.CreateIntCast(
            from_value, m_Runtime.GetLLVMType( to_type ), IsSigned( from_type ) );
    }

    assert( IsFloatingPoint( from_type ) && "from_type should be floating point" );
    if( IsSigned( to_type ) )
        return builder.CreateFPToSI( from_value, m_Runtime.GetLLVMType( to_type ) );
    else
        return builder.CreateFPToUI( from_value, m_Runtime.GetLLVMType( to_type ) );
}

llvm::Value* LLVMWriter::GenerateMatrixCast( llvm::Value* from_value,
                                             Type from_type,
                                             Type to_type,
                                             IRBuilder& builder )
{
    assert( IsMatrixType( from_type ) && "Trying to create a matrix cast from a non-matrix type" );
    assert( IsMatrixType( to_type ) && "Trying to create a matrix cast to a non-matrix type" );
    assert( GetNumRowsInType( to_type ) == GetNumRowsInType( from_type ) &&
            "Trying to create a matrix cast between different sized types" );
    assert( GetNumColumnsInType( to_type ) == GetNumColumnsInType( from_type ) &&
            "Trying to create a matrix cast between different sized types" );
    
    unsigned num_columns = GetNumColumnsInType( to_type );
    Type from_column_type = GetMatrixColumnType( from_type );
    Type to_column_type = GetMatrixColumnType( to_type );
    
    llvm::Value* ret = llvm::UndefValue::get( m_Runtime.GetLLVMType( to_type ) );
    //
    // Iterate over all the columns, and cast each of those
    //
    for( unsigned i = 0; i < num_columns; ++i )
    {
        llvm::Value* column = builder.CreateExtractValue( from_value, { i } );
        llvm::Value* casted_column =
            GenerateScalarOrVectorCast( column, from_column_type, to_column_type, builder );
        ret = builder.CreateInsertValue( ret, casted_column, { i } );
    }
    
    return ret;
}

} // namespace Compiler
} // namespace JoeLang
