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

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/casting.hpp>
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

void LLVMWriter::GenerateFunction( const Function& function )
{
    llvm::Function* llvm_function;
    //
    // If we've not seen it before, generate a declaration
    //
    auto i = m_GeneratedFunctions.find( &function );
    if( i == m_GeneratedFunctions.end() )
    {
        llvm_function = GenerateFunctionDeclaration( function );
        m_GeneratedFunctions[&function] = llvm_function;
        
    }
    else 
    {
        llvm_function = i->second;
        
        //
        // If we already have a definition, we can return
        //
        if( !llvm_function->empty() )
            return;
    }
    
    GenerateNodeFunctionDependencies( function.GetCodeDag() );
    
    //
    // Now we can define the function
    //
    
    llvm::BasicBlock* body =
        llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "entry", llvm_function );

    IRBuilder builder( m_Runtime.GetLLVMContext() );
    
    builder.SetInsertPoint( body );
    
    const std::vector<Variable_sp>& parameters = function.GetParameters();
    assert( parameters.size() == llvm_function->arg_size() && "Mismatch in number of parameters" );

    auto arg_iterator = llvm_function->arg_begin();
    for( unsigned i = 0; i < parameters.size(); ++i, ++arg_iterator )
    {
        // todo in out and inout things
        Variable& parameter = *parameters[i];
        assert( parameter.IsParameter() && "non-parameter in function parameter list" );

        llvm::Value* v = builder.CreateAlloca( m_Runtime.GetLLVMType( parameter.GetType() ) );
        builder.CreateStore( arg_iterator, v );
        parameter.SetParameterPointer( v );
    }

    GenerateStatement( function.GetCodeDag(), builder );
}

llvm::Function* LLVMWriter::GenerateFunctionDeclaration( const Function& function )
{
    llvm::Type* llvm_return_type = m_Runtime.GetLLVMType( function.GetReturnType() );
    std::vector<llvm::Type*> llvm_parameter_types;
    
    const std::vector<CompleteType>& parameter_types = function.GetParameterTypes();
    llvm_parameter_types.reserve( parameter_types.size() );
    for( const auto& p : parameter_types )
        llvm_parameter_types.push_back( m_Runtime.GetLLVMType( p ) );

    llvm::FunctionType* prototype =
        llvm::FunctionType::get( llvm_return_type, llvm_parameter_types, false );

    llvm::Function* llvm_function = llvm::Function::Create(
        prototype, llvm::Function::ExternalLinkage, function.GetIdentifier(), &m_Module );
    return llvm_function;    
}

void LLVMWriter::GenerateNodeFunctionDependencies( const Node& node )
{
    //
    // Generate all the functions this depends on
    //
    std::set<const Node*> dependencies =
        node.GetDescendantsOfNodeType( NodeType::FunctionIdentifier );
    
    for( const Node* n : dependencies )
    {
        const Function& dependency = cast<FunctionNode>( *n ).GetFunction();
        GenerateFunction( dependency );
    }
}

StateAssignmentBase_up LLVMWriter::GenerateStateAssignment(
    const StateAssignmentNode& state_assignment_node )
{
    const StateBase& state = state_assignment_node.GetState();
    const ExpressionNode& assigned_node = state_assignment_node.GetAssignedExpression();

    assert( assigned_node.GetType().GetType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );
    
    //
    // Make sure we have everything before codegen
    //
    GenerateNodeFunctionDependencies( assigned_node );
    
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
// All the functions dealing with generating statements
//--------------------------------------------------------------------------------------------------

void LLVMWriter::GenerateStatement( const Node& statement, IRBuilder& builder ) const
{
    switch( statement.GetNodeType() )
    {
    case NodeType::Sequence:
        return GenerateSequence( statement, builder );
    case NodeType::Return:
        if( statement.GetNumChildren() == 0 )
            return GenerateVoidReturn( builder );
        return GenerateReturn( cast<ExpressionNode>( statement.GetChild( 0 ) ), builder );
    default:
        assert( false && "Trying to generate a uhandled statement type" );
    }
}

void LLVMWriter::GenerateSequence( const Node& sequence, IRBuilder& builder ) const
{
    assert( sequence.GetNodeType() == NodeType::Sequence &&
            "GenerateSequence given a non-sequence" );
    for( const Node& node : sequence.GetChildren() )
        GenerateStatement( node, builder );
}

void LLVMWriter::GenerateVoidReturn( IRBuilder& builder ) const
{
    builder.CreateRetVoid();
}

void LLVMWriter::GenerateReturn( const ExpressionNode& returned, IRBuilder& builder ) const
{
    builder.CreateRet( GenerateValue( returned, builder ) );
}

//--------------------------------------------------------------------------------------------------
// All the functions dealing with generating llvm values
//--------------------------------------------------------------------------------------------------

llvm::Value* LLVMWriter::GenerateValue( const ExpressionNode& expression, IRBuilder& builder ) const
{
    switch( expression.GetNodeType() )
    {
    case NodeType::Constant:
        return GenerateConstant( cast<ConstantNodeBase>( expression ), builder );
    case NodeType::Cast:
        return GenerateCast( cast<CastNode>( expression ), builder );
        
    case NodeType::Call:
        return GenerateCall( expression, builder );
        
    case NodeType::ExtractElement:
        return GenerateExtractElement(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
        
    case NodeType::Swizzle:
        return GenerateSwizzle(
            expression.GetOperand( 0 ), cast<SwizzleNode>( expression ).GetSwizzle(), builder );
    
    case NodeType::VectorConstructor:
        return GenerateVectorConstructor( expression, builder );
        
    case NodeType::MatrixConstructor:
        return GenerateMatrixConstructor( expression, builder );
    
    //
    // Binary Operators
    //
    case NodeType::LogicalOr:
    case NodeType::BitwiseOr:
        return GenerateOr( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::LogicalAnd:
    case NodeType::BitwiseAnd:
        return GenerateAnd( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::BitwiseExclusiveOr:
        return GenerateExclusiveOr(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareEqual:
        return GenerateCompareEqual(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareNotEqual:
        return GenerateCompareNotEqual(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareLessThan:
        return GenerateCompareLessThan(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareGreaterThan:
        return GenerateCompareGreaterThan(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareLessThanEquals:
        return GenerateCompareLessThanEquals(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::CompareGreaterThanEquals:
        return GenerateCompareGreaterThanEquals(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::LeftShift:
        return GenerateLeftShift( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::RightShift:
        return GenerateRightShift(
            expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::Add:
        return GenerateAdd( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::Subtract:
        return GenerateSubtract( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::Multiply:
        return GenerateMultiply( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::Divide:
        return GenerateDivide( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
    case NodeType::Modulo:
        return GenerateModulo( expression.GetOperand( 0 ), expression.GetOperand( 1 ), builder );
        
        
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        return nullptr;
    }
}

llvm::Value* LLVMWriter::GenerateCast( const CastNode& expression, IRBuilder& builder ) const
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

llvm::Value* LLVMWriter::GenerateConstant( const ConstantNodeBase& expression,
                                           IRBuilder& builder ) const
{
    Type joelang_type = expression.GetType();
    llvm::Type* llvm_type = m_Runtime.GetLLVMType( joelang_type );
    switch( joelang_type )
    {
    case Type::BOOL:
        return llvm::ConstantInt::get(
            llvm_type, static_cast<const ConstantNode<jl_bool>&>( expression ).GetConstant() );
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

llvm::Value* LLVMWriter::GenerateCall( const ExpressionNode& expression, IRBuilder& builder ) const
{
    assert( expression.GetNodeType() == NodeType::Call && "GenerateCall given a non-call node" );
    
    const Function& function =
        cast<FunctionNode>( expression.GetChildren().back().get() ).GetFunction();
    
    assert( m_GeneratedFunctions.find( &function ) != m_GeneratedFunctions.end() &&
            "Trying to call an undeclared function" );
    
    std::vector<llvm::Value*> llvm_arguments;
    llvm_arguments.reserve( expression.GetNumChildren() - 1 );
            
    for( unsigned i = 0; i < expression.GetNumChildren() - 1; ++i )
        llvm_arguments.push_back( GenerateValue( expression.GetOperand( i ), builder ) );
    
    return builder.CreateCall( m_GeneratedFunctions.at( &function ), std::move( llvm_arguments ) );
}   

llvm::Value* LLVMWriter::GenerateExtractElement( const ExpressionNode& expression,
                                                 const ExpressionNode& index,
                                                 IRBuilder& builder ) const
{
    return builder.CreateExtractElement( GenerateValue( expression, builder ),
                                         GenerateValue( index, builder ) );
}

llvm::Value* LLVMWriter::GenerateSwizzle( const ExpressionNode& expression,
                                          const Swizzle& swizzle,
                                          IRBuilder& builder ) const
{  
    llvm::Value* value = GenerateValue( expression, builder );
    
    //
    // If we only have one swizzle index we want to extract an element
    //
    if( swizzle.GetSize() == 1 )
    {
        //
        // if e is already a scalar type (the expression was myscalar.x)
        // just return the scalar
        //
        if( expression.GetType().IsScalarType() )
        {
            assert( swizzle.GetIndex( 0 ) == 0 && "Invalid swizzle index" );
            return value;
        }

        //
        // Otherwise extract the element
        //
        return builder.CreateExtractElement(
            value,
            llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ), swizzle.GetIndex( 0 ) ) );
    }

    //
    // We are swizzling and returning a vector
    //

    //
    // If this isn't a vector, package it into one
    //
    if( expression.GetType().IsScalarType() )
        value = builder.CreateInsertElement(
            llvm::UndefValue::get( llvm::VectorType::get( value->getType(), 1 ) ),
            value,
            llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ), 0 ) );

    std::vector<unsigned> indices( swizzle.begin(), swizzle.end() );

    return builder.CreateShuffleVector(
        value,
        llvm::UndefValue::get( value->getType() ),
        llvm::ConstantDataVector::get( m_Runtime.GetLLVMContext(), indices ) );
}

llvm::Value* LLVMWriter::GenerateVectorConstructor( const ExpressionNode& constructor,
                                                    IRBuilder& builder ) const
{
    const CompleteType& type = constructor.GetType();
    llvm::Value* ret = llvm::UndefValue::get( m_Runtime.GetLLVMType( type ) );
    for( unsigned i = 0; i < type.GetNumElements(); ++i )
        ret = builder.CreateInsertElement(
            ret,
            GenerateValue( constructor.GetOperand( i ), builder ),
            llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ), i ) );
    return ret;
}
    
llvm::Value* LLVMWriter::GenerateMatrixConstructor( const ExpressionNode& constructor,
                                                    IRBuilder& builder ) const
{
    const CompleteType& type = constructor.GetType();
    llvm::Value* ret = llvm::UndefValue::get( m_Runtime.GetLLVMType( type ) );
    for( unsigned i = 0; i < type.GetNumMatrixColumns(); ++i )
        ret = builder.CreateInsertValue(
            ret, GenerateValue( constructor.GetOperand( i ), builder ), { i } ); 
    return ret;
}

//
// Binary Operators
//

llvm::Value* LLVMWriter::GenerateOr( const ExpressionNode& lhs,
                                     const ExpressionNode& rhs,
                                     IRBuilder& builder ) const
{
    return builder.CreateOr( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

llvm::Value* LLVMWriter::GenerateAnd( const ExpressionNode& lhs,
                                      const ExpressionNode& rhs,
                                      IRBuilder& builder ) const
{
    return builder.CreateAnd( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

llvm::Value* LLVMWriter::GenerateExclusiveOr( const ExpressionNode& lhs,
                                              const ExpressionNode& rhs,
                                              IRBuilder& builder ) const
{
    return builder.CreateXor( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

llvm::Value* LLVMWriter::GenerateCompareEqual( const ExpressionNode& lhs,
                                               const ExpressionNode& rhs,
                                               IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
        return builder.CreateICmpEQ( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFCmpOEQ( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    else if( lhs.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall(
            RuntimeFunction::STRING_EQUAL,
            { GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) },
            builder );
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareNotEqual( const ExpressionNode& lhs,
                                                  const ExpressionNode& rhs,
                                                  IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
        return builder.CreateICmpNE( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFCmpONE( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    else if( lhs.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall(
            RuntimeFunction::STRING_NOTEQUAL,
            { GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) },
            builder );
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareLessThan( const ExpressionNode& lhs,
                                                  const ExpressionNode& rhs,
                                                  IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return builder.CreateICmpSLT( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
        else
            return builder.CreateICmpULT( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return builder.CreateFCmpOLT( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareGreaterThan( const ExpressionNode& lhs,
                                                     const ExpressionNode& rhs,
                                                     IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return builder.CreateICmpSGT( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
        else
            return builder.CreateICmpUGT( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return builder.CreateFCmpOGT( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                                        const ExpressionNode& rhs,
                                                        IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return builder.CreateICmpSLE( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
        else
            return builder.CreateICmpULE( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return builder.CreateFCmpOLE( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                           const ExpressionNode& rhs,
                                                           IRBuilder& builder ) const
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return builder.CreateICmpSGE( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
        else
            return builder.CreateICmpUGE( GenerateValue( lhs, builder ),
                                          GenerateValue( rhs, builder ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return builder.CreateFCmpOGE( GenerateValue( lhs, builder ),
                                      GenerateValue( rhs, builder ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateLeftShift( const ExpressionNode& lhs,
                                            const ExpressionNode& rhs,
                                            IRBuilder& builder ) const
{
    return builder.CreateShl( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

llvm::Value* LLVMWriter::GenerateRightShift( const ExpressionNode& lhs,
                                             const ExpressionNode& rhs,
                                             IRBuilder& builder ) const
{
    if( lhs.GetType().IsSigned() )
        return builder.CreateAShr( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else
        return builder.CreateLShr( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

llvm::Value* LLVMWriter::GenerateAdd( const ExpressionNode& lhs,
                                      const ExpressionNode& rhs,
                                      IRBuilder& builder ) const
{
    if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFAdd( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsIntegral() )
        return builder.CreateAdd( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().GetType() == Type::STRING )
    {
        //llvm::Value* ret = m_Runtime.CreateRuntimeCall(
                                                //RuntimeFunction::STRING_CONCAT,
                                                //{GenerateValue( lhs, builder ),
                                                 //GenerateValue( rhs, builder )},
                                                //builder );
        //m_Temporaries.push( ret );
        //return ret;
        assert( false && "Complete me" );
    }

    assert( false && "Trying to Add unhandled types" );
    return nullptr;
}


llvm::Value* LLVMWriter::GenerateSubtract( const ExpressionNode& lhs,
                                           const ExpressionNode& rhs,
                                           IRBuilder& builder ) const
{
    if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFSub( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsIntegral() )
        return builder.CreateSub( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    
    assert( false && "Trying to subtract unhandled types" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateMultiply( const ExpressionNode& lhs,
                                           const ExpressionNode& rhs,
                                           IRBuilder& builder ) const
{
    if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFMul( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsIntegral() )
        return builder.CreateMul( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    
    assert( false && "Trying to multiply unhandled types" );
    return nullptr;
}



llvm::Value* LLVMWriter::GenerateDivide( const ExpressionNode& lhs,
                                         const ExpressionNode& rhs,
                                         IRBuilder& builder ) const
{
    if( lhs.GetType().IsFloatingPoint() )
        return builder.CreateFDiv( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsIntegral() )
        if( lhs.GetType().IsSigned() )
            return builder.CreateSDiv( GenerateValue( lhs, builder ),
                                       GenerateValue( rhs, builder ) );
        else 
            return builder.CreateUDiv( GenerateValue( lhs, builder ),
                                       GenerateValue( rhs, builder ) );
    
    assert( false && "Trying to divide unhandled types" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateModulo( const ExpressionNode& lhs,
                                         const ExpressionNode& rhs,
                                         IRBuilder& builder ) const
{
    if( lhs.GetType().IsSigned() )
        return builder.CreateSRem( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
    else if( lhs.GetType().IsIntegral() )
        return builder.CreateURem( GenerateValue( lhs, builder ), GenerateValue( rhs, builder ) );
}

//
// Helpers
//
llvm::Value* LLVMWriter::GenerateScalarOrVectorCast( llvm::Value* from_value,
                                                     Type from_type,
                                                     Type to_type,
                                                     IRBuilder& builder ) const
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
                                             IRBuilder& builder ) const
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
