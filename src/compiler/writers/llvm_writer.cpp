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
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/IRBuilder.h>

#include <compiler/code_dag/cast_node.hpp>
#include <compiler/code_dag/constant_node.hpp>
#include <compiler/code_dag/expression_node.hpp>
#include <compiler/code_dag/function_node.hpp>
#include <compiler/code_dag/pointer_expression_node.hpp>
#include <compiler/code_dag/state_assignment_node.hpp>
#include <compiler/code_dag/swizzle_node.hpp>
#include <compiler/code_dag/swizzle_store_node.hpp>
#include <compiler/code_dag/variable_node.hpp>
#include <compiler/code_dag/zero_node.hpp>
#include <compiler/semantic_analysis/complete_type.hpp>
#include <compiler/semantic_analysis/function.hpp>
#include <compiler/semantic_analysis/swizzle.hpp>
#include <compiler/semantic_analysis/type_properties.hpp>
#include <compiler/semantic_analysis/variable.hpp>
#include <compiler/support/casting.hpp>
#include <compiler/support/generic_value.hpp>
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
      m_ExecutionEngine( m_Runtime.GetExecutionEngine() ),
      m_Builder( m_Runtime.GetLLVMContext() )
{    
}

LLVMWriter::~LLVMWriter()
{
    assert( m_LocalVariables.empty() && "Leftover local variables" );
    assert( m_GeneratedValues.empty() && "Leftover generated values" );
}

void LLVMWriter::AddGlobalVariable( const Variable& variable )
{
    assert( m_GlobalVariables.find( &variable ) == m_GlobalVariables.end() &&
            "Adding a variable twice" );
    assert( variable.IsGlobal() && "AddGlobalVariable given a non-global variable" );
 
    llvm::Type* llvm_type = m_Runtime.GetLLVMType( variable.GetType() );
    llvm::Constant* init;
    if( !variable.GetInitializer().GetType().IsUnknown() )
        init = GenerateGenericValue( variable.GetInitializer() );
    else
        init = llvm::UndefValue::get( llvm_type );

    auto linkage = variable.IsUniform() ? llvm::GlobalVariable::ExternalLinkage
                                        : llvm::GlobalVariable::PrivateLinkage;

    llvm::GlobalVariable* global_variable = new llvm::GlobalVariable(
        m_Module,
        llvm_type,
        variable.IsConst() &&
            !variable.IsUniform(), //If something is uniform, it may be changed from outside
        linkage,
        init,
        variable.GetName() );
    //
    // The address of non const variables is important because we can modify
    // them from outside
    //
    global_variable->setUnnamedAddr( variable.IsConst() && variable.IsUniform() );
    
    m_GlobalVariables[&variable] = global_variable;
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
        //
        // Return, because we've already declared it and it will be defined later
        //
        return;
    }
    
    GenerateNodeFunctionDependencies( function.GetCodeDag() );
    
    //
    // Now we can define the function
    //
    
    llvm::BasicBlock* body =
        llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "entry", llvm_function );

    assert( !m_Builder.GetInsertBlock() && "Builder is still inserting into a block" );
    
    m_Builder.SetInsertPoint( body );
    
    const std::vector<Variable_sp>& parameters = function.GetParameters();
    assert( parameters.size() == llvm_function->arg_size() && "Mismatch in number of parameters" );

    auto arg_iterator = llvm_function->arg_begin();
    for( unsigned i = 0; i < parameters.size(); ++i, ++arg_iterator )
    {
        // todo in out and inout things
        const Variable& parameter = *parameters[i];
        assert( parameter.IsParameter() && "non-parameter in function parameter list" );

        // Todo, if the variable is never written to, don't bother alloc
        llvm::Value* v = m_Builder.CreateAlloca( m_Runtime.GetLLVMType( parameter.GetType() ) );
        m_Builder.CreateStore( arg_iterator, v );
        m_LocalVariables[&parameter] = v;
    }

    GenerateStatement( function.GetCodeDag() );
    
    assert( !llvm::verifyFunction( *llvm_function, llvm::PrintMessageAction ) &&
            "Function not valid" );
    
    GenerateFunctionCleanup();

    llvm_function->dump();
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
        node.GetDescendantsWithNodeType( NodeType::FunctionIdentifier );
    
    for( const Node* n : dependencies )
    {
        const Function& dependency = cast<FunctionNode>( *n ).GetFunction();
        GenerateFunction( dependency );
    }
}

GenericValue LLVMWriter::EvaluateExpression( const ExpressionNode& expression )
{
    GenerateNodeFunctionDependencies( expression );
    
    llvm::Function* function;

#define GET_VALUE( type ) \
    case JoeLangType<type>::value: \
        ret = GenericValue( WrapExpression<type>( expression, function )() ); \
        break;

#define GET_VALUE_N( type )                                                          \
    GET_VALUE( type ) GET_VALUE( type##2 ) GET_VALUE( type##3 ) GET_VALUE( type##4 ) \
        GET_VALUE( type##2x2 ) GET_VALUE( type##2x3 ) GET_VALUE( type##2x4 )         \
        GET_VALUE( type##3x2 ) GET_VALUE( type##3x3 ) GET_VALUE( type##3x4 )         \
        GET_VALUE( type##4x2 ) GET_VALUE( type##4x3 ) GET_VALUE( type##4x4 )

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
    //case Type::STRING:
        //ret = GenericValue( WrapStringExpression( expression, function )() );
        //break;
    default:
        assert( false && "Trying to run a function returning an unhandled type" );
    }

#undef GET_VALUE_N
#undef GET_VALUE

    m_ExecutionEngine.freeMachineCodeForFunction( function );
    function->eraseFromParent();

    return ret;
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
    
    llvm::Function* llvm_function;
    
#define CREATE_STATE_ASSIGNMENT( type ) \
    case JoeLangType<type>::value: \
        return StateAssignmentBase_up(                                           \
            new StateAssignment<type>( static_cast<const State<type>&>( state ), \
                                       WrapExpression<type>( assigned_node, llvm_function ) ) ); \
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
    
    llvm_function->setName( state.GetName() + "_assignment" );
    
 #undef CREATE_STATE_ASSIGNMENT_N
 #undef CREATE_STATE_ASSIGNMENT
}

void* LLVMWriter::WrapExpressionCommon( const ExpressionNode& expression,
                                        llvm::Function*& llvm_function )
{
    assert( !expression.GetType().IsArrayType() && "Can't wrap an array type" );

    llvm::Type* return_type = m_Runtime.GetWrapperLLVMType( expression.GetType() );
    llvm::FunctionType* prototype = llvm::FunctionType::get(
        m_Runtime.GetLLVMType( Type::VOID ), { return_type->getPointerTo() }, false );
    llvm_function = llvm::Function::Create(
        prototype, llvm::Function::ExternalLinkage, "expression_evaluation", &m_Module );

    llvm::BasicBlock* body =
        llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "entry", llvm_function );

    assert( !m_Builder.GetInsertBlock() && "Builder is still inserting into a block" );
    
    m_Builder.SetInsertPoint( body );
    
    // The code for the expression
    llvm::Value* v = GenerateValue( expression );
    
    if( expression.GetType().IsString() )
    {
        m_StringTemporaries.push( v );
        
        //
        // This doesn't need to be cleaned up as we're passing ownership to the host application
        //
        v = m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_COPY, { v }, m_Builder );
    }

    v = m_Runtime.CreateDeepCopy( v, return_type, m_Builder );
    
    m_Builder.CreateStore( v, llvm_function->arg_begin() );
    
    GenerateExpressionCleanup();
    
    m_Builder.CreateRetVoid();
    
    GenerateFunctionCleanup();

    assert( !llvm::verifyFunction( *llvm_function, llvm::PrintMessageAction ) &&
            "Function not valid" );
    
    void* function_ptr = m_ExecutionEngine.getPointerToFunction( llvm_function );
    
    return function_ptr;
}
    
template <typename T>
std::function<T()> LLVMWriter::WrapExpression( const ExpressionNode& expression,
                                               llvm::Function*& llvm_function )
{
    void* function_ptr = WrapExpressionCommon( expression, llvm_function );

    return[function_ptr]()->T{ T t[1];
        reinterpret_cast<void ( * )( T* )>( function_ptr )( t );
        return t[0];
    }
    ;
}

//--------------------------------------------------------------------------------------------------
// Cleanup
//--------------------------------------------------------------------------------------------------

void LLVMWriter::GenerateExpressionCleanup()
{
    while( !m_StringTemporaries.empty() )
    {
        llvm::Value* v = m_StringTemporaries.top();
        m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_DESTROY, { v }, m_Builder );
        m_StringTemporaries.pop();
    }
    
    m_GeneratedValues.clear();
}

void LLVMWriter::GenerateFunctionCleanup()
{
    assert( m_StringTemporaries.empty() && "Leftover temporaries" );
    assert( m_GeneratedValues.empty() && "Leftover values" );
    m_LocalVariables.clear(); 
    m_Builder.ClearInsertionPoint();
}

//--------------------------------------------------------------------------------------------------
// All the functions dealing with generating statements
//--------------------------------------------------------------------------------------------------

void LLVMWriter::GenerateStatement( const Node& statement )
{
    switch( statement.GetNodeType() )
    {
    case NodeType::Sequence:
        return GenerateSequence( statement );
    case NodeType::Conditional:
        return GenerateConditional(
            cast<ExpressionNode>( statement.GetChild( 0 ) ),
            statement.GetChild( 1 ),
            statement.GetNumChildren() == 3 ? &statement.GetChild( 2 ) : nullptr );
    case NodeType::Return:
        if( statement.GetNumChildren() == 0 )
            return GenerateVoidReturn();
        return GenerateReturn( cast<ExpressionNode>( statement.GetChild( 0 ) ) );
    default:
        assert( false && "Trying to generate a uhandled statement type" );
    }
}

void LLVMWriter::GenerateSequence( const Node& sequence )
{
    assert( sequence.GetNodeType() == NodeType::Sequence &&
            "GenerateSequence given a non-sequence" );
    for( const Node& node : sequence.GetChildren() )
        GenerateStatement( node );
}

void LLVMWriter::GenerateConditional( const ExpressionNode& condition,
                                      const Node& true_statement,
                                      const Node* else_statement )
{
    llvm::Value* condition_value = GenerateValue( condition );
    GenerateExpressionCleanup();
    
    llvm::Function* current_function = m_Builder.GetInsertBlock()->getParent();
    llvm::BasicBlock* true_block =
        llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "if_true", current_function );
    llvm::BasicBlock* else_block =
        else_statement
            ? llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(), "else", current_function )
            : nullptr;
    llvm::BasicBlock* continue_block =
        else_statement ? nullptr : llvm::BasicBlock::Create(
                                       m_Runtime.GetLLVMContext(),
                                       m_Builder.GetInsertBlock()->getName() + "_continue",
                                       current_function ); 
    
    m_Builder.CreateCondBr(
        condition_value, true_block, else_statement ? else_block : continue_block );
    
    m_Builder.SetInsertPoint( true_block );
    
    GenerateStatement( true_statement );
    
    bool needs_continue = false;
    
    if( !true_block->back().isTerminator() )
    {
        if( !continue_block )
            continue_block =
                llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(),
                                          m_Builder.GetInsertBlock()->getName() + "_continue",
                                          current_function );
        m_Builder.CreateBr( continue_block );
        needs_continue = true;
    }
            
    if( else_statement )
    {
        m_Builder.SetInsertPoint( else_block );
        GenerateStatement( *else_statement );
        if( !else_block->back().isTerminator() )
        {
            if( !continue_block )
                continue_block =
                    llvm::BasicBlock::Create( m_Runtime.GetLLVMContext(),
                                              m_Builder.GetInsertBlock()->getName() + "_continue",
                                              current_function );
            m_Builder.CreateBr( continue_block );
            needs_continue = true;
        }
    }
   
    if( needs_continue )
    {
        m_Builder.SetInsertPoint( continue_block );
    }
    else
    {
        m_Builder.ClearInsertionPoint(); 
    }
}

void LLVMWriter::GenerateVoidReturn()
{
    assert( m_GeneratedValues.empty() && "Leftover values in GeneratedValues" );
    m_Builder.CreateRetVoid();
}

void LLVMWriter::GenerateReturn( const ExpressionNode& returned )
{
    assert( m_GeneratedValues.empty() && "Leftover values in GeneratedValues" );
    
    llvm::Value* returned_value = GenerateValue( returned );
    
    GenerateExpressionCleanup();
    
    m_Builder.CreateRet( returned_value );
    
}

//--------------------------------------------------------------------------------------------------
// All the functions dealing with generating llvm values
//--------------------------------------------------------------------------------------------------

llvm::Value* LLVMWriter::GenerateValue( const ExpressionNode& expression )
{
    //
    // If we've already generated this value for this expression, just returned that to not
    // evaluate it twice
    //
    auto i = m_GeneratedValues.find( &expression );
    if( i != m_GeneratedValues.end() )
        return i->second;
    
    if( expression.GetNodeType() == NodeType::SwizzleStore )
        assert( false && "complete me" );
    
    if( isa<PointerExpressionNode>( expression ) )
        return m_Builder.CreateLoad( GenerateAddress( cast<PointerExpressionNode>( expression ) ) );
    
    switch( expression.GetNodeType() )
    {
    case NodeType::Constant:
        return GenerateConstant( cast<ConstantNodeBase>( expression ) );
       
    case NodeType::Zero:
        return GenerateZero( cast<ZeroNode>( expression ).GetType() );
        
    case NodeType::Cast:
        return GenerateCast( cast<CastNode>( expression ) );
        
    case NodeType::Call:
        return GenerateCall( expression );
        
    case NodeType::ExtractElement:
        return GenerateExtractElement( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::InsertElement:
        return GenerateInsertElement( /* vec = */ expression.GetOperand( 0 ),
                                      /* element = */ expression.GetOperand( 1 ),
                                      /* index = */ expression.GetOperand( 2 ) );
        
    case NodeType::Swizzle:
        return GenerateSwizzle( expression.GetOperand( 0 ),
                                cast<SwizzleNode>( expression ).GetSwizzle() );
        
    case NodeType::Select:
        return GenerateSelect( expression.GetOperand( 0 ),
                               expression.GetOperand( 1 ),
                               /* condition = */ expression.GetOperand( 2 ) );
    
    case NodeType::VectorConstructor:
        return GenerateVectorConstructor( expression );
        
    case NodeType::MatrixConstructor:
        return GenerateMatrixConstructor( expression );
        
    case NodeType::PostIncrement:
        return GeneratePostIncrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
        
    case NodeType::PostDecrement:
        return GeneratePostDecrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
    
    //
    // Binary Operators
    //
    case NodeType::LogicalOr:
    case NodeType::BitwiseOr:
        return GenerateOr( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::LogicalAnd:
    case NodeType::BitwiseAnd:
        return GenerateAnd( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::BitwiseExclusiveOr:
        return GenerateExclusiveOr( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::CompareEqual:
        return GenerateCompareEqual( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::CompareNotEqual:
        return GenerateCompareNotEqual( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::CompareLessThan:
        return GenerateCompareLessThan( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::CompareGreaterThan:
        return GenerateCompareGreaterThan( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::CompareLessThanEquals:
        return GenerateCompareLessThanEquals( expression.GetOperand( 0 ),
                                              expression.GetOperand( 1 ) );
        
    case NodeType::CompareGreaterThanEquals:
        return GenerateCompareGreaterThanEquals( expression.GetOperand( 0 ),
                                                 expression.GetOperand( 1 ) );
        
    case NodeType::LeftShift:
        return GenerateLeftShift( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::RightShift:
        return GenerateRightShift( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::Add:
        return GenerateAdd( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::Subtract:
        return GenerateSubtract( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::Multiply:
        return GenerateMultiply( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::Divide:
        return GenerateDivide( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
    case NodeType::Modulo:
        return GenerateModulo( expression.GetOperand( 0 ), expression.GetOperand( 1 ) );
        
        
    //
    // Unary operators
    //
    case NodeType::Negate:
        return GenerateNegate( expression.GetOperand( 0 ) );
        
    case NodeType::BitwiseNot:
        return GenerateBitwiseNot( expression.GetOperand( 0 ) );
        
    case NodeType::LogicalNot:
        return GenerateLogicalNot( expression.GetOperand( 0 ) );
        
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        
        return nullptr;
    }
}

llvm::Value* LLVMWriter::GenerateAddress( const PointerExpressionNode& expression )
{
    auto i = m_GeneratedValues.find( &expression );
    if( i != m_GeneratedValues.end() )
        return i->second;
    
    switch( expression.GetNodeType() )
    {
    case NodeType::Store:
        return GenerateStore( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                              expression.GetOperand( 1 ) );
    case NodeType::SwizzleStore:
        return GenerateSwizzleStore( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                                     expression.GetOperand( 1 ),
                                     cast<SwizzleStoreNode>( expression ).GetSwizzle() );
    case NodeType::VariableIdentifier:
        return GenerateVariableAddress( cast<VariableNode>( expression ) );
    case NodeType::ArrayIndex:
        return GenerateArrayIndex( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ),
                                   expression.GetOperand( 1 ) );
    case NodeType::PreIncrement:
        return GeneratePreIncrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
    case NodeType::PreDecrement:
        return GeneratePreDecrement( cast<PointerExpressionNode>( expression.GetOperand( 0 ) ) );
    default:
        assert( false && "Trying to generate the llvm value of an unhanded expression type" );
        break;
        return nullptr;
    }
}

//
// Addresses
//

llvm::Value* LLVMWriter::GenerateStore( const PointerExpressionNode& address,
                                        const ExpressionNode& assigned )
{
    llvm::Value* address_value = GenerateAddress( address );
    llvm::Value* assigned_value = GenerateValue( assigned );
    m_Builder.CreateStore( assigned_value, address_value );
    return address_value;
}

llvm::Value* LLVMWriter::GenerateSwizzleStore( const PointerExpressionNode& address,
                                               const ExpressionNode& assigned,
                                               const Swizzle& swizzle )
{
    assert( false && "complete me" );
    std::abort();
}

llvm::Value* LLVMWriter::GenerateVariableAddress( const VariableNode& variable_node )
{
    auto i = m_GlobalVariables.find( &variable_node.GetVariable() );
    if( i != m_GlobalVariables.end() )
        return i->second;
    
    auto j = m_LocalVariables.find( &variable_node.GetVariable() );
    if( j != m_LocalVariables.end() )
        return j->second;
    
    assert( false && "Couldn't find variable" );
    std::abort();
}

llvm::Value* LLVMWriter::GenerateArrayIndex( const PointerExpressionNode& address,
                                             const ExpressionNode& index )
{
    assert( false && "complete me" );
    std::abort();
}

llvm::Value* LLVMWriter::GeneratePreIncrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

llvm::Value* LLVMWriter::GeneratePreDecrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

//
// Values
//

llvm::Value* LLVMWriter::GenerateCast( const CastNode& expression )
{
    assert( expression.GetNodeType() == NodeType::Cast &&
            "Trying to generate a cast from a non-cast node" );
    
    Type from_type = cast<ExpressionNode>( expression.GetChild( 0 ) ).GetType().GetType();
    Type to_type = expression.GetType().GetType();
    
    llvm::Value* from_value = GenerateValue( cast<ExpressionNode>( expression.GetChild( 0 ) ) );
    
    if( IsScalarType( from_type ) || IsVectorType( from_type ) )
        return GenerateScalarOrVectorCast( from_value, from_type, to_type );
    else if( IsMatrixType( from_type ) )
        return GenerateMatrixCast( from_value, from_type, to_type );
    
    assert( false && "Trying to cast an unhandled type" );
    return nullptr;
}

llvm::Constant* LLVMWriter::GenerateConstant( const ConstantNodeBase& expression )
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

llvm::Constant* LLVMWriter::GenerateZero( Type type )
{
    return llvm::Constant::getNullValue( m_Runtime.GetLLVMType( type ) );
}

llvm::Value* LLVMWriter::GenerateCall( const ExpressionNode& expression )
{
    assert( expression.GetNodeType() == NodeType::Call && "GenerateCall given a non-call node" );
    
    const Function& function =
        cast<FunctionNode>( expression.GetChildren().back().get() ).GetFunction();
    
    assert( m_GeneratedFunctions.find( &function ) != m_GeneratedFunctions.end() &&
            "Trying to call an undeclared function" );
    
    std::vector<llvm::Value*> llvm_arguments;
    llvm_arguments.reserve( expression.GetNumChildren() - 1 );
            
    for( unsigned i = 0; i < expression.GetNumChildren() - 1; ++i )
        llvm_arguments.push_back( GenerateValue( expression.GetOperand( i ) ) );
    
    return m_Builder.CreateCall( m_GeneratedFunctions.at( &function ),
                                 std::move( llvm_arguments ) );
}   

llvm::Value* LLVMWriter::GenerateExtractElement( const ExpressionNode& vector,
                                                 const ExpressionNode& index )
{
    return m_Builder.CreateExtractElement( GenerateValue( vector ), GenerateValue( index ) );
}

llvm::Value* LLVMWriter::GenerateInsertElement( const ExpressionNode& vector,
                                                const ExpressionNode& element,
                                                const ExpressionNode& index )
{
    return m_Builder.CreateInsertElement(
        GenerateValue( vector ), GenerateValue( element ), GenerateValue( index ) );
}

llvm::Value* LLVMWriter::GenerateSwizzle( const ExpressionNode& expression, const Swizzle& swizzle )
{  
    llvm::Value* value = GenerateValue( expression );
    
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
        return m_Builder.CreateExtractElement(
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
        value = m_Builder.CreateInsertElement(
            llvm::UndefValue::get( llvm::VectorType::get( value->getType(), 1 ) ),
            value,
            llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ), 0 ) );

    std::vector<unsigned> indices( swizzle.begin(), swizzle.end() );

    return m_Builder.CreateShuffleVector(
        value,
        llvm::UndefValue::get( value->getType() ),
        llvm::ConstantDataVector::get( m_Runtime.GetLLVMContext(), indices ) );
}

llvm::Value* LLVMWriter::GenerateSelect( const ExpressionNode& true_expression,
                                         const ExpressionNode& false_expression,
                                         const ExpressionNode& condition )
{
    return m_Builder.CreateSelect( GenerateValue( condition ),
                                   GenerateValue( true_expression ),
                                   GenerateValue( false_expression ) );
}

llvm::Value* LLVMWriter::GenerateVectorConstructor( const ExpressionNode& constructor )
{
    const CompleteType& type = constructor.GetType();
    llvm::Value* ret = llvm::UndefValue::get( m_Runtime.GetLLVMType( type ) );
    for( unsigned i = 0; i < type.GetNumElements(); ++i )
        ret = m_Builder.CreateInsertElement(
            ret,
            GenerateValue( constructor.GetOperand( i ) ),
            llvm::ConstantInt::get( m_Runtime.GetLLVMType( Type::INT ), i ) );
    return ret;
}
    
llvm::Value* LLVMWriter::GenerateMatrixConstructor( const ExpressionNode& constructor )
{
    const CompleteType& type = constructor.GetType();
    llvm::Value* ret = llvm::UndefValue::get( m_Runtime.GetLLVMType( type ) );
    for( unsigned i = 0; i < type.GetNumMatrixColumns(); ++i )
        ret =
            m_Builder.CreateInsertValue( ret, GenerateValue( constructor.GetOperand( i ) ), { i } ); 
    return ret;
}

llvm::Value* LLVMWriter::GeneratePostIncrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

llvm::Value* LLVMWriter::GeneratePostDecrement( const PointerExpressionNode& address )
{
    assert( false && "complete me" );
    std::abort();
}

//
// Binary Operators
//

llvm::Value* LLVMWriter::GenerateOr( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return m_Builder.CreateOr( GenerateValue( lhs ), GenerateValue( rhs ) );
}

llvm::Value* LLVMWriter::GenerateAnd( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return m_Builder.CreateAnd( GenerateValue( lhs ), GenerateValue( rhs ) );
}

llvm::Value* LLVMWriter::GenerateExclusiveOr( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return m_Builder.CreateXor( GenerateValue( lhs ), GenerateValue( rhs ) );
}

llvm::Value* LLVMWriter::GenerateCompareEqual( const ExpressionNode& lhs,
                                               const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
        return m_Builder.CreateICmpEQ( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFCmpOEQ( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_EQUAL,
                                            { GenerateValue( lhs ), GenerateValue( rhs ) },
                                            m_Builder );
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareNotEqual( const ExpressionNode& lhs,
                                                  const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
        return m_Builder.CreateICmpNE( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFCmpONE( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_NOTEQUAL,
                                            { GenerateValue( lhs ), GenerateValue( rhs ) },
                                            m_Builder );
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareLessThan( const ExpressionNode& lhs,
                                                  const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateICmpSLT( GenerateValue( lhs ), GenerateValue( rhs ) );
        else
            return m_Builder.CreateICmpULT( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return m_Builder.CreateFCmpOLT( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareGreaterThan( const ExpressionNode& lhs,
                                                     const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateICmpSGT( GenerateValue( lhs ), GenerateValue( rhs ) );
        else
            return m_Builder.CreateICmpUGT( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return m_Builder.CreateFCmpOGT( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                                        const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateICmpSLE( GenerateValue( lhs ), GenerateValue( rhs ) );
        else
            return m_Builder.CreateICmpULE( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return m_Builder.CreateFCmpOLE( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                           const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateICmpSGE( GenerateValue( lhs ), GenerateValue( rhs ) );
        else
            return m_Builder.CreateICmpUGE( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    else if( lhs.GetType().IsFloatingPoint() )
    {
        return m_Builder.CreateFCmpOGE( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    
    assert( false && "Trying to compare an unhandled type" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateLeftShift( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    return m_Builder.CreateShl( GenerateValue( lhs ), GenerateValue( rhs ) );
}

llvm::Value* LLVMWriter::GenerateRightShift( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsSigned() )
        return m_Builder.CreateAShr( GenerateValue( lhs ), GenerateValue( rhs ) );
    else
        return m_Builder.CreateLShr( GenerateValue( lhs ), GenerateValue( rhs ) );
}

llvm::Value* LLVMWriter::GenerateAdd( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFAdd( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsIntegral() )
        return m_Builder.CreateAdd( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().GetType() == Type::STRING )
    {
        llvm::Value* ret =
            m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_CONCAT,
                                         { GenerateValue( lhs ), GenerateValue( rhs ) },
                                         m_Builder );
        m_StringTemporaries.push( ret );
        return ret;
    }

    assert( false && "Trying to Add unhandled types" );
    return nullptr;
}


llvm::Value* LLVMWriter::GenerateSubtract( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFSub( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsIntegral() )
        return m_Builder.CreateSub( GenerateValue( lhs ), GenerateValue( rhs ) );
    
    assert( false && "Trying to subtract unhandled types" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateMultiply( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFMul( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsIntegral() )
        return m_Builder.CreateMul( GenerateValue( lhs ), GenerateValue( rhs ) );
    
    assert( false && "Trying to multiply unhandled types" );
    return nullptr;
}



llvm::Value* LLVMWriter::GenerateDivide( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsFloatingPoint() )
        return m_Builder.CreateFDiv( GenerateValue( lhs ), GenerateValue( rhs ) );
    else if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateSDiv( GenerateValue( lhs ), GenerateValue( rhs ) );
        else 
            return m_Builder.CreateUDiv( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    
    assert( false && "Trying to divide unhandled types" );
    return nullptr;
}

llvm::Value* LLVMWriter::GenerateModulo( const ExpressionNode& lhs, const ExpressionNode& rhs )
{
    if( lhs.GetType().IsIntegral() )
    {
        if( lhs.GetType().IsSigned() )
            return m_Builder.CreateSRem( GenerateValue( lhs ), GenerateValue( rhs ) );
        else 
            return m_Builder.CreateURem( GenerateValue( lhs ), GenerateValue( rhs ) );
    }
    assert( false && "Trying to get the modulo of an unhandled type" );
    std::abort();
}

llvm::Value* LLVMWriter::GenerateNegate( const ExpressionNode& expression )
{
    if( expression.GetType().IsIntegral() )
        return m_Builder.CreateNeg( GenerateValue( expression ) );
    else if( expression.GetType().IsFloatingPoint() )
        return m_Builder.CreateFNeg( GenerateValue( expression ) );
    assert( false && "Trying to negate an unhandled type" );
    std::abort();
}

llvm::Value* LLVMWriter::GenerateBitwiseNot( const ExpressionNode& expression )
{
    return m_Builder.CreateNot( GenerateValue( expression ) );
}

llvm::Value* LLVMWriter::GenerateLogicalNot( const ExpressionNode& expression )
{
    return m_Builder.CreateIsNull( GenerateValue( expression ) );
}
//
// Helpers
//
llvm::Value* LLVMWriter::GenerateScalarOrVectorCast( llvm::Value* from_value,
                                                     Type from_type,
                                                     Type to_type )
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
            return m_Builder.CreateFCmpOEQ(
                from_value, llvm::ConstantFP::getNullValue( m_Runtime.GetLLVMType( from_type ) ) );
        return m_Builder.CreateIsNotNull( from_value );
    }

    if( IsFloatingPoint( to_type ) )
    {
        if( IsFloatingPoint( from_type ) )
            return m_Builder.CreateFPCast( from_value, m_Runtime.GetLLVMType( to_type ) );
        else if( IsSigned( from_type ) )
            return m_Builder.CreateSIToFP( from_value, m_Runtime.GetLLVMType( to_type ) );
        else
        {
            assert( IsIntegral( from_type ) && !IsSigned( from_type ) &&
                    "we've let a bad type through" );
            return m_Builder.CreateUIToFP( from_value, m_Runtime.GetLLVMType( to_type ) );
        }
    }

    assert( IsIntegral( to_type ) && "Type should be integral" );
    if( IsIntegral( from_type ) )
    {
        return m_Builder.CreateIntCast(
            from_value, m_Runtime.GetLLVMType( to_type ), IsSigned( from_type ) );
    }

    assert( IsFloatingPoint( from_type ) && "from_type should be floating point" );
    if( IsSigned( to_type ) )
        return m_Builder.CreateFPToSI( from_value, m_Runtime.GetLLVMType( to_type ) );
    else
        return m_Builder.CreateFPToUI( from_value, m_Runtime.GetLLVMType( to_type ) );
}

llvm::Value* LLVMWriter::GenerateMatrixCast( llvm::Value* from_value, Type from_type, Type to_type )
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
        llvm::Value* column = m_Builder.CreateExtractValue( from_value, { i } );
        llvm::Value* casted_column =
            GenerateScalarOrVectorCast( column, from_column_type, to_column_type );
        ret = m_Builder.CreateInsertValue( ret, casted_column, { i } );
    }
    
    return ret;
}

llvm::Constant* LLVMWriter::GenerateConstantInt( Type type, unsigned long integer_value )
{
    assert( IsIntegral( type ) && "GenerateConstantInt given a non-integral type" );
    assert( IsScalarType( type ) && "GenerateConstantInt given a non-scalar type" );
    return llvm::ConstantInt::get( m_Runtime.GetLLVMType( type ), integer_value );
}

llvm::Constant* LLVMWriter::GenerateConstantFloat( Type type, double float_value )
{
    assert( IsFloatingPoint( type ) && "GenerateConstantFloat given a non-floating type" );
    assert( IsScalarType( type ) && "GenerateConstantFloat given a non-scalar type" );
    return llvm::ConstantFP::get( m_Runtime.GetLLVMType( type ), float_value );
}

llvm::Constant* LLVMWriter::GenerateConstantIntVector( Type type,
                                                       const std::vector<unsigned long>& ints )
{
    assert( IsIntegral( type ) && "GenerateConstantIntVector given a non-integral type" );
    assert( IsVectorType( type ) && "GenerateConstantIntVector given a non-vector type" );
    assert( ints.size() == GetNumElementsInType( type ) &&
            "GenerateConstantIntVector given wrong number of values" );
    std::vector<llvm::Constant*> data( GetNumElementsInType( type ) );
    for( unsigned i = 0; i < GetNumElementsInType( type ); ++i )
        data[i] = GenerateConstantInt( GetScalarType( type ), ints[i] );
    return llvm::ConstantVector::get( data );
}

llvm::Constant* LLVMWriter::GenerateConstantFloatVector( Type type,
                                                         const std::vector<double>& floats )
{
    assert( IsFloatingPoint( type ) && "GenerateConstantFloatVector given a non-floating type" );
    assert( IsVectorType( type ) && "GenerateConstantFloatVector given a non-vector type" );
    assert( floats.size() == GetNumElementsInType( type ) &&
            "GenerateConstantFloatVector given wrong number of values" );
    std::vector<llvm::Constant*> data( GetNumElementsInType( type ) );
    for( unsigned i = 0; i < GetNumElementsInType( type ); ++i )
        data[i] = GenerateConstantFloat( GetScalarType( type ), floats[i] );
    return llvm::ConstantVector::get( data );
}

llvm::Constant* LLVMWriter::GenerateConstantIntMatrix( Type type,
                                                       const std::vector<unsigned long>& ints )
{
    assert( IsIntegral( type ) && "GenerateConstantIntMatrix given a non-integral type" );
    assert( IsMatrixType( type ) && "GenerateConstantIntMatrix given a non-matrix type" );
    assert( ints.size() == GetNumElementsInType( type ) &&
            "GenerateConstantIntMatrix given wrong number of values" );
    std::vector<llvm::Constant*> data( GetNumColumnsInType( type ) );
    for( unsigned i = 0; i < GetNumColumnsInType( type ); ++i )
    {
        std::vector<unsigned long> column_data(
            ints.begin() + i * GetNumRowsInType( type ),
            ints.begin() + ( i + 1 ) * GetNumRowsInType( type ) );
        data[i] = GenerateConstantIntVector( GetMatrixColumnType( type ), column_data );
    }
    return llvm::ConstantArray::get( llvm::cast<llvm::ArrayType>( m_Runtime.GetLLVMType( type ) ),
                                     data );
}

llvm::Constant* LLVMWriter::GenerateConstantFloatMatrix( Type type,
                                                         const std::vector<double>& floats )
{
    assert( IsFloatingPoint( type ) && "GenerateConstantFloatMatrix given a non-floating type" );
    assert( IsMatrixType( type ) && "GenerateConstantFloatMatrix given a non-matrix type" );
    assert( floats.size() == GetNumElementsInType( type ) &&
            "GenerateConstantFloatMatrix given wrong number of values" );
    std::vector<llvm::Constant*> data( GetNumColumnsInType( type ) );
    for( unsigned i = 0; i < GetNumColumnsInType( type ); ++i )
    {
        std::vector<double> column_data( floats.begin() + i * GetNumRowsInType( type ),
                                         floats.begin() + ( i + 1 ) * GetNumRowsInType( type ) );
        data[i] = GenerateConstantFloatVector( GetMatrixColumnType( type ), column_data );
    }
    return llvm::ConstantArray::get( llvm::cast<llvm::ArrayType>( m_Runtime.GetLLVMType( type ) ),
                                     data );
}


llvm::Constant* LLVMWriter::GenerateGenericValue( const GenericValue& generic_value )
{
    Type type = generic_value.GetType().GetType();
    
#define CODE_INTEGER( Type, TYPE ) \
    case TYPE: \
        return GenerateConstantInt( TYPE, generic_value.Get##Type() );

#define CODE_INTEGER_VECTOR( Type, TYPE, n ) \
    case TYPE: \
    { \
        auto v = generic_value.Get##Type(); \
        std::vector<jl_ulong> values( &v[0], &v[0] + ( n ) ); \
        return GenerateConstantIntVector( TYPE, values ); \
    }
    
#define CODE_INTEGER_MATRIX( Type, TYPE, n ) \
    case TYPE: \
    { \
        auto m = generic_value.Get##Type(); \
    std::vector<jl_ulong> values( &m[0][0], &m[0][0] + ( n ) ); \
        return GenerateConstantIntMatrix( TYPE, values ); \
    }
    
#define CODE_INTEGER_N( Type, TYPE ) \
    CODE_INTEGER( Type, TYPE ); \
    CODE_INTEGER_VECTOR( Type##2, TYPE##2, 2 );\
    CODE_INTEGER_VECTOR( Type##3, TYPE##3, 3 );\
    CODE_INTEGER_VECTOR( Type##4, TYPE##4, 4 );\
    CODE_INTEGER_MATRIX( Type##2x2, TYPE##2x2, 2 * 2 );\
    CODE_INTEGER_MATRIX( Type##2x3, TYPE##2x3, 2 * 3 );\
    CODE_INTEGER_MATRIX( Type##2x4, TYPE##2x4, 2 * 4 );\
    CODE_INTEGER_MATRIX( Type##3x2, TYPE##3x2, 3 * 2 );\
    CODE_INTEGER_MATRIX( Type##3x3, TYPE##3x3, 3 * 3 );\
    CODE_INTEGER_MATRIX( Type##3x4, TYPE##3x4, 3 * 4 );\
    CODE_INTEGER_MATRIX( Type##4x2, TYPE##4x2, 4 * 2 );\
    CODE_INTEGER_MATRIX( Type##4x3, TYPE##4x3, 4 * 3 );\
    CODE_INTEGER_MATRIX( Type##4x4, TYPE##4x4, 4 * 4 );

#define CODE_FLOATING( Type, TYPE ) \
    case TYPE: \
        return GenerateConstantFloat( TYPE, generic_value.Get##Type() );

#define CODE_FLOATING_VECTOR( Type, TYPE, n ) \
    case TYPE: \
    { \
        auto v = generic_value.Get##Type(); \
    std::vector<jl_double> values( &v[0], &v[0] + ( n ) ); \
        return GenerateConstantFloatVector( TYPE, values ); \
    }
    
#define CODE_FLOATING_MATRIX( Type, TYPE, n ) \
    case TYPE: \
    { \
        auto m = generic_value.Get##Type(); \
    std::vector<jl_double> values( &m[0][0], &m[0][0] + ( n ) ); \
        return GenerateConstantFloatMatrix( TYPE, values ); \
    }    
    
#define CODE_FLOATING_N( Type, TYPE ) \
    CODE_FLOATING( Type, TYPE );\
    CODE_FLOATING_VECTOR( Type##2, TYPE##2, 2 );\
    CODE_FLOATING_VECTOR( Type##3, TYPE##3, 3 );\
    CODE_FLOATING_VECTOR( Type##4, TYPE##4, 4 ); \
    CODE_FLOATING_MATRIX( Type##2x2, TYPE##2x2, 2 * 2 ); \
    CODE_FLOATING_MATRIX( Type##2x3, TYPE##2x3, 2 * 3 ); \
    CODE_FLOATING_MATRIX( Type##2x4, TYPE##2x4, 2 * 4 ); \
    CODE_FLOATING_MATRIX( Type##3x2, TYPE##3x2, 3 * 2 ); \
    CODE_FLOATING_MATRIX( Type##3x3, TYPE##3x3, 3 * 3 ); \
    CODE_FLOATING_MATRIX( Type##3x4, TYPE##3x4, 3 * 4 ); \
    CODE_FLOATING_MATRIX( Type##4x2, TYPE##4x2, 4 * 2 ); \
    CODE_FLOATING_MATRIX( Type##4x3, TYPE##4x3, 4 * 3 ); \
    CODE_FLOATING_MATRIX( Type##4x4, TYPE##4x4, 4 * 4 );
    
    switch( type )
    {
    CODE_INTEGER_N( Bool, Type::BOOL );
    CODE_INTEGER_N( Char, Type::CHAR );
    CODE_INTEGER_N( Short, Type::SHORT );
    CODE_INTEGER_N( Int, Type::INT );
    CODE_INTEGER_N( Long, Type::LONG );
    CODE_INTEGER_N( UChar, Type::UCHAR );
    CODE_INTEGER_N( UShort, Type::USHORT );
    CODE_INTEGER_N( UInt, Type::UINT );
    CODE_INTEGER_N( ULong, Type::ULONG );

    CODE_FLOATING_N( Float, Type::FLOAT );
    CODE_FLOATING_N( Double, Type::DOUBLE );
    default:
        assert( false && "Trying to generate a generic value of unhandled type" );
    }
    
#undef CODE_FLOATING_N
#undef CODE_FLOATING_VECTOR
#undef CODE_FLOATING
#undef CODE_INTEGER_N
#undef CODE_INTEGER_VECTOR
#undef CODE_INTEGER
}

} // namespace Compiler
} // namespace JoeLang
