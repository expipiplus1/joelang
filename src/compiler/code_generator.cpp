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
#include <iostream>
#include <memory>
#include <stack>
#include <string>
#include <utility>
#include <vector>

#include <x86intrin.h>

#include <llvm/BasicBlock.h>
#include <llvm/Function.h>
#include <llvm/GlobalVariable.h>
#include <llvm/IRBuilder.h>
#include <llvm/Module.h>
#include <llvm/PassManager.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Target/TargetData.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>

#include <joelang/context.hpp>
#include <joelang/state.hpp>
#include <joelang/state_assignment.hpp>
#include <joelang/technique.hpp>
#include <compiler/casting.hpp>
#include <compiler/complete_type.hpp>
#include <compiler/function.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/runtime.hpp>
#include <compiler/type_properties.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/statements/compound_statement.hpp>
#include <runtime/types.hpp>

namespace JoeLang
{
namespace Compiler
{

CodeGenerator::CodeGenerator( Runtime& runtime )
    :m_Runtime( runtime )
    ,m_LLVMModule( runtime.GetModule() )
    ,m_LLVMBuilder( m_Runtime.GetLLVMContext() )
    ,m_LLVMExecutionEngine( llvm::ExecutionEngine::createJIT( 
                                                   m_LLVMModule,
                                                   nullptr,
                                                   nullptr,
                                                   llvm::CodeGenOpt::Aggressive,
                                                   false ) )
    ,m_LLVMFunctionPassManager( new llvm::FunctionPassManager( m_LLVMModule ) )
    ,m_LLVMModulePassManager( new llvm::PassManager() )
{
    assert( m_LLVMExecutionEngine && "Couldn't create a jit" );

    llvm::PassManagerBuilder pass_manager_builder;
    pass_manager_builder.OptLevel = 3;
    pass_manager_builder.Inliner = llvm::createFunctionInliningPass();
    pass_manager_builder.populateFunctionPassManager(
                                                   *m_LLVMFunctionPassManager );
    pass_manager_builder.populateModulePassManager(
                                                   *m_LLVMModulePassManager );

    m_LLVMFunctionPassManager->doInitialization();
}

CodeGenerator::~CodeGenerator()
{
}

void CodeGenerator::GenerateFunctions(
                                     const std::vector<Function_sp>& functions )
{
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
                                                    const TranslationUnit& ast )
{
    std::vector<Technique> techniques;
    for( const auto& declaration : ast.GetDeclarations() )
    {
        if( isa<TechniqueDeclaration>(declaration) )
        {
            TechniqueDeclaration& t =
                    static_cast<TechniqueDeclaration&>( *declaration.get() );
            techniques.push_back( t.GenerateTechnique( *this ) );
        }
    }

    OptimizeModule();

    m_LLVMModule->dump();

    return std::move(techniques);
}

std::unique_ptr<llvm::ExecutionEngine> CodeGenerator::TakeExecutionEngine()
{
    return std::move( m_LLVMExecutionEngine );
}

std::unique_ptr<StateAssignmentBase> CodeGenerator::GenerateStateAssignment(
        const StateBase& state,
        const Expression& expression,
        const std::string& name )
{
    /// TODO assigning arrays
    assert( expression.GetType().GetType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );

    llvm::Function* function = CreateFunctionFromExpression( expression, name );
    void* function_ptr = m_LLVMExecutionEngine->getPointerToFunction(function);

    //
    // Cast to the appropriate type
    //
    StateAssignmentBase* sa;
    switch( state.GetType() )
    {
    case Type::BOOL:
        sa = new StateAssignment<jl_bool>
         ( static_cast<const State<bool>&>(state),
           reinterpret_cast<bool(*)()>(function_ptr) );
        break;
    case Type::FLOAT:
        sa = new StateAssignment<jl_float>
         ( static_cast<const State<jl_float>&>(state),
           reinterpret_cast<jl_float(*)()>(function_ptr) );
        break;
    case Type::FLOAT4:
    {
        // TODO move this function to llvm
        std::function<jl_float4()> wrapper = [function_ptr]()
        {
            __m128 r;
            r = reinterpret_cast<__m128(*)()>(function_ptr)();
            jl_float4* f = reinterpret_cast<jl_float4*>(&r);
            return *f;
        };
        sa = new StateAssignment<jl_float4>
         ( static_cast<const State<jl_float4>&>(state),
           wrapper );
        break;
    }
    case Type::DOUBLE:
        sa = new StateAssignment<jl_double>
         ( static_cast<const State<jl_double>&>(state),
           reinterpret_cast<jl_double(*)()>(function_ptr) );
        break;
    case Type::I8:
        sa = new StateAssignment<jl_i8>
         ( static_cast<const State<jl_i8>&>(state),
           reinterpret_cast<jl_i8(*)()>(function_ptr) );
        break;
    case Type::I16:
        sa = new StateAssignment<jl_i16>
         ( static_cast<const State<jl_i16>&>(state),
           reinterpret_cast<jl_i16(*)()>(function_ptr) );
        break;
    case Type::I32:
        sa = new StateAssignment<jl_i32>
         ( static_cast<const State<jl_i32>&>(state),
           reinterpret_cast<jl_i32(*)()>(function_ptr) );
        break;
    case Type::I64:
        sa = new StateAssignment<jl_i64>
         ( static_cast<const State<jl_i64>&>(state),
           reinterpret_cast<jl_i64(*)()>(function_ptr) );
        break;
    case Type::U8:
        sa = new StateAssignment<jl_u8>
         ( static_cast<const State<jl_u8>&>(state),
           reinterpret_cast<jl_u8(*)()>(function_ptr) );
        break;
    case Type::U16:
        sa = new StateAssignment<jl_u16>
         ( static_cast<const State<jl_u16>&>(state),
           reinterpret_cast<jl_u16(*)()>(function_ptr) );
        break;
    case Type::U32:
        sa = new StateAssignment<jl_u32>
         ( static_cast<const State<jl_u32>&>(state),
           reinterpret_cast<jl_u32(*)()>(function_ptr) );
        break;
    case Type::U64:
        sa = new StateAssignment<jl_u64>
         ( static_cast<const State<jl_u64>&>(state),
           reinterpret_cast<jl_u64(*)()>(function_ptr) );
        break;
    case Type::STRING:
    {
        // Cast from string to std::string and destroy original
        const auto ToString = [function_ptr]()
                                 {
                                    jl_string s =
                                          reinterpret_cast<jl_string(*)()>(
                                                              function_ptr)();
                                    std::string ret(
                                          reinterpret_cast<const char*>(s.data),
                                          s.size);
                                   delete[] s.data;
                                   return ret;
                                 };
        sa = new StateAssignment<std::string>(
                 static_cast<const State<std::string>&>(state),
                 ToString );
        break;
    }
    default:
        assert( false && "Generating a stateassignment of unhandled type" );
        sa = nullptr;
    }
    return std::unique_ptr<StateAssignmentBase>( sa );
}

GenericValue CodeGenerator::EvaluateExpression( const Expression& expression )
{
    /// TODO is this really necessary?
    //assert( expression.IsConst() &&
            //"Trying to evaluate a non-const expression" );
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    auto insert_point = m_LLVMBuilder.saveAndClearIP();

    llvm::Function* function = CreateFunctionFromExpression(
                                                        expression,
                                                        "TemporaryEvaluation" );
    // Make this function private
    function->setLinkage( llvm::GlobalVariable::PrivateLinkage );
    void* function_ptr = m_LLVMExecutionEngine->getPointerToFunction(function);

    //
    // Extract the result
    //
    GenericValue ret;
    switch( expression.GetType().GetType() )
    {
    case Type::BOOL:
        ret = GenericValue( reinterpret_cast<jl_bool(*)()>(function_ptr)() );
        break;
    case Type::I8:
        ret = GenericValue( reinterpret_cast<jl_i8(*)()>(function_ptr)() );
        break;
    case Type::I16:
        ret = GenericValue( reinterpret_cast<jl_i16(*)()>(function_ptr)() );
        break;
    case Type::I32:
        ret = GenericValue( reinterpret_cast<jl_i32(*)()>(function_ptr)() );
        break;
    case Type::I64:
        ret = GenericValue( reinterpret_cast<jl_i64(*)()>(function_ptr)() );
        break;
    case Type::U8:
        ret = GenericValue( reinterpret_cast<jl_u8(*)()>(function_ptr)() );
        break;
    case Type::U16:
        ret = GenericValue( reinterpret_cast<jl_u16(*)()>(function_ptr)() );
        break;
    case Type::U32:
        ret = GenericValue( reinterpret_cast<jl_u32(*)()>(function_ptr)() );
        break;
    case Type::U64:
        ret = GenericValue( reinterpret_cast<jl_u64(*)()>(function_ptr)() );
        break;
    case Type::FLOAT:
        ret = GenericValue( reinterpret_cast<jl_float(*)()>(function_ptr)() );
        break;
    case Type::FLOAT4:
        {
            __m128 m = reinterpret_cast<__m128(*)()>(function_ptr)();
            ret = GenericValue( *reinterpret_cast<jl_float4*>(&m) );
            break;
        }
    case Type::DOUBLE:
        ret = GenericValue( reinterpret_cast<jl_double(*)()>(function_ptr)() );
        break;
    case Type::STRING:
        ret = GenericValue( reinterpret_cast<jl_string(*)()>(function_ptr)() );
        break;
    default:
        assert( false &&
                "Trying to run a function returning an unhandled type" );
    }

    /// Cant remove the function because it trashes global variables...
    m_LLVMExecutionEngine->freeMachineCodeForFunction( function );
    function->eraseFromParent();

    m_LLVMBuilder.restoreIP( insert_point );

    return ret;
}

//
// Type Construction
//

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
    llvm::Value* ret = llvm::UndefValue::get(
                                m_Runtime.GetLLVMType( CompleteType( type ) ) );
    // Loop over all the arguments inserting as many elements as necessary
    unsigned p = 0;
    for( const auto& argument : arguments )
    {
        llvm::Value* argument_value = argument->CodeGen( *this );
        if( argument->GetType().IsVectorType() )
        {
            for( unsigned i = 0; i < argument->GetType().GetNumElements(); ++i )
            {
                llvm::Value* new_element = m_LLVMBuilder.CreateExtractElement(
                        argument_value,
                        CreateInteger( i, Type::U32 ) );
                ret = m_LLVMBuilder.CreateInsertElement(
                                                ret,
                                                new_element,
                                                CreateInteger( p, Type::U32 ) );
                ++p;
            }
        }
        else
        {
            assert( argument->GetType().IsScalarType() &&
                    "Trying to use an unhandled type in vector constructor" );
            ret = m_LLVMBuilder.CreateInsertElement(
                                                ret,
                                                argument_value,
                                                CreateInteger( p, Type::U32 ) );
            ++p;
        }
    }
    assert( p == GetNumElementsInType( type ) &&
            "constructing a vector with an incorrect number of elements" );
    return ret;
}

llvm::Value* CodeGenerator::CreateScalarConstructor(
                                                    Type type,
                                                    const Expression& argument )
{
    return argument.CodeGen( *this );
}

//
// Cast Operators
//

llvm::Value* CodeGenerator::CreateCast( const Expression& e,
                                        const CompleteType& type )
{
    /// todo make this better

    const CompleteType& e_type  = e.GetType();
    llvm::Value* e_value = e.CodeGen( *this );

    assert( !type.IsArrayType() && "todo, casting to arrays" );
    assert( !e_type.IsArrayType() && "todo, casting from arrays" );
    assert( !e_type.IsUnknown() && !type.IsUnknown() &&
            "Can't cast an unknown type" );

    if( !e_value )
        return nullptr;

    if( type.IsVoid() )
    {
        assert( e_type.IsVoid() && "Can only cast void to void" );
        // we've already codegened it
        return nullptr;
    }
    //
    // For a cast to bool, compare to zero
    //
    if( type.GetType() == Type::BOOL )
    {
        if( IsFloatingPoint( e_type.GetBaseType() ) )
            return m_LLVMBuilder.CreateFCmpOEQ(
                        e_value,
                        llvm::ConstantFP::getNullValue( e_value->getType() ) );
        return m_LLVMBuilder.CreateIsNotNull( e_value );
    }

    if( IsFloatingPoint( type.GetBaseType() ) )
    {
        if( IsFloatingPoint( e_type.GetBaseType() ) )
            return m_LLVMBuilder.CreateFPCast( e_value,
                                               m_Runtime.GetLLVMType( type ) );
        if( IsSigned( e_type.GetBaseType() ) )
            return m_LLVMBuilder.CreateSIToFP( e_value,
                                               m_Runtime.GetLLVMType( type ) );
        return m_LLVMBuilder.CreateUIToFP( e_value,
                                           m_Runtime.GetLLVMType( type ) );
    }

    assert( IsIntegral( type.GetBaseType() ) && "Type should be integral" );
    if( IsIntegral( e_type.GetBaseType() ) )
    {
        return m_LLVMBuilder.CreateIntCast( e_value,
                                            m_Runtime.GetLLVMType( type ),
                                            IsSigned( e_type.GetBaseType() ) );
    }

    assert( IsFloatingPoint( e_type.GetBaseType() ) &&
            "e_type should be floating point" );
    if( IsSigned( type.GetBaseType() ) )
        return m_LLVMBuilder.CreateFPToSI( e_value,
                                           m_Runtime.GetLLVMType( type ) );
    else
        return m_LLVMBuilder.CreateFPToUI( e_value,
                                           m_Runtime.GetLLVMType( type ) );
}

//
// Unary Operators
//


llvm::Value* CodeGenerator::CreateNeg( const Expression& e )
{
    return m_LLVMBuilder.CreateNeg( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateNot( const Expression& e )
{
    return m_LLVMBuilder.CreateNot( e.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLNot( const Expression& e )
{
    return m_LLVMBuilder.CreateIsNotNull( e.CodeGen( *this ) );
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
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l,
                                        const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ),
                                   r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsIntegral() )
        return m_LLVMBuilder.CreateICmpEQ( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().IsFloatingPoint() )
        return m_LLVMBuilder.CreateFCmpOEQ( l.CodeGen( *this ),
                                            r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_EQUAL,
                                            {l.CodeGen( *this ),
                                             r.CodeGen( *this )},
                                            m_LLVMBuilder );
    assert( false && "Trying to compare unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateNeq( const Expression& l,
                                       const Expression& r )
{
    if( l.GetType().IsIntegral() )
        return m_LLVMBuilder.CreateICmpNE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().IsFloatingPoint() )
        return m_LLVMBuilder.CreateFCmpONE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
        return m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_NOTEQUAL,
                                            {l.CodeGen( *this ),
                                             r.CodeGen( *this )},
                                            m_LLVMBuilder );
    assert( false && "Trying to compare unhandled type" );
    return nullptr;
}

llvm::Value* CodeGenerator::CreateLT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_LLVMBuilder.CreateFAdd( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else if( l.GetType().IsIntegral() )
        return m_LLVMBuilder.CreateAdd( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
    else if( l.GetType().GetType() == Type::STRING )
    {
        llvm::Value* ret = m_Runtime.CreateRuntimeCall(
                                                RuntimeFunction::STRING_CONCAT,
                                                {l.CodeGen( *this ),
                                                 r.CodeGen( *this )},
                                                m_LLVMBuilder );
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
        return m_LLVMBuilder.CreateFSub( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateSub( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_LLVMBuilder.CreateFMul( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateMul( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsFloatingPoint() )
        return m_LLVMBuilder.CreateFDiv( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        if( l.GetType().IsSigned() )
            return m_LLVMBuilder.CreateSDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
        else
            return m_LLVMBuilder.CreateUDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetType() == r.GetType() &&
            "Type mismatch in code gen for binary operator" );
    if( l.GetType().IsSigned() )
        return m_LLVMBuilder.CreateSRem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateURem( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
}

//
// Ternary Operators
//

llvm::Value* CodeGenerator::CreateSelect( const Expression& condition,
                                          const Expression& true_expression,
                                          const Expression& false_expression )
{
    return m_LLVMBuilder.CreateSelect( condition.CodeGen( *this ),
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
    return m_LLVMBuilder.CreateLoad( ptr );
}

llvm::Value* CodeGenerator::CreateArrayIndexPointerTo( const Expression& array,
                                                       const Expression& index )
{
    llvm::Value* array_ptr = m_LLVMBuilder.CreateConstGEP2_32(
                                    array.CodeGenPointerTo( *this ),
                                    0, 0 );
    return m_LLVMBuilder.CreateGEP( array_ptr, index.CodeGen( *this ) );
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
            m_LLVMBuilder.CreateRetVoid();
        else
            m_LLVMBuilder.CreateRet( v );
    }
    else
    {
        m_LLVMBuilder.CreateRetVoid();
    }
}

//
// Constants
//
llvm::Constant* CodeGenerator::CreateInteger( unsigned long long value,
                                              Type type )
{
    assert( IsIntegral( type ) &&
            "Trying to create an integer constant of non-integer type" );
    return llvm::ConstantInt::get( m_Runtime.GetLLVMType( type ),
                                   value,
                                   IsSigned( type ) );
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
    assert( value.size() == GetVectorSize( type ) &&
            "Wrong number of values to construct vector constant" );
    std::vector<llvm::Constant*> data;
    data.reserve( value.size() );
    for( double d : value )
        data.push_back( CreateFloating( d, GetElementType( type ) ) );
    return llvm::ConstantVector::get( data );
}

llvm::Constant* CodeGenerator::CreateString( const std::string& value )
{
    /// TODO use CreateGlobalString here
    //
    // Set up the globalvariable holding the characters in an array
    //

    // u32 here because that's what's used in string for size
    llvm::Constant* size_constant = CreateInteger( value.size(), Type::U32 );

    llvm::ArrayType* array_type = llvm::ArrayType::get(
                                         m_Runtime.GetLLVMType( Type::U8 ),
                                         value.size() );

    std::vector<llvm::Constant*> characters;
    for( char c : value )
        characters.push_back( CreateInteger( c, Type::U8 ) );

    llvm::Constant* data_constant = llvm::ConstantArray::get( array_type,
                                                              characters );

    llvm::GlobalVariable* data_array = new llvm::GlobalVariable(
                                           *m_LLVMModule,
                                           array_type,
                                           true,
                                           llvm::GlobalVariable::PrivateLinkage,
                                           data_constant,
                                           "string_data" );

    llvm::Constant* zero = CreateInteger( 0, Type::U32 );
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
    llvm::GlobalVariable* ret = new llvm::GlobalVariable(
                                     *m_LLVMModule,
                                     t,
                                     is_const,
                                     llvm::GlobalVariable::PrivateLinkage,
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
    if( variable.IsConst() && variable.IsParameter() )
        return variable.GetLLVMPointer();
    return m_LLVMBuilder.CreateLoad( variable.GetLLVMPointer() );
}

llvm::Value* CodeGenerator::CreateAssignment( const Expression& variable,
                                              const Expression& e )
{
    assert( variable.IsLValue() &&
            "Trying to codegen an assignment to a RValue" );
    assert( !variable.IsConst() &&
            "Trying to codegen an assignment to a const variable" );
    assert( variable.GetType() == e.GetType() &&
            "Trying to assign a variable with a different type" );
    llvm::Value* assigned_value = e.CodeGen( *this );
    m_LLVMBuilder.CreateStore( assigned_value,
                               variable.CodeGenPointerTo( *this ) );
    return m_LLVMBuilder.CreateLoad( variable.CodeGenPointerTo( *this ) );
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
                                                m_LLVMModule );
    assert( function && "Error generating llvm function" );
    return function;
}

void CodeGenerator::CreateFunctionDefinition( llvm::Function* function,
                                              const CompoundStatement_up& body )
{
    assert( function && "Trying to define a null function" );
    assert( body && "Trying to define function with a null body" );

    llvm::BasicBlock* llvm_body = llvm::BasicBlock::Create(
                                                     m_Runtime.GetLLVMContext(),
                                                     "entry",
                                                     function );

    m_LLVMBuilder.SetInsertPoint( llvm_body );

    assert( body->AlwaysReturns() &&
            "Generating code for a statement which doesn't always return" );

    body->CodeGen( *this );
    OptimizeFunction( *function );
}

void CodeGenerator::BindFunctionParameters(
                                    llvm::Function* function,
                                    std::vector<Variable_sp>& parameters ) const
{
    assert( function->arg_size() == parameters.size() &&
            "mismatch in the number of parameters to function" );

    auto arg_iterator = function->arg_begin();
    for( unsigned i = 0; i < parameters.size(); ++i, ++arg_iterator )
    {
        assert( arg_iterator != function->arg_end() &&
                "llvm arg iterator overrun" );
        assert( arg_iterator->getType() == m_Runtime.GetLLVMType(
                                                   parameters[i]->GetType() ) &&
                "Type mismatch in function parameters" );

        parameters[i]->SetParameterPointer( arg_iterator );
    }
    assert( arg_iterator == function->arg_end() &&
            "llvm arg iterator underrun" );
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
    return m_LLVMBuilder.CreateCall( function->GetLLVMFunction(),
                                     std::move(llvm_arguments) );
}

void CodeGenerator::CreateDestroyTemporaryCalls()
{
    while( !m_Temporaries.empty() )
    {
        llvm::Value* v = m_Temporaries.top();
        m_Runtime.CreateRuntimeCall( RuntimeFunction::STRING_DESTROY,
                                     {v},
                                     m_LLVMBuilder );
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
                                                m_LLVMModule );
    assert( function && "Error generating llvm function" );

    llvm::BasicBlock* body = llvm::BasicBlock::Create(
                                                    m_Runtime.GetLLVMContext(),
                                                    "entry",
                                                    function );
    m_LLVMBuilder.SetInsertPoint( body );

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
                                         m_LLVMBuilder );
    }
    CreateDestroyTemporaryCalls();
    assert( v && "Invalid expression llvm::Value*" );
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    //
    // Set it as the return value for the function
    //
    m_LLVMBuilder.CreateRet( v );
    assert( !llvm::verifyFunction( *function, llvm::PrintMessageAction ) &&
            "Function not valid" );

    OptimizeFunction( *function );
    return function;
}

void CodeGenerator::OptimizeFunction( llvm::Function& function )
{
    m_LLVMFunctionPassManager->run( function );
}

void CodeGenerator::OptimizeModule()
{
    assert( m_LLVMModule && "Trying to optimize a null module" );

    m_LLVMModulePassManager->run( *m_LLVMModule );
}

} // namespace Compiler
} // namespace JoeLang

