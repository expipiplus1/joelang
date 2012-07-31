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

#include <llvm/BasicBlock.h>
#include <llvm/Function.h>
#include <llvm/GlobalVariable.h>
#include <llvm/IRBuilder.h>
#include <llvm/Module.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/JIT.h>

#include <engine/context.hpp>
#include <engine/state.hpp>
#include <engine/state_assignment.hpp>
#include <engine/technique.hpp>
#include <engine/internal/type_properties.hpp>
#include <compiler/casting.hpp>
#include <compiler/generic_value.hpp>
#include <compiler/runtime.hpp>
#include <compiler/variable.hpp>
#include <compiler/tokens/expression.hpp>
#include <compiler/tokens/declaration.hpp>
#include <compiler/tokens/definition.hpp>
#include <compiler/tokens/translation_unit.hpp>
#include <runtime/types.hpp>

namespace JoeLang
{
namespace Compiler
{

CodeGenerator::CodeGenerator( Runtime& runtime )
    :m_Runtime( runtime )
    ,m_LLVMModule( runtime.GetModule() )
    ,m_LLVMBuilder( m_Runtime.GetLLVMContext() )
    ,m_LLVMExecutionEngine( llvm::ExecutionEngine::createJIT( m_LLVMModule ) )
{
    assert( m_LLVMExecutionEngine && "Couldn't create a jit" );
}

CodeGenerator::~CodeGenerator()
{
}

void CodeGenerator::GenerateCode(
                 const TranslationUnit& ast,
                 std::vector<Technique>& techniques,
                 std::unique_ptr<llvm::ExecutionEngine>& llvm_execution_engine )
{
    for( const auto& declaration : ast.GetDeclarations() )
    {
        if( isa<TechniqueDeclaration>(declaration) )
        {
            TechniqueDeclaration& t =
                    static_cast<TechniqueDeclaration&>( *declaration.get() );
            techniques.push_back( t.GenerateTechnique( *this ) );
        }
        else if( isa<VariableDeclarationList>(declaration) )
        {
            VariableDeclarationList& v =
                    static_cast<VariableDeclarationList&>( *declaration.get() );
            v.CodeGen( *this );
        }
    }
    llvm_execution_engine = std::move( m_LLVMExecutionEngine );

    m_LLVMModule->dump();
}

std::unique_ptr<StateAssignmentBase> CodeGenerator::GenerateStateAssignment(
        const StateBase& state,
        const Expression& expression,
        const std::string& name )
{
    /// TODO assigning arrays
    assert( expression.GetReturnType() == state.GetType() &&
            "Type mismatch in state assignment code gen" );

    llvm::Function* function = CreateFunctionPtrFromExpression( expression,
                                                                name );
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

    llvm::Function* function = CreateFunctionPtrFromExpression(
                                                        expression,
                                                        "TemporaryEvaluation" );
    // Make this function private
    function->setLinkage( llvm::GlobalVariable::PrivateLinkage );
    void* function_ptr = m_LLVMExecutionEngine->getPointerToFunction(function);

    //
    // Extract the result
    //
    GenericValue ret;
    switch( expression.GetReturnType() )
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
    //m_LLVMExecutionEngine->freeMachineCodeForFunction( function );
    //function->eraseFromParent();

    m_LLVMBuilder.restoreIP( insert_point );

    return ret;
}

//
// Cast Operators
//

llvm::Value* CodeGenerator::CreateCast( const Expression& e, Type type )
{
    Type         e_type = e.GetReturnType();
    llvm::Value* e_code = e.CodeGen( *this );

    if( !e_code )
        return nullptr;

    //
    // For a cast to bool, compare to zero
    //
    if( type == Type::BOOL )
    {
        if( IsFloatingPoint( e_type ) )
            return m_LLVMBuilder.CreateFCmpOEQ(
                        e_code,
                        llvm::ConstantFP::getNullValue( e_code->getType() ) );
        return m_LLVMBuilder.CreateIsNotNull( e_code );
    }

    if( IsFloatingPoint( type ) )
    {
        if( IsFloatingPoint( e_type ) )
            return m_LLVMBuilder.CreateFPCast( e_code,
                                               m_Runtime.GetLLVMType( type ) );
        if( IsSigned( e_type ) )
            return m_LLVMBuilder.CreateSIToFP( e_code,
                                               m_Runtime.GetLLVMType( type ) );
        return m_LLVMBuilder.CreateUIToFP( e_code,
                                           m_Runtime.GetLLVMType( type ) );
    }

    assert( IsIntegral( type ) && "Type should be integral" );
    if( IsIntegral( e_type ) )
    {
        return m_LLVMBuilder.CreateIntCast( e_code,
                                            m_Runtime.GetLLVMType( type ),
                                            IsSigned( e_type ) );
    }

    assert( IsFloatingPoint( e_type ) && "e_type should be floating point" );
    if( IsSigned( type ) )
        return m_LLVMBuilder.CreateFPToSI( e_code,
                                           m_Runtime.GetLLVMType( type ) );
    else
        return m_LLVMBuilder.CreateFPToUI( e_code,
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
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLAnd( const Expression& l,
                                        const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ), r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateOr( const Expression& l, const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateOr( l.CodeGen( *this ),
                                   r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateXor( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateXor( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAnd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAnd( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateEq( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsIntegral( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateICmpEQ( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFCmpOEQ( l.CodeGen( *this ),
                                            r.CodeGen( *this ) );
    else if( l.GetReturnType() == Type::STRING )
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
    if( IsIntegral( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateICmpNE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFCmpONE( l.CodeGen( *this ),
                                           r.CodeGen( *this ) );
    else if( l.GetReturnType() == Type::STRING )
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
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGT( const Expression& l,
                                      const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGT( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateLTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSLE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateGTE( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateICmpSGE( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShl( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateShl( l.CodeGen( *this ),
                                    r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateShr( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    return m_LLVMBuilder.CreateAShr( l.CodeGen( *this ),
                                     r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateAdd( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFAdd( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else if( IsIntegral( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateAdd( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
    else if( l.GetReturnType() == Type::STRING )
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
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFSub( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateSub( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMul( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFMul( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        return m_LLVMBuilder.CreateMul( l.CodeGen( *this ),
                                        r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateDiv( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsFloatingPoint( l.GetReturnType() ) )
        return m_LLVMBuilder.CreateFDiv( l.CodeGen( *this ),
                                         r.CodeGen( *this ) );
    else
        if( IsSigned( l.GetReturnType() ) )
            return m_LLVMBuilder.CreateSDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
        else
            return m_LLVMBuilder.CreateUDiv( l.CodeGen( *this ),
                                             r.CodeGen( *this ) );
}

llvm::Value* CodeGenerator::CreateMod( const Expression& l,
                                       const Expression& r )
{
    assert( l.GetReturnType() == r.GetReturnType() &&
            "Type mismatch in code gen for binary operator" );
    if( IsSigned( l.GetReturnType() ) )
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
            "Trying to create a floating point constant of non-floating type" );
    return llvm::ConstantFP::get( m_Runtime.GetLLVMType( type ),
                                  value );
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
                                           llvm::GlobalVariable::CommonLinkage,
                                           data_constant,
                                           "string_data" );

    llvm::Constant *zero = CreateInteger( 0, Type::U32 );
    llvm::Constant* args[] = { zero, zero };
    llvm::Constant* data_ptr =
                    llvm::ConstantExpr::getInBoundsGetElementPtr( data_array,
                                                                  args );

    llvm::Constant* string = llvm::ConstantStruct::get(
          llvm::cast<llvm::StructType>(m_Runtime.GetLLVMType(Type::STRING)),
          std::vector<llvm::Constant*>
            {size_constant, data_ptr} );
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
                                Type type,
                                std::vector<unsigned> array_extents,
                                bool is_const,
                                const GenericValue& initializer,
                                const std::string& name )
{
    assert( ( initializer.GetType() == Type::UNKNOWN_TYPE &&
              type == initializer.GetUnderlyingType() ) ||
            "Initializer type mismatch" );
    assert( ( initializer.GetType() == Type::UNKNOWN_TYPE ||
              array_extents == initializer.GetArrayExtents() ) &&
            "Initializer array extents mismatch" );
    llvm::Type* t = m_Runtime.GetLLVMType( type, array_extents );
    llvm::Constant* init;
    if( initializer.GetType() != Type::UNKNOWN_TYPE )
        init = initializer.CodeGen( *this );
    else
        init = llvm::Constant::getNullValue( t );
    llvm::GlobalVariable* ret = new llvm::GlobalVariable(
                                     *m_LLVMModule,
                                     t,
                                     is_const,
                                     llvm::GlobalVariable::CommonLinkage,
                                     init,
                                     name );
    return ret;
}

llvm::Value* CodeGenerator::CreateVariableRead( const Variable& variable )
{
    return m_LLVMBuilder.CreateLoad( variable.GetLLVMPointer() );
}

void CodeGenerator::CreateVariableAssignment( const Expression& variable,
                                              const Expression& e )
{
    assert( variable.IsLValue() &&
            "Trying to codegen an assignment to a RValue" );
    assert( !variable.IsConst() &&
            "Trying to codegen an assignment to a const variable" );
    assert( variable.GetReturnType() == e.GetReturnType() &&
            "Trying to assign a variable with a different type" );
    llvm::Value* assigned_value = e.CodeGen( *this );
    m_LLVMBuilder.CreateStore( assigned_value,
                               variable.CodeGenPointerTo( *this ) );
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

llvm::Function* CodeGenerator::CreateFunctionPtrFromExpression(
                                                const Expression& expression,
                                                std::string name )
{
    assert( m_Temporaries.empty() && "Leftover temporaries" );

    /// Todo overload for get type from expression
    llvm::Type* return_type = m_Runtime.GetLLVMType(
                                                expression.GetUnderlyingType(),
                                                expression.GetArrayExtents() );
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
    if( expression.GetReturnType() == Type::STRING )
    {
        assert( expression.GetArrayExtents().size() == 0 &&
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

    return function;
}

} // namespace Compiler
} // namespace JoeLang

