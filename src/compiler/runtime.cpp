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

#include "runtime.hpp"

#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/ADT/OwningPtr.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/system_error.h>
#include <llvm/Support/TargetSelect.h>

#include <engine/types.hpp>
#include <engine/internal/type_properties.hpp>

namespace JoeLang
{
namespace Compiler
{

Runtime::Runtime()
    :m_LLVMContext( llvm::getGlobalContext() )
{
    llvm::InitializeNativeTarget();

    llvm::OwningPtr<llvm::MemoryBuffer> buffer;
    llvm::MemoryBuffer::getFile( "runtime.bc", buffer );
    assert( buffer && "Couldn't load runtime library" );
    m_RuntimeModule = llvm::ParseBitcodeFile ( buffer.get(),
                                               m_LLVMContext );
    assert( m_RuntimeModule && "Couldn't parse runtime library" );


    m_StringEqualFunction = m_RuntimeModule->getFunction( "String_Equal" );
    assert( m_StringEqualFunction && "Can't find String_Equal in runtime" );
    m_StringNotEqualFunction = m_RuntimeModule->getFunction( "String_NotEqual");
    assert( m_StringNotEqualFunction &&
           "Can't find String_NotEqual in runtime" );
    m_StringConcatFunction = m_RuntimeModule->getFunction( "String_Concat" );
    assert( m_StringConcatFunction && "Can't find String_Concat in runtime" );

    llvm::Type* size_type = GetLLVMType( Type::U32 );
    llvm::Type* char_ptr_type = llvm::Type::getInt8PtrTy( m_LLVMContext );
    m_StringType = llvm::StructType::create( std::vector<llvm::Type*>
                                               {size_type, char_ptr_type} );
    assert( m_StringType &&
            "Can't find String type" );
    /// TODO make assertions about string type;
}

Runtime::~Runtime()
{
    delete m_RuntimeModule;
}

llvm::LLVMContext& Runtime::GetLLVMContext()
{
    return m_LLVMContext;
}

//
// String functions
//

llvm::Function* Runtime::GetStringConcatFunction() const
{
    return m_StringConcatFunction;
}

//
// Misc
//

llvm::Type* Runtime::GetLLVMType( Type base_type,
                                  const std::vector<unsigned>& array_extents )
{
    llvm::Type* t;
    if( base_type == Type::DOUBLE )
        t = llvm::Type::getDoubleTy( m_LLVMContext );
    else if( base_type == Type::FLOAT )
        t = llvm::Type::getFloatTy( m_LLVMContext );
    else if( base_type == Type::BOOL )
        t = llvm::Type::getInt1Ty( m_LLVMContext );
    else if( IsIntegral( base_type ) )
        t = llvm::Type::getIntNTy( m_LLVMContext, SizeOf(base_type)*8 );
    else if( base_type == Type::STRING )
        t = m_StringType;
    else
    {
        assert( false && "Trying to get the llvm::Type of an unhandled Type" );
        return nullptr;
    }

    for( auto extent = array_extents.rbegin();
         extent != array_extents.rend();
         ++extent )
        t = llvm::ArrayType::get( t, *extent );

    return t;
}

} // namespace Compiler
} // namespace JoeLang

