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

#pragma once

#include <memory>
#include <vector>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{

class DeclarationSpecifier;
using DeclarationSpecifier_up = std::unique_ptr<DeclarationSpecifier>;
class Parser;
class SemaAnalyzer;
class StorageClassSpecifier;
using StorageClassSpecifier_up = std::unique_ptr<StorageClassSpecifier>;
class TypeQualifierSpecifier;
using TypeQualifierSpecifier_up = std::unique_ptr<TypeQualifierSpecifier>;
enum class TypeSpec;
class TypeSpecifier;
using TypeSpecifier_up = std::unique_ptr<TypeSpecifier>;

/**
  * \class DeclSpecs
  * \brief A class to hold declaration specifier information
  *
  */
class DeclSpecs
{
public:
    DeclSpecs();

    bool AnalyzeDeclSpecs(
                         const std::vector<DeclarationSpecifier_up>& decl_specs,
                         SemaAnalyzer& sema );

    void SetIsIn( bool is_in );
    void SetIsUniform( bool is_uniform );
    void SetIsVarying( bool is_varying );

    bool IsConst() const;
    bool IsUniform() const;
    bool IsVarying() const;
    bool IsStatic() const;
    bool IsIn() const;
    bool IsOut() const;
    bool IsInline() const;
    Type GetType() const;

    static
    Type DeduceType( std::vector<TypeSpec> type_specs, SemaAnalyzer& sema );
private:
    bool m_IsConst;
    bool m_IsUniform;
    bool m_IsVarying;
    bool m_IsStatic;
    bool m_IsExtern;
    bool m_IsIn;
    bool m_IsOut;
    bool m_IsInline;
    Type m_Type;
};

/**
  * \class DeclarationSpecifier
  * \ingroup Tokens
  * \brief Matches any declaration specifier
  *
  * DeclarationSpecifier = TypeSpecifier
  *                      | TypeQualifierSpecifier
  *                      | StorageClassSpecifier
  */
class DeclarationSpecifier : public JoeLang::Compiler::Token
{
public:
    explicit
    DeclarationSpecifier( TokenTy sub_class_id );
    virtual
    ~DeclarationSpecifier();

    static
    bool Parse( Parser& parser, DeclarationSpecifier_up& token );

    /** Used for casting **/
    static
    bool classof( const Token* d );
    static
    bool classof( const DeclarationSpecifier* d );
private:
};

//
// The ordering here is imporant for VariableDeclarationList::PerformSema
// Types which are incompatible with others must be at the top
//
enum class TypeSpec
{
    VOID,
    STRING,

    FLOAT,
    FLOAT2,
    FLOAT3,
    FLOAT4,
    FLOAT2x2,
    FLOAT2x3,
    FLOAT2x4,
    FLOAT3x2,
    FLOAT3x3,
    FLOAT3x4,
    FLOAT4x2,
    FLOAT4x3,
    FLOAT4x4,

    DOUBLE,
    DOUBLE2,
    DOUBLE3,
    DOUBLE4,
    DOUBLE2x2,
    DOUBLE2x3,
    DOUBLE2x4,
    DOUBLE3x2,
    DOUBLE3x3,
    DOUBLE3x4,
    DOUBLE4x2,
    DOUBLE4x3,
    DOUBLE4x4,

    BOOL,
    BOOL2,
    BOOL3,
    BOOL4,
    BOOL2x2,
    BOOL2x3,
    BOOL2x4,
    BOOL3x2,
    BOOL3x3,
    BOOL3x4,
    BOOL4x2,
    BOOL4x3,
    BOOL4x4,

    CHAR2,
    CHAR3,
    CHAR4,
    CHAR2x2,
    CHAR2x3,
    CHAR2x4,
    CHAR3x2,
    CHAR3x3,
    CHAR3x4,
    CHAR4x2,
    CHAR4x3,
    CHAR4x4,

    SHORT2,
    SHORT3,
    SHORT4,
    SHORT2x2,
    SHORT2x3,
    SHORT2x4,
    SHORT3x2,
    SHORT3x3,
    SHORT3x4,
    SHORT4x2,
    SHORT4x3,
    SHORT4x4,

    INT2,
    INT3,
    INT4,
    INT2x2,
    INT2x3,
    INT2x4,
    INT3x2,
    INT3x3,
    INT3x4,
    INT4x2,
    INT4x3,
    INT4x4,

    LONG2,
    LONG3,
    LONG4,
    LONG2x2,
    LONG2x3,
    LONG2x4,
    LONG3x2,
    LONG3x3,
    LONG3x4,
    LONG4x2,
    LONG4x3,
    LONG4x4,

    UCHAR2,
    UCHAR3,
    UCHAR4,
    UCHAR2x2,
    UCHAR2x3,
    UCHAR2x4,
    UCHAR3x2,
    UCHAR3x3,
    UCHAR3x4,
    UCHAR4x2,
    UCHAR4x3,
    UCHAR4x4,

    USHORT2,
    USHORT3,
    USHORT4,
    USHORT2x2,
    USHORT2x3,
    USHORT2x4,
    USHORT3x2,
    USHORT3x3,
    USHORT3x4,
    USHORT4x2,
    USHORT4x3,
    USHORT4x4,

    UINT2,
    UINT3,
    UINT4,
    UINT2x2,
    UINT2x3,
    UINT2x4,
    UINT3x2,
    UINT3x3,
    UINT3x4,
    UINT4x2,
    UINT4x3,
    UINT4x4,

    ULONG2,
    ULONG3,
    ULONG4,
    ULONG2x2,
    ULONG2x3,
    ULONG2x4,
    ULONG3x2,
    ULONG3x3,
    ULONG3x4,
    ULONG4x2,
    ULONG4x3,
    ULONG4x4,

    UCHAR,
    CHAR,
    USHORT,
    SHORT,
    UINT,
    INT,
    ULONG,
    LONG,
    SIGNED,
    UNSIGNED
};
/**
  * \class TypeSpecifier
  * \ingroup Tokens
  * \brief A class to hold one type specifier
  *
  * TypeSpecifier =   'void' | 'char' | 'short' | 'int' | 'long' | 'float'
  *                 | 'float2' | 'float3' | 'float4' | 'double' | 'signed' 
  *                 | 'unsigned' | 'string' ...
  */
class TypeSpecifier : public JoeLang::Compiler::DeclarationSpecifier
{
public:
    explicit
    TypeSpecifier( TypeSpec type_spec );
    virtual
    ~TypeSpecifier();

    TypeSpec GetSpecifier() const;

    Type GetType() const;

    static
    bool Parse( Parser& parser, TypeSpecifier_up& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const TypeSpecifier* d );
private:
    TypeSpec m_TypeSpec;
};

enum class TypeQualifier
{
    CONST,
    VOLATILE,
    INLINE // Not really a type qualifier, but is's ignored like volatile
};

/**
  * \class TypeQualifierSpecifier
  * \ingroup Tokens
  * \brief matches a type qualifier
  *
  * TypeQualifierSpecifier = 'const' | 'volatile' | 'inline'
  */
class TypeQualifierSpecifier : public JoeLang::Compiler::DeclarationSpecifier
{
public:
    explicit
    TypeQualifierSpecifier( TypeQualifier type_qual );
    virtual
    ~TypeQualifierSpecifier();

    TypeQualifier GetQualifier() const;

    static
    bool Parse( Parser& parer, TypeQualifierSpecifier_up& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const TypeQualifierSpecifier* d );
private:
    TypeQualifier m_TypeQualifier;
};

enum class StorageClass
{
    STATIC,
    EXTERN,
    UNIFORM,
    VARYING,
    IN,
    OUT,
    INOUT
};

/**
  * \class StorageClassSpecifier
  * \ingroup Tokens
  * \brief A class to hold one storage class specifier
  *
  * StorageClassSpecifier = 'static' | 'extern' | 'uniform' | 'varying'
  */
class StorageClassSpecifier : public JoeLang::Compiler::DeclarationSpecifier
{
public:
    explicit
    StorageClassSpecifier( StorageClass type_spec );
    virtual
    ~StorageClassSpecifier();

    StorageClass GetStorageClass() const;

    static
    bool Parse( Parser& parser, StorageClassSpecifier_up& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const StorageClassSpecifier* d );
private:
    StorageClass m_StorageClass;
};

} // namespace Compiler
} // namespace JoeLang
