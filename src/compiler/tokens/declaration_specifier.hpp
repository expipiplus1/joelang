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
class TypeQualifierSpecifier;
using TypeQualifierSpecifier_up = std::unique_ptr<TypeQualifierSpecifier>;
class TypeSpecifier;
using TypeSpecifier_up = std::unique_ptr<TypeSpecifier>;
class StorageClassSpecifier;
using StorageClassSpecifier_up = std::unique_ptr<StorageClassSpecifier>;
enum class TypeSpec;

/**
  * \class DeclSpecs
  * \brief A class to hold declaration specifier information
  *
  */
class DeclSpecs
{
public:
    DeclSpecs();

    void AnalyzeDeclSpecs(
                         const std::vector<DeclarationSpecifier_up>& decl_specs,
                         SemaAnalyzer& sema );

    bool IsConst() const;
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

// The ordering here is imporant for VariableDeclarationList::PerformSema
enum class TypeSpec
{
    VOID,
    STRING,
    FLOAT,
    FLOAT4,
    DOUBLE,
    BOOL,
    CHAR,
    SHORT,
    INT,
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
  *                 | 'float4' | 'double' | 'signed' | 'unsigned' | 'string'
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
