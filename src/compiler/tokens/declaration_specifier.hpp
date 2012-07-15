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

#include <compiler/tokens/token.hpp>

namespace JoeLang
{
enum class Type;

namespace Compiler
{

class Parser;


/**
  * \class DeclSpecs
  * \brief A class to hold declaration specifier information
  *
  */
class DeclSpecs
{
public:
    DeclSpecs( bool is_const, Type type );

    bool IsConst() const;
    Type GetType() const;

private:
    bool m_IsConst;
    Type m_Type;
};

/**
  * \class DeclarationSpecifier
  * \ingroup Tokens
  * \brief Matches any declaration specifier
  *
  * DeclarationSpecifier = TypeSpecifier | TypeQualifier | StorageClassSpecifier
  */
class DeclarationSpecifier : public JoeLang::Compiler::Token
{
public:
    explicit
    DeclarationSpecifier( TokenTy sub_class_id );
    virtual
    ~DeclarationSpecifier();

    virtual
    void Print( int depth ) const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<DeclarationSpecifier>& token );

    /** Used for casting **/
    static
    bool classof( const Token* d );
    static
    bool classof( const DeclarationSpecifier* d );
private:
};

/**
  * \class TypeSpecifier
  * \ingroup Tokens
  * \brief A class to hold one type specifier
  *
  * TypeSpecifier =   'void' | 'char' | 'short' | 'int' | 'long' | 'float'
  *                 | 'double' | 'signed' | 'unsigned' | 'string'
  */
class TypeSpecifier : public JoeLang::Compiler::DeclarationSpecifier
{
public:
    // the ordering here is imporant for VariableDeclarationList::PerformSema
    enum class TypeSpec
    {
        VOID,
        STRING,
        FLOAT,
        DOUBLE,
        BOOL,
        CHAR,
        SHORT,
        INT,
        LONG,
        SIGNED,
        UNSIGNED
    };

    explicit
    TypeSpecifier( TypeSpec type_spec );
    virtual
    ~TypeSpecifier();

    virtual
    void Print( int depth ) const override;

    TypeSpec GetSpecifier() const;

    static
    bool Parse( Parser& parser, std::unique_ptr<TypeSpecifier>& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const TypeSpecifier* d );
private:
    TypeSpec m_TypeSpec;
};

/**
  * \class TypeQualifier
  * \ingroup Tokens
  * \brief matches a type qualifier
  *
  * TypeQualifier = 'const' | 'volatile'
  */
class TypeQualifier : public JoeLang::Compiler::DeclarationSpecifier
{
public:
    enum class TypeQual
    {
        CONST,
        VOLATILE
    };

    explicit
    TypeQualifier( TypeQual type_qual );
    virtual
    ~TypeQualifier();

    virtual
    void Print( int depth ) const override;

    TypeQual GetQualifier() const;

    static
    bool Parse( Parser& parer, std::unique_ptr<TypeQualifier>& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const TypeQualifier* d );
private:
    TypeQual m_TypeQual;
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
    enum class StorageClass
    {
        STATIC,
        EXTERN,
        UNIFORM,
        VARYING
    };

    explicit
    StorageClassSpecifier( StorageClass type_spec );
    virtual
    ~StorageClassSpecifier();

    virtual
    void Print( int depth ) const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<StorageClassSpecifier>& token );

    static
    bool classof( const DeclarationSpecifier* d );
    static
    bool classof( const StorageClassSpecifier* d );
private:
    StorageClass m_StorageClass;
};

} // namespace Compiler
} // namespace JoeLang
