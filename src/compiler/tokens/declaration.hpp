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

#pragma once

#include <memory>
#include <string>
#include <vector>

#include <compiler/tokens/token.hpp>

namespace JoeLang
{

class Technique;

namespace Compiler
{

class CodeGenerator;
class DeclarationSpecifier;
class Declarator;
class Parser;
class PassDeclarationOrIdentifier;
class PassDefinition;
class SemaAnalyzer;
class TechniqueDefinition;

/**
  * \class DeclarationBase
  * \ingroup Tokens
  * \brief Abstract class for top level declarations
  *
  * DeclarationBase =   EmptyDeclaration
  *                   | PassDeclaration
  *                   | TechniqueDeclaration
  */
class DeclarationBase : public JoeLang::Compiler::Token
{
public:
    enum class DeclarationTy
    {
        EmptyDeclaration,
        PassDeclaration,
        TechniqueDeclaration,
        VariableOrFunctionDeclaration
    };

    DeclarationBase( DeclarationTy sub_class_id );
    virtual
    ~DeclarationBase    ();

    /**
      * Performs semantic ananysis on the declaration
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) = 0;


    /**
      * Parses any top level declaration
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully
      *   false if the parse failed
      */
    static
    bool Parse          ( Parser&                           parser,
                          std::unique_ptr<DeclarationBase>& token );

    /** Used for casting **/
    DeclarationTy GetSubClassID() const;
    /** Used for casting **/
    static
    bool classof( const DeclarationBase* d );

private:
    // Subclass identifier for casts
    const DeclarationTy m_subClassID;
};

/**
  * \class EmptyDeclaration
  * \ingroup Tokens
  * \brief Matches an empty declaration
  *
  * EmptyDeclaration = ';'
  */
class EmptyDeclaration : public JoeLang::Compiler::DeclarationBase
{
public:
    EmptyDeclaration    ();
    virtual
    ~EmptyDeclaration   ();

    /**
      * Performs semantic ananysis on the declaration
      * This function does nothing
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual
    void Print          ( int depth ) const override;

    /**
      * Parses an empty declaration
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse          ( Parser&                            parser,
                          std::unique_ptr<EmptyDeclaration>& token );

    static
    bool classof( const DeclarationBase* d );
    static
    bool classof( const EmptyDeclaration* e );
};

/**
  * \class PassDeclaration
  * \ingroup Tokens
  * \brief Matches a pass declaration or definition
  *
  * PassDeclaration =   'pass' identifier ';'
  *                   | 'pass' identifier PassDefinition
  */
class PassDeclaration : public JoeLang::Compiler::DeclarationBase
{
public:
    /**
      * \param name
      *   The identifier for this pass
      * \param definition
      *   This pass's definition if it has one, otherwise nullptr
      */
    PassDeclaration ( std::string                       name,
                      std::unique_ptr<PassDefinition>   definition );

    virtual
    ~PassDeclaration();

    /**
      * Performs semantic ananysis on the declaration
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual
    void    Print   ( int depth ) const override;

    /** \returns this pass's name **/
    const std::string&                      GetName         () const;
    /** \returns whether this pass declaration has a definition **/
    bool                                    HasDefinition   () const;
    /** \returns this pass declaration's definition if it has one, otherwise
      * nullptr **/
    const std::unique_ptr<PassDefinition>&  GetDefinition   () const;

    /**
      * Parses a pass declaration
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \returns
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                std::unique_ptr<PassDeclaration>& token );

    static
    bool classof( const DeclarationBase* d );
    static
    bool classof( const PassDeclaration* e );
private:
    /** This pass declaration's identifier **/
    std::string                     m_name;
    /** Thie pass declaration's definition if it has one, otherwise nullptr **/
    std::unique_ptr<PassDefinition> m_definition;
};

/**
  * \class TechniqueDeclaration
  * \ingroup Tokens
  * \brief Matches a technique definition
  *
  * TechniqueDeclaration = 'technique' identifier '{'
  *                        (PassDeclarationOrIdentifier)* '}'
  */
class TechniqueDeclaration : public JoeLang::Compiler::DeclarationBase
{
public:
    using PassDeclarationVector =
                    std::vector< std::unique_ptr<PassDeclarationOrIdentifier> >;

    /**
      * This constructor Asserts on null declarations or identifiers
      * \param name
      *   The identifier for this technique
      * \param passes
      *   A vector of pass declarations or identifiers
      */
    TechniqueDeclaration( std::string name,
                          PassDeclarationVector passes );

    virtual
    ~TechniqueDeclaration();

    /**
      * Performs semantic ananysis on the declaration
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    /** \returns this technique's name **/
    const std::string& GetName() const;

    /**
      * Generates the technique represented by this token.
      * \param code_gen
      *   The code generator used for creating llvm code.
      * \returns The technique
      */
    Technique GenerateTechnique( CodeGenerator& code_gen ) const;

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual
    void Print( int depth ) const;

    /**
      * Parses a technique declaration
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser, std::unique_ptr<TechniqueDeclaration>& token );

    static
    bool classof( const DeclarationBase* d );
    static
    bool classof( const TechniqueDeclaration* e );
private:
    /** This technique's identifier **/
    std::string m_name;
    /** This technique's passes **/
    PassDeclarationVector m_passes;
};

//------------------------------------------------------------------------------
// VariableOrFunctionDeclaration
//------------------------------------------------------------------------------
/**
  * \class VariableOrFunctionDeclaration
  * \ingroup Tokens
  * \brief A C declaration
  *
  * VariableOrFunctionDeclaration = DeclarationSpecifiers identifier ';'
  */
class VariableOrFunctionDeclaration : public JoeLang::Compiler::DeclarationBase
{
public:
    using DeclSpecsVector = std::vector<std::unique_ptr<DeclarationSpecifier> >;
    using DeclaratorVector = std::vector<std::unique_ptr<Declarator> >;

    explicit
    VariableOrFunctionDeclaration( DeclSpecsVector decl_specs );
    virtual
    ~VariableOrFunctionDeclaration();

    virtual
    void Print( int depth ) const override;

    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<VariableOrFunctionDeclaration>& token );

private:
    DeclSpecsVector m_declSpecs;
};

/**
  * \class DeclarationSpecifiers
  * \ingroup Tokens
  * \brief Matches any declaration specifier
  *
  * DeclarationSpecifiers = (TypeSpecifier | TypeQualifier
  *                          | StorageClassSpecifier)*
  */
class DeclarationSpecifier : public JoeLang::Compiler::Token
{
public:
    DeclarationSpecifier();
    virtual
    ~DeclarationSpecifier();

    virtual
    void Print( int depth ) const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<DeclarationSpecifier>& token );
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
    enum class TypeSpec
    {
        VOID,
        BOOL,
        CHAR,
        SHORT,
        INT,
        LONG,
        SIGNED,
        UNSIGNED,
        FLOAT,
        DOUBLE,
        STRING
    };

    explicit
    TypeSpecifier( TypeSpec type_spec );
    virtual
    ~TypeSpecifier();

    virtual
    void Print( int depth ) const override;

    static
    bool Parse( Parser& parser, std::unique_ptr<TypeSpecifier>& token );

private:
    TypeSpec m_typeSpec;
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

    static
    bool Parse( Parser& parer, std::unique_ptr<TypeQualifier>& token );

private:
    TypeQual m_typeQual;
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

private:
    StorageClass m_storageClass;
};

} // namespace Compiler
} // namespace JoeLang
