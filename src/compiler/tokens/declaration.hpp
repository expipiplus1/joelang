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

#include <compiler/tokens/declaration_specifier.hpp>
#include <compiler/tokens/token.hpp>

namespace JoeLang
{

class Technique;

namespace Compiler
{

class CodeGenerator;
class CompoundStatement;
typedef std::unique_ptr<CompoundStatement> CompoundStatement_up;
class DeclarationSpecifier;
class InitDeclarator;
using InitDeclarator_up = std::unique_ptr<InitDeclarator>;
class Declarator;
using Declarator_up = std::unique_ptr<Declarator>;
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
    explicit
    DeclarationBase( TokenTy sub_class_id );
    virtual
    ~DeclarationBase    ();

    /**
      * Performs semantic ananysis on the declaration
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) = 0;

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
    static
    bool classof( const Token* t );
    static
    bool classof( const DeclarationBase* d );
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

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

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

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    /** \returns this pass's name **/
    const std::string&                      GetName         () const;
    /** \returns whether this pass declaration has a definition **/
    bool                                    HasDefinition   () const;
    /** \returns this pass declaration's definition if it has one, otherwise
      * nullptr **/
    const std::unique_ptr<PassDefinition>&  GetDefinition   () const;
    /** \returns this pass declaration's definition if it has one, otherwise
      * nullptr **/
    std::unique_ptr<PassDefinition>         TakeDefinition  ();

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
    std::string                     m_Name;
    /** Thie pass declaration's definition if it has one, otherwise nullptr **/
    std::unique_ptr<PassDefinition> m_Definition;
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

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

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
    std::string m_Name;
    /** This technique's passes **/
    PassDeclarationVector m_Passes;
};

/**
  * \class VariableListOrFunctionDefinition
  * \ingroup Tokens
  * \brief Helper to match declarations or definitions
  *
  * VariableListOrFunctionDefinition =   VariableDeclarationList
  *                                    | FunctionDefinition
  */
class VariableListOrFunctionDefinition
        : public JoeLang::Compiler::DeclarationBase
{
public:
    using DeclSpecsVector = std::vector<std::unique_ptr<DeclarationSpecifier> >;
    using DeclaratorVector = std::vector<std::unique_ptr<InitDeclarator> >;

    VariableListOrFunctionDefinition( TokenTy sub_class_id );
    virtual
    ~VariableListOrFunctionDefinition();

    /**
      * Parses a VariableListOrFunctionDefinition
      * This will parse the common tokens and then diverge
      * \param parser
      *   The current Parser
      * \param token
      *   The returned token on a successful parse
      * \return
      *   true upon parsing successfully,
      *   false if the parse failed
      */
    static
    bool Parse( Parser& parser,
                std::unique_ptr<VariableListOrFunctionDefinition>& token );
};

/**
  * \class VariableDeclarationList
  * \ingroup Tokens
  * \brief A C declaration
  *
  * VariableDeclarationList = DeclarationSpecifier+ (Declarator ',')+ ';'
  */
class VariableDeclarationList :
                    public JoeLang::Compiler::VariableListOrFunctionDefinition
{
public:
    using DeclSpecsVector = std::vector<std::unique_ptr<DeclarationSpecifier> >;
    using DeclaratorVector = std::vector<std::unique_ptr<InitDeclarator> >;

    /** This constructor asserts if given no decl_specs **/
    VariableDeclarationList( DeclSpecsVector decl_specs,
                             DeclaratorVector declarators);
    virtual
    ~VariableDeclarationList();

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool classof( const Token* t );
    static
    bool classof( const VariableDeclarationList* d );
private:
    DeclSpecsVector m_DeclSpecs;
    DeclaratorVector m_Declarators;
};

/**
  * \class FunctionDefinition
  * \ingroup Tokens
  * \brief A function definition
  *
  * FunctionDefinition = DeclarationSpecifier+ Declarator CompoundStatement
  */
class FunctionDefinition :
                    public JoeLang::Compiler::VariableListOrFunctionDefinition
{
public:
    using DeclSpecsVector = std::vector<std::unique_ptr<DeclarationSpecifier> >;

    /** This asserts that there is at least one declaration specifier and that
      * neither declarator or body are null **/
    FunctionDefinition( DeclSpecsVector decl_specs,
                        Declarator_up declarator,
                        CompoundStatement_up body );
    virtual
    ~FunctionDefinition();

    virtual
    bool PerformSema( SemaAnalyzer& sema ) override;

    static
    bool classof( const Token* t );
    static
    bool classof( const FunctionDefinition* d );
private:
    DeclSpecsVector m_DeclarationSpecifiers;
    Declarator_up m_Declarator;
    CompoundStatement_up m_Body;
};



} // namespace Compiler
} // namespace JoeLang
