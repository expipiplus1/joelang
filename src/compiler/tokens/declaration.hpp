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
namespace Compiler
{

class SemaAnalyzer;
class Parser;
class PassDefinition;
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
        TechniqueDeclaration
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
  * TechniqueDeclaration = 'technique' identifier TechniqueDefinition
  */
class TechniqueDeclaration : public JoeLang::Compiler::DeclarationBase
{
public:
    /**
      * This constructor asserts on a null definition
      * \param name
      *   The identifier for this technique
      * \param definition
      *   The definition for this technique
      */
    TechniqueDeclaration( std::string name,
                          std::unique_ptr<TechniqueDefinition> definition );

    virtual
    ~TechniqueDeclaration();

    /**
      * Performs semantic ananysis on the declaration
      * \param sema
      *   The AstBuilder which contains the symbol table and things
      */
    virtual
    void PerformSema( SemaAnalyzer& sema ) override;

    /** \returns this technique's definition **/
    const TechniqueDefinition& GetDefinition() const;

    /** \returns this technique's name **/
    const std::string& GetName() const;

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
    /** This technique's definition **/
    std::unique_ptr<TechniqueDefinition> m_definition;
};

/**
  * \class PassDeclarationOrIdentifier
  * \ingroup Tokens
  * \brief A helper token for TechniqueDefinitions
  *
  * This token matches either an identifier or a pass declaration
  *
  * PassDeclarationOrIdentifier =   identifier ';'
  *                               | PassDeclaration
  */
class PassDeclarationOrIdentifier : public JoeLang::Compiler::Token
{
public:
    /**
      * This constructor will assert if the definition is not null and the name
      * is not of zero length
      * \param identifier
      *   The identifier for this pass
      * \param declaration
      *   This pass's declaration if it has one, otherwise nullptr
      */
    PassDeclarationOrIdentifier( std::string                      identifier,
                                 std::unique_ptr<PassDeclaration> declaration );

    virtual
    ~PassDeclarationOrIdentifier();

    /**
      * Prints this node in the CST
      * \param depth
      *   The indentation at which to print
      */
    virtual
    void    Print   ( int depth ) const override;

    /** \returns if this token is an identifier for a pass **/
    bool                   IsIdentifier    () const;
    /** This funciton will assert if this token represents a declaration
      * \returns this token's identifier **/
    const std::string&     GetIdentifier   () const;
    /** This function will assert if this token represents an identifier
      * \returns this token's declaration **/
    const PassDeclaration& GetDeclaration  () const;
    PassDeclaration&       GetDeclaration  ();

    /**
      * Parses a pass declaration or identifier
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
                std::unique_ptr<PassDeclarationOrIdentifier>& token );

private:
    /** This pass declaration's identifier if it has one, otherwise "" **/
    std::string                      m_identifier;
    /** This token's declaration if it has one, otherwise nullptr **/
    std::unique_ptr<PassDeclaration> m_declaration;
};


} // namespace Compiler
} // namespace JoeLang
