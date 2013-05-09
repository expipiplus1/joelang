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

#include <compiler/tokens/expressions/expression.hpp>
#include <compiler/tokens/token.hpp>
#include <joelang/types.hpp>

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

namespace llvm
{
    class Value;
};

namespace JoeLang
{
    enum class Type;

    namespace Compiler
    {
        enum class TerminalType;

        class CodeGenerator;
        class GenericValue;
        class Parser;
        class SemaAnalyzer;
    } // namespace Compiler
} // namespace JoeLang

namespace JoeLang
{
namespace Compiler
{

/**
  * \class LiteralExpression
  * \ingroup Expressions
  * \brief Matches any kind of literal
  *
  * LiteralExpression =   IntegerLiteralExpression
  *                     | FloatingLiteralExpression
  *                     | StringLiteralExpressionon
  *                     | CharacterLiteralExpression
  *                     | BooleanLiteralExpression
  */
class LiteralExpression : public JoeLang::Compiler::Expression
{
public:
    LiteralExpression( TokenTy sub_class_id );

    /**
      * Does nothing
      */
    virtual
    bool ResolveIdentifiers( SemaAnalyzer& sema ) override;

    /**
      * Does nothing
      */
    virtual
    bool PerformSema( SemaAnalyzer& sema ) override final;

    virtual
    std::set<Function_sp> GetCallees() const override final;

    virtual
    std::set<Variable_sp> GetVariables() const override final;

    virtual
    std::set<Variable_sp> GetWrittenToVariables(
                                        bool is_assigned ) const override final;

    virtual
    bool IsConst() const override;

    virtual
    GenericValue GetValue() const = 0;

    static
    bool Parse( Parser& parser,
                Expression_up& token );

    static
    std::unique_ptr<LiteralExpression> Create( const GenericValue& v );

    static
    bool classof( const Token* e );
    static
    bool classof( const LiteralExpression* e );
};

/**
  * \class IntegerLiteralExpression
  * \ingroup Expressions
  * \brief Matches integer literals
  *
  * IntegerLiteralExpression = integer_literal
  */
class IntegerLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    /** \enum Suffix
      *   A enumeration of the possible integer suffixes **/
    enum class Suffix
    {
        NONE,
        CHAR,
        INT,
        LONG,
        SHORT,
        UNSIGNED,
        UNSIGNED_CHAR,
        UNSIGNED_INT,
        UNSIGNED_LONG,
        UNSIGNED_SHORT
    };

    /**
      * \param value
      *   The value of this literal
      * \param suffix
      *   The integer suffix
      */
    explicit
    IntegerLiteralExpression( jl_ulong value,
                              Suffix suffix );
    virtual
    ~IntegerLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    GenericValue GetValue() const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<IntegerLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const IntegerLiteralExpression* e );
private:
    /**
      * This function parses an integer into value and suffix
      * \param string
      *   The string to parse to an integer
      * \param value
      *   The value of the integer
      * \param suffix
      *   The suffix at the end of the string
      * \returns true if parsed successfully
      */
    static
    bool ParseInteger( std::string string,
                       jl_ulong&  value,
                       Suffix&     suffix );

    jl_ulong m_Value;
    Suffix m_Suffix;
};

/**
  * \class FloatingLiteralExpression
  * \ingroup Expressions
  * \brief Matches floating literals
  *
  * FloatingLiteralExpression = floating_literal
  */
class FloatingLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    /**
      * \enum Suffix
      *   An enumeration of the possible floating point suffixes
      * \todo implement half and fixed
      */
    enum class Suffix
    {
        NONE,
        SINGLE,
    };

    FloatingLiteralExpression( double value, Suffix suffix );
    virtual
    ~FloatingLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    GenericValue GetValue() const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<FloatingLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const FloatingLiteralExpression* e );
private:
    /**
      * This function parses an float into value and suffix
      * \param string
      *   The string to parse to an float
      * \param value
      *   The value of the float
      * \param suffix
      *   The suffix at the end of the string
      * \returns true if parsed successfully
      */
    static
    bool ParseFloat( std::string string,
                     double& value,
                     Suffix& suffix );

    double m_Value;
    Suffix m_Suffix;
};

/**
  * \class BooleanLiteralExpression
  * \ingroup Expressions
  * \brief Matches boolean literals
  *
  * BooleanLiteralExpression = 'true' | 'false'
  */
class BooleanLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    BooleanLiteralExpression( bool value );
    virtual
    ~BooleanLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    GenericValue GetValue() const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<BooleanLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const BooleanLiteralExpression* e );
private:
    bool m_Value;
};

/**
  * \class StringLiteralExpression
  * \ingroup Expressions
  * \brief Matches string literals
  *
  * StringLiteralExpression = string_literal
  */
class StringLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    StringLiteralExpression( std::string value );
    virtual
    ~StringLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    GenericValue GetValue() const override;

    const std::string& GetString() const;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<StringLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const StringLiteralExpression* e );
private:
    /**
      * This function parses an unescaped and quoted string into it's value
      * \param string
      *   The quoted and escaped string
      * \param unescaped_string
      *   The value of the literal
      * \returns true if parsed successfully
      */
    static
    bool UnquoteAndUnescapeString( const std::string& string,
                                   std::string& unescaped_string );

    std::string m_Value;
};

/**
  * \class CharacterLiteralExpression
  * \ingroup Expressions
  * \brief Matches character literals
  *
  * CharacterLiteralExpression = character_literal
  */
class CharacterLiteralExpression : public JoeLang::Compiler::LiteralExpression
{
public:
    explicit
    CharacterLiteralExpression( char value );
    virtual
    ~CharacterLiteralExpression();

    virtual
    llvm::Value* CodeGen( CodeGenerator& code_gen ) const;

    virtual
    void Write( ShaderWriter& shader_writer ) const override;

    virtual
    CompleteType GetType() const override;

    virtual
    GenericValue GetValue() const override;

    static
    bool Parse( Parser& parser,
                std::unique_ptr<CharacterLiteralExpression>& token );

    static
    bool classof( const Expression* e );
    static
    bool classof( const CharacterLiteralExpression* e );
private:
    /**
      * This function parses an unescaped and quoted char into it's value
      * \param character
      *   The quoted and escaped character
      * \param unescaped_char
      *   The value of the literal
      * \returns true if parsed successfully
      */
    static
    bool UnquoteAndUnescapeChar( const std::string& character,
                                 char& unescaped_char );

    char m_Value;
};

} // namespace Compiler
} // namespace JoeLang
