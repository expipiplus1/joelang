/*
    Copyright 2011 Joe Hermaszewski. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are
    permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice, this list
    of conditions and the following disclaimer in the documentation and/or other materials
    provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY JOE HERMASZEWSKI "AS IS" AND ANY EXPRESS OR IMPLIED
    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JOE HERMASZEWSKI OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
    ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    The views and conclusions contained in the software and documentation are those of the
    authors and should not be interpreted as representing official policies, either expressed
    or implied, of Joe Hermaszewski.
*/

#pragma once

#include <string>

namespace JoeLang
{
namespace Lexer
{

class KeywordTokenMatcher;

enum TokenType
{
    // Misc
    WHITESPACE,
    IDENTIFIER,
    END_OF_FILE,

    // Punctuation
    OPEN_BRACE,
    CLOSE_BRACE,
    SEMICOLON,
    EQUALS,

    // Keywords
    PASS,
    TECHNIQUE,
};

class TokenMatcher
{
public:
    TokenMatcher( TokenType terminal_type, bool is_significant = true );
    virtual ~TokenMatcher();

    TokenType GetTokenType() const;
    bool      IsSignificant() const;
    virtual std::string Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const = 0;

private:
    TokenType m_tokenType;
    bool m_isSignificant;
};

class LiteralTokenMatcher : public JoeLang::Lexer::TokenMatcher
{
public:
    LiteralTokenMatcher( TokenType token_type, std::string literal, bool is_significant = true );
    LiteralTokenMatcher( TokenType token_type, std::string literal, std::string name, bool is_significant = true );
    virtual ~LiteralTokenMatcher();

    virtual std::string Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const;

private:
    std::string m_literal;
    std::string m_name;
};

class KeywordTokenMatcher : public JoeLang::Lexer::TokenMatcher
{
public:
    KeywordTokenMatcher( TokenType token_type, std::string literal, bool is_significant = true );
    KeywordTokenMatcher( TokenType token_type, std::string literal, std::string name, bool is_significant = true );
    virtual ~KeywordTokenMatcher();

    //
    // Match if the input matches m_literal, and the next character isn't in [a-zA-Z0-9_]
    //
    virtual std::string Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const;

private:
    std::string m_literal;
    std::string m_name;
};

class IdentifierTokenMatcher : public JoeLang::Lexer::TokenMatcher
{
public:
    IdentifierTokenMatcher( TokenType token_type = TokenType::IDENTIFIER, bool is_significant = true );
    virtual ~IdentifierTokenMatcher();

    virtual std::string Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const;

private:
    static bool IsNonDigit( char c );
    static bool IsDigitOrNonDigit( char c );
};

class WhitespaceTokenMatcher : public JoeLang::Lexer::TokenMatcher
{
public:
    WhitespaceTokenMatcher( TokenType token_type = TokenType::WHITESPACE, bool is_significant = false );
    virtual ~WhitespaceTokenMatcher();

    virtual std::string Match( std::string::const_iterator string_begin, std::string::const_iterator string_end ) const;
private:
    static bool IsWhitespace( char c );
};

} // namespace Lexer
} // namespace JoeLang
