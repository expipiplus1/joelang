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

namespace JoeLang
{
namespace Compiler
{

/**
  * \def CHECK_PARSER
  * Checks if the parser is good to continue, if it isn't return from the
  * current function.
  *
  * To be used after calling any Parser::Expect* function and continuing on a
  * false result.
  */
#define CHECK_PARSER \
    if( !parser.Good() ) return false; \
    else (void)0

#define CHECK_CODE_GENERATOR \
    if( !code_generator.Good() ) return nullptr; \
    else (void)0

/**
  * \defgroup Tokens
  * Tokens must implement a static member function Parse taking a reference to a
  * Parser and a reference to a pointer to the token type. Parse should return
  * true if the token was parsed successfully, otherwise false. It should also
  * return a pointer to the successfully parsed token or nullptr in token
  */

/**
  * \class Token
  * \ingroup Tokens
  * \brief Abstract class for any token in the CST
  */
class Token
{
public:
    enum class TokenTy
    {
        TranslationUnit,

        InitDeclarator,
        Declarator,
        ArraySpecifier,
        Initializer,

        PassDefinition,
        PassDeclarationOrIdentifier,

        TypeSpecifier,
        TypeQualifier,
        StorageClassSpecifier,
        DeclarationSpecifier_Start = TypeSpecifier,
        DeclarationSpecifier_End = StorageClassSpecifier,

        EmptyDeclaration,
        PassDeclaration,
        TechniqueDeclaration,
        VariableDeclarationList,
        FunctionDefinition,
        Declaration_Start = EmptyDeclaration,
        Declaration_End = FunctionDefinition,

        AssignmentOperator,

        SubscriptOperator,
        ArgumentListOperator,
        MemberAccessOperator,
        IncrementOrDecrementOperator,
        PostfixOperator_Start = SubscriptOperator,
        PostfixOperator_End = IncrementOrDecrementOperator,

        StateAssignmentStatement,

        AssignmentExpression,
        ConditionalExpression,

        LogicalOrExpression,
        LogicalAndExpression,
        InclusiveOrExpression,
        ExclusiveOrExpression,
        AndExpression,
        EqualityExpression,
        RelationalExpression,
        ShiftExpression,
        AdditiveExpression,
        MultiplicativeExpression,
        BinaryOperatorExpression_Start = LogicalOrExpression,
        BinaryOperatorExpression_End = MultiplicativeExpression,

        CastExpression,
        UnaryExpression,
        PostfixExpression,
        IdentifierExpression,

        IntegerLiteralExpression,
        FloatingLiteralExpression,
        BooleanLiteralExpression,
        StringLiteralExpression,
        CharacterLiteralExpression,
        LiteralExpression_Start = IntegerLiteralExpression,
        LiteralExpression_End = CharacterLiteralExpression,

        Expression_Start = AssignmentExpression,
        Expression_End = CharacterLiteralExpression,
    };

    explicit
    Token( TokenTy sub_class_id );
    virtual
    ~Token();

    virtual
    void Print( int depth ) const = 0;

    /** Used for casting **/
    TokenTy GetSubClassID() const;
    /** Used for casting **/
    static
    bool classof( const Token* t );
private:
    const TokenTy m_SubClassID;
};

} // namespace Compiler
} // namespace JoeLang

