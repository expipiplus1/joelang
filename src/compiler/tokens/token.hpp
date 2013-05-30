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
  *
  * Most Tokens have three states: Parsed, the token has been generated from
  * parsing the stream from the lexer; Analyzed, the token has undergone
  * semantic analysis (this should only happen once); CodeGened, the token has
  * generated some instructions to llvm (this should only happen once).
  */

/**
  * \class Token
  * \ingroup Tokens
  * \brief Abstract class for any token in the CST
  */
class Token
{
public:
    // todo, make this consistent with NodeTy, in terms of start and end points
    enum class TokenTy
    {
        //
        // Miscellaneous
        //

        TranslationUnit,

        InitDeclarator,
        Declarator,
        ArraySpecifier,
        Initializer,

        PassDefinition,
        PassDeclarationOrIdentifier,

        FunctionSpecifier,
        Parameter,
        SemanticSpecifier,

        //
        // DeclarationSpecifiers
        //

        DeclarationSpecifier_Start,
        TypeSpecifier,
        TypeQualifierSpecifier,
        StorageClassSpecifier,
        DeclarationSpecifier_End,

        //
        // Declarations
        //

        Declaration_Start,
        EmptyDeclaration,
        PassDeclaration,
        TechniqueDeclaration,
        VariableDeclarationList,
        FunctionDefinition,
        Declaration_End,

        //
        // Statements
        //

        Statement_Start,
        CompoundStatement,
        EmptyStatement,
        ExpressionStatement,
        ReturnStatement,
        Statement_End,

        //
        // Pass Statements
        //

        PassStatement_Start,
        StateAssignmentStatement,
        CompileStatement,
        PassStatement_End,

        //
        // PostfixOperators
        //
        PostfixOperator_Start,
        SubscriptOperator,
        ArgumentListOperator,
        MemberAccessOperator,
        IncrementOrDecrementOperator,
        PostfixOperator_End,

        //
        // Assignment Operators
        //

        AssignmentOperator,

        //
        // Expressions
        //
        Expression_Start,

        AssignmentExpression,
        ConditionalExpression,

        BinaryOperatorExpression_Start,
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
        BinaryOperatorExpression_End,

        CastExpression,
        UnaryExpression,
        PostfixExpression,
        TypeConstructorExpression,
        IdentifierExpression,

        LiteralExpression_Start,
        IntegerLiteralExpression,
        FloatingLiteralExpression,
        BooleanLiteralExpression,
        StringLiteralExpression,
        CharacterLiteralExpression,
        LiteralExpression_End,
        Expression_End = LiteralExpression_End,
    };

    explicit
    Token( TokenTy sub_class_id );
    virtual
    ~Token();

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

