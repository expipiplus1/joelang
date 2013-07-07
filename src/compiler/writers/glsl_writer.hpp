/*
    Copyright 2013 Joe Hermaszewski. All rights reserved.

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

#include <deque>
#include <map>
#include <set>
#include <sstream>
#include <string>

#include <compiler/code_dag/node_manager.hpp>

namespace JoeLang
{
class Context;

namespace Compiler
{

class CompileStatementNode;
class CompleteType;
class Function;
class Variable;

class GLSLWriter
{
public:
    static
    std::string GenerateGLSL( const Context& context,
                              const CompileStatementNode& compile_statement );

private:
    GLSLWriter( const Context& context );
    ~GLSLWriter();

    std::string Generate( const CompileStatementNode& compile_statement );

    //
    // Variable Writing
    //
    void WriteInputVariables( std::set<const Variable*> input_variables );

    void WriteOutputVariables( std::set<const Variable*> output_variables );

    void WriteUniformVariables( std::set<const Variable*> uniform_variables );

    unsigned GetVariableAttributeNumber( const Variable& variable );

    //
    // Function writing
    //
    void WriteFunctionDeclarations( std::set<const Function*> functions );

    void WriteFunctionDefinitions( std::set<const Function*> functions );

    void WriteFunctionHeader( const Function& function );

    void WriteMainFunction( const CompileStatementNode& compile_statement );

    //
    // Types
    //
    std::string GetTypeString( const CompleteType& type );

    std::string GetVariableTypeString( const Variable& variable );

    //
    // Misc
    //

    void NewLine( unsigned n = 1 );

    enum class IdentifierType;

    std::string MangleIdentifier( std::string identifier, IdentifierType identifier_type );

    void Error( const std::string& message );

    void Warning( const std::string& message );

    std::stringstream m_Source;

    const Context& m_Context;
    NodeManager m_NodeManager;

    std::map<const Variable*, const std::string> m_VariableNames;

    const static
    std::string s_GLSLVersion;

    //
    // All the functions to do with generating statements
    //

    void GenerateStatement( const StatementNode& statement_node );

    // Note, this doesn't insert braces
    void GenerateCompoundStatement( const StatementNode& sequence_node );

    void GenerateExpressionStatement( const ExpressionNode& expression );

    void GenerateConditional( const ExpressionNode& condition,
                              const StatementNode& true_statement,
                              const StatementNode* else_statement );

    void GenerateVoidReturn();

    void GenerateReturn( const ExpressionNode& returned );

    void GenerateTemporaryAssignment( unsigned temporary_number, const ExpressionNode& expression );

    //
    // Value generation
    //

    std::string GenerateValue( const ExpressionNode& expression );

    std::string GenerateAddress( const PointerExpressionNode& expression );

    //
    // Temporaries
    //

    std::string GetTemporaryIdentifier( unsigned temporary_number );

    //
    // Addresses
    //

    std::string GenerateStore( const PointerExpressionNode& address,
                               const ExpressionNode& assigned );

    std::string GenerateSwizzleStore( const PointerExpressionNode& address,
                                      const ExpressionNode& assigned,
                                      const Swizzle& swizzle );

    std::string GenerateVariableAddress( const VariableNode& variable_node );

    std::string GenerateArrayIndex( const PointerExpressionNode& address,
                                    const ExpressionNode& index );

    std::string GeneratePreIncrement( const PointerExpressionNode& address );

    std::string GeneratePreDecrement( const PointerExpressionNode& address );

    //
    // Values
    //

    std::string GenerateTemporaryRead( unsigned temporary_number );

    std::string GenerateCast( const CastNode& expression );

    std::string GenerateConstant( const ConstantNodeBase& expression );

    std::string GenerateZero( Type type );

    std::string GenerateCall( const ExpressionNode& expression );

    std::string GenerateExtractElement( const ExpressionNode& vector, const ExpressionNode& index );

    std::string GenerateInsertElement( const ExpressionNode& vector,
                                       const ExpressionNode& element,
                                       const ExpressionNode& index );

    std::string GenerateSwizzle( const ExpressionNode& expression, const Swizzle& swizzle );

    std::string GenerateSelect( const ExpressionNode& true_expression,
                                const ExpressionNode& false_expression,
                                const ExpressionNode& condition );

    std::string GenerateVectorConstructor( const ExpressionNode& constructor );

    std::string GenerateMatrixConstructor( const ExpressionNode& constructor );

    std::string GeneratePostIncrement( const PointerExpressionNode& address );

    std::string GeneratePostDecrement( const PointerExpressionNode& address );

    //
    // Binary Operators
    //
    std::string GenerateOr( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateAnd( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateExclusiveOr( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateCompareEqual( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateCompareNotEqual( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateCompareLessThan( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateCompareGreaterThan( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateCompareLessThanEquals( const ExpressionNode& lhs,
                                               const ExpressionNode& rhs );

    std::string GenerateCompareGreaterThanEquals( const ExpressionNode& lhs,
                                                  const ExpressionNode& rhs );

    std::string GenerateLeftShift( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateRightShift( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateAdd( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateSubtract( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateMultiply( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateDivide( const ExpressionNode& lhs, const ExpressionNode& rhs );

    std::string GenerateModulo( const ExpressionNode& lhs, const ExpressionNode& rhs );


    //
    // Unary operators
    //

    std::string GenerateNegate( const ExpressionNode& expression );

    std::string GenerateBitwiseNot( const ExpressionNode& expression );

    std::string GenerateLogicalNot( const ExpressionNode& expression );

};

} // namespace Compiler
} // namespace JoeLang
