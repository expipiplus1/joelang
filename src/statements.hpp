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

#include <vector>

namespace JoeLang
{
    namespace Expressions
    {
        class StateAssignmentExpression;
    } // namespace Expressions

    namespace Statements
    {
        class StateAssignmentStatementSeq;

        class Statement
        {
        public:
            Statement() = default;
            virtual ~Statement() = default;
        };

        class StateAssignmentStatement
        {
        public:
            StateAssignmentStatement() = default;
            virtual ~StateAssignmentStatement() noexcept = default;
        };

        class SingleStateAssignmentStatement : public StateAssignmentStatement
        {
        public:
            explicit SingleStateAssignmentStatement( Expressions::StateAssignmentExpression* state_assignment_expression );
            SingleStateAssignmentStatement( const SingleStateAssignmentStatement& other ) = delete;
            SingleStateAssignmentStatement& operator = ( const SingleStateAssignmentStatement& other ) = delete;

            virtual ~SingleStateAssignmentStatement() noexcept;

        private:
            Expressions::StateAssignmentExpression* m_stateAssignmentExpression;
        };

        class CompoundStateAssignmentStatement : public StateAssignmentStatement
        {
        public:
            explicit CompoundStateAssignmentStatement( StateAssignmentStatementSeq* state_assignment_statement_seq );
            CompoundStateAssignmentStatement( const CompoundStateAssignmentStatement& other ) = delete;
            CompoundStateAssignmentStatement& operator = ( const CompoundStateAssignmentStatement& other ) = delete;

            virtual ~CompoundStateAssignmentStatement() noexcept;

        private:
            StateAssignmentStatementSeq* m_stateAssignmentStatementSeq;
        };

        class StateAssignmentStatementSeq
        {
        public:
            StateAssignmentStatementSeq() = default;
            StateAssignmentStatementSeq( const StateAssignmentStatementSeq& other ) = delete;
            StateAssignmentStatementSeq& operator = ( const StateAssignmentStatementSeq& other ) = delete;
            StateAssignmentStatementSeq( StateAssignmentStatementSeq&& other );
            StateAssignmentStatementSeq& operator = ( StateAssignmentStatementSeq&& other );

            ~StateAssignmentStatementSeq() noexcept;

            const std::vector<StateAssignmentStatement*>& GetStateAssignmentStatements() const;
            void AppendStateAssignmentStatement( StateAssignmentStatement* state_assignment_statement );

        private:
            std::vector<StateAssignmentStatement*> m_stateAssignmentStatements;
        };
    } // namespace Statements
} // namespace JoeLang
