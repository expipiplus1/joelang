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
#include <vector>

namespace JoeLang
{
    namespace Expressions
    {
        class Expression
        {
        public:
            Expression() = default;
            virtual ~Expression();
        };

        class AssignmentExpression : public Expression
        {
        public:
            AssignmentExpression() = default;
            virtual ~AssignmentExpression();
        };

        class StateAssignmentExpression : public AssignmentExpression
        {
        public:
            StateAssignmentExpression( std::string state_name,
                                       Expression* assigned_expression );
            StateAssignmentExpression( const StateAssignmentExpression& other ) = delete;
            StateAssignmentExpression& operator = ( const StateAssignmentExpression& other ) = delete;
            StateAssignmentExpression( StateAssignmentExpression&& other );
            StateAssignmentExpression& operator = ( StateAssignmentExpression&& other );

            virtual ~StateAssignmentExpression();

        private:
            std::string m_stateName;
            Expression* m_assignedExpression;
        };

        class StateAssignmentExpressionSeq
        {
        public:
            StateAssignmentExpressionSeq() = default;
            StateAssignmentExpressionSeq( const StateAssignmentExpressionSeq& other ) = delete;
            StateAssignmentExpressionSeq& operator = ( const StateAssignmentExpressionSeq& other ) = delete;
            StateAssignmentExpressionSeq( StateAssignmentExpressionSeq&& other );
            StateAssignmentExpressionSeq& operator = ( StateAssignmentExpressionSeq&& other );

            ~StateAssignmentExpressionSeq();

            const std::vector<StateAssignmentExpression*>& GetStateAssignmentExpressions() const;
            void AppendStateAssignmentExpression( StateAssignmentExpression* state_assignment_expression );

        private:
            std::vector<StateAssignmentExpression*> m_stateAssignmentExpressions;
        };
    }
} // namespace JoeLang
