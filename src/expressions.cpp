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

#include "expressions.hpp"

#include <utility>

namespace JoeLang
{
    namespace Expressions
    {
        Expression::~Expression() = default;

        AssignmentExpression::~AssignmentExpression() = default;

        StateAssignmentExpression::StateAssignmentExpression( std::string state_name,
                                                              Expression* assigned_expression )
            :AssignmentExpression()
            ,m_stateName( state_name )
            ,m_assignedExpression( assigned_expression )
        {
        }

        StateAssignmentExpression::StateAssignmentExpression( StateAssignmentExpression&& other )
        {
            std::swap( m_stateName, other.m_stateName );
            //m_stateName = std::move( other.m_stateName );
            m_assignedExpression = other.m_assignedExpression;
            other.m_assignedExpression = nullptr;
        }

        StateAssignmentExpression& StateAssignmentExpression::operator = ( StateAssignmentExpression&& other )
        {
            if( this != &other )
            {
                std::swap( m_stateName, other.m_stateName );
                m_assignedExpression = other.m_assignedExpression;
                other.m_assignedExpression = nullptr;
            }
            return *this;
        }

        StateAssignmentExpression::~StateAssignmentExpression()
        {
            if( m_assignedExpression != nullptr )
                delete m_assignedExpression;
        }

        StateAssignmentExpressionSeq::StateAssignmentExpressionSeq( StateAssignmentExpressionSeq&& other )
        {
            *this = std::move( other );
        }

        StateAssignmentExpressionSeq& StateAssignmentExpressionSeq::operator = ( StateAssignmentExpressionSeq&& other )
        {
            if( this != &other )
            {
                std::swap( m_stateAssignmentExpressions, other.m_stateAssignmentExpressions );
            }
            return *this;
        }

        StateAssignmentExpressionSeq::~StateAssignmentExpressionSeq()
        {
            for( auto p : m_stateAssignmentExpressions )
                delete p;
        }

        void StateAssignmentExpressionSeq::AppendStateAssignmentExpression( StateAssignmentExpression* state_assignment_expression )
        {
            m_stateAssignmentExpressions.push_back( state_assignment_expression );
        }
    } // namespace Expressions
} // namespace JoeLang
