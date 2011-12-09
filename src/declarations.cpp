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

#include "declarations.hpp"

#include <iostream>
#include <ostream>
#include <string>
#include <utility>
#include "expressions.hpp"

namespace JoeLang
{
    namespace Declarations
    {
        TechniqueDeclaration::TechniqueDeclaration( Expressions::StateAssignmentExpressionSeq* state_assignment_expression_seq )
            :m_stateAssignmentExpressionSeq( state_assignment_expression_seq )
        {
        }

        TechniqueDeclaration::TechniqueDeclaration( TechniqueDeclaration&& other )
        {
            *this = std::move( other );
        }

        TechniqueDeclaration& TechniqueDeclaration::operator = ( TechniqueDeclaration&& other )
        {
            if( this != &other )
            {
                std::swap( m_stateAssignmentExpressionSeq, other.m_stateAssignmentExpressionSeq );
            }
            return *this;
        }

        TechniqueDeclaration::~TechniqueDeclaration() noexcept
        {
            delete m_stateAssignmentExpressionSeq;
        }

        DeclarationSeq::DeclarationSeq( DeclarationSeq&& other )
        {
            m_declarations = std::move( other.m_declarations );
        }

        DeclarationSeq& DeclarationSeq::operator = ( DeclarationSeq&& other )
        {
            if( this != &other )
            {
                std::swap( m_declarations, other.m_declarations );
            }
            return *this;
        }

        DeclarationSeq::~DeclarationSeq() noexcept
        {
            for( auto p : m_declarations )
                delete p;
        }

        const std::vector<Declaration*>& DeclarationSeq::GetDeclarations() const
        {
            return m_declarations;
        }

        void DeclarationSeq::AppendDeclaration( Declaration* declaration )
        {
            m_declarations.push_back( declaration );
        }
    } // namespace Declarations
} // namespace JoeLang
