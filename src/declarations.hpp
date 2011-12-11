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

#include <memory>
#include <vector>

namespace JoeLang
{
    namespace Statements
    {
        class CompoundStateAssignmentStatement;
    } // namespace Statements

    namespace Declarations
    {
        class Declaration
        {
        public:
            Declaration() = default;
            virtual ~Declaration() = default;
        };

        class PassDeclaration : public Declaration
        {
        public:
            explicit PassDeclaration( Statements::CompoundStateAssignmentStatement* compound_state_assignment_statement );
            PassDeclaration( const PassDeclaration& other ) = delete;
            PassDeclaration& operator = ( const PassDeclaration& other ) = delete;

            virtual ~PassDeclaration() noexcept = default;

        private:
            std::unique_ptr<Statements::CompoundStateAssignmentStatement> m_compoundStateAssignmentStatement;
        };

        class PassDeclarationSeq
        {
        public:
            PassDeclarationSeq() = default;
            PassDeclarationSeq( const PassDeclarationSeq& other ) = delete;
            PassDeclarationSeq& operator = ( const PassDeclarationSeq& other ) = delete;

            ~PassDeclarationSeq() noexcept = default;

            const std::vector<std::unique_ptr<PassDeclaration>>& GetPassDeclarations() const;
            void AppendPassDeclaration(PassDeclaration* pass_declaration );

        private:
            std::vector<std::unique_ptr<PassDeclaration>> m_passDeclarations;
        };

        class TechniqueDeclaration : public Declaration
        {
        public:
            explicit TechniqueDeclaration( PassDeclarationSeq* pass_declaration_seq );
            TechniqueDeclaration( const TechniqueDeclaration& other ) = delete;
            TechniqueDeclaration& operator = ( const TechniqueDeclaration& other ) = delete;

            virtual ~TechniqueDeclaration() noexcept = default;

        private:
            std::unique_ptr<PassDeclarationSeq> m_passDeclarationSeq;
        };

        class DeclarationSeq
        {
        public:
            DeclarationSeq() = default;
            DeclarationSeq( const DeclarationSeq& other ) = delete;
            DeclarationSeq& operator = ( const DeclarationSeq& other ) = delete;

            ~DeclarationSeq() noexcept = default;

            const std::vector<std::unique_ptr<Declaration>>& GetDeclarations() const;
            void AppendDeclaration( Declaration* declaration );

        private:
            std::vector<std::unique_ptr<Declaration>> m_declarations;
        };
    }
} // namespace JoeLang
