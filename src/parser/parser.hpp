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
#include <set>
#include <string>
#include <vector>

#include <parser/symbol_table.hpp>

namespace JoeLang
{

//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

class Context;
class StateBase;

//------------------------------------------------------------------------------
// Parser
//------------------------------------------------------------------------------

namespace Compiler
{

enum class TerminalType;

class Lexer;
class Token;
class TranslationUnit;

class Parser
{
public:
    Parser() = delete;
    explicit
    Parser( const Context& c );
    ~Parser();

    void                                    Print() const;

    bool                                    Parse( const std::string& string );

    SymbolTable&                            GetSymbolTable();

    const StateBase*                        GetNamedState( const std::string& name ) const;

    std::size_t                             GetLexerPosition() const;
    const std::unique_ptr<TranslationUnit>& GetTranslationUnit() const;

    //
    // Functions for detecting and reporting errors
    //
    void Error  ();
    void Error  ( std::string error_message );
    bool Good   () const;

    //
    // Functions for reading Tokens and Terminals
    //
    bool ExpectTerminal     ( TerminalType terminal_type );
    bool ExpectTerminal     ( TerminalType terminal_type,
                              std::string& string );

    template< typename T, typename U >
    bool Expect             ( std::unique_ptr<U>& token );
    template< typename T >
    bool Expect             ();

    template<typename T, typename U>
    bool ExpectSequenceOf   ( std::vector< std::unique_ptr<U> >& token_sequence );

    template<typename T>
    bool ExpectAnyOf        ( std::unique_ptr<Token>& token );
    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf        ( std::unique_ptr<Token>& token );
    template<typename T>
    bool ExpectAnyOf        ();
    template<typename T, typename T1, typename... Rest>
    bool ExpectAnyOf        ();

private:
    std::unique_ptr<TranslationUnit>    m_translationUnit;

    std::unique_ptr<Compiler::Lexer>    m_lexer;
    SymbolTable                         m_symbolTable;

    const Context&                      m_context;

    bool                                m_good = true;
    std::string                         m_errorMessage;
    std::set<TerminalType>              m_expectedTerminals;

};

} // namespace Compiler
} // namespace JoeLang

#include "parser-inl.hpp"
