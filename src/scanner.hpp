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

//
// Flex expects the signature of yylex to be defined in the macro YY_DECL, and
// the C++ parser expects it to be declared. We can factor both as follows.
//
#ifndef YY_DECL

#define	YY_DECL                                 \
    JoeLang::Parser::token_type                 \
    JoeLang::Scanner::lex(                      \
        JoeLang::Parser::semantic_type* yylval, \
        JoeLang::Parser::location_type* yylloc  \
    )
#endif

#ifndef __FLEX_LEXER_H
#define yyFlexLexer JoeLangFlexLexer
#include <FlexLexer.h>
#undef yyFlexLexer
#endif

#include "declarations.hpp"
#include "expressions.hpp"
#include "statements.hpp"

#include "parser.hpp"

namespace JoeLang
{
    class Scanner : public JoeLangFlexLexer
    {
    public:
        //
        // Create a new scanner object. The streams arg_yyin and arg_yyout
        // default to cin and cout, but that assignment is only made when
        // initializing in yylex().
        //
        Scanner( std::istream* arg_yyin = nullptr,
                 std::ostream* arg_yyout = nullptr );

        virtual ~Scanner() noexcept;

        //
        // This is the main lexing function. It is generated by flex according
        // to the macro declaration YY_DECL above. The generated bison parser
        // then calls this virtual function to fetch new tokens.
        //
        virtual Parser::token_type lex(
            Parser::semantic_type* yylval,
            Parser::location_type* yylloc );

        //
        // Enable debug output (via arg_yyout) if compiled into the scanner.
        //
        void SetDebug( bool b );
    };

} // namespace JoeLang
