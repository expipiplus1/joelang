%{
//
// C/C++ Declarations
//

#include <memory>
#include <string>
#include <declarations.hpp>

%}

//
// yacc/bison Declarations
//

//
// Require bison 2.3 or later
//
%require "2.3"

//
// add debug output code to generated parser. disable this for release versions.
//
%debug

//
// write out a header file containing the token defines
//
%defines

//
// use C++ skeleton file
//
%skeleton "lalr1.cc"

//
// namespace to enclose parser in
//
%name-prefix="JoeLang"

//
// set the parser's class identifier
//
%define "parser_class_name" "Parser"

//
// keep track of the current position within the input
//
%locations
%initial-action
{
    // initialize the initial location object
    @$.begin.filename = @$.end.filename = &driver.GetStreamName();
};

//
// The driver is passed by reference to the parser and to the scanner. This
// provides a simple but effective pure interface, not relying on global
// variables.
//
%parse-param { class Driver& driver }

//
// verbose error messages
//
%error-verbose

//
// Grammar Types
//

//
// All the types of node
//
%union
{
    JoeLang::Declarations::DeclarationSeq*  declaration_seq;
    JoeLang::Declarations::Declaration*     declaration;
    int              integer_literal;
}

//
// Define the tokens as used in scanner.lpp
//

%token                      END	     0	"end of file"

// keywords
%token                      TECHNIQUE  "technique"

// punctuation
%token                      OPEN_BRACE  "{"
%token                      CLOSE_BRACE "}"

//
// Define the compound types
//

%type <declaration_seq> translation_unit
%type <declaration_seq> declaration_seq
%type <declaration>     declaration
%type <declaration>     technique_declaration

//
// Do not delete the translation unit, because we pass it to the parsingcontext
// where it is dealt with
//
//%destructor { delete $$; } translation_unit
%destructor { delete $$; } declaration_seq
%destructor { delete $$; } declaration

//
//
//

%{

#include "driver.hpp"
#include "scanner.hpp"

/* this "connects" the bison parser in the driver to the flex scanner class
 * object. it defines the yylex() function call to pull the next token from the
 * current lexer object of the driver context. */
#undef yylex
#define yylex driver.GetLexer()->lex

%}

%start translation_unit

//
// Grammar rules
//
%%

translation_unit :
        // Empty
        {
            $$ = nullptr;
            driver.GetParsingContext().SetDeclarationSeq( $$ );
        }
    |
        declaration_seq
        {
            $$ = $1;
            driver.GetParsingContext().SetDeclarationSeq( $$ );
        };

declaration_seq :
        declaration
        {
            $$ = new Declarations::DeclarationSeq();
            $$->AppendDeclaration( $1 );
        }
    |
        declaration_seq declaration
        {
            $$ = $1;
            $1->AppendDeclaration( $2 );
        };

declaration :
        technique_declaration;

technique_declaration :
        TECHNIQUE OPEN_BRACE CLOSE_BRACE
        {
            $$ = new Declarations::TechniqueDeclaration();
        };


%%
//
// Additional code
//

void JoeLang::Parser::error( const Parser::location_type&   l,
                             const std::string&             m )
{
    driver.error(l, m);
}
