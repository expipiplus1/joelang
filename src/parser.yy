%{
//
// C/C++ Declarations
//

#include <memory>
#include <string>
#include <expression.hpp>

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
%union {
    class Expression*   expression;
    std::string*        literal;
    int                 integer_literal;
}

//
// Define the tokens as used in scanner.lpp
//

%token      	            END	     0	"end of file"
%token <integer_literal>    INTEGER     "integer"

//
// Define the compound types
//

%type <expression>  start
%type <expression>  expression
%type <expression>  constant_expression
%type <expression>  add_expression

//
// Do not delete start, because we pass it to the parsingcontext where it is
// dealt with
//
//%destructor { delete $$; } start
%destructor { delete $$; } expression
%destructor { delete $$; } constant_expression
%destructor { delete $$; } add_expression

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

%start start

//
// Grammar rules
//
%%

start :
        expression
        {
            $$ = $1;
            driver.GetParsingContext().SetExpression( $$ );
        };

expression :
        add_expression
        {
            $$ = $1;
        };

constant_expression :
        INTEGER
        {
            $$ = new ConstantExpression( $1 );
        }

add_expression :
        constant_expression
        {
            $$ = $1;
        }
    |
        add_expression '+' constant_expression
        {
            $$ = new AddExpression( $1, $3 );
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
