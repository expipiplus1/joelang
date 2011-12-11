%{
//
// C/C++ Declarations
//

#include <memory>
#include <string>
#include <declarations.hpp>
#include <expressions.hpp>
#include <statements.hpp>

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
    JoeLang::Declarations::Declaration*     declaration;
    JoeLang::Declarations::DeclarationSeq*  declaration_seq;

    JoeLang::Declarations::PassDeclaration* pass_declaration;
    JoeLang::Declarations::PassDeclarationSeq* pass_declaration_seq;

    JoeLang::Expressions::StateAssignmentExpression*  state_assignment_expression;

    JoeLang::Statements::StateAssignmentStatement* state_assignment_statement;
    JoeLang::Statements::StateAssignmentStatementSeq* state_assignment_statement_seq;

    std::string*     identifier_string;
    int              integer_literal;
}

//
// Define the tokens as used in scanner.lpp
//

%token  END	     0	"end of file"

%token<identifier_string> IDENTIFIER "identifier"

// keywords
%token  TECHNIQUE  "technique"
%token  PASS       "pass"

// punctuation
%token  OPEN_BRACE  "{"
%token  CLOSE_BRACE "}"
%token  SEMICOLON   ";"

// operators
%token  EQUALS      "="

//
// Define the compound types
//

%type <declaration_seq> translation_unit
%type <declaration>     declaration
%type <declaration>     technique_declaration
%type <declaration_seq> declaration_seq

%type <pass_declaration> pass_declaration
%type <pass_declaration_seq> pass_declaration_seq

%type <state_assignment_expression> state_assignment_expression

%type <state_assignment_statement> state_assignment_statement
%type <state_assignment_statement> compound_state_assignment_statement
%type <state_assignment_statement_seq> state_assignment_statement_seq

//
// Do not delete the translation unit, because we pass it to the parsingcontext
// where it is dealt with
//
//%destructor { delete $$; } translation_unit
%destructor { delete $$; } declaration
%destructor { delete $$; } technique_declaration
%destructor { delete $$; } declaration_seq

%destructor { delete $$; } pass_declaration
%destructor { delete $$; } pass_declaration_seq

%destructor { delete $$; } state_assignment_expression

%destructor { delete $$; } state_assignment_statement
%destructor { delete $$; } compound_state_assignment_statement
%destructor { delete $$; } state_assignment_statement_seq

%destructor { delete $$; } IDENTIFIER

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
            $$ = new Declarations::TechniqueDeclaration( new Declarations::PassDeclarationSeq() );
        }
    |
        TECHNIQUE OPEN_BRACE pass_declaration_seq CLOSE_BRACE
        {
            $$ = new Declarations::TechniqueDeclaration( $3 );
        };

pass_declaration_seq :
        pass_declaration
        {
            $$ = new Declarations::PassDeclarationSeq();
            $$->AppendPassDeclaration( $1 );
        }
    |
        pass_declaration_seq pass_declaration
        {
            $$ = $1;
            $$->AppendPassDeclaration( $2 );
        };

pass_declaration :
        PASS compound_state_assignment_statement
        {
            $$ = new Declarations::PassDeclaration( dynamic_cast<Statements::CompoundStateAssignmentStatement*>( $2 ) );
        };

compound_state_assignment_statement :
        OPEN_BRACE CLOSE_BRACE
        {
            //
            // Return a compound statement with an empty sequence in
            //
            $$ = new Statements::CompoundStateAssignmentStatement( new Statements::StateAssignmentStatementSeq() );
        }
    |
        OPEN_BRACE state_assignment_statement_seq CLOSE_BRACE
        {
            $$ = new Statements::CompoundStateAssignmentStatement( $2 );
        };

state_assignment_statement_seq :
        state_assignment_statement
        {
            $$ = new Statements::StateAssignmentStatementSeq();
            $$->AppendStateAssignmentStatement( $1 );
        }
    |
        state_assignment_statement_seq state_assignment_statement
        {
            $$ = $1;
            $1->AppendStateAssignmentStatement( $2 );
        };

state_assignment_statement :
        state_assignment_expression SEMICOLON
        {
            $$ = new Statements::SingleStateAssignmentStatement( $1 );
        }
    |
        compound_state_assignment_statement;

state_assignment_expression :
        IDENTIFIER EQUALS IDENTIFIER
        {
            //
            // TODO the identifier here should probably give an index into a
            // table of states
            //
            $$ = new Expressions::StateAssignmentExpression( *$1, nullptr );
            delete $1;
            delete $3;
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
