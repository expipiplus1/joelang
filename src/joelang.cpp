#include <iostream>
#include <string>
#include "driver.hpp"
#include "expression.hpp"
#include "parsingcontext.hpp"

int main( int argc, char** argv )
{
    JoeLang::ParsingContext parsing_context;
    JoeLang::Driver driver( parsing_context );

    if( !driver.parse_string( std::string( "1+1" ) ) )
        return 1;

    parsing_context.GetExpression()->Print( std::cout );
}
