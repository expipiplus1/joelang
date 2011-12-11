#include <iostream>
#include <string>

#include "declarations.hpp"
#include "driver.hpp"
#include "parsingcontext.hpp"

int main( int argc, char** argv )
{
    JoeLang::ParsingContext parsing_context;
    JoeLang::Driver driver( parsing_context );

    if( !driver.parse_string( std::string( "technique{pass{a=b;b=c;}pass{{}}}\n" ) ) )
        std::cout << "Couldn't parse\n";
    else
        std::cout << parsing_context.GetDeclarationSeq()->GetDeclarations().size() << std::endl;
}
