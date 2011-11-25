#include <iostream>
#include <string>
#include "driver.hpp"

int main( int argc, char** argv )
{
    JoeLang::Driver driver;

    return driver.parse_string( std::string( "1+1+2" ) );

}
