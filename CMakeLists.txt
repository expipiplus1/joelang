cmake_minimum_required(VERSION 2.8)

project( joelang )

if( CMAKE_COMPILER_IS_GNUCXX )
    set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -nostdinc++" )
    set( joelang_libraries c++ )
    set( joelang_include_directories /usr/include/c++/v1 )
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -O0 -g -Wall")

add_executable( joelang src/joelang.cpp
                        src/declaration.cpp
                        src/declaration.hpp
                        src/lexer.cpp
                        src/lexer.hpp
                        src/parser.cpp
                        src/parser.hpp
                        src/terminal.cpp
                        src/terminal.hpp
                        src/token.cpp
                        src/token.hpp
                        src/token_matcher.cpp
                        src/token_matcher.hpp
                        src/translation_unit.cpp
                        src/translation_unit.hpp )

target_link_libraries( joelang ${joelang_libraries} )
include_directories( ${joelang_include_directories} )

