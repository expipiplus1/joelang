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
                        src/parser.cpp
                        src/parser.hpp )

target_link_libraries( joelang ${joelang_libraries} )
include_directories( ${joelang_include_directories} )

