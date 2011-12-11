cmake_minimum_required(VERSION 2.8)

project( joelang )

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -stdlib=libc++ -O0 -g -Wall")

add_executable( joelang src/joelang.cpp )
