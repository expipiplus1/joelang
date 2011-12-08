cmake_minimum_required(VERSION 2.8)

project( joelang )

find_package( BISON REQUIRED )
find_package( FLEX REQUIRED )

BISON_TARGET( JoeLangParser src/parser.yy
              ${CMAKE_CURRENT_BINARY_DIR}/parser.cpp
              COMPILE_FLAGS "--defines=${CMAKE_CURRENT_BINARY_DIR}/parser.hpp" )
FLEX_TARGET( JoeLangScanner src/scanner.ll
             ${CMAKE_CURRENT_BINARY_DIR}/scanner.cpp )
ADD_FLEX_BISON_DEPENDENCY( JoeLangScanner JoeLangParser )

set ( BISON_EXTRA_HEADERS
      ${PROJECT_BINARY_DIR}/position.hh
      ${PROJECT_BINARY_DIR}/location.hh
      ${PROJECT_BINARY_DIR}/stack.hh )

set_property( DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
              "${BISON_EXTRA_HEADERS}")

include_directories( joelang src )
include_directories( joelang ${CMAKE_CURRENT_BINARY_DIR} )

set(CMAKE_XCODE_ATTRIBUTE_GCC_VERSION "com.apple.compilers.llvm.clang.1_0")
set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LANGUAGE_STANDARD "c++0x")
set(CMAKE_XCODE_ATTRIBUTE_CLANG_CXX_LIBRARY "libc++")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -stdlib=libc++ -g -Wall")

add_executable( joelang src/joelang.cpp
                        ${BISON_JoeLangParser_OUTPUTS}
                        ${FLEX_JoeLangScanner_OUTPUTS}
                        src/declarations.cpp
                        src/declarations.hpp
                        src/driver.cpp
                        src/driver.hpp
                        src/expressions.cpp
                        src/expressions.hpp
                        src/parsingcontext.cpp
                        src/parsingcontext.hpp
                        src/scanner.hpp )
