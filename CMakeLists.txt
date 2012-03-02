cmake_minimum_required(VERSION 2.8)

project( joelang )

if( CMAKE_COMPILER_IS_GNUCXX )
else() #clang
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -O0 -g -Wall")

add_executable( joelang src/joelang.cpp
                        src/engine/effect.cpp
                        src/engine/effect.hpp
                        src/engine/pass.cpp
                        src/engine/pass.hpp
                        src/engine/state_assignment.cpp
                        src/engine/state_assignment.hpp
                        src/engine/technique.cpp
                        src/engine/technique.hpp
                        src/parser/lexer.cpp
                        src/parser/lexer.hpp
                        src/parser/parser.cpp
                        src/parser/parser.hpp
                        src/parser/parser-inl.hpp
                        src/parser/terminal_types.cpp
                        src/parser/terminal_types.hpp
                        src/parser/tokens/declaration.cpp
                        src/parser/tokens/declaration.hpp
                        src/parser/tokens/definition.cpp
                        src/parser/tokens/definition.hpp
                        src/parser/tokens/expression.cpp
                        src/parser/tokens/expression.hpp
                        src/parser/tokens/state_assignment_statement.cpp
                        src/parser/tokens/state_assignment_statement.hpp
                        src/parser/tokens/token.cpp
                        src/parser/tokens/token.hpp
                        src/parser/tokens/translation_unit.cpp
                        src/parser/tokens/translation_unit.hpp )

set( joelang_include_directories ${joelang_SOURCE_DIR}/src )
target_link_libraries( joelang ${joelang_libraries} )
include_directories( ${joelang_include_directories} )

