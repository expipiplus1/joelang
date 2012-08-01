cmake_minimum_required(VERSION 2.8)

project( joelang )

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -msse")

find_package( LLVM REQUIRED )
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )

add_executable( joelang src/joelang.cpp
                        src/engine/context.cpp
                        src/engine/context.hpp
                        src/engine/effect.cpp
                        src/engine/effect.hpp
                        src/engine/pass.cpp
                        src/engine/pass.hpp
                        src/engine/state_assignment.cpp
                        src/engine/state_assignment.hpp
                        src/engine/state_assignment-inl.hpp
                        src/engine/technique.cpp
                        src/engine/technique.hpp
                        src/engine/types.hpp
                        src/engine/state.cpp
                        src/engine/state.hpp
                        src/engine/state-inl.hpp
                        src/engine/internal/type_properties.cpp
                        src/engine/internal/type_properties.hpp
                        src/compiler/casting.hpp
                        src/compiler/code_generator.cpp
                        src/compiler/code_generator.hpp
                        src/compiler/effect_factory.cpp
                        src/compiler/effect_factory.hpp
                        src/compiler/generic_value.cpp
                        src/compiler/generic_value.hpp
                        src/compiler/function.cpp
                        src/compiler/function.hpp
                        src/compiler/lexer.cpp
                        src/compiler/lexer.hpp
                        src/compiler/parser.cpp
                        src/compiler/parser.hpp
                        src/compiler/parser-inl.hpp
                        src/compiler/runtime.cpp
                        src/compiler/runtime.hpp
                        src/compiler/sema_analyzer.cpp
                        src/compiler/sema_analyzer.hpp
                        src/compiler/symbol_table.cpp
                        src/compiler/symbol_table.hpp
                        src/compiler/terminal_types.cpp
                        src/compiler/terminal_types.hpp
                        src/compiler/variable.cpp
                        src/compiler/variable.hpp
                        src/compiler/tokens/assignment_operator.cpp
                        src/compiler/tokens/assignment_operator.hpp
                        src/compiler/tokens/binary_operator_expression.cpp
                        src/compiler/tokens/binary_operator_expression.hpp
                        src/compiler/tokens/declaration_specifier.cpp
                        src/compiler/tokens/declaration_specifier.hpp
                        src/compiler/tokens/declaration.cpp
                        src/compiler/tokens/declaration.hpp
                        src/compiler/tokens/declarator.cpp
                        src/compiler/tokens/declarator.hpp
                        src/compiler/tokens/definition.cpp
                        src/compiler/tokens/definition.hpp
                        src/compiler/tokens/expression.cpp
                        src/compiler/tokens/expression.hpp
                        src/compiler/tokens/initializer.cpp
                        src/compiler/tokens/initializer.hpp
                        src/compiler/tokens/literal_expression.cpp
                        src/compiler/tokens/literal_expression.hpp
                        src/compiler/tokens/postfix_operator.cpp
                        src/compiler/tokens/postfix_operator.hpp
                        src/compiler/tokens/state_assignment_statement.cpp
                        src/compiler/tokens/state_assignment_statement.hpp
                        src/compiler/tokens/token.cpp
                        src/compiler/tokens/token.hpp
                        src/compiler/tokens/translation_unit.cpp
                        src/compiler/tokens/translation_unit.hpp )

#
# Various custom commands
#

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test.jfx
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/test.jfx ${CMAKE_CURRENT_BINARY_DIR}/test.jfx
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/test.jfx )

add_custom_target( copy_files DEPENDS "${PROJECT_BINARY_DIR}/test.jfx" )
add_dependencies( joelang copy_files )

set( runtime_sources src/runtime/string.cpp
                     src/runtime/string.hpp
                     src/runtime/types.hpp )

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/runtime.bc
    COMMAND clang++ -O4 -std=c++11 -c -emit-llvm ${CMAKE_CURRENT_SOURCE_DIR}/src/runtime/string.cpp -o ${CMAKE_CURRENT_BINARY_DIR}/runtime.bc
    DEPENDS ${runtime_sources} )

add_custom_target( compile_runtime DEPENDS "${PROJECT_BINARY_DIR}/runtime.bc"
                                   SOURCES ${runtime_sources} )
add_dependencies( joelang compile_runtime )

#
# Linking
#

llvm_map_components_to_libraries( REQ_LLVM_LIBRARIES core jit native bitreader )
set( joelang_libraries ${REQ_LLVM_LIBRARIES} )
add_definitions( ${LLVM_DEFINITIONS} )

set( joelang_include_directories ${joelang_SOURCE_DIR}/src )
target_link_libraries( joelang ${joelang_libraries} )
include_directories( ${joelang_include_directories} )

#
# add a target to generate API documentation with Doxygen
#
find_package( Doxygen )
if( DOXYGEN_FOUND )
    configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
                    ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target( doc
                       ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                       COMMENT "Generating API documentation with Doxygen" VERBATIM )
endif( DOXYGEN_FOUND )
