cmake_minimum_required(VERSION 2.8)

project( joelang )

set( joelang_CXX_FLAGS "-fno-exceptions -Wall -std=c++11 -msse" CACHE STRING "joelang compiler options" )
set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${joelang_CXX_FLAGS}" )

find_package( LLVM REQUIRED )
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
if( WIN32 )
    set( joelang_LIBRARY_DIRS ${LLVM_LIBRARY_DIRS} CACHE STRING "LLVM library dirs for windows" )
endif()

find_package(                       OpenGL      REQUIRED )

include_directories( ${joemath_INCLUDE_DIR} ${glee_INCLUDE_DIR} )

#
# Submdules
#
set( UCPP_STAND_ALONE FALSE )
add_subdirectory( ucpp EXCLUDE_FROM_ALL )

#
# The library
#

add_library( joelang include/joelang/context.hpp
                     include/joelang/effect.hpp
                     include/joelang/pass.hpp
                     include/joelang/program.hpp
                     include/joelang/shader.hpp
                     include/joelang/state_assignment.hpp
                     include/joelang/technique.hpp
                     include/joelang/types.hpp
                     include/joelang/state.hpp
                     include/joelang/inl/state_assignment-inl.hpp
                     include/joelang/inl/state-inl.hpp
                     src/engine/context.cpp
                     src/engine/effect.cpp
                     src/engine/opengl_states.cpp
                     src/engine/opengl_states.hpp
                     src/engine/pass.cpp
                     src/engine/program.cpp
                     src/engine/shader.cpp
                     src/engine/state_assignment.cpp
                     src/engine/technique.cpp
                     src/engine/state.cpp
                     src/compiler/casting.hpp
                     src/compiler/code_generator.cpp
                     src/compiler/code_generator.hpp
                     src/compiler/complete_type.cpp
                     src/compiler/complete_type.hpp
                     src/compiler/effect_factory.cpp
                     src/compiler/effect_factory.hpp
                     src/compiler/entry_function.cpp
                     src/compiler/entry_function.hpp
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
                     src/compiler/semantic.cpp
                     src/compiler/semantic.hpp
                     src/compiler/semantic_info.cpp
                     src/compiler/semantic_info.hpp
                     src/compiler/shader_writer.cpp
                     src/compiler/shader_writer.hpp
                     src/compiler/terminal_types.cpp
                     src/compiler/terminal_types.hpp
                     src/compiler/type_properties.cpp
                     src/compiler/type_properties.hpp
                     src/compiler/ucpp.cpp
                     src/compiler/ucpp.hpp
                     src/compiler/variable.cpp
                     src/compiler/variable.hpp
                     src/compiler/inl/shader_writer-inl.hpp
                     src/compiler/tokens/declaration_specifier.cpp
                     src/compiler/tokens/declaration_specifier.hpp
                     src/compiler/tokens/declaration.cpp
                     src/compiler/tokens/declaration.hpp
                     src/compiler/tokens/declarator_specifier.cpp
                     src/compiler/tokens/declarator_specifier.hpp
                     src/compiler/tokens/declarator.cpp
                     src/compiler/tokens/declarator.hpp
                     src/compiler/tokens/definition.cpp
                     src/compiler/tokens/definition.hpp
                     src/compiler/tokens/initializer.cpp
                     src/compiler/tokens/initializer.hpp
                     src/compiler/tokens/parameter.cpp
                     src/compiler/tokens/parameter.hpp
                     src/compiler/tokens/token.cpp
                     src/compiler/tokens/token.hpp
                     src/compiler/tokens/translation_unit.cpp
                     src/compiler/tokens/translation_unit.hpp
                     src/compiler/tokens/expressions/assignment_operator.cpp
                     src/compiler/tokens/expressions/assignment_operator.hpp
                     src/compiler/tokens/expressions/binary_operator_expression.cpp
                     src/compiler/tokens/expressions/binary_operator_expression.hpp
                     src/compiler/tokens/expressions/expression.cpp
                     src/compiler/tokens/expressions/expression.hpp
                     src/compiler/tokens/expressions/literal_expression.cpp
                     src/compiler/tokens/expressions/literal_expression.hpp
                     src/compiler/tokens/expressions/postfix_operator.cpp
                     src/compiler/tokens/expressions/postfix_operator.hpp
                     src/compiler/tokens/pass_statements/compile_statement.cpp
                     src/compiler/tokens/pass_statements/compile_statement.hpp
                     src/compiler/tokens/pass_statements/pass_statement.cpp
                     src/compiler/tokens/pass_statements/pass_statement.hpp
                     src/compiler/tokens/pass_statements/state_assignment_statement.cpp
                     src/compiler/tokens/pass_statements/state_assignment_statement.hpp
                     src/compiler/tokens/statements/compound_statement.cpp
                     src/compiler/tokens/statements/compound_statement.hpp
                     src/compiler/tokens/statements/empty_statement.cpp
                     src/compiler/tokens/statements/empty_statement.hpp
                     src/compiler/tokens/statements/expression_statement.cpp
                     src/compiler/tokens/statements/expression_statement.hpp
                     src/compiler/tokens/statements/return_statement.cpp
                     src/compiler/tokens/statements/return_statement.hpp
                     src/compiler/tokens/statements/statement.cpp
                     src/compiler/tokens/statements/statement.hpp )

target_link_libraries( joelang ucpp )

#
# Test executable
#
add_executable( joelang_test src/joelang.cpp )
target_link_libraries( joelang_test joelang ${OPENGL_LIBRARY} glee )

#
# Various custom commands
#

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/test.jfx
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/test.jfx ${CMAKE_CURRENT_BINARY_DIR}/test.jfx
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/test.jfx )

add_custom_target( joelang_copy_files DEPENDS "${PROJECT_BINARY_DIR}/test.jfx" )
add_dependencies( joelang joelang_copy_files )

set( runtime_sources src/runtime/string.cpp
                     src/runtime/string.hpp
                     src/runtime/types.hpp )

string( REPLACE " " ";" JOELANG_RUNTIME_CXX_FLAGS ${CMAKE_CXX_FLAGS} )

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set( JOELANG_RUNTIME_COMPILER ${CMAKE_CXX_COMPILER} )
else()
    set( JOELANG_RUNTIME_COMPILER clang++ )
endif()

add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/runtime.bc
    COMMAND ${JOELANG_RUNTIME_COMPILER} -O4 -std=c++11 ${JOELANG_RUNTIME_CXX_FLAGS} ${CLANG_FLAGS} -I${joemath_INCLUDE_DIR} -c -emit-llvm ${CMAKE_CURRENT_SOURCE_DIR}/src/runtime/string.cpp -o ${CMAKE_BINARY_DIR}/runtime.bc
    DEPENDS ${runtime_sources} )

if( ${CMAKE_GENERATOR} STREQUAL "Xcode" )
    add_custom_target( compile_runtime DEPENDS "${CMAKE_BINARY_DIR}/runtime.bc" )
else()
    add_custom_target( compile_runtime DEPENDS "${CMAKE_BINARY_DIR}/runtime.bc"
                                       SOURCES ${runtime_sources} )
endif()

add_dependencies( joelang compile_runtime )

#
# Linking
#

#llvm_map_components_to_libraries( REQ_LLVM_LIBRARIES core jit native bitreader ipa ipo linker )
llvm_map_components_to_libraries( REQ_LLVM_LIBRARIES core jit native bitreader ipo )
set( joelang_LIBS ${REQ_LLVM_LIBRARIES} )
add_definitions( ${LLVM_DEFINITIONS} )

list( APPEND joelang_INCLUDE_DIRS ${joelang_SOURCE_DIR} ${joelang_SOURCE_DIR}/include ${joelang_SOURCE_DIR}/src )
target_link_libraries( joelang ${joelang_LIBS} )
include_directories( ${joelang_INCLUDE_DIRS} )

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
