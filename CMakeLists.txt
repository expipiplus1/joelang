cmake_minimum_required(VERSION 2.8)

project( joelang )

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------

if( NOT DEFINED JOELANG_WITH_OPENGL )
    set( JOELANG_WITH_OPENGL OFF CACHE BOOL "Enable builtin OpenGL state setting" )
endif()

set( JOELANG_RUNTIME_PATH ${CMAKE_CURRENT_BINARY_DIR}/runtime.bc CACHE FILENAME "The filename for the joelang runtime" )

configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/include/joelang/config.h.in ${CMAKE_CURRENT_BINARY_DIR}/include/joelang/config.h )
list( APPEND JOELANG_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/include )

#-------------------------------------------------------------------------------
# Find all the required packages
#-------------------------------------------------------------------------------

find_package( LLVM REQUIRED )
include_directories( ${LLVM_INCLUDE_DIRS} )
list( APPEND JOELANG_LIBRARY_DIRS ${LLVM_LIBRARY_DIRS} )
list( APPEND JOELANG_INTERNAL_INCLUDE_DIRS ${LLVM_INCLUDE_DIRS} )

if( ${JOELANG_WITH_OPENGL} )
    find_package( OpenGL REQUIRED )
    list( APPEND JOELANG_INTERNAL_INCLUDE_DIRS ${OPENGL_INCLUDE_DIR} )

    if( NOT DEFINED GLEE_INCLUDE_DIR )
        message( FATAL_ERROR "glee include dir isn't set" )
    endif()
    if( NOT DEFINED GLEE_LIB )
        message( FATAL_ERROR "glee lib isn't set" )
    endif()

    list( APPEND JOELANG_INTERNAL_INCLUDE_DIRS ${GLEE_INCLUDE_DIR} )
    list( APPEND JOELANG_INTERNAL_LIBS ${OPENGL_gl_LIBRARY} ${GLEE_LIB} )
endif()

if( NOT DEFINED JOEMATH_INCLUDE_DIR )
    message( FATAL_ERROR "JoeMath include dir isn't set" ) 
endif()
get_filename_component(JOEMATH_INCLUDE_DIR ${JOEMATH_INCLUDE_DIR} REALPATH)
list( APPEND JOELANG_INTERNAL_INCLUDE_DIRS ${JOEMATH_INCLUDE_DIR} )


#-------------------------------------------------------------------------------
# Submodules
#-------------------------------------------------------------------------------
set( UCPP_STAND_ALONE FALSE )
add_subdirectory( ucpp EXCLUDE_FROM_ALL )
list( APPEND JOELANG_INTERNAL_LIBS ucpp )

#
# The library
#

set( joelang_sources include/joelang/context.hpp
                     include/joelang/effect.hpp
                     include/joelang/parameter.hpp
                     include/joelang/pass.hpp
                     include/joelang/program.hpp
                     include/joelang/shader.hpp
                     include/joelang/state_assignment.hpp
                     include/joelang/technique.hpp
                     include/joelang/types.hpp
                     include/joelang/state.hpp
                     include/joelang/inl/effect-inl.hpp
                     include/joelang/inl/parameter-inl.hpp
                     include/joelang/inl/state_assignment-inl.hpp
                     include/joelang/inl/state-inl.hpp
                     src/engine/context.cpp
                     src/engine/effect.cpp
                     src/engine/opengl_states.cpp
                     src/engine/opengl_states.hpp
                     src/engine/parameter.cpp
                     src/engine/parameter_watcher.cpp
                     src/engine/parameter_watcher.hpp
                     src/engine/pass.cpp
                     src/engine/program.cpp
                     src/engine/shader.cpp
                     src/engine/state_assignment.cpp
                     src/engine/technique.cpp
                     src/engine/state.cpp
                     src/engine/inl/parameter_watcher-inl.hpp
                     src/compiler/code_dag/cast_node.cpp
                     src/compiler/code_dag/cast_node.hpp
                     src/compiler/code_dag/compile_statement_node.cpp
                     src/compiler/code_dag/compile_statement_node.hpp
                     src/compiler/code_dag/constant_node.cpp
                     src/compiler/code_dag/constant_node.hpp
                     src/compiler/code_dag/constant_node-inl.hpp
                     src/compiler/code_dag/expression_node.cpp
                     src/compiler/code_dag/expression_node.hpp
                     src/compiler/code_dag/function_node.cpp
                     src/compiler/code_dag/function_node.hpp
                     src/compiler/code_dag/node.cpp
                     src/compiler/code_dag/node.hpp
                     src/compiler/code_dag/node_manager.cpp
                     src/compiler/code_dag/node_manager.hpp
                     src/compiler/code_dag/node_manager-inl.hpp
                     src/compiler/code_dag/pass_node.cpp
                     src/compiler/code_dag/pass_node.hpp
                     src/compiler/code_dag/pointer_expression_node.cpp
                     src/compiler/code_dag/pointer_expression_node.hpp
                     src/compiler/code_dag/state_assignment_node.cpp
                     src/compiler/code_dag/state_assignment_node.hpp
                     src/compiler/code_dag/swizzle_node.cpp
                     src/compiler/code_dag/swizzle_node.hpp
                     src/compiler/code_dag/swizzle_store_node.cpp
                     src/compiler/code_dag/swizzle_store_node.hpp
                     src/compiler/code_dag/technique_node.cpp
                     src/compiler/code_dag/technique_node.hpp
                     src/compiler/code_dag/variable_node.cpp
                     src/compiler/code_dag/variable_node.hpp
                     src/compiler/code_dag/zero_node.cpp
                     src/compiler/code_dag/zero_node.hpp
                     src/compiler/effect_factory.cpp
                     src/compiler/effect_factory.hpp
                     src/compiler/lexer/lexer.cpp
                     src/compiler/lexer/lexer.hpp
                     src/compiler/lexer/terminal_types.cpp
                     src/compiler/lexer/terminal_types.hpp
                     src/compiler/lexer/ucpp.cpp
                     src/compiler/lexer/ucpp.hpp
                     src/compiler/parser/parser.cpp
                     src/compiler/parser/parser.hpp
                     src/compiler/parser/parser-inl.hpp
                     src/compiler/semantic_analysis/complete_type.cpp
                     src/compiler/semantic_analysis/complete_type.hpp
                     src/compiler/semantic_analysis/entry_function.cpp
                     src/compiler/semantic_analysis/entry_function.hpp
                     src/compiler/semantic_analysis/function.cpp
                     src/compiler/semantic_analysis/function.hpp
                     src/compiler/semantic_analysis/sema_analyzer.cpp
                     src/compiler/semantic_analysis/sema_analyzer.hpp
                     src/compiler/semantic_analysis/semantic.cpp
                     src/compiler/semantic_analysis/semantic.hpp
                     src/compiler/semantic_analysis/swizzle.cpp
                     src/compiler/semantic_analysis/swizzle.hpp
                     src/compiler/semantic_analysis/type_properties.cpp
                     src/compiler/semantic_analysis/type_properties.hpp
                     src/compiler/semantic_analysis/variable.cpp
                     src/compiler/semantic_analysis/variable.hpp
                     src/compiler/support/casting.hpp
                     src/compiler/support/generic_value.cpp
                     src/compiler/support/generic_value.hpp
                     src/compiler/tokens/declaration.cpp
                     src/compiler/tokens/declaration.hpp
                     src/compiler/tokens/declaration_specifier.cpp
                     src/compiler/tokens/declaration_specifier.hpp
                     src/compiler/tokens/declarator.cpp
                     src/compiler/tokens/declarator.hpp
                     src/compiler/tokens/declarator_specifier.cpp
                     src/compiler/tokens/declarator_specifier.hpp
                     src/compiler/tokens/definition.cpp
                     src/compiler/tokens/definition.hpp
                     src/compiler/tokens/expressions/assignment_expression.cpp
                     src/compiler/tokens/expressions/assignment_expression.hpp
                     src/compiler/tokens/expressions/assignment_operator.cpp
                     src/compiler/tokens/expressions/assignment_operator.hpp
                     src/compiler/tokens/expressions/binary_operator_expression.cpp
                     src/compiler/tokens/expressions/binary_operator_expression.hpp
                     src/compiler/tokens/expressions/cast_expression.cpp
                     src/compiler/tokens/expressions/cast_expression.hpp
                     src/compiler/tokens/expressions/conditional_expression.cpp
                     src/compiler/tokens/expressions/conditional_expression.hpp
                     src/compiler/tokens/expressions/expression.cpp
                     src/compiler/tokens/expressions/expression.hpp
                     src/compiler/tokens/expressions/identifier_expression.cpp
                     src/compiler/tokens/expressions/identifier_expression.hpp
                     src/compiler/tokens/expressions/literal_expression.cpp
                     src/compiler/tokens/expressions/literal_expression.hpp
                     src/compiler/tokens/expressions/postfix_expression.cpp
                     src/compiler/tokens/expressions/postfix_expression.hpp
                     src/compiler/tokens/expressions/postfix_operator.cpp
                     src/compiler/tokens/expressions/postfix_operator.hpp
                     src/compiler/tokens/expressions/primary_expression.cpp
                     src/compiler/tokens/expressions/primary_expression.hpp
                     src/compiler/tokens/expressions/type_constructor_expression.cpp
                     src/compiler/tokens/expressions/type_constructor_expression.hpp
                     src/compiler/tokens/expressions/unary_expression.cpp
                     src/compiler/tokens/expressions/unary_expression.hpp
                     src/compiler/tokens/initializer.cpp
                     src/compiler/tokens/initializer.hpp
                     src/compiler/tokens/parameter.cpp
                     src/compiler/tokens/parameter.hpp
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
                     src/compiler/tokens/statements/if_statement.cpp
                     src/compiler/tokens/statements/if_statement.hpp
                     src/compiler/tokens/statements/return_statement.cpp
                     src/compiler/tokens/statements/return_statement.hpp
                     src/compiler/tokens/statements/statement.cpp
                     src/compiler/tokens/statements/statement.hpp
                     src/compiler/tokens/token.cpp
                     src/compiler/tokens/token.hpp
                     src/compiler/tokens/translation_unit.cpp
                     src/compiler/tokens/translation_unit.hpp
                     src/compiler/writers/code_generator.cpp
                     src/compiler/writers/code_generator.hpp
                     src/compiler/writers/code_generator_wrappers.cpp
                     src/compiler/writers/dot_writer.cpp
                     src/compiler/writers/dot_writer.hpp
                     src/compiler/writers/glsl_writer.cpp
                     src/compiler/writers/glsl_writer.hpp
                     src/compiler/writers/llvm_writer.cpp
                     src/compiler/writers/llvm_writer.hpp
                     src/compiler/writers/runtime.cpp
                     src/compiler/writers/runtime.hpp
                     src/compiler/writers/semantic_info.cpp
                     src/compiler/writers/semantic_info.hpp
                     src/compiler/writers/shader_writer.cpp
                     src/compiler/writers/shader_writer.hpp
                     src/compiler/writers/shader_writer-inl.hpp )

#set_source_files_properties( ${joelang_sources} PROPERTIES COMPILE_FLAGS "-std=c++11 -Wall" )
add_definitions( -std=c++11 )
add_definitions( -Wall )

add_library( joelang ${joelang_sources} )

#-------------------------------------------------------------------------------
# Linking
#-------------------------------------------------------------------------------

llvm_map_components_to_libraries( REQ_LLVM_LIBRARIES core jit native bitreader ipo )
list( APPEND JOELANG_INTERNAL_LIBS ${REQ_LLVM_LIBRARIES} )
add_definitions( ${LLVM_DEFINITIONS} )

link_directories( ${JOELANG_LIBRARY_DIRS} )

target_link_libraries( joelang ${JOELANG_INTERNAL_LIBS} )

list( APPEND JOELANG_LIBS joelang )

#-------------------------------------------------------------------------------
# Pch and unity
#-------------------------------------------------------------------------------

include( cotire )
cotire( joelang )

target_link_libraries( joelang_unity ${JOELANG_LIBS} )

#-------------------------------------------------------------------------------
# Include dirs
#-------------------------------------------------------------------------------

list( APPEND JOELANG_INTERNAL_INCLUDE_DIRS ${joelang_SOURCE_DIR} ${joelang_SOURCE_DIR}/src )
list( APPEND JOELANG_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include ${joelang_BINARY_DIR}/include )
include_directories( ${JOELANG_INCLUDE_DIRS} ${JOELANG_INTERNAL_INCLUDE_DIRS} )


#-------------------------------------------------------------------------------
# Build the runtime
#-------------------------------------------------------------------------------
add_subdirectory( runtime EXCLUDE_FROM_ALL )
add_dependencies( joelang compile_runtime )

#-------------------------------------------------------------------------------
# Testing
#-------------------------------------------------------------------------------
add_subdirectory( tests EXCLUDE_FROM_ALL )

#--------------------------------------------------------------------------------
# Set global properties for the rest of the build
#--------------------------------------------------------------------------------
set_property( GLOBAL PROPERTY JOELANG_LIBRARY_DIRS ${JOELANG_LIBRARY_DIRS} )
set_property( GLOBAL PROPERTY JOELANG_INCLUDE_DIRS ${JOELANG_INCLUDE_DIRS} )
set_property( GLOBAL PROPERTY JOELANG_LIBS ${JOELANG_LIBS} )

#--------------------------------------------------------------------------------
# Documentation
#--------------------------------------------------------------------------------
find_package( Doxygen )
if( DOXYGEN_FOUND )
    configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
                    ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target( doc
                       ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
                       WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                       COMMENT "Generating API documentation with Doxygen" VERBATIM )
endif( DOXYGEN_FOUND )
