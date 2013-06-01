cmake_minimum_required(VERSION 2.8)

#-------------------------------------------------------------------------------
# Configuration
#-------------------------------------------------------------------------------

if( NOT DEFINED JOELANG_RUNTIME_PATH )
    message( FATAL_ERROR "JoeLang runtime path not set" )
endif()

if( NOT DEFINED JOEMATH_INCLUDE_DIR )
    message( FATAL_ERROR "JoeLang runtime path not set" )
endif()

#-------------------------------------------------------------------------------
# Compile the runtime
#-------------------------------------------------------------------------------

# why doesn't this work?
#set( CMAKE_CXX_OUTPUT_EXTENSION ".bc" CACHE STRING "" FORCE ) 

# we need to use clang
#if( ${CMAKE_CXX_COMPLER} NOT EQUAL clang++ )
    #set( CMAKE_CXX_COMPILER clang++ CACHE STRING "" FORCE )
#endif()

# emit bitcode
#set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -emit-llvm" CACHE STRING "" FORCE )
#set( CMAKE_CXX_COMPILE_OBJECT "<CMAKE_CXX_COMPILER>  <DEFINES> <FLAGS> -o <OBJECT> -c <SOURCE>" )
#
#find_program( LLVM_LINK_EXECUTABLE llvm-link )
#set( CMAKE_CXX_LINK_EXECUTABLE ${LLVM_LINK_EXECUTABLE} CACHE STRING "" FORCE )

#include_directories( ${JOEMATH_INCLUDE_DIR} )

#add_library( runtime ${runtime_sources} )
#set_target_properties( runtime PROPERTIES PREFIX "" SUFFIX "" OUTPUT_NAME "${JOELANG_RUNTIME_PATH}" )

set( runtime_sources float.cpp
                     float.hpp
                     string.cpp
                     string.hpp
                     types.hpp )

set( JOELANG_RUNTIME_CXX_FLAGS ${CMAKE_CXX_FLAGS} -std=c++11 -O4 -msse4.1 -Wno-return-type-c-linkage )

# Use the same compiler if possible
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    set( JOELANG_RUNTIME_COMPILER ${CMAKE_CXX_COMPILER} )
else()
    set( JOELANG_RUNTIME_COMPILER clang++ )
endif()

find_program( LLVM_LINK_EXECUTABLE llvm-link )
set( JOELANG_RUNTIME_LINKER ${LLVM_LINK_EXECUTABLE} )

get_filename_component( JOEMATH_INCLUDE_DIR ${JOEMATH_INCLUDE_DIR} REALPATH )

foreach( runtime_source ${runtime_sources} )
    get_filename_component( extension ${runtime_source} EXT )
    if( ${extension} STREQUAL ".cpp" )
        get_filename_component( bitcode_file ${runtime_source} NAME)
        set( bitcode_file ${CMAKE_CURRENT_BINARY_DIR}/${bitcode_file}.bc )
        add_custom_command(
            OUTPUT ${bitcode_file}
            COMMAND ${JOELANG_RUNTIME_COMPILER} -O4 -std=c++11 ${JOELANG_RUNTIME_CXX_FLAGS} -I${JOEMATH_INCLUDE_DIR} -c -emit-llvm ${CMAKE_CURRENT_SOURCE_DIR}/${runtime_source} -o ${bitcode_file}
            DEPENDS ${runtime_sources} 
            IMPLICIT_DEPENDS ${runtime_source} )
        list( APPEND runtime_bitcode_files ${bitcode_file} )
    endif()
endforeach()

add_custom_command(
    OUTPUT ${JOELANG_RUNTIME_PATH}
    COMMAND ${JOELANG_RUNTIME_LINKER} -o ${JOELANG_RUNTIME_PATH} ${runtime_bitcode_files}
    DEPENDS ${runtime_bitcode_files} )

add_custom_target( compile_runtime DEPENDS ${JOELANG_RUNTIME_PATH} 
                                   SOURCES ${runtime_sources} )
