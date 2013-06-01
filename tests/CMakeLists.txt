enable_testing()

#-------------------------------------------------------------------------------
# Verify things are as they should be
#-------------------------------------------------------------------------------
if( NOT DEFINED JOELANG_INCLUDE_DIRS )
    message( FATAL_ERROR "JoeLang include dirs not set" )
endif()
if( NOT DEFINED JOELANG_LIBS )
    message( FATAL_ERROR "JoeLang libs not set" )
endif()

#-------------------------------------------------------------------------------
# The testing program
#-------------------------------------------------------------------------------
add_executable( joelang_tester EXCLUDE_FROM_ALL tester.cpp )
target_link_libraries( joelang_tester ${JOELANG_LIBS} )
include_directories( ${JOELANG_INCLUDE_DIRS} )

add_definitions( -std=c++11 )

#-------------------------------------------------------------------------------
# The tests
#-------------------------------------------------------------------------------
file( GLOB test_files "*.jfx" )

add_custom_target( check_joelang
                   COMMAND ${CMAKE_CTEST_COMMAND}
                   DEPENDS joelang_tester
                   SOURCES ${test_files} )

foreach( test_file ${test_files} )
    get_filename_component( test_name ${test_file} NAME )
    add_test( ${test_name} joelang_tester ${test_file} )
endforeach()
