enable_testing()

#
# Testing
#
add_executable( joelang_tester EXCLUDE_FROM_ALL tester.cpp )
target_link_libraries( joelang_tester joelang )
include_directories( ${joelang_SOURCE_DIR}/include )

set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${joemath_CXX_FLAGS}" )

file( GLOB test_files "*.jfx" )

add_custom_target( check
                   COMMAND ${CMAKE_CTEST_COMMAND}
                   DEPENDS joelang_tester
                   SOURCES ${test_files} )

foreach( test_file ${test_files} )
    get_filename_component( test_name ${test_file} NAME )
    add_test( ${test_name} joelang_tester ${test_file} )
endforeach()
