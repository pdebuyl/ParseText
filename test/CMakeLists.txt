add_subdirectory(fortran_tester)

foreach(test_name test_PT test_fail_0 test_fail_1 test_default_0)
add_executable(${test_name} ${test_name}.f90)
target_link_libraries(${test_name} ParseText fortran_tester)
add_test(${test_name} ${test_name})
endforeach(test_name)

set_tests_properties(test_fail_1 PROPERTIES WILL_FAIL TRUE)

file(COPY sample_file DESTINATION .)
