add_executable(test_PT test_PT.f90 tester.f90)
target_link_libraries(test_PT ParseText)

add_executable(test_fail_0 test_fail_0.f90 tester.f90)
target_link_libraries(test_fail_0 ParseText)

add_executable(test_default_0 test_default_0.f90 tester.f90)
target_link_libraries(test_default_0 ParseText)

add_test(NAME test_PT COMMAND test_PT WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
add_test(NAME test_fail_0 COMMAND test_fail_0 WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
add_test(NAME test_default_0 COMMAND test_default_0 WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)

