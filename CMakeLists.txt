cmake_minimum_required(VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(ParseText Fortran)
FIND_PACKAGE(Git)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
ENABLE_TESTING()
if (CMAKE_Fortran_COMPILER MATCHES "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
endif (CMAKE_Fortran_COMPILER MATCHES "gfortran")


add_custom_target(ParseText_version ${CMAKE_COMMAND} -D
SRC=${CMAKE_CURRENT_SOURCE_DIR} -D
F95_MOD_DIR=${CMAKE_Fortran_MODULE_DIRECTORY} -D
CMAKE_MODULE_PATH=${CMAKE_MODULE_PATH} -D
CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -D
CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS} -P
${CMAKE_CURRENT_SOURCE_DIR}/cmake/version.cmake)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY}) 
add_library(ParseText ParseText.f90)
add_dependencies(ParseText ParseText_version)

add_executable(example example.f90)
target_link_libraries(example ParseText)

add_executable(test_PT test/test_PT.f90)
target_link_libraries(test_PT ParseText)

add_executable(test_fail_0 test/test_fail_0.f90)
target_link_libraries(test_fail_0 ParseText)

add_executable(test_default_0 test/test_default_0.f90)
target_link_libraries(test_default_0 ParseText)

add_test(NAME test_PT COMMAND test_PT WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
add_test(NAME test_fail_0 COMMAND test_fail_0 WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
add_test(NAME test_default_0 COMMAND test_default_0 WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
set_tests_properties(test_PT test_fail_0 test_default_0 PROPERTIES PASS_REGULAR_EXPRESSION "success")
set_tests_properties(test_fail_0 PROPERTIES WILL_FAIL true)

install(TARGETS ParseText LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)


