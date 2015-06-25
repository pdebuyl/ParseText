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

add_test(NAME test_PT COMMAND test_PT WORKING_DIRECTORY ${CMAKE_HOME_DIRECTORY}/test)
set_tests_properties(test_PT PROPERTIES PASS_REGULAR_EXPRESSION "success")

install(TARGETS ParseText LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)


