cmake_minimum_required(VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(ParseText Fortran)
FIND_PACKAGE(Git)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

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

install(TARGETS ParseText LIBRARY DESTINATION lib ARCHIVE DESTINATION lib)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/ DESTINATION include)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

