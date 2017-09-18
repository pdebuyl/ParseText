cmake_minimum_required(VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

project(ParseText Fortran)
FIND_PACKAGE(Git)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

if (CMAKE_Fortran_COMPILER MATCHES "gfortran")
  # gfortran
  set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
endif (CMAKE_Fortran_COMPILER MATCHES "gfortran")

option(PT_HDF5 "Enable HDF5 storage" OFF)

if(PT_HDF5)
  find_package(HDF5 COMPONENTS Fortran REQUIRED)
  include_directories(${HDF5_Fortran_INCLUDE_DIR} ${HDF5_Fortran_INCLUDE_DIRS})
  add_definitions(-DPT_HDF5)
endif(PT_HDF5)

add_custom_target(ParseText_version ${CMAKE_COMMAND} -D
SRC=${CMAKE_CURRENT_SOURCE_DIR} -D
F95_MOD_DIR=${CMAKE_Fortran_MODULE_DIRECTORY} -D
CMAKE_MODULE_PATH=${CMAKE_MODULE_PATH} -D
CMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER} -D
CMAKE_Fortran_FLAGS=${CMAKE_Fortran_FLAGS} -P
${CMAKE_CURRENT_SOURCE_DIR}/cmake/version.cmake)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY}) 
add_library(ParseText ParseText.F90)
if(PT_HDF5)
  target_link_libraries(ParseText ${HDF5_Fortran_LIBRARIES} ${CMAKE_DL_LIBS})
endif(PT_HDF5)
add_dependencies(ParseText ParseText_version)

add_executable(example example.f90)
target_link_libraries(example ParseText)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
endif()

