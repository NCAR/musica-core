find_package(PkgConfig REQUIRED)
include(FetchContent)

################################################################################
# NetCDF library

pkg_check_modules(netcdff IMPORTED_TARGET REQUIRED netcdf-fortran)

################################################################################
# json-fortran library

if(NOT ENABLE_YAML)
  find_path(JSON_INCLUDE_DIR json_module.mod
    DOC "json-fortran include directory (must include json_*.mod files)"
    PATHS
      $ENV{JSON_FORTRAN_HOME}/lib
      /opt/local/lib
      /usr/local/lib
      /usr/local/lib64)
  find_library(JSON_LIB jsonfortran
    DOC "json-fortran library"
    PATHS
      $ENV{JSON_FORTRAN_HOME}/lib
      /opt/local/lib
      /usr/local/lib
      /usr/local/lib64)
  include_directories(${JSON_INCLUDE_DIR})
endif()

################################################################################
# yaml-cpp lirary

if(ENABLE_YAML)
  FetchContent_Declare(
    yaml-cpp
    GIT_REPOSITORY https://github.com/jbeder/yaml-cpp/
    GIT_TAG 0.8.0
  )
  FetchContent_MakeAvailable(yaml-cpp)
endif()

################################################################################
# Memory check
if(ENABLE_MEMCHECK)
  find_file(MEMCHECK_SUPPRESS_FILE
    DOC "Suppression file for memory checking"
    NAMES openmpi-valgrind.supp
    PATHS
      /usr/share/openmpi
      /usr/lib64/openmpi/share
      /usr/lib64/openmpi/share/openmpi
      /usr/share)
  if(MEMCHECK_SUPPRESS_FILE)
    set(MEMCHECK_SUPPRESS "--suppressions=${PROJECT_SOURCE_DIR}/test/valgrind.supp --suppressions=${MEMCHECK_SUPPRESS_FILE}")
  else()
    set(MEMCHECK_SUPPRESS "--suppressions=${PROJECT_SOURCE_DIR}/test/valgrind.supp")
  endif()
endif()

################################################################################
# OpenMP
if(ENABLE_OPENMP)
  find_package(OpenMP)
  if(OpenMP_Fortran_FOUND)
    message(STATUS "Compiling with OpenMP support")
    add_definitions(-DMUSICA_USE_OPENMP)
  else()
    message(FATAL_ERROR "OpenMP package not found")
  endif()
endif()

######################################################################