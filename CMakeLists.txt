################################################################################
# Preamble

cmake_minimum_required(VERSION 3.17)
set(CMAKE_USER_MAKE_RULES_OVERRIDE "cmake/SetDefaults.cmake")

project(
  musicacore
  VERSION 0.4.0
  LANGUAGES Fortran CXX C
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${PROJECT_SOURCE_DIR}/cmake/cmake-modules")

message ( STATUS "CMake build configuration for musicacore(${CMAKE_BUILD_TYPE}) ${PROJECT_VERSION}" )

if(${CMAKE_VERSION} VERSION_LESS "3.21") 
  if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    set(PROJECT_IS_TOP_LEVEL TRUE)
  else()
    set(PROJECT_IS_TOP_LEVEL FALSE)
  endif()
endif()

################################################################################
# Projet wide setup options

include(CMakeDependentOption)
option(ENABLE_COVERAGE "Enable code coverage output" OFF)
option(ENABLE_MPI "Enable MPI parallel support" OFF)
cmake_dependent_option(ENABLE_OPENMP "Enable OpemMP support" OFF "ENABLE_MPI" OFF)
option(ENABLE_MEMCHECK "Enable memory checking in tests" ON)
option(ENABLE_UTIL_ONLY "Only include utility modules in build" OFF)
option(ENABLE_YAML "Uses YAML parser instead of JSON" OFF)

# Add flags for various compilers
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
  add_definitions(-DMUSICA_USING_INTEL)
elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  add_definitions(-DMUSICA_USING_GNU)
elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")
  add_definitions(-DMUSICA_USING_PGI)
endif()

# YAML flag
if(ENABLE_YAML)
  add_definitions(-DUSE_YAML)
else()
  add_definitions(-DUSE_JSON)
endif()

# MPI
if(ENABLE_MPI)
  add_definitions(-DMUSICA_USE_MPI)
endif()

include(GNUInstallDirs)
set(INSTALL_PREFIX "musicacore-${PROJECT_VERSION}")
set(INSTALL_MOD_DIR "${INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}")

# Set up include and lib directories
set(MUSICA_MOD_DIR "${PROJECT_BINARY_DIR}/include")
set(MUSICA_LIB_DIR "${PROJECT_BINARY_DIR}/lib")

################################################################################
# Dependencies

include(cmake/dependencies.cmake)

################################################################################
# MUSICA library

add_library(musicacore_object)

target_link_libraries(musicacore_object PUBLIC PkgConfig::netcdff)
if(ENABLE_YAML)
  target_link_libraries(musicacore_object PUBLIC yaml-cpp::yaml-cpp)
else()
  target_link_libraries(musicacore_object PUBLIC ${JSON_LIB})
endif()

set_target_properties(musicacore_object PROPERTIES
  Fortran_MODULE_DIRECTORY ${MUSICA_MOD_DIR}
)

target_include_directories(musicacore_object
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
    $<INSTALL_INTERFACE:${INSTALL_INCLUDE_DIR}>
)

add_library(musicacore $<TARGET_OBJECTS:musicacore_object>)
add_library(musica::musicacore ALIAS musicacore)

add_subdirectory(src)

set_target_properties(musicacore PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${MUSICA_LIB_DIR}
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
)

target_link_libraries(musicacore
  PUBLIC
    PkgConfig::netcdff
    ${JSON_LIB} 
)

target_include_directories(musicacore 
  PUBLIC  
    $<BUILD_INTERFACE:${MUSICA_MOD_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
    $<INSTALL_INTERFACE:${INSTALL_MOD_DIR}>
    $<INSTALL_INTERFACE:${INSTALL_INCLUDE_DIR}>
)

################################################################################
# MUSICA tests

if(PROJECT_IS_TOP_LEVEL)
  # Test code coverage
  if(ENABLE_COVERAGE)
    include(CodeCoverage)
    if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
      setup_target_for_coverage_lcov(
          NAME coverage
          EXECUTABLE "ctest"
          EXCLUDE "${PROJECT_SOURCE_DIR}/test/*"
          BASE_DIRECTORY "${PROJECT_SOURCE_DIR}/src")
    endif()
  endif()

  enable_testing()
  add_subdirectory(test)
endif()

################################################################################
# Packaging

# only include packaging if we are the top level project being built
if(PROJECT_IS_TOP_LEVEL)
  add_subdirectory(packaging)
endif()

################################################################################
