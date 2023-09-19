################################################################################
# Utility functions for creating tests

if(ENABLE_MEMCHECK)
  find_program(MEMORYCHECK_COMMAND "valgrind")
endif()

################################################################################
# build and add a test with the standard include paths and linked libraries

function(create_standard_test)
  set(prefix TEST)
  set(singleValues NAME)
  set(multiValues SOURCES)
  include(CMakeParseArguments)
  cmake_parse_arguments(${prefix} " " "${singleValues}" "${multiValues}" ${ARGN})
  add_executable(test_${TEST_NAME} ${TEST_SOURCES})
  target_link_libraries(test_${TEST_NAME} PUBLIC musicacore)
  if(ENABLE_OPENMP)
    target_link_libraries(test_${TEST_NAME} PUBLIC OpenMP::OpenMP_Fortran)
  endif()
  target_include_directories(test_${TEST_NAME} PUBLIC ${CMAKE_BINARY_DIR}/src)
  add_musica_test(${TEST_NAME} test_${TEST_NAME} "")
endfunction(create_standard_test)

################################################################################
# Add a test with memory checking

function(add_musica_test test_name test_binary test_args)
  if(ENABLE_MPI)
    add_test(NAME ${test_name} COMMAND mpirun -v -np 2 ${CMAKE_BINARY_DIR}/${test_binary} ${test_args})
  else()
    add_test(NAME ${test_name} COMMAND ${test_binary} ${test_args})
  endif()
  set(MEMORYCHECK_COMMAND_OPTIONS "--error-exitcode=1 --trace-children=yes --leak-check=full --gen-suppressions=all ${MEMCHECK_SUPPRESS}")
  set(memcheck "${MEMORYCHECK_COMMAND} ${MEMORYCHECK_COMMAND_OPTIONS}")
  separate_arguments(memcheck)
  if(ENABLE_MPI AND MEMORYCHECK_COMMAND AND ENABLE_MEMCHECK)
    add_test(NAME memcheck_${test_name}
      COMMAND mpirun -v -np 2 ${memcheck} ${CMAKE_BINARY_DIR}/${test_binary} ${test_args}
             WORKING_DIRECTORY ${working_dir})
    # add dependency between memcheck and previous test
    # https://stackoverflow.com/a/66931930/5217293
    set_tests_properties(${test_name} PROPERTIES FIXTURES_SETUP f_${test_name})
    set_tests_properties(memcheck_${test_name} PROPERTIES FIXTURES_REQUIRED f_${test_name})
  elseif(MEMORYCHECK_COMMAND AND ENABLE_MEMCHECK)
    add_test(NAME memcheck_${test_name}
             COMMAND ${memcheck} ${CMAKE_BINARY_DIR}/${test_binary} ${test_args}
             WORKING_DIRECTORY ${working_dir})
  endif()
endfunction(add_musica_test)

################################################################################
# Link musicacore to a test and add it to the suite as a bash script

macro(add_std_test_script test_name script_path)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} PUBLIC musicacore)
  if(ENABLE_OPENMP)
    target_link_libraries(${test_name} PUBLIC OpenMP::OpenMP_Fortran)
  endif()
  add_test(NAME ${test_name} COMMAND ${script_path})
endmacro(add_std_test_script)

################################################################################
