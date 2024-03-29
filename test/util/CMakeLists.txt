################################################################################
# Test utilities

include(test_util)

################################################################################
# Utility tests

create_standard_test(NAME util_array SOURCES array.F90)

create_standard_test(NAME util_assert SOURCES assert.F90)
add_executable(util_assert_failure assert.F90)
add_std_test_script(util_assert_failure assert.sh)

create_standard_test(NAME util_config SOURCES config.F90)

create_standard_test(NAME util_convert SOURCES convert.F90)

create_standard_test(NAME util_data_type SOURCES data_type.F90)

create_standard_test(NAME util_datetime SOURCES datetime.F90)

add_executable(util_logger logger.F90)
add_std_test_script(util_logger util_logger)

create_standard_test(NAME util_map SOURCES map.F90)
if(NOT ENABLE_YAML)
  add_executable(util_map_failure map.F90)
  add_std_test_script(util_map_failure map.sh)
endif()

create_standard_test(NAME util_mpi SOURCES mpi.F90)

create_standard_test(NAME util_string SOURCES string.F90)
add_executable(util_string_failure string.F90)
add_std_test_script(util_string_failure string.sh)

add_subdirectory(io)

################################################################################
