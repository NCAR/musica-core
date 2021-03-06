################################################################################
# Link musicalib to a test and add it to the suite

macro(add_std_test test_name)
  target_include_directories(${test_name} PUBLIC ${CMAKE_BINARY_DIR}/src)
  target_link_libraries(${test_name} musicalib)
  add_test(NAME ${test_name} COMMAND ${test_name})
endmacro(add_std_test)

################################################################################
# Copy test data

add_custom_target(copy_unit_data ALL ${CMAKE_COMMAND} -E copy_directory
  ${CMAKE_CURRENT_SOURCE_DIR}/data ${CMAKE_BINARY_DIR}/test/data)

################################################################################
# MusicBox tests

add_executable(util_array util/array.F90)
add_std_test(util_array)

add_executable(util_convert util/convert.F90)
add_std_test(util_convert)

add_executable(util_data_type util/data_type.F90)
add_std_test(util_data_type)

add_executable(util_datetime util/datetime.F90)
add_std_test(util_datetime)

add_executable(util_property util/property.F90)
add_std_test(util_property)

add_executable(util_property_set util/property_set.F90)
add_std_test(util_property_set)

add_executable(util_string util/string.F90)
add_std_test(util_string)

add_executable(config config.F90)
add_std_test(config)

add_executable(domain_cell domains/cell.F90)
add_std_test(domain_cell)

add_executable(grid_example
               examples/O2_effective_cross_sections/O2_optics.F90
               examples/O2_effective_cross_sections/test_grid_example.F90)
add_std_test(grid_example)

################################################################################
