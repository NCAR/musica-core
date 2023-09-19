################################################################################
# Test utilities

include(test_util)

################################################################################
# Copy test data

add_custom_target(copy_unit_data ALL ${CMAKE_COMMAND} -E copy_directory
  ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/test)

################################################################################
# Musica-core tests

if(NOT ENABLE_UTIL_ONLY)
  create_standard_test(NAME grid SOURCES grid.F90)

  create_standard_test(NAME initial_conditions SOURCES initial_conditions.F90
                                                        mock_domain.F90
                                                        mock_domain_iterator.F90
                                                        mock_domain_state.F90
                                                        mock_domain_state_accessor.F90
                                                        mock_domain_state_mutator.F90)

  create_standard_test(NAME interpolator SOURCES interpolator.F90)

  create_standard_test(NAME property SOURCES property.F90)

  create_standard_test(NAME property_set SOURCES property_set.F90)
  add_executable(test_property_set_failure property_set.F90)
  add_std_test_script(test_property_set_failure property_set.sh)

  add_subdirectory(domains)
  add_subdirectory(interpolator_strategies)
  add_subdirectory(lookup)
endif()

add_subdirectory(util)

################################################################################
