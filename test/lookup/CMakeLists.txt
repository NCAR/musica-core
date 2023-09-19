################################################################################
# Test utilities

include(test_util)

################################################################################
# Lookup tests

create_standard_test(NAME lookup_axis SOURCES axis.F90)
add_executable(lookup_axis_failure axis.F90)
add_std_test_script(lookup_axis_failure axis.sh)

create_standard_test(NAME lookup_2D_axis SOURCES 2D_axis.F90)

################################################################################
