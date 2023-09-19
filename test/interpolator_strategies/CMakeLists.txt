################################################################################
# Test utilities

include(test_util)

################################################################################
# Interpolator strategy tests

create_standard_test(NAME interpolator_binned_mass SOURCES binned_mass.F90)

create_standard_test(NAME interpolator_linear_1D SOURCES linear_1D.F90)

################################################################################
