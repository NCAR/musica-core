! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the musica_interpolator_linear_1D module

!> Test module for the linear 1D interpolator strategy
program test_interpolator_linear_1D

#ifdef MUSICA_USE_OPENMP
  use omp_lib
#endif

  implicit none

#ifdef MUSICA_USE_OPENMP
  write(*,*) "Testing with ", omp_get_max_threads( ), " threads"
#else
  write(*,*) "Testing without OpenMP support"
#endif
  !$omp parallel
  call test_linear_1D_strategy( )
  !$omp end parallel
  call example( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_linear_1D_strategy( )

    use musica_assert,                 only : assert, almost_equal
    use musica_constants,              only : dk => musica_dk
    use musica_grid,                   only : grid_t
    use musica_interpolator,           only : interpolator_t
    use musica_interpolator_linear_1D, only : strategy
    use musica_string,                 only : string_t

    type(grid_t) :: from_grid, to_grid
    type(string_t) :: units
    type(interpolator_t) :: interp
    real(kind=dk), allocatable :: from_mp(:), to_mp(:)
    real(kind=dk), allocatable :: from_array(:), to_array(:)

    units = "foos"

    from_mp    = (/ 1.0_dk, 11.0_dk, 51.0_dk, 61.0_dk /)
    from_array = (/ 5.0_dk, 10.0_dk,  2.0_dk,  0.0_dk /)
    from_grid = grid_t( from_mp, units )

    to_mp    = (/ 0.0_dk, 6.0_dk, 11.0_dk, 56.0_dk, 57.0_dk /)
    to_array = (/ 0.0_dk, 0.0_dk,  0.0_dk,  0.0_dk,  0.0_dk /)
    to_grid = grid_t( to_mp, units )

    interp = interpolator_t( strategy, from_grid, to_grid )
    call interp%interpolate( from_array, to_array )

    call assert( 890909528, almost_equal( to_array(1),  5.0_dk ) )
    call assert( 887550755, almost_equal( to_array(2),  7.5_dk ) )
    call assert( 999869100, almost_equal( to_array(3), 10.0_dk ) )
    call assert( 547236947, almost_equal( to_array(4),  1.0_dk ) )
    call assert( 712129544, almost_equal( to_array(5),  0.8_dk ) )

  end subroutine test_linear_1D_strategy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine example( )

    use musica_assert,                 only : assert, almost_equal

    !! [Linear interpolator example]
    use musica_constants,              only : dk => musica_dk
    use musica_grid,                   only : grid_t
    use musica_interpolator,           only : interpolator_t
    use musica_interpolator_linear_1D, only : strategy
    use musica_string,                 only : string_t

    type(interpolator_t) :: a
    type(grid_t) :: from_grid, to_grid
    type(string_t) :: units
    real(kind=dk), allocatable :: mid_points(:)
    real(kind=dk) :: x(3), y(6)

    units = "foos"

    mid_points = (/ 0.0, 10.0, 20.0 /)
    from_grid = grid_t( mid_points, units )

    mid_points = (/ 0.0, 5.0, 10.0, 15.0, 20.0, 25.0 /)
    to_grid = grid_t( mid_points, units )

    x = (/ 10.0, 20.0, 30.0 /)

    a = interpolator_t( strategy, from_grid, to_grid )

    call a%interpolate( x, y )

    write(*,*) y
    !! [Linear interpolator example]

    call assert( 178147421, almost_equal( y(1), 10.0_dk ) )
    call assert( 855416264, almost_equal( y(2), 15.0_dk ) )
    call assert( 402784111, almost_equal( y(3), 20.0_dk ) )
    call assert( 297635607, almost_equal( y(4), 25.0_dk ) )
    call assert( 127478703, almost_equal( y(5), 30.0_dk ) )
    call assert( 574846549, almost_equal( y(6), 30.0_dk ) )

  end subroutine example

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_interpolator_linear_1D
