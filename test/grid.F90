! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the musica_grid module

!> Test module for the musica_grid module
module test_grid

  use musica_assert
  use musica_grid

  implicit none
  private

  public :: test_grid_t

  type, extends(grid_t) :: foo_grid_t
  end type foo_grid_t

  type, extends(grid_t) :: bar_grid_t
  end type bar_grid_t

  interface foo_grid_t
    procedure :: foo_grid_constructor
  end interface

  interface bar_grid_t
    procedure :: bar_grid_constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor for foo_grid_t
  function foo_grid_constructor( ) result( foo )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t

    type(foo_grid_t) :: foo

    type(string_t) :: units
    real(kind=dk) :: lower_bounds(3), upper_bounds(3)

    lower_bounds = (/ 10.0_dk, 100.0_dk, 200.0_dk /)
    upper_bounds = (/ 60.0_dk, 150.0_dk, 300.0_dk /)
    units = "foos"

    call foo%private_constructor_bounds( lower_bounds, upper_bounds, units )

  end function foo_grid_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor for bar_grid_t
  function bar_grid_constructor( ) result( bar )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t

    type(bar_grid_t) :: bar

    type(string_t) :: units
    real(kind=dk) :: midpoints(3)

    midpoints = (/ 5.0_dk, 10.0_dk, 20.0_dk /)
    units = "bars"

    call bar%private_constructor_midpoints( midpoints, units )

  end function bar_grid_constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Test grid_t functionality
  subroutine test_grid_t( )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t

    type(grid_t) :: a, b, c, d
    type(foo_grid_t) :: foo
    type(bar_grid_t) :: bar

    real(kind=dk), allocatable :: lower_bounds(:), upper_bounds(:)
    real(kind=dk), allocatable :: midpoints(:)
    real(kind=dk), allocatable :: compare_bounds(:)
    type(grid_section_t) :: a_sect, b_sect, d_sect, foo_sect, bar_sect
    class(grid_iterator_t), pointer :: a_iter, b_iter, d_iter, foo_iter, bar_iter
    type(string_t) :: units

    lower_bounds = (/ 10.0_dk, 100.0_dk, 200.0_dk /)
    upper_bounds = (/ 60.0_dk, 150.0_dk, 300.0_dk /)
    units = "foos"
    a = grid_t( lower_bounds, upper_bounds, units )
    c = grid_t( lower_bounds, upper_bounds, units )

    lower_bounds(2) = 125.0_dk
    upper_bounds(2) = 174.0_dk
    b = grid_t( lower_bounds, upper_bounds, units )

    midpoints    = (/ 5.0_dk,   10.0_dk,  20.0_dk /)
    units = "bars"
    d = grid_t( midpoints, units )

    deallocate( lower_bounds )
    deallocate( upper_bounds )
    deallocate( midpoints    )

    foo = foo_grid_t( )
    bar = bar_grid_t( )

    ! test comparison operators
    call assert( 222304556, a .eq. c )
    call assert( 901478933, a .ne. b )
    call assert( 105174199, c .ne. b )
    call assert( 664860390, a .eq. foo )
    call assert( 389554982, foo .eq. c )
    call assert( 831658521, foo .ne. b )
    call assert( 875863208, a .ne. bar )
    call assert( 367298030, a .ne. d )
    call assert( 818024649, foo .ne. bar )
    call assert( 760186090, d .eq. bar )

    ! test grid property accessors
    call assert( 501446629, a%number_of_sections( )   .eq. 3 )
    call assert( 869145662, d%number_of_sections( )   .eq. 3 )
    call assert( 498087856, foo%number_of_sections( ) .eq. 3 )
    call assert( 193782353, bar%number_of_sections( ) .eq. 3 )

    call assert( 149150968, c%units( )   .eq. "foos" )
    call assert( 886163904, foo%units( ) .eq. "foos" )
    call assert( 300836391, d%units( )   .eq. "bars" )
    call assert( 860522582, bar%units( ) .eq. "bars" )

    a_iter   => a%iterator( )
    b_iter   => b%iterator( )
    d_iter   => d%iterator( )
    foo_iter => foo%iterator( )
    bar_iter => bar%iterator( )

    ! grid section 1
    call assert( 200083629, a_iter%next( ) )
    call assert( 142245070, b_iter%next( ) )
    call assert( 144026198, d_iter%next( ) )
    call assert( 937096565, foo_iter%next( ) )
    call assert( 205223530, bar_iter%next( ) )

    call assert( 807984355, a%lower_bound( a_iter )     .eq. 10.0_dk )
    call assert( 253849594, b%lower_bound( b_iter )     .eq. 10.0_dk )
    call assert( 701217440, foo%lower_bound( foo_iter ) .eq. 10.0_dk )
    call assert( 198505984, almost_equal( d%lower_bound( d_iter ),     2.5_dk ) )
    call assert( 430312515, almost_equal( bar%lower_bound( bar_iter ), 2.5_dk ) )

    call assert( 349676366, almost_equal( a%mid_point( a_iter ),      35.0_dk ) )
    call assert( 797044212, almost_equal( b%mid_point( b_iter ),     35.0_dk ) )
    call assert( 344412059, almost_equal( foo%mid_point( foo_iter ), 35.0_dk ) )
    call assert( 984734399, d%mid_point( d_iter )     .eq. 5.0_dk )
    call assert( 532102246, bar%mid_point( bar_iter ) .eq. 5.0_dk )

    call assert( 531060536, a%upper_bound( a_iter )     .eq. 60.0_dk )
    call assert( 425912032, b%upper_bound( b_iter )     .eq. 60.0_dk )
    call assert( 873279878, foo%upper_bound( foo_iter ) .eq. 60.0_dk )
    call assert( 253438065, almost_equal( d%upper_bound( d_iter ),     7.5_dk ) )
    call assert( 418330662, almost_equal( bar%upper_bound( bar_iter ), 7.5_dk ) )

    call assert( 985598223, a%range( a_iter )     .eq. 60.0_dk - 10.0_dk )
    call assert( 815441319, b%range( b_iter )     .eq. 60.0_dk - 10.0_dk )
    call assert( 645284415, foo%range( foo_iter ) .eq. 60.0_dk - 10.0_dk )
    call assert( 922083828, almost_equal( d%range( d_iter ),     5.0_dk ) )
    call assert( 406348809, almost_equal( bar%range( bar_iter ), 5.0_dk ) )

    call assert( 284078551, a%property_index( a_iter )     .eq. 1 )
    call assert( 475127511, b%property_index( b_iter )     .eq. 1 )
    call assert( 922495357, foo%property_index( foo_iter ) .eq. 1 )
    call assert( 240002973, d%property_index( d_iter )     .eq. 1 )
    call assert( 687370819, bar%property_index( bar_iter ) .eq. 1 )

    a_sect   = a%section( a_iter )
    b_sect   = b%section( b_iter )
    d_sect   = d%section( d_iter )
    foo_sect = foo%section( foo_iter )
    bar_sect = bar%section( bar_iter )

    call assert( 389679350, a_sect .eq. b_sect )
    call assert( 134813703, a_sect .eq. foo_sect )
    call assert( 620909180, d_sect .eq. bar_sect )
    call assert( 963128522, a_sect .ne. d_sect )
    call assert( 505232062, foo_sect .ne. bar_sect )

    call assert( 388226111, a_sect%overlap( b_sect )   .eq. 50.0_dk )
    call assert( 864656798, a_sect%overlap( foo_sect ) .eq. 50.0_dk )

    call assert( 997127781, a_sect%lower_bound( )   .eq. 10.0_dk )
    call assert( 976975143, b_sect%lower_bound( )   .eq. 10.0_dk )
    call assert( 189293489, foo_sect%lower_bound( ) .eq. 10.0_dk )
    call assert( 279142133, almost_equal( d_sect%lower_bound( ),   2.5_dk ) )
    call assert( 110890763, almost_equal( bar_sect%lower_bound( ), 2.5_dk ) )

    call assert( 160820063, almost_equal( a_sect%mid_point( ),    35.0_dk ) )
    call assert( 555613657, almost_equal( b_sect%mid_point( ),   35.0_dk ) )
    call assert( 102981504, almost_equal( foo_sect%mid_point( ), 35.0_dk ) )
    call assert( 719792433, d_sect%mid_point( )   .eq. 5.0_dk )
    call assert( 267160280, bar_sect%mid_point( ) .eq. 5.0_dk )

    call assert( 919136584, a_sect%upper_bound( )   .eq. 60.0_dk )
    call assert( 748979680, b_sect%upper_bound( )   .eq. 60.0_dk )
    call assert( 296347527, foo_sect%upper_bound( ) .eq. 60.0_dk )
    call assert( 217944801, almost_equal( d_sect%upper_bound( ),   7.5_dk ) )
    call assert( 665312647, almost_equal( bar_sect%upper_bound( ), 7.5_dk ) )

    call assert( 126190623, a_sect%range( )   .eq. 50.0_dk )
    call assert( 921042118, b_sect%range( )   .eq. 50.0_dk )
    call assert( 750885214, foo_sect%range( ) .eq. 50.0_dk )
    call assert( 158653003, almost_equal( d_sect%range( ),   5.0_dk ) )
    call assert( 542917983, almost_equal( bar_sect%range( ), 5.0_dk ) )

    ! grid section 2
    call assert( 979594498, a_iter%next( ) )
    call assert( 526962345, b_iter%next( ) )
    call assert( 421813841, foo_iter%next( ) )
    call assert( 767554673, d_iter%next( ) )
    call assert( 992191363, bar_iter%next( ) )

    a_sect   = a%section( a_iter )
    b_sect   = b%section( b_iter )
    foo_sect = foo%section( foo_iter )
    d_sect   = d%section( d_iter )
    bar_sect = bar%section( bar_iter )

    call assert( 427390868, a_sect   .ne. b_sect )
    call assert( 188866791, foo_sect .ne. b_sect )
    call assert( 190772325, foo_sect .eq. a_sect )
    call assert( 538105971, d_sect .eq. bar_sect )
    call assert( 197792163, a_sect .ne. d_sect )
    call assert( 757478354, foo_sect .ne. bar_sect )

    call assert( 294467590, a_sect%overlap( b_sect )   .eq. 150.0_dk - 125.0_dk )
    call assert( 856059315, a_sect%overlap( foo_sect ) .eq. 150.0_dk - 100.0_dk )

    call assert( 629969386, a_sect%range( )   .eq. 150.0_dk - 100.0_dk )
    call assert( 284391271, b_sect%range( )   .eq. 174.0_dk - 125.0_dk )
    call assert( 346040898, foo_sect%range( ) .eq. 150.0_dk - 100.0_dk )
    call assert( 419070080, almost_equal( d_sect%range( ),   7.5_dk ) )
    call assert( 248913176, almost_equal( bar_sect%range( ), 7.5_dk ) )

    ! grid section 3
    call assert( 251656937, a_iter%next( ) )
    call assert( 699024783, b_iter%next( ) )
    call assert( 246392630, foo_iter%next( ) )
    call assert( 573886358, d_iter%next( ) )
    call assert( 233572550, bar_iter%next( ) )

    ! end of grid
    call assert( 358710975, .not. a_iter%next( ) )
    call assert( 188554071, .not. b_iter%next( ) )
    call assert( 635921917, .not. foo_iter%next( ) )
    call assert( 288052336, .not. d_iter%next( ) )
    call assert( 230213777, .not. bar_iter%next( ) )

    ! check overlap function
    call assert( 773356593, a%overlap( c ) .eq. 200.0_dk )
    call assert( 371845453, a%overlap( b ) .eq. 175.0_dk )

    ! check index assignment function
    call d_iter%set( 3 )
    call bar_iter%set( 3 )
    call assert( 897406301, almost_equal( d%upper_bound( d_iter ),     25.0_dk ) )
    call assert( 565715573, almost_equal( bar%upper_bound( bar_iter ), 25.0_dk ) )

    deallocate( a_iter   )
    deallocate( b_iter   )
    deallocate( d_iter   )
    deallocate( foo_iter )
    deallocate( bar_iter )

  end subroutine test_grid_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module test_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Driver for musica_grid tests
program test_grid_driver

  use test_grid
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
  call test_grid_t( )
  !$omp end parallel

end program test_grid_driver

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
