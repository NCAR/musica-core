!> \file
!> Tests for the musica_array module

!> Tests for the musica_array module
program test_util_array

  use musica_assert,                   only : assert, almost_equal
  use musica_array
  use musica_constants,                only : musica_ik, musica_rk, musica_dk
  use musica_string,                   only : string_t

  implicit none

  call test_array_functions( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests array functions
  subroutine test_array_functions( )

    type(string_t), allocatable :: str_array(:)
    real(kind=musica_dk), allocatable :: dbl_array(:)
    real(kind=musica_rk), allocatable :: flt_array(:)
    integer(kind=musica_ik), allocatable :: int_array(:)
    logical, allocatable :: bool_array(:)
    type(string_t) :: str
    integer(kind=musica_ik) :: idx

    allocate( str_array(  0 ) )
    allocate( dbl_array(  0 ) )
    allocate( flt_array(  0 ) )
    allocate( int_array(  0 ) )
    allocate( bool_array( 0 ) )

    str = "foo"
    str_array = [ str_array, str ]
    str = "bar"
    str_array = [ str_array, str ]
    str = "foObar"
    str_array = [ str_array, str ]

    call assert( 301097835, size( str_array ) .eq. 3 )
    call assert( 184681299, find_string_in_array( str_array, "foo", idx ) )
    call assert( 360841928, idx .eq. 1 )
    call assert( 520470218, find_string_in_array( str_array, "foObar", idx,   &
                                                  case_sensitive = .true. ) )
    call assert( 745106908, idx .eq. 3 )
    call assert( 239900503, .not. find_string_in_array( str_array, "fooBar",  &
                                              idx, case_sensitive = .true. ) )
    call assert( 234636196, .not. find_string_in_array( str_array,            &
                                                        "not there", idx ) )
    str = "bar"
    call assert( 911905039, find_string_in_array( str_array, str, idx ) )
    call assert( 689173883, idx .eq. 2 )
    str = "Bar"
    call assert( 183967478, .not. find_string_in_array( str_array, str, idx,  &
                                                   case_sensitive = .true. ) )
    str = "not there"
    call assert( 231277423, .not. find_string_in_array( str_array, str, idx ) )

    deallocate( str_array )
    allocate( str_array( 3 ) )

    str_array( 1 ) = "foo.BaR"
    str_array( 2 ) = "Bar.foO"
    str_array( 3 ) = "justfoo"

    call assert( 100527721, find_string_in_split_array( str_array, "foo", ".",&
                                                        1, idx ) )
    call assert( 253438465, idx .eq. 1 )
    call assert( 192693428, find_string_in_split_array( str_array, "foo", ".",&
                                                        2, idx ) )
    call assert( 522478622, idx .eq. 2 )
    call assert( 634796967, .not. find_string_in_split_array( str_array,      &
                              "foo", ".", 2, idx, case_sensitive = .true. ) )
    call assert( 747115312, find_string_in_split_array( str_array, "BaR", ".",&
                              2, idx, case_sensitive = .true. ) )
    call assert( 859433657, idx .eq. 1 )

    if( allocated( dbl_array ) ) deallocate( dbl_array )
    dbl_array = calculate_linear_array( 1.0_musica_dk, 5.0_musica_dk, 5 )
    call assert( 781682679, size( dbl_array ) .eq. 5 )
    call assert( 106319370, dbl_array( 1 ) .eq. 1.0_musica_dk )
    call assert( 824180612, almost_equal( dbl_array( 2 ), 2.0_musica_dk ) )
    call assert( 654023708, almost_equal( dbl_array( 3 ), 3.0_musica_dk ) )
    call assert( 201391555, almost_equal( dbl_array( 4 ), 4.0_musica_dk ) )
    call assert( 996243050, dbl_array( 5 ) .eq. 5.0_musica_dk )

    if( allocated( dbl_array ) ) deallocate( dbl_array )
    dbl_array =                                                               &
        calculate_logarithmic_array( 1.0_musica_dk, 10000.0_musica_dk, 5 )
    call assert( 764888814, size( dbl_array ) .eq. 5 )
    call assert( 312256661, dbl_array( 1 ) .eq. 1.0_musica_dk )
    call assert( 142099757, almost_equal( dbl_array( 2 ), 10.0_musica_dk ) )
    call assert( 589467603, almost_equal( dbl_array( 3 ), 100.0_musica_dk ) )
    call assert( 136835450, almost_equal( dbl_array( 4 ), 1000.0_musica_dk ) )
    call assert( 931686945, dbl_array( 5 ) .eq. 10000.0_musica_dk )

  end subroutine test_array_functions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_util_array
