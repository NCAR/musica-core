! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The test_file_util program

!> Tests for the simple file io functions
program test_file_util

  use musica_assert
  use musica_file_util

  implicit none

  call test_get_file_data( )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Tests the get_file_data( ) function
  subroutine test_get_file_data( )

    use musica_constants,              only : dk => musica_dk
    use musica_string,                 only : string_t

    type(string_t) :: file_name, var_name
    real(kind=dk), allocatable :: var1D(:)
    real(kind=dk), allocatable :: var3D(:,:,:)

    ! 1D unallocated variable
    file_name = "data/file_util_test_data.nc"
    var_name  = "foo"
    call get_file_data( file_name, var_name, var1D, "file_util tests" )
    call assert( 485967824, allocated( var1D ) )
    call assert( 662102856, size( var1D ) .eq. 4 )
    call assert( 434107393, almost_equal( var1D( 1 ), 15.32_dk ) )
    call assert( 653479776, almost_equal( var1D( 2 ), 3.14_dk  ) )
    call assert( 483322872, almost_equal( var1D( 3 ), 26.71_dk ) )
    call assert( 313165968, almost_equal( var1D( 4 ), 19.34_dk ) )
    deallocate( var1D )

    ! 1D pre-allocated variable
    allocate( var1D( 3 ) )
    var_name = "bar"
    call get_file_data( file_name, var_name, var1D, "file_util tests" )
    call assert( 259451197, allocated( var1D ) )
    call assert( 254186890, size( var1D ) .eq. 3 )
    call assert( 878881481, almost_equal( var1D( 1 ), 51.43_dk  ) )
    call assert( 875522708, almost_equal( var1D( 2 ), 123.01_dk ) )
    call assert( 422890555, almost_equal( var1D( 3 ), 32.61_dk  ) )
    deallocate( var1D )

    ! 3D unallocated variable
    var_name = "foobar"
    call get_file_data( file_name, var_name, var3D, "file_util tests" )
    call assert( 628827846, allocated( var3D ) )
    call assert( 688571939, size( var3D, 1 ) .eq. 1 )
    call assert( 230675479, size( var3D, 2 ) .eq. 3 )
    call assert( 125526975, size( var3D, 3 ) .eq. 4 )
    call assert( 850105763, almost_equal( var3D( 1, 1, 1 ), 532.123_dk  ) )
    call assert( 231414897, almost_equal( var3D( 1, 2, 1 ), 1.5e28_dk   ) )
    call assert( 343733242, almost_equal( var3D( 1, 3, 1 ), 42.5_dk     ) )
    call assert( 723638505, almost_equal( var3D( 1, 1, 2 ), 39.25_dk    ) )
    call assert( 835956850, almost_equal( var3D( 1, 2, 2 ), 4293.12_dk  ) )
    call assert( 383324697, almost_equal( var3D( 1, 3, 2 ), 9753.231_dk ) )
    call assert( 926217023, almost_equal( var3D( 1, 1, 3 ), 3.25e-19_dk ) )
    call assert( 473584870, almost_equal( var3D( 1, 2, 3 ), 4.629e10_dk ) )
    call assert( 368436366, almost_equal( var3D( 1, 3, 3 ), 7264.12_dk  ) )
    call assert( 133271062, almost_equal( var3D( 1, 1, 4 ), 8.4918e7_dk ) )
    call assert( 757965653, almost_equal( var3D( 1, 2, 4 ), 13.2_dk     ) )
    call assert( 310597807, almost_equal( var3D( 1, 3, 4 ), 8293.12_dk  ) )
    deallocate( var3D )

    ! 3D pre-allocated variable
    var_name = "foobar"
    allocate( var3D( 1, 3, 4 ) )
    call get_file_data( file_name, var_name, var3D, "file_util tests" )
    call assert( 506458779, allocated( var3D ) )
    call assert( 618777124, size( var3D, 1 ) .eq. 1 )
    call assert( 166144971, size( var3D, 2 ) .eq. 3 )
    call assert( 895988066, size( var3D, 3 ) .eq. 4 )
    call assert( 443355913, almost_equal( var3D( 1, 1, 1 ), 532.123_dk  ) )
    call assert( 338207409, almost_equal( var3D( 1, 2, 1 ), 1.5e28_dk   ) )
    call assert( 168050505, almost_equal( var3D( 1, 3, 1 ), 42.5_dk     ) )
    call assert( 615418351, almost_equal( var3D( 1, 1, 2 ), 39.25_dk    ) )
    call assert( 727736696, almost_equal( var3D( 1, 2, 2 ), 4293.12_dk  ) )
    call assert( 892629293, almost_equal( var3D( 1, 3, 2 ), 9753.231_dk ) )
    call assert( 439997140, almost_equal( var3D( 1, 1, 3 ), 3.25e-19_dk ) )
    call assert( 334848636, almost_equal( var3D( 1, 2, 3 ), 4.629e10_dk ) )
    call assert( 164691732, almost_equal( var3D( 1, 3, 3 ), 7264.12_dk  ) )
    call assert( 612059578, almost_equal( var3D( 1, 1, 4 ), 8.4918e7_dk ) )
    call assert( 441902674, almost_equal( var3D( 1, 2, 4 ), 13.2_dk     ) )
    call assert( 606795271, almost_equal( var3D( 1, 3, 4 ), 8293.12_dk  ) )

  end subroutine test_get_file_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_file_util
