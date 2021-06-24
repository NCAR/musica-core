! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_file_util module

!> Simple file IO functions using NetCDF
!!
!! This is a temporary solution for the refactored MAM code. It should
!! eventually be merged with the other musica-core IO code to form a
!! comprehensive solution for file operations.
!!
!! Host models that want to use io libraries other than NetCDF can replace
!! these with their own wrapper functions.
module musica_file_util

  implicit none
  private

  public :: get_file_data

  !> Gets data from a file
  !!
  !! Simple file read that opens a file, reads data into the provided
  !! variable, and closes the file.
  !!
  !! For 1+D reads, if the array is allocated the shape must match the data
  !! present in the file. If the array is not allocated, it is allocated to
  !! fit the file data.
  interface get_file_data
    procedure :: get_file_data_1D_real
    procedure :: get_file_data_3D_real
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a 1D array from a file
  subroutine get_file_data_1D_real( file_name, variable_name, container,      &
      requestor_name )

    use musica_assert,                 only : die_msg
    use musica_constants,              only : musica_dk
    use musica_string,                 only : string_t, to_char
    use netcdf,                        only : nf90_get_var

    class(string_t),                   intent(in)    :: file_name
    class(string_t),                   intent(in)    :: variable_name
    real(kind=musica_dk), allocatable, intent(inout) :: container(:)
    character(len=*),                  intent(in)    :: requestor_name

    integer :: file_id, var_id
    integer, allocatable :: dim_sizes(:)
    type(string_t) :: id_str

    id_str = "variable '"//variable_name//"' in file '"//file_name//"'"
    file_id   = open_file( file_name )
    var_id    = variable_id( file_id, file_name, variable_name )
    dim_sizes = variable_dimensions( file_id, var_id, file_name,              &
                                     variable_name )
    call assert_msg( 417924137, size( dim_sizes ) .eq. 1,                     &
                     "Wrong number of dimensions for "//id_str%to_char( )//   &
                     ": Expected 1 got "//                                    &
                     trim( to_char( size( dim_sizes ) ) ) )
    if( allocated( container ) ) then
      call assert_msg( 838421799, size( container ) .eq. dim_sizes(1),        &
                       "Wrong sized container for "//id_str%to_char( )//      &
                       ": Expected "//trim( to_char( dim_sizes(1) ) )//" got "&
                       //trim( to_char( size( container ) ) ) )
    else
      allocate( container( dim_sizes(1) ) )
    end if
    call check_status( 185116662, nf90_get_var( file_id, var_id, container ), &
        "Error getting values for variable '"//variable_name%to_char( )//     &
        "' in file '"//file_name%to_char( )//"'" )
    call close_file( file_id )

  end subroutine get_file_data_1D_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a 3D array from a file
  subroutine get_file_data_3D_real( file_name, variable_name, container,      &
      requestor_name )

    use musica_assert,                 only : die_msg
    use musica_constants,              only : musica_dk
    use musica_string,                 only : string_t, to_char
    use netcdf,                        only : nf90_get_var

    class(string_t),                   intent(in)    :: file_name
    class(string_t),                   intent(in)    :: variable_name
    real(kind=musica_dk), allocatable, intent(inout) :: container(:,:,:)
    character(len=*),                  intent(in)    :: requestor_name

    integer :: file_id, var_id, i_dim
    integer, allocatable :: dim_sizes(:)
    type(string_t) :: id_str

    id_str = "variable '"//variable_name//"' in file '"//file_name//"'"
    file_id   = open_file( file_name )
    var_id    = variable_id( file_id, file_name, variable_name )
    dim_sizes = variable_dimensions( file_id, var_id, file_name,              &
                                     variable_name )
    call assert_msg( 710814022, size( dim_sizes ) .eq. 3,                     &
                     "Wrong number of dimensions for "//id_str%to_char( )//   &
                     ": Expected 3 got "//                                    &
                     trim( to_char( size( dim_sizes ) ) ) )
    if( allocated( container ) ) then
      do i_dim = 1, 3
        call assert_msg( 652975463, size( container ) .eq. dim_sizes( i_dim ),&
                         "Wrong sized container for "//id_str%to_char( )//    &
                         ": Expected "//trim( to_char( dim_sizes( i_dim ) ) ) &
                         //" got "//trim( to_char( size( container ) ) ) )
      end do
    else
      allocate( container( dim_sizes(1), dim_sizes(2), dim_sizes(3) ) )
    end if
    call check_status( 906700591, nf90_get_var( file_id, var_id, container ), &
        "Error getting values for variable '"//variable_name%to_char( )//     &
        "' in file '"//file_name%to_char( )//"'" )
    call close_file( file_id )

  end subroutine get_file_data_3D_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Opens a file and returns its identifier
  integer function open_file( file_name )

    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_open, NF90_NOWRITE

    class(string_t), intent(in) :: file_name

    call check_status( 960503743,                                             &
        nf90_open( file_name%to_char( ), NF90_NOWRITE, open_file ),           &
        "Error openning file '"//file_name%to_char( )//"'" )

  end function open_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a variable id for a file
  integer function variable_id( file_id, file_name, variable_name )

    use musica_string,                 only : string_t
    use netcdf,                        only : nf90_inq_varid

    integer,         intent(in) :: file_id
    class(string_t), intent(in) :: file_name
    class(string_t), intent(in) :: variable_name

    call check_status( 385476926,                                             &
        nf90_inq_varid( file_id, variable_name%to_char( ), variable_id ),     &
        "Cannot find '"//variable_name%to_char( )//"' in file '"//            &
        file_name%to_char( )//"'" )

  end function variable_id

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the dimensions of a file variable
  function variable_dimensions( file_id, variable_id, file_name,              &
      variable_name ) result( dim_sizes )

    use musica_string,                 only : string_t, to_char
    use netcdf,                        only : nf90_inquire_variable,          &
                                              nf90_inquire_dimension

    integer, allocatable        :: dim_sizes(:)
    integer, intent(in)         :: file_id
    integer, intent(in)         :: variable_id
    class(string_t), intent(in) :: file_name
    class(string_t), intent(in) :: variable_name

    integer :: n_dims, i_dim
    integer, allocatable :: dimids(:)
    type(string_t) :: id_str

    id_str = "variable '"//variable_name//"' in file '"//file_name//"'"
    call check_status( 498133588,                                             &
        nf90_inquire_variable( file_id, variable_id, ndims = n_dims ),       &
        "Error getting number of dimensions for "//id_str%to_char( ) )
    allocate( dimids( n_dims ) )
    call check_status( 151841652,                                             &
        nf90_inquire_variable( file_id, variable_id, dimids = dimids ),       &
        "Error getting dimesions for "//id_str%to_char( ) )
    allocate( dim_sizes( n_dims ) )
    do i_dim = 1, n_dims
      call check_status( 696639512,                                           &
          nf90_inquire_dimension( file_id, dimids( i_dim ),                   &
                                  len = dim_sizes( i_dim ) ),                 &
          "Error getting dimesion size "//trim( to_char( i_dim ) )//" for "// &
          id_str%to_char( ) )
    end do

  end function variable_dimensions

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Closes a file
  subroutine close_file( file_id )

    use netcdf,                        only : nf90_close

    integer, intent(in) :: file_id

    call check_status( 864578162, nf90_close( file_id ),                      &
                       "Error closing file" )

  end subroutine close_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Checks a NetCDF status code and fail with a message if an error occurred
  subroutine check_status( code, status, error_message )

    use netcdf,                        only : NF90_NOERR, nf90_strerror

    !> Unique code to associate with any failure
    integer,          intent(in) :: code
    !> NetCDF status code
    integer,          intent(in) :: status
    !> Error message to display on failure
    character(len=*), intent(in) :: error_message

    if( status .eq. NF90_NOERR ) return
    call die_msg( 969476730, "NetCDF error: "//trim( error_message )//": "//  &
                  trim( nf90_strerror( status ) ) )

  end subroutine check_status

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_file_util
