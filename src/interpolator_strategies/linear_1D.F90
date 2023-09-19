! Copyright (C) 2020 National Center for Atmospheric Research,
! and National Technology & Engineering Solutions of Sandia, LLC (NTESS)

! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_interpolator_linear module

!> Linear strategy for 1D interpolators
module musica_interpolator_linear_1D

  implicit none
  private

  public :: strategy

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> This helper performs a binary search on sorted grid data, computing the
  !! index of the first point within the grid data that is no larger than the
  !! desired value.
  function lower_bound( array, val ) result( lb )

    use musica_constants,              only : dk => musica_dk

    !> Grid data
    real(kind=dk), intent(in) :: array(:)
    !> Cutoff value
    real(kind=dk), intent(in) :: val

    integer :: lb, low, mid, high

    ! Till our paths cross...
    low = 1
    high = size( array ) + 1
    do while( low < high )
      mid = low + ( high - low ) / 2
      if( val <= array( mid ) ) then
        high = mid
      else
        low = mid + 1
      end if
    end do

    if( low < size( array ) + 1 ) then
      if( array( low ) < val ) low = low + 1
    end if
    lb = low

  end function lower_bound

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Perform a linear 1D interpolation
  !!
  !! Note: not optimized. Use during initialization or consider optimizing.
  !!
  !! An example of how to use the interpolator object, which uses the linear
  !! 1D interpolation strategy follows:
  !!
  !! \snippet test/interpolator_strategies/linear_1D.F90 Linear interpolator example
  !!
  !! Output:
  !! \code{bash}
  !!   10.000000000000000        15.000000000000000        20.000000000000000        25.000000000000000        30.000000000000000        30.000000000000000
  !! \endcode
  function strategy( from_grid, to_grid ) result( map )

    use musica_constants,              only : dk => musica_dk
    use musica_grid,                   only : grid_t, grid_iterator_t
    use musica_interpolator,           only : interpolator_element_t

    class(interpolator_element_t), allocatable :: map(:)
    class(grid_t), intent(in) :: from_grid
    class(grid_t), intent(in) :: to_grid

    class(grid_iterator_t), pointer :: iter_from, iter_to
    class(grid_iterator_t), pointer :: iter_from_last
    type(interpolator_element_t), allocatable :: local_map(:)
    type(interpolator_element_t) :: element
    real(kind=dk), allocatable :: grid_data(:)
    integer :: i_data, lb

    allocate( local_map(0) )
    allocate( grid_data( from_grid%number_of_sections( ) ) )
    iter_from => from_grid%iterator( )
    i_data = 1
    do while( iter_from%next( ) )
      grid_data( i_data ) = from_grid%mid_point( iter_from )
      i_data = i_data + 1
    end do
    iter_to        => to_grid%iterator( )
    iter_from_last => from_grid%iterator( )
    do while( iter_to%next( ) )
      lb = lower_bound( grid_data, to_grid%mid_point( iter_to ) )
      if( lb .eq. 1 ) then
        call iter_from%set( 1 )
        element = interpolator_element_t( iter_from, iter_to, 1.0_dk )
        local_map = [ local_map, element ]
      else if( lb .ge. from_grid%number_of_sections( ) + 1 ) then
        call iter_from%set( from_grid%number_of_sections( ) )
        element = interpolator_element_t( iter_from, iter_to, 1.0_dk )
        local_map = [ local_map, element ]
      else
        call iter_from%set( lb )
        call iter_from_last%set( lb - 1 )
        element = interpolator_element_t( iter_from_last, iter_to,            &
          1.0_dk - ( to_grid%mid_point( iter_to ) -                           &
                     from_grid%mid_point( iter_from_last ) ) /                &
                   ( from_grid%mid_point( iter_from ) -                       &
                     from_grid%mid_point( iter_from_last ) ) )
        local_map = [ local_map, element ]
        element = interpolator_element_t( iter_from, iter_to,                 &
          1.0_dk - ( from_grid%mid_point( iter_from ) -                       &
                     to_grid%mid_point( iter_to ) ) /                         &
                   ( from_grid%mid_point( iter_from ) -                       &
                     from_grid%mid_point( iter_from_last ) ) )
        local_map = [ local_map, element ]
      end if
    end do
    allocate( map, source = local_map )
    deallocate( iter_from      )
    deallocate( iter_to        )
    deallocate( iter_from_last )

  end function strategy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_interpolator_linear_1D
