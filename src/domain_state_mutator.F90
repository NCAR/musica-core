! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_domain_state_mutator module

!> The abstract domain_state_mutator_t type and related functions
module musica_domain_state_mutator

  use musica_property,                 only : property_ptr

  implicit none
  private

  public :: domain_state_mutator_t, domain_state_mutator_ptr

  !> Abstract domain state mutator
  type, abstract :: domain_state_mutator_t
    !> Property modified by the mutator
    type(property_ptr) :: property_
  contains
    !> Returns the modifiable property
    procedure :: property
    !> Attaches a property to the mutator (should only be called by domain_t)
    procedure :: attach_property
  end type domain_state_mutator_t

  !> Unique pointer to domain_state_mutator_t objects
  type domain_state_mutator_ptr
    class(domain_state_mutator_t), pointer :: val_ => null( )
  contains
    final :: finalize, finalize_1D
  end type domain_state_mutator_ptr

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the modifiable property
  function property( this )

    use musica_assert,                 only : assert
    use musica_property,               only : property_t
    use musica_string,                 only : string_t

    !> Modifiable property
    class(property_t), pointer :: property
    !> Domain state mutator
    class(domain_state_mutator_t), intent(in) :: this

    type(string_t) :: defined_by

    call assert( 120361651, associated( this%property_%val_ ) )
    defined_by = this%property_%val_%defined_by( )
    property => property_t( this%property_%val_, defined_by%to_char( ) )

  end function property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Attaches a property to the mutator (should only be called by domain_t)
  subroutine attach_property( this, property )

    use musica_assert,                 only : assert
    use musica_property,               only : property_t
    use musica_string,                 only : string_t

    !> Mutator
    class(domain_state_mutator_t), intent(inout) :: this
    !> Property to attach to the mutator
    class(property_t), intent(in) :: property

    type(string_t) :: defined_by

    defined_by = property%defined_by( )
    call assert( 826693335, .not. associated( this%property_%val_ ) )
    this%property_%val_ => property_t( property, defined_by%to_char( ) )

  end subroutine attach_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes a unique mutator pointer
  subroutine finalize( this )

    !> Domain pointer
    type(domain_state_mutator_ptr), intent(inout) :: this

    if( associated( this%val_ ) ) then
      deallocate( this%val_ )
      this%val_ => null( )
    end if

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finalizes an array of unique mutator pointers
  subroutine finalize_1D( this )

    !> Domain pointer
    type(domain_state_mutator_ptr), intent(inout) :: this(:)

    integer :: i_mut

    do i_mut = 1, size( this )
      if( associated( this( i_mut )%val_ ) ) then
        deallocate( this( i_mut )%val_ )
        this( i_mut )%val_ => null( )
      end if
    end do

  end subroutine finalize_1D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_domain_state_mutator
