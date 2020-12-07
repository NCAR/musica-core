! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_component module

!> The abstract component_t type and related functions
module musica_component

  implicit none
  private

  public :: component_t, component_ptr

  !> Model component
  !!
  !! Model components calculate diagnostics for a given model state and/or
  !! advance the model state for a given timestep.
  !!
  !! \todo add full description and example usage for component_t
  !!
  type, abstract :: component_t
  contains
    !> Advance the model state for a given timestep
    procedure(advance_state), deferred :: advance_state
    !> Save the component configuration for future simulations
    procedure(preprocess_input), deferred :: preprocess_input
  end type component_t

  !> Pointer for component_t objects
  type :: component_ptr
    class(component_t), pointer :: val_
  end type component_ptr

interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Advance the model state for a given timestep
  subroutine advance_state( this, domain_state, domain_element,               &
      current_time__s, time_step__s )
    use musica_constants,              only : musica_dk
    use musica_domain_iterator,        only : domain_iterator_t
    use musica_domain_state,           only : domain_state_t
    import component_t
    !> Model component
    class(component_t), intent(inout) :: this
    !> Domain state
    class(domain_state_t), intent(inout) :: domain_state
    !> Domain element to advance state for
    class(domain_iterator_t), intent(in) :: domain_element
    !> Current simulation time [s]
    real(kind=musica_dk), intent(in) :: current_time__s
    !> Time step to advance state by [s]
    real(kind=musica_dk), intent(in) :: time_step__s
  end subroutine advance_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Save the component configuration for future simulations
  subroutine preprocess_input( this, config, output_path )
    use musica_config,                 only : config_t
    import component_t
    !> Model component
    class(component_t), intent(inout) :: this
    !> Model component configuration
    type(config_t), intent(out) :: config
    !> Folder to save input data to
    character(len=*), intent(in) :: output_path
  end subroutine preprocess_input

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end interface

end module musica_component
