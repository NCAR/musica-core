! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Utility module for YAML parser

!> Utility module for YAML parser
module musica_yaml_util

  use iso_c_binding
  use musica_constants,                only : musica_ik, musica_rk, musica_dk

  implicit none
  public

  !> C wrapper functions for YAML parser
  interface

    !> Constructor from a YAML string
    function yaml_create_from_string_c(yaml_string)                           &
        bind(c, name="yaml_create_from_string")
      use iso_c_binding
      implicit none
      type(c_ptr)                               :: yaml_create_from_string_c
      character(len=1, kind=c_char), intent(in) :: yaml_string(*)
    end function yaml_create_from_string_c

    !> Constructor from a YAML file
    function yaml_create_from_file_c(file_path)                               &
        bind(c, name="yaml_create_from_file")
      use iso_c_binding
      implicit none
      type(c_ptr)                               :: yaml_create_from_file_c
      character(len=1, kind=c_char), intent(in) :: file_path(*)
    end function yaml_create_from_file_c

    !> Output YAML configuration to a file
    subroutine yaml_to_file_c(node, file_path) bind(c, name="yaml_to_file")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: file_path(*)
    end subroutine yaml_to_file_c

    !> Get the number of elements
    function yaml_size_c(node) bind(c, name="yaml_size")
      use iso_c_binding
      implicit none
      integer(kind=c_int) :: yaml_size_c
      type(c_ptr), value  :: node
    end function yaml_size_c

    !> Get a sub-node by key
    function yaml_get_node_c(node, key, found) bind(c, name="yaml_get_node")
      use iso_c_binding
      implicit none
      type(c_ptr) :: yaml_get_node_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_node_c

    !> Get a string by key
    function yaml_get_string_c(node, key, found, size) bind(c, name="yaml_get_string")
      use iso_c_binding
      implicit none
      type(c_ptr) :: yaml_get_string_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
      integer(kind=c_int),  intent(out) :: size
    end function yaml_get_string_c

    !> Node destructor
    subroutine yaml_delete_node_c(node) bind(c, name="yaml_delete_node")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: node
    end subroutine yaml_delete_node_c

    !> String destructor
    subroutine yaml_delete_string_c(string) bind(c, name="yaml_delete_string")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: string
    end subroutine yaml_delete_string_c

  end interface

end module musica_yaml_util