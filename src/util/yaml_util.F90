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

  !> Interoperable string type
  type, bind(c) :: string_t_c
    type(c_ptr) :: ptr_
    integer(c_int) :: size_
  end type string_t_c

  !> Interoperable array type for strings
  type, bind(c) :: string_array_t_c
    type(c_ptr) :: ptr_
    integer(c_int) :: size_
  end type string_array_t_c

  !> Interoperable array type for doubles
  type, bind(c) :: double_array_t_c
    type(c_ptr) :: ptr_
    integer(c_int) :: size_
  end type double_array_t_c

  !> Interoperable array type for nodes
  type, bind(c) :: node_array_t_c
    type(c_ptr) :: ptr_
    integer(c_int) :: size_
  end type node_array_t_c

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

    !> Outputs YAML configuration to a file
    subroutine yaml_to_file_c(node, file_path) bind(c, name="yaml_to_file")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: file_path(*)
    end subroutine yaml_to_file_c

    !> Gets the number of elements
    function yaml_size_c(node) bind(c, name="yaml_size")
      use iso_c_binding
      implicit none
      integer(kind=c_int) :: yaml_size_c
      type(c_ptr), value  :: node
    end function yaml_size_c

    !> Gets an beginning iterator for a node
    function yaml_begin_c(node) bind(c, name="yaml_begin")
      use iso_c_binding
      implicit none
      type(c_ptr) :: yaml_begin_c
      type(c_ptr), value :: node
    end function yaml_begin_c

    !> Gets an ending iterator for a node
    function yaml_end_c(node) bind(c, name="yaml_end")
      use iso_c_binding
      implicit none
      type(c_ptr) :: yaml_end_c
      type(c_ptr), value :: node
    end function yaml_end_c

    !> Increments an iterator
    !!
    !! Returns true if incremented iterator is < end, false otherwise
    function yaml_increment_c(iter, end) bind(c, name="yaml_increment")
      use iso_c_binding
      implicit none
      logical(kind=c_bool) :: yaml_increment_c
      type(c_ptr), value :: iter
      type(c_ptr), value :: end
    end function yaml_increment_c

    !> Gets the key associated with an iterator
    function yaml_key_c(iter) bind(c, name="yaml_key")
      use iso_c_binding
      import :: string_t_c
      implicit none
      type(string_t_c) :: yaml_key_c
      type(c_ptr), value :: iter
    end function yaml_key_c

    !> Gets a sub-node by key
    function yaml_get_node_c(node, key, found) bind(c, name="yaml_get_node")
      use iso_c_binding
      implicit none
      type(c_ptr) :: yaml_get_node_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_node_c

    !> Gets a string by key
    function yaml_get_string_c(node, key, found) bind(c, name="yaml_get_string")
      use iso_c_binding
      import :: string_t_c
      implicit none
      type(string_t_c) :: yaml_get_string_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_string_c

    !> Gets an integer by key
    function yaml_get_int_c(node, key, found) bind(c, name="yaml_get_int")
      use iso_c_binding
      implicit none
      integer(kind=c_int) :: yaml_get_int_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_int_c

    !> Gets a float by key
    function yaml_get_float_c(node, key, found) bind(c, name="yaml_get_float")
      use iso_c_binding
      implicit none
      real(kind=c_float) :: yaml_get_float_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_float_c

    !> Gets a double by key
    function yaml_get_double_c(node, key, found) bind(c, name="yaml_get_double")
      use iso_c_binding
      implicit none
      real(kind=c_double) :: yaml_get_double_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_double_c

    !> Gets a boolean by key
    function yaml_get_bool_c(node, key, found) bind(c, name="yaml_get_bool")
      use iso_c_binding
      implicit none
      logical(kind=c_bool) :: yaml_get_bool_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_bool_c

    !> Gets a string array by key
    function yaml_get_string_array_c(node, key, found)                        &
        bind(c, name="yaml_get_string_array")
      use iso_c_binding
      import :: string_array_t_c
      implicit none
      type(string_array_t_c) :: yaml_get_string_array_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_string_array_c

    !> Gets a double array by key
    function yaml_get_double_array_c(node, key, found)                        &
        bind(c, name="yaml_get_double_array")
      use iso_c_binding
      import :: double_array_t_c
      implicit none
      type(double_array_t_c) :: yaml_get_double_array_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_double_array_c

    !> Gets a node array by key
    function yaml_get_node_array_c(node, key, found)                          &
        bind(c, name="yaml_get_node_array")
      use iso_c_binding
      import :: node_array_t_c
      implicit none
      type(node_array_t_c) :: yaml_get_node_array_c
      type(c_ptr), value :: node
      character(len=1, kind=c_char), intent(in) :: key(*)
      logical(kind=c_bool), intent(out) :: found
    end function yaml_get_node_array_c

    !> Node destructor
    subroutine yaml_delete_node_c(node) bind(c, name="yaml_delete_node")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: node
    end subroutine yaml_delete_node_c

    !> String destructor
    subroutine yaml_delete_string_c(string) bind(c, name="yaml_delete_string")
      use iso_c_binding
      import :: string_t_c
      implicit none
      type(string_t_c), value :: string
    end subroutine yaml_delete_string_c

    !> String array destructor
    subroutine yaml_delete_string_array_c(array)                              &
        bind(c, name="yaml_delete_string_array")
      use iso_c_binding
      import :: string_array_t_c
      implicit none
      type(string_array_t_c), value :: array
    end subroutine yaml_delete_string_array_c

    !> Double array destructor
    subroutine yaml_delete_double_array_c(array)                              &
        bind(c, name="yaml_delete_double_array")
      use iso_c_binding
      import :: double_array_t_c
      implicit none
      type(double_array_t_c), value :: array
    end subroutine yaml_delete_double_array_c

    !> Node array destructor
    subroutine yaml_delete_node_array_c(array)                                &
        bind(c, name="yaml_delete_node_array")
      use iso_c_binding
      import :: node_array_t_c
      implicit none
      type(node_array_t_c), value :: array
    end subroutine yaml_delete_node_array_c

    !> Iterator destructor
    subroutine yaml_delete_iterator_c(iter)                                   &
        bind(c, name="yaml_delete_iterator")
      use iso_c_binding
      implicit none
      type(c_ptr), value :: iter
    end subroutine yaml_delete_iterator_c

  end interface

end module musica_yaml_util