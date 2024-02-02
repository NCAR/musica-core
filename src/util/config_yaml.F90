! Copyright (C) 2020 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The musica_config module

!> The config_t type and related functions
module musica_config

  use iso_c_binding
  use musica_constants,                only : musica_ik, musica_rk, musica_dk
  use musica_iterator,                 only : iterator_t

  implicit none
  private

  public :: config_t

  !> Model configuration data
  !!
  !! Instances of type \c config_t can be used to access model configuration
  !! data in \c json format. If there is a need to use model configuration
  !! in another format (e.g., XML) in the future, an abstract \c config_t
  !! type could be set up, that this type and an XML-based type could extend.
  !! The rest of the model code would be unaffected.
  !!
  !! It is assumed that most configuration datasets will be small enough that
  !! returned subsets of configuration data can just be a copy of the original
  !! data (instead of using a pointer to the start of the subset in the original
  !! dataset, or something like this). This avoids ownership problems with
  !! cleaning up the memory after a \c config_t object goes out of scope.
  !!
  !! Only use \c config_t objects during initialization. They are not designed
  !! for efficiency.
  !!
  !! **IMPORTANT:** The order of elements is arbitrary. No user of a \c config_t
  !! object can assume anything by the order of key-value pairs in the data.
  !! This dataset:
  !! \code{json}
  !!   {
  !!     "foo" : 1,
  !!     "bar" : 2,
  !!     "foobar" : 3
  !!   }
  !! \endcode
  !! ... is the same as:
  !! \code{json}
  !!   {
  !!     "bar" : 2,
  !!     "foobar" : 3,
  !!     "foo" : 1
  !!   }
  !! \endcode
  !!
  !! There is no guarantee that an iterator over the elements of a config_t
  !! object will return them in the same order they exist in the original
  !! file or string.
  !!
  !! Example of a config_t object generated from a file:
  !! \code{f90}
  !!   use musica_config,                   only : config_t
  !!   use musica_constants,                only : musica_dk, musica_ik
  !!   use musica_iterator,                 only : iterator_t
  !!   use musica_string,                   only : string_t
  !!
  !!   character(len=*), parameter :: my_name = "config file example"
  !!   type(config_t) :: main_config, sub_config, sub_real_config
  !!   real(musica_dk) :: my_real
  !!   integer(musica_ik) :: my_int
  !!   type(string_t) :: my_string
  !!   class(iterator_t), pointer :: iter
  !!   logical :: found
  !!
  !!   call main_config%from_file( 'data/config_example.json' )
  !!
  !!   ! this would fail with an error if 'a string' is not found
  !!   call main_config%get( "a string", my_string, my_name )
  !!   write(*,*) "a string value: ", my_string
  !!
  !!   ! add the found argument to avoid failure if the pair is not found
  !!   call main_config%get( "my int", my_int, my_name, found = found )
  !!   if( found ) then
  !!     write(*,*) "my int value: ", my_int
  !!   else
  !!     write(*,*) "'my int' was not found"
  !!   end if
  !!
  !!   ! when you get a subset of the properties, a new config_t object is
  !!   ! created containing the subset data. The two config_t objects are
  !!   ! independent of one another after this point.
  !!   call main_config%get( "other props", sub_config, my_name )
  !!   call sub_config%get( "an int", my_int, my_name )
  !!   write(*,*) "other props->an int value: ", my_int
  !!
  !!   ! property values need a standard unit to convert to.
  !!   ! time units must be passed the standard unit 's'
  !!   ! (non-standard units may be used in the config file, but you cannot
  !!   !  request non-standard units in the model.)
  !!   call sub_config%get( "some time", "s", my_real, my_name )
  !!   write(*,*) "other props->some time value: ", my_real, " s"
  !!
  !!   ! units are case-insensitive
  !!   call sub_config%get( "a pressure", "pa", my_real, my_name )
  !!   write(*,*) "other props->a pressure value: ", my_real, " Pa"
  !!
  !!   ! you can iterate over a set of key-value pairs. but remember that
  !!   ! the order is always arbitrary. you also must provide the right type
  !!   ! of variable for the values.
  !!   call main_config%get( "real props", sub_real_config, my_name )
  !!   iter => sub_real_config%get_iterator( )
  !!   do while( iter%next( ) )
  !!     my_string = sub_real_config%key( iter )
  !!     call sub_real_config%get( iter, my_real, my_name )
  !!     write(*,*) my_string, " value: ", my_real
  !!   end do
  !!
  !!   ! you can also get the number of child objects before iterating over
  !!   ! them, if you want to allocate an array or something first
  !!   write(*,*) "number of children: ", sub_real_config%number_of_children( )
  !!
  !!   ! you can add key-value pairs with the add function
  !!   call main_config%add( "my new int", 43, my_name )
  !!   call main_config%get( "my new int", my_int, my_name )
  !!   write(*,*) "my new int value: ", my_int
  !!
  !!   ! clean up memory
  !!   deallocate( iter )
  !! \endcode
  !!
  !! `data/config_example.json`:
  !! \code{json}
  !!   {
  !!     "my int" : 12,
  !!     "other props" : {
  !!       "some time [min]" : 12,
  !!       "a pressure [bar]" : 103.4,
  !!       "an int" : 45
  !!     },
  !!     "real props" : {
  !!       "foo" : 14.2,
  !!       "bar" : 64.2,
  !!       "foobar" : 920.4
  !!     },
  !!     "a string" : "foo"
  !!   }
  !! \endcode
  !!
  !! Output:
  !! \code{bash}
  !!  a string value:   foo
  !!  my int value:           12
  !!  other props->an int value:           45
  !!  other props->some time value:    720.00000000000000       s
  !!  other props->a pressure value:    10340000.000000000       Pa
  !!   foo  value:    14.199999999999999
  !!   bar  value:    64.200000000000003
  !!   foobar  value:    920.39999999999998
  !!  number of children:            3
  !!  my new int value:           43
  !! \endcode
  !!
  type :: config_t
    private
    !> Pointer to YAML node
    type(c_ptr) :: node_ = c_null_ptr
  contains
    !> Empties the configuration
    procedure :: empty
    !> Loads a configuration with data from a file
    procedure :: from_file => construct_from_file
    !> Writes a configuration to a file
    procedure :: to_file
    !> Returns the number of child objects
    procedure :: number_of_children
    !> Gets an iterator for the configuration data
    procedure :: get_iterator
    !> Gets the key name for a key-value pair
    procedure :: key
    !> @name Gets some configuration data
    !!
    !! Each function includes optional \c found and \c default arguments. If
    !! neither is included and the data are not found, execution is stopped
    !! with an error message.
    !!
    !! If a \c default value is included and the data are not found, the
    !! returned argument is set to this default value, otherwise it is set to
    !! a standard default value.
    !!
    !! If the \c found argument is included and the data are found, \c found
    !! is set to \c true, otherwise it is set to \c false.
    !! @{
    procedure, private :: get_config
    procedure, private :: get_string_string_default
    procedure, private :: get_string
    procedure, private :: get_property
    procedure, private :: get_int
    procedure, private :: get_float
    procedure, private :: get_double
    procedure, private :: get_logical
    procedure, private :: get_string_array
    procedure, private :: get_double_array
    procedure, private :: get_config_array
    procedure, private :: get_from_iterator
    procedure, private :: get_property_from_iterator
    procedure, private :: get_array_from_iterator
    generic :: get => get_config, get_string, get_string_string_default,      &
                      get_property, get_int, get_float, get_double,           &
                      get_logical, get_string_array, get_double_array,        &
                      get_config_array, get_from_iterator,                    &
                      get_property_from_iterator, get_array_from_iterator
    !> @}
    !> @name Adds a named piece of configuration data
    !! @{
    procedure, private :: add_config
    procedure, private :: add_char_array
    procedure, private :: add_string
    procedure, private :: add_property
    procedure, private :: add_int
    procedure, private :: add_float
    procedure, private :: add_double
    procedure, private :: add_logical
    procedure, private :: add_string_array
    procedure, private :: add_double_array
    procedure, private :: add_config_array
    generic :: add => add_config, add_char_array, add_string, add_property,  &
                      add_int, add_float, add_double, add_logical,           &
                      add_string_array, add_double_array, add_config_array
    !> @}
    !> @name Assignment
    !! @{
    procedure, private :: config_assign_config
    procedure, private :: config_assign_string
    procedure, private :: config_assign_char
    procedure, private, pass(config) :: string_assign_config
    generic :: assignment(=) => config_assign_config, config_assign_string,   &
                                config_assign_char, string_assign_config
    !> @}
    !> Merges another config_t object into the config_t object
    procedure :: merge_in
    !> Validates the format of the configuration file
    procedure :: validate
    !> Print the raw contents of the configuration
    procedure :: print => do_print
    !> Returns the number of bytes required to pack the object onto a buffer
    procedure :: pack_size
    !> Packs the object onto a character buffer
    procedure :: mpi_pack
    !> Unpacks an object from a character buffer
    procedure :: mpi_unpack
    !> Cleans up memory
    final :: finalize, finalize_1D_array
    !> Find a JSON key by prefix
    procedure, private :: find_by_prefix
  end type config_t

  !> Configuration data iterator
  type, extends(iterator_t) :: config_iterator_t
    !> Pointer to the configuration data
    class(config_t), pointer :: config_ => null( )
    !> Current index in the data set
    integer(kind=musica_ik) :: id_ = 0
  contains
    !> Advances to the next key-value pair
    procedure :: next => iterator_next
    !> Resets the iterator
    procedure :: reset => iterator_reset
  end type config_iterator_t

  !> C wrapper functions for YAML parser
  interface
    function yaml_create_from_string_c(yaml_string)                           &
        bind(c, name="yaml_create_from_string")
      use iso_c_binding
      implicit none
      type(c_ptr)                               :: yaml_create_from_string_c
      character(len=1, kind=c_char), intent(in) :: yaml_string(*)
    end function

    function yaml_create_from_file_c(file_path)                               &
        bind(c, name="yaml_create_from_file")
      use iso_c_binding
      implicit none
      type(c_ptr)                               :: yaml_create_from_file_c
      character(len=1, kind=c_char), intent(in) :: file_path(*)
    end function

    !> Destructor
    subroutine yaml_delete_c(node) bind(c, name="yaml_delete")
       use iso_c_binding
       implicit none
       type(c_ptr), value :: node
    end subroutine
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Empties the configuration
  subroutine empty( this )

    !> Configuration
    class(config_t), intent(out) :: this

    call initialize_config_t( this )

  end subroutine empty

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs a configuration from a file
  subroutine construct_from_file( this, file_name )

    use musica_assert,                 only : die

    !> New configuration
    class(config_t), intent(out) :: this
    !> File name containing configuration data
    character(len=*), intent(in) :: file_name

    character(len=1, kind=c_char), allocatable :: c_file_name(:)

    c_file_name = to_c_string( file_name )
    select type( this )
    type is( config_t )
      call finalize( this )
    class default
      call die( 316253716 )
    end select
    this%node_ = yaml_create_from_file_c( c_file_name )

  end subroutine construct_from_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Writes a configuration to a file
  subroutine to_file( this, file_name )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> File name to save configuration with
    character(len=*), intent(in) :: file_name
#if 0
    call this%core_%print( this%value_, file_name )
#endif
  end subroutine to_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of child objects
  function number_of_children( this )

    use musica_assert,                 only : assert

    !> Number of child objects
    integer(kind=musica_ik) :: number_of_children
    !> Configuration
    class(config_t), intent(inout) :: this
#if 0
    integer(kind=json_ik) :: n_children

    call assert( 344123447, associated( this%value_ ) )
    call this%core_%info( this%value_, n_children = n_children )
    number_of_children = int( n_children, kind=musica_ik )
#endif
    number_of_children = 0
  end function number_of_children

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an interator for the configuration data
  function get_iterator( this )

    use musica_assert,                 only : assert

    !> Pointer to the iterator
    class(iterator_t), pointer :: get_iterator
    !> Configuration
    class(config_t), intent(in), target :: this
#if 0
    call assert( 753334332, associated( this%value_ ) )
    allocate( config_iterator_t :: get_iterator )
    select type( iter => get_iterator )
      type is( config_iterator_t )
        iter%config_ => this
    end select
#endif
    get_iterator => null( )
  end function get_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the key name using an iterator
  function key( this, iterator )

    use musica_assert,                 only : assert, die_msg
    use musica_string,                 only : string_t

    !> Key name
    type(string_t) :: key
    !> Configuration
    class(config_t), intent(inout) :: this
    !> Configuration iterator
    class(iterator_t), intent(in) :: iterator
#if 0
    type(json_value), pointer :: j_obj
    character(kind=json_ck, len=:), allocatable :: j_key

    call assert( 524223947, associated( this%value_ ) )
    select type( iterator )
      class is( config_iterator_t )
        call this%core_%get_child( this%value_,                               &
                                   int( iterator%id_, kind=json_ik ), j_obj )
        call this%core_%info( j_obj, name = j_key )
        key = j_key
      class default
        call die_msg( 789668190, "Iterator type mismatch. Expected "//        &
                      "config_iterator_t" )
    end select
#endif
    key = ""
  end function key

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a subset of the configuration data
  subroutine get_config( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    class(config_t), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    class(config_t), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    type(json_value), pointer :: j_obj
    logical(kind=json_lk) :: l_found
    character(kind=json_ck, len=:), allocatable :: str_tmp
    type(string_t) :: default_value

    call assert( 290964177, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get_child( this%value_, key, j_obj, l_found )

    call assert_msg( 202757635, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( l_found ) then
      call this%core_%print_to_string( j_obj, str_tmp )
      call this%core_%parse( value%value_, str_tmp )
    else
      if( present( default ) ) then
        value = default_value
      else
        value = ""
      end if
    end if
#endif
  end subroutine get_config

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a string from the configuration data
  subroutine get_string_string_default( this, key, value, caller, default,    &
      found )

    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    class(string_t), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    class(string_t), intent(in) :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found

    call get_string( this, key, value, caller, default = default%val_,        &
                     found = found )

  end subroutine get_string_string_default

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a string from the configuration data
  subroutine get_string( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    class(string_t), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    character(len=*), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    logical(kind=json_lk) :: l_found
    type(string_t) :: default_value

    call assert( 381676645, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, value%val_, l_found )

    call assert_msg( 506864358, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( .not. l_found ) then
      if( present( default ) ) then
        value = default_value
      else
        value = ""
      end if
    end if
#endif
  end subroutine get_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a property from the configuration data
  subroutine get_property( this, key, units, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg
    use musica_convert,                only : convert_t
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Units for the property
    character(len=*), intent(in) :: units
    !> Returned value
    real(kind=musica_dk), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    real(kind=musica_dk), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    type(string_t) :: full_key
    real(kind=musica_dk) :: tmp_val, default_value
    type(json_value), pointer :: j_obj
    logical(kind=json_lk) :: l_found
    type(convert_t) :: convert

    call assert( 830950025, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call find_by_prefix( this, key, this%value_, j_obj, full_key, l_found )

    if( l_found ) then
      call this%core_%get( j_obj, tmp_val )
    end if

    call assert_msg( 501600051, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( l_found ) then
      convert = convert_t( units, get_property_units( full_key%val_ ) )
      value = convert%to_standard( tmp_val )
    else
      if( present( default ) ) then
        value = default_value
      else
        value = 0.0d0
      end if
    end if
#endif
  end subroutine get_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an integer from the configuration data
  subroutine get_int( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    integer(kind=musica_ik), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    integer(kind=musica_ik), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    integer(kind=musica_ik) :: default_value
    logical(kind=json_lk) :: l_found

    call assert( 545116003, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, value, l_found )

    call assert_msg( 168054983, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( .not. l_found ) then
      if( present( default ) ) then
        value = default_value
      else
        value = 0
      end if
    end if
#endif
  end subroutine get_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a single-precision real number from the configuration data
  subroutine get_float( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    real(kind=musica_rk), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    real(kind=musica_rk), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    real(kind=json_rk) :: tmp_value, default_value
    logical(kind=json_lk) :: l_found

    call assert( 482013137, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, tmp_value, l_found )

    call assert_msg( 497840177, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    value = tmp_value
    if( present( found ) ) found = l_found

    if( .not. l_found ) then
      if( present( default ) ) then
        value = default_value
      else
        value = 0.0
      end if
    end if
#endif
  end subroutine get_float

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a double-precision real number from the configuration data
  subroutine get_double( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    real(kind=musica_dk), intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    real(kind=musica_dk), intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    real(kind=musica_dk) :: default_value
    logical(kind=json_lk) :: l_found

    call assert( 483918671, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, value, l_found )

    call assert_msg( 273655782, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( .not. l_found ) then
      if( present( default ) ) then
        value = default_value
      else
        value = 0.0d0
      end if
    end if
#endif
  end subroutine get_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a boolean value from the configuration data
  subroutine get_logical( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    logical, intent(out) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    logical, intent(in), optional :: default
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    logical(kind=json_lk) :: l_found, default_value

    call assert( 368241553, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, value, l_found )

    call assert_msg( 714306082, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( .not. l_found ) then
      if( present( default ) ) then
        value = default_value
      else
        value = .false.
      end if
    end if
#endif
  end subroutine get_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an array of strings from the configuration data
  subroutine get_string_array( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    type(string_t), allocatable, intent(out) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    type(string_t), intent(in), optional :: default(:)
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    type(json_value), pointer :: j_obj, child, next
    integer(kind=json_ik) :: n_child, i_string
    logical(kind=json_lk) :: l_found
    type(string_t), allocatable :: default_value(:)

    call assert( 941388433, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, j_obj, l_found )
    if( l_found ) then
      call this%core_%info( j_obj, n_children = n_child )
      allocate( value( n_child ) )
    end if

    call assert_msg( 640725796, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( l_found ) then
      child => null( )
      next  => null( )
      i_string = 1
      call this%core_%get_child( j_obj, child )
      do while( associated( child ) )
        call this%core_%get( child, value( i_string )%val_ )
        call this%core_%get_next( child, next )
        child => next
        i_string = i_string + 1
      end do
    else
      if( present( default ) ) then
        value = default_value
      end if
    end if
#endif
  end subroutine get_string_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an array of doubles from the configuration data
  subroutine get_double_array( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    real(musica_dk), allocatable, intent(out) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    real(musica_dk), intent(in), optional :: default(:)
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    type(json_value), pointer :: j_obj, child, next
    integer(kind=json_ik) :: n_child, i_string
    logical(kind=json_lk) :: l_found
    real(musica_dk), allocatable :: default_value(:)

    call assert( 941388433, associated( this%value_ ) )
    if( present( default ) ) then
      default_value = default
    endif
    call this%core_%get( this%value_, key, j_obj, l_found )
    if( l_found ) then
      call this%core_%info( j_obj, n_children = n_child )
      allocate( value( n_child ) )
    end if

    call assert_msg( 640725796, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( l_found ) then
      child => null( )
      next  => null( )
      i_string = 1
      call this%core_%get_child( j_obj, child )
      do while( associated( child ) )
        call this%core_%get( child, value( i_string ) )
        call this%core_%get_next( child, next )
        child => next
        i_string = i_string + 1
      end do
    else
      if( present( default ) ) then
        value = default_value
      end if
    end if
#endif
  end subroutine get_double_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an array of config_t objects
  subroutine get_config_array( this, key, value, caller, default, found )

    use musica_assert,                 only : assert, assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key used to find value
    character(len=*), intent(in) :: key
    !> Returned value
    type(config_t), allocatable, intent(out) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
    !> Default value if not found
    type(config_t), intent(in), optional :: default(:)
    !> Flag indicating whether key was found
    logical, intent(out), optional :: found
#if 0
    type(json_value), pointer :: j_obj, child, next
    character(kind=json_ck, len=:), allocatable :: str_tmp
    integer(kind=json_ik) :: n_child, i_config
    logical(kind=json_lk) :: l_found
    type(config_t), allocatable :: default_value(:)

    call assert( 970756834, associated( this%value_ ) )
    if( present( default ) ) default_value = default
    call this%core_%get( this%value_, key, j_obj, l_found )
    if( l_found ) then
      call this%core_%info( j_obj, n_children = n_child )
      allocate( value( n_child ) )
    end if

    call assert_msg( 737497064, l_found .or. present( default )               &
                     .or. present( found ), "Key '"//trim( key )//            &
                     "' requested by "//trim( caller )//" not found" )

    if( present( found ) ) found = l_found

    if( l_found ) then
      child => null( )
      next  => null( )
      i_config = 1
      call this%core_%get_child( j_obj, child )
      do while( associated( child ) )
        call this%core_%print_to_string( child, str_tmp )
        call this%core_%parse( value( i_config )%value_, str_tmp )
        call this%core_%get_next( child, next )
        child => next
        i_config = i_config + 1
      end do
    else
      if( present( default ) ) then
        value = default_value
      end if
    end if
#endif
  end subroutine get_config_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a value using an iterator
  !!
  !! \todo the get functions should be changed so that the search by name
  !!       functions call search by index functions
  subroutine get_from_iterator( this, iterator, value, caller )

    use musica_assert,                 only : assert, die_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Iterator to use to find value
    class(iterator_t), intent(in) :: iterator
    !> Returned value
    class(*), intent(inout) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(json_value), pointer :: j_obj
    character(kind=json_ck, len=:), allocatable :: key, str_tmp

    call assert( 878285567, associated( this%value_ ) )
    select type( iterator )
      class is( config_iterator_t )
        call this%core_%get_child( this%value_, iterator%id_, j_obj )
        call this%core_%info( j_obj, name = key )
        select type( value )
          type is( config_t )
            call this%core_%print_to_string( j_obj, str_tmp )
            call finalize( value )
            call this%core_%parse( value%value_, str_tmp )
          type is( integer( musica_ik ) )
            call this%get_int( key, value, caller )
          type is( real( musica_rk ) )
            call this%get_float( key, value, caller )
          type is( real( musica_dk ) )
            call this%get_double( key, value, caller )
          type is( logical )
            call this%get_logical( key, value, caller )
          type is( string_t )
            call this%get_string( key, value, caller )
          class default
            call die_msg( 898465007, "Unknown type for get function." )
        end select
      class default
        call die_msg( 888551443, "Iterator type mismatch. Expected "//        &
                      "config_iterator_t" )
    end select
#endif
  end subroutine get_from_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a property value using an iterator
  !!
  !! \todo the get functions should be changed so that the search by name
  !!       functions call search by index functions
  subroutine get_property_from_iterator( this, iterator, units, ret_val,      &
      caller )

    use musica_assert,                 only : assert, die_msg
    use musica_convert,                only : convert_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Iterator to use to find value
    class(iterator_t), intent(in) :: iterator
    !> Standard units for the property
    character(len=*), intent(in) :: units
    !> Returned value
    real(kind=musica_dk), intent(out) :: ret_val
    !> Name of the calling function (only used for error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(json_value), pointer :: j_obj
    character(kind=json_ck, len=:), allocatable :: key
    real(json_rk) :: tmp_val
    type(convert_t) :: convert

    call assert( 197657951, associated( this%value_ ) )
    select type( iterator )
      class is( config_iterator_t )
        call this%core_%get_child( this%value_, iterator%id_, j_obj )
        call this%core_%info( j_obj, name = key )
        call this%core_%get( j_obj, tmp_val )
        convert = convert_t( units, get_property_units( key ) )
        ret_val = convert%to_standard( tmp_val )
      class default
        call die_msg( 946966665, "Iterator type mismatch. Expected "//        &
                      "config_iterator_t" )
    end select
#endif
  end subroutine get_property_from_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets an array value using an iterator
  !!
  !! \todo the get functions should be changed so that the search by name
  !!       functions call search by index functions
  subroutine get_array_from_iterator( this, iterator, value, caller )

    use musica_assert,                 only : assert, die_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Iterator to use to find value
    class(iterator_t), intent(in) :: iterator
    !> Returned value
    type(string_t), allocatable, intent(out) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(json_value), pointer :: j_obj
    character(kind=json_ck, len=:), allocatable :: key

    call assert( 429464482, associated( this%value_ ) )
    select type( iterator )
      class is( config_iterator_t )
        call this%core_%get_child( this%value_, iterator%id_, j_obj )
        call this%core_%info( j_obj, name = key )
        call this%get_string_array( key, value, caller )
      class default
        call die_msg( 858322486, "Iterator type mismatch. Expected "//        &
                      "config_iterator_t" )
    end select
#endif
  end subroutine get_array_from_iterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a subset of configuration data
  subroutine add_config( this, key, value, caller )

    use musica_assert,                 only : assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    type(config_t), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    character(kind=json_ck, len=:), allocatable :: json_string
    type(json_value), pointer :: a

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call assert_msg( 114917526, associated( value%value_ ),                   &
                     "Trying to add uninitialized config_t object by "//      &
                     caller )
    call this%core_%print_to_string( value%value_, json_string )
    call this%core_%parse( a, json_string )
    call this%core_%rename( a, key )
    call this%core_%add( this%value_, a )
#endif
  end subroutine add_config

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a string to the configuration data
  subroutine add_char_array( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    character(len=*), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%add( this%value_, key, value )
#endif
  end subroutine add_char_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a string to the configuration data
  subroutine add_string( this, key, value, caller )

    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    type(string_t), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%add( this%value_, key, value%val_ )
#endif
  end subroutine add_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a property to the configuration data
  subroutine add_property( this, key, units, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key to insert
    character(len=*), intent(in) :: key
    !> Units for value
    character(len=*), intent(in) :: units
    !> Value to set
    real(kind=musica_dk), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    character(kind=json_ck, len=:), allocatable :: full_key

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    full_key = get_full_key( key, units )
    call this%core_%add( this%value_, full_key, value )
#endif
  end subroutine add_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds an integer to the configuration data
  subroutine add_int( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    integer, intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%add( this%value_, key, value )
#endif
  end subroutine add_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a single-precision real number to the configuration data
  subroutine add_float( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    real(kind=musica_rk), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    real(kind=json_rk) :: tmp_value

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    tmp_value = value
    call this%core_%add( this%value_, key, tmp_value )
#endif
  end subroutine add_float

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a double-precision real number to the configuration data
  subroutine add_double( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    real(kind=musica_dk), intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%add( this%value_, key, value )
#endif
  end subroutine add_double

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a boolean to the configuration data
  subroutine add_logical( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    logical, intent(in) :: value
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%add( this%value_, key, value )
#endif
  end subroutine add_logical

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a string array to the configuration data
  subroutine add_string_array( this, key, value, caller )

    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    type(string_t), intent(in) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(json_value), pointer :: array
    integer :: i_str

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%create_array( array, key )
    do i_str = 1, size( value )
      call this%core_%add( array, "", value( i_str )%val_ )
    end do
    call this%core_%add( this%value_, array )
#endif
  end subroutine add_string_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a double array to the configuration data
  subroutine add_double_array( this, key, value, caller )

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key to insert
    character(len=*), intent(in) :: key
    !> Value to set
    real(kind=musica_dk), intent(in) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(json_value), pointer :: array
    integer :: i_val

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%create_array( array, key )
    do i_val = 1, size( value )
      call this%core_%add( array, "", value( i_val ) )
    end do
    call this%core_%add( this%value_, array )
#endif
  end subroutine add_double_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds a config_t array to the configuration data
  subroutine add_config_array( this, key, value, caller )

    use musica_assert,                 only : assert_msg

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Key in insert
    character(len=*), intent(in) :: key
    !> Value to set
    type(config_t), intent(in) :: value(:)
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    character(kind=json_ck, len=:), allocatable :: json_string
    type(json_value), pointer :: array, obj
    integer :: i_config

    if( .not. associated( this%value_ ) ) call initialize_config_t( this )
    call this%core_%create_array( array, key )
    do i_config = 1, size( value )
      call assert_msg( 238294384, associated( value( i_config )%value_ ),     &
                       "Trying to add uninitialized config_t object by "//    &
                       caller )
      call this%core_%print_to_string( value( i_config )%value_, json_string )
      call this%core_%parse( obj, json_string )
      call this%core_%add( array, obj )
    end do
    call this%core_%add( this%value_, array )
#endif
  end subroutine add_config_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assigns a config_t from a config_t
  subroutine config_assign_config( a, b )

    use musica_assert,                 only : assert

    !> Configuration to assign to
    class(config_t), intent(out) :: a
    !> Configuration to assign from
    class(config_t), intent(in) :: b
#if 0
    character(kind=json_ck, len=:), allocatable :: json_string

    call assert( 756693105, associated( b%value_ ) )
    call a%core_%print_to_string( b%value_, json_string )
    call a%core_%initialize( )
    call a%core_%parse( a%value_, json_string )
#endif
  end subroutine config_assign_config

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assigns a config_t from a string
  subroutine config_assign_string( config, string )

    use musica_string,                 only : string_t

    !> Configuration to assign to
    class(config_t), intent(out) :: config
    !> String to assign from
    class(string_t), intent(in) :: string

    call initialize_config_t( config, string = string%val_ )

  end subroutine config_assign_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assigns a config_t from a character array
  subroutine config_assign_char( config, string )

    !> Configuration to assign to
    class(config_t), intent(out) :: config
    !> String to assign from
    character(len=*), intent(in) :: string

    call initialize_config_t( config, string = string )

  end subroutine config_assign_char

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Assigns a string from a configuration
  subroutine string_assign_config( string, config )

    use musica_assert,                 only : assert
    use musica_string,                 only : string_t

    !> String to assign to
    type(string_t), intent(out) :: string
    !> Configuration to assign from
    class(config_t), intent(in) :: config
#if 0
    type(json_core) :: tmp_core

    call assert( 948908181, associated( config%value_ ) )
    call tmp_core%initialize( )
    call tmp_core%print_to_string( config%value_, string%val_ )
#endif
  end subroutine string_assign_config

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Cleans up memory
  subroutine finalize( this )

    !> Configuration
    type(config_t), intent(inout) :: this

    if( c_associated( this%node_) ) then
      call yaml_delete_c( this%node_ )
      this%node_ = c_null_ptr
    end if

  end subroutine finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Cleans up memory
  subroutine finalize_1D_array( this )

    !> Configuration
    type(config_t), intent(inout) :: this(:)

    integer(kind=musica_ik) :: i_elem

    do i_elem = 1, size( this )
      if( c_associated( this( i_elem )%node_ ) ) then
        call yaml_delete_c( this( i_elem )%node_ )
        this( i_elem )%node_ = c_null_ptr
      end if
    end do

  end subroutine finalize_1D_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the property name from a key
  function get_property_name( key ) result( prop_name )

    use musica_string,                 only : string_t

    !> Property name
    type(string_t) :: prop_name
    !> Key
    character(len=*), intent(in) :: key

    integer :: b

    b = index( key, '[' )
    prop_name = trim( key(1:b-1) )

  end function get_property_name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets the property units from a key
  function get_property_units( key ) result( units )

    use musica_string,                 only : string_t

    !> Units
    type(string_t) :: units
    !> Key
    character(len=*), intent(in) :: key

    integer :: b1, b2

    b1 = index( key, '[' )
    b2 = index( key, ']' )
    units = trim( key(b1+1:b2-1) )

  end function get_property_units

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Gets a full key to use for a property
  function get_full_key( property_name, units ) result( key )

    !> Full key
    character(len=:), allocatable :: key
    !> Property name
    character(len=*), intent(in) :: property_name
    !> Property units
    character(len=*), intent(in) :: units

    key = trim( property_name )//" ["//trim( units )//"]"

  end function get_full_key

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds a full key name by a prefix
  !!
  !! Returns the first instance of the prefix if found
  subroutine find_by_prefix( this, prefix, parent, child, full_key, found )

    use musica_assert,                 only : assert
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Prefix to search for (first instance is returned)
    character(len=*), intent(in) :: prefix
    !> JSON object to search
    class(*), pointer, intent(in) :: parent
    !> JSON object found
    class(*), pointer, intent(out) :: child
    !> Full key found
    type(string_t), intent(out) :: full_key
    !> Flag indicating whether the key was found
    logical, intent(out) :: found
#if 0
    character(kind=json_ck, len=:), allocatable :: tmp_key
    type(json_value), pointer :: next
    logical(kind=json_lk) :: l_found
    integer :: length

    call assert( 299899977, associated( this%value_ ) )
    length = len( trim( prefix ) )
    child => null( )
    next  => null( )
    call this%core_%get_child( parent, int( 1, kind=json_ik ), child, l_found )
    do while( associated( child ) .and. l_found )
      call this%core_%info( child, name = tmp_key )
      if( len( tmp_key ) .gt. length ) then
        if( tmp_key(1:length) .eq. trim( prefix ) ) then
          full_key = tmp_key
          found = .true.
          return
        end if
      end if
      call this%core_%get_next( child, next )
      child => next
    end do
    child => null( )
    found = .false.
    full_key = ""
#endif
  end subroutine find_by_prefix

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Merges another config_t object into the config_t object
  recursive subroutine merge_in( this, other, caller )

    use musica_assert,                 only : assert, die_msg
    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Configuration to merge in
    class(config_t), intent(inout) :: other
    !> Name of the calling function (only for use in error messages)
    character(len=*), intent(in) :: caller
#if 0
    type(config_t) :: this_sub_config, other_sub_config
    type(json_value), pointer :: this_child, other_child
    integer(kind=json_ik) :: this_type, other_type, i_child, n_children
    logical(kind=json_lk) :: found
    character(kind=json_ck, len=:), allocatable :: j_key
    type(string_t) :: json_str

    call assert( 836100870, associated( this%value_  ) )
    call assert( 844609972, associated( other%value_ ) )
    call this%core_%info( this%value_, var_type = this_type )
    call assert( 499484152, this_type .eq. json_object )
    call other%core_%info( other%value_, var_type = other_type,               &
                          n_children = n_children )
    call assert( 771883082, other_type .eq. json_object )
    do i_child = 1, n_children
      call other%core_%get_child( other%value_, i_child, other_child, found )
      call assert( 183421173, found .and. associated( other_child ) )
      call other%core_%info( other_child, name = j_key,                       &
                            var_type = other_type )
      call this%core_%get( this%value_, j_key, this_child, found )
      if( found ) then
        call this%core_%info( this_child, var_type = this_type )
        if( this_type .eq. json_object .and. other_type .eq. json_object ) then
          call this%core_%serialize( this_child, json_str%val_ )
          this_sub_config = json_str
          deallocate( json_str%val_ )
          call other%core_%serialize( other_child, json_str%val_ )
          other_sub_config = json_str
          deallocate( json_str%val_ )
          call this_sub_config%merge_in( other_sub_config, caller )
          call this%core_%remove( this_child, destroy = .true. )
          call this%core_%rename( this_sub_config%value_, j_key )
          call this%core_%add( this%value_, this_sub_config%value_ )
          this_sub_config%value_ => null( )
        else
          call die_msg( 530025829, "Un-mergable duplicate key '"//j_key//     &
                        "' in config_t merge request by "//caller )
        end if
      else
        call other%core_%serialize( other_child, json_str%val_ )
        other_sub_config = json_str
        call other%core_%rename( other_sub_config%value_, j_key )
        call this%core_%add( this%value_, other_sub_config%value_ )
        other_sub_config%value_ => null( )
      end if
    end do
#endif
  end subroutine merge_in

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns true if the given key is in the given list of keys
  logical function find_key_in_list( key_to_find, list ) result( found )

    use musica_string,                 only : string_t

    type(string_t), intent(in) :: key_to_find
    type(string_t), intent(in) :: list(:)
#if 0
    integer :: i_elem

    found = .false.
    do i_elem = 1, size( list )
      if( key_to_find .eq. list( i_elem ) ) then
        found = .true.
        exit
      end if
    end do
#endif
  end function find_key_in_list

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Validates the format of the configuration data
  !!
  !! Ensures that the required keys are present, and that user-defined keys
  !! start with a "`__`" prefix.
  logical function validate( this, required_keys, optional_keys )

    use musica_string,                 only : string_t

    !> Configuration
    class(config_t), intent(inout) :: this
    !> Required keys
    type(string_t),  intent(in) :: required_keys(:)
    !> Optional keys
    type(string_t),  intent(in) :: optional_keys(:)

    integer :: n_req_found
    logical :: is_valid
    type(string_t) :: key
    class(iterator_t), pointer :: iter
    character(len=:), allocatable :: error_message

    ! validates JSON format, including check for duplicate keys

    validate = .true.
    n_req_found = 0
    iter => this%get_iterator( )
    do while( iter%next( ) )
      key = this%key( iter )
      if( key%length( ) .ge. 2 ) then
        if( key%substring( 1, 2 ) .eq. "__" ) cycle
      end if
      if( find_key_in_list( key, required_keys ) ) then
        n_req_found = n_req_found + 1
      else if( .not. find_key_in_list( key, optional_keys ) ) then
        validate = .false.
        exit
      end if
    end do
    if( n_req_found .ne. size( required_keys ) ) validate = .false.
    deallocate( iter )

  end function validate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Print out the raw contents of the configuration
  subroutine do_print( this )

    use musica_string

    !> Configuration
    class(config_t), intent(inout) :: this
#if 0
    type(string_t) :: str

    call this%core_%serialize( this%value_, str%val_ )
    write(*,*) str
#endif
  end subroutine do_print

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the size of a binary buffer required to pack the object
  integer function pack_size( this, comm )

    use musica_mpi

    class(config_t),   intent(inout) :: this ! configuration to pack
    integer, optional, intent(in)    :: comm ! MPI communicator
#if 0
#ifdef MUSICA_USE_MPI
    character(len=:), allocatable :: json_string

    call this%core_%serialize( this%value_, json_string )
    pack_size = musica_mpi_pack_size( len( json_string ), comm ) +            &
                musica_mpi_pack_size( json_string, comm )
#else
    pack_size = 0
#endif
#endif
    pack_size = 0
  end function pack_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Packs the configuration onto a character buffer
  subroutine mpi_pack( this, buffer, position, comm )

    use musica_assert,                 only : assert
    use musica_mpi

    !> Configuration to pack
    class(config_t),   intent(inout) :: this
    !> Memory buffer
    character,         intent(inout) :: buffer(:)
    !> Current buffer position
    integer,           intent(inout) :: position
    !> MPI communicator
    integer, optional, intent(in)    :: comm
#if 0
#ifdef MUSICA_USE_MPI
    integer :: prev_position
    character(len=:), allocatable :: json_string

    call this%core_%serialize( this%value_, json_string )
    prev_position = position
    call musica_mpi_pack( buffer, position, len( json_string ), comm )
    call musica_mpi_pack( buffer, position, json_string,        comm )
    call assert( 599029464,                                                   &
                 position - prev_position <= this%pack_size( comm ) )
#endif
#endif
  end subroutine mpi_pack

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Unpacks the configuration from a character buffer
  subroutine mpi_unpack( this, buffer, position, comm )

    use musica_assert,                 only : assert
    use musica_mpi

    !> Configuration to unpack
    class(config_t),   intent(out)   :: this
    !> Memory buffer
    character,         intent(inout) :: buffer(:)
    !> Current buffer position
    integer,           intent(inout) :: position
    !> MPI communicator
    integer, optional, intent(in)    :: comm
#if 0
#ifdef MUSICA_USE_MPI
    integer :: prev_position, string_size
    character(len=:), allocatable :: json_string

    prev_position = position
    call musica_mpi_unpack( buffer, position, string_size, comm )
    allocate( character(len=string_size) :: json_string )
    call musica_mpi_unpack( buffer, position, json_string, comm )
    call this%core_%initialize( )
    call this%core_%parse( this%value_, json_string )
    call assert( 354240136,                                                   &
                 position - prev_position <= this%pack_size( comm ) )
#endif
#endif
  end subroutine mpi_unpack

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Advances the iterator
  !!
  !! Returns false if the end of the collection has been reached
  logical function iterator_next( this )

    !> Iterator
    class(config_iterator_t), intent(inout) :: this

    this%id_ = this%id_ + 1
    if( this%id_ .le. this%config_%number_of_children( ) ) then
      iterator_next = .true.
    else
      iterator_next = .false.
    end if

  end function iterator_next

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Resets the iterator
  subroutine iterator_reset( this, parent )

    !> Iterator
    class(config_iterator_t), intent(inout) :: this
    !> Iterator for parent model element
    class(iterator_t), intent(in), optional :: parent

    this%id_ = 0

  end subroutine iterator_reset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Initialize a config_t object
  subroutine initialize_config_t( config, string )

    use musica_assert,                 only : die

    !> Configuration
    class(config_t),            intent(inout) :: config
    !> YAML string
    character(len=*), optional, intent(in)    :: string

    character(len=1, kind=c_char), allocatable :: c_string(:)
    integer :: N, i

    select type(config)
    type is(config_t)
      call finalize( config )
      if( present( string ) ) then
        c_string = to_c_string( string )
        config%node_ = yaml_create_from_string_c( c_string )
      else
        config%node_ = yaml_create_from_string_c( (/ c_null_char /) )
      end if
    class default
      call die( 288394178 )
    end select

  end subroutine initialize_config_t

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Convert a fortran character array to a c string
  function to_c_string( f_string ) result( c_string )

    character(len=1, kind=c_char), allocatable :: c_string(:)
    character(len=*),              intent(in)  :: f_string

    integer :: N, i

    N = len_trim( f_string )
    allocate( c_string( N + 1 ) )
    do i = 1, N
      c_string(i) = f_string(i:i)
    end do
    c_string( N + 1 ) = c_null_char   

  end function to_c_string

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module musica_config

