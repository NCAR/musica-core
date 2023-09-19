######################################################################
# Input/Output source

target_sources(musicacore_object
  PRIVATE
    file.F90
    file_dimension.F90
    file_dimension_range.F90
    file_dimension_factory.F90
    file_factory.F90
    file_paired_variable.F90
    file_updater.F90
    file_variable.F90
    file_variable_factory.F90
    input_output_processor.F90
)

add_subdirectory(file_dimensions)
add_subdirectory(file_variables)
add_subdirectory(files)

######################################################################
