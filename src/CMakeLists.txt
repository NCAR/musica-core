################################################################################
# MUSICA library

if(NOT ENABLE_UTIL_ONLY)
  target_sources(musicacore_object
    PRIVATE 
      component.F90 
      component_set.F90 
      domain.F90 
      domain_factory.F90 
      domain_iterator.F90 
      domain_state.F90 
      domain_state_accessor.F90 
      domain_state_mutator.F90 
      domain_target_cells.F90 
      domain_target_columns.F90 
      domain_target_model_top_cells.F90 
      domain_target_surface_cells.F90 
      evolving_conditions.F90 
      grid.F90 
      initial_conditions.F90 
      interpolator.F90 
      lookup/axis.F90 
      property.F90 
      property_set.F90 
  )

  add_subdirectory(domains)
  add_subdirectory(input_output)
  add_subdirectory(interpolator_strategies)
  add_subdirectory(lookup)
endif()

add_subdirectory(util)

################################################################################