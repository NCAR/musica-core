######################################################################
# Utility source

target_sources(musicacore_object
  PRIVATE
    array.F90
    assert.F90
    constants.F90
    convert.F90
    data_type.F90
    datetime.F90
    iterator.F90
    io.F90
    logger.F90
    map.F90
    mpi.F90
    string.F90
    target.F90
)

if(ENABLE_YAML)
  target_sources(musicacore_object
    PRIVATE
      config_yaml.F90
  )
else()
  target_sources(musicacore_object
    PRIVATE
      config_json.F90
  )
endif()

add_subdirectory(io)

######################################################################
