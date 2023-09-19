######################################################################
# Utility source

target_sources(musicacore_object
  PRIVATE
    array.F90
    assert.F90
    constants.F90
    config.F90
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

add_subdirectory(io)

######################################################################
