cmake_minimum_required(VERSION 3.10)
project(Nastran95_Project Fortran)

# Set the default Fortran compiler
set(CMAKE_Fortran_COMPILER gfortran CACHE STRING "Fortran compiler")

# Set the Fortran compilation flags depending on the compiler
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -std=legacy -fdec -fno-range-check -fno-automatic")
#  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -w -fallow-invalid-boz -fallow-argument-mismatch -fdec")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /Qipo /O3 /MP /integer-size=64 /Qdiag-disable:10448 /Qdiag-disable:warn")
endif()

function(efile out_var)
  set(options)
  set(oneValueArgs GLOB)
  set(multiValueArgs EXCLUDE)
  cmake_parse_arguments(EFILE "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  # Use file(GLOB ...) to collect all files matching the provided patterns
  file(GLOB_RECURSE temp_files CONFIGURE_DEPENDS ${EFILE_GLOB})

  # Apply exclusions
  foreach(exclude_pattern ${EFILE_EXCLUDE})
    # Filtering out files from the list that match the exclude patterns
    list(FILTER temp_files EXCLUDE REGEX "${exclude_pattern}")
  endforeach()

  # Set the results to the output variable
  set(${out_var} "${temp_files}" PARENT_SCOPE)
endfunction()


# Collect source files
efile(BD_SOURCES GLOB "bd/*.f" EXCLUDE "bd/semdbd.f")
efile(MIS_SOURCES GLOB "mis/*.f")

foreach(SRC_FILE ${MIS_SOURCES})
  set_source_files_properties(${SRC_FILE} PROPERTIES Fortran_PREPROCESS OFF)
endforeach()

efile(MDS_SOURCES GLOB "mds/*.f" EXCLUDE "mds/chkfil.f")

# Create object libraries
add_library(nasmis_obj OBJECT ${MIS_SOURCES})
add_library(nasmds_obj OBJECT ${MDS_SOURCES})
add_library(nasbd_obj OBJECT ${BD_SOURCES})

# Combine object libraries into a single library
add_library(nas $<TARGET_OBJECTS:nasmis_obj>
                $<TARGET_OBJECTS:nasmds_obj>
                $<TARGET_OBJECTS:nasbd_obj>)

# Create executables
add_executable(nastran "bin/nastrn.f" $<TARGET_OBJECTS:nasbd_obj>)
target_link_libraries(nastran PRIVATE nas)
set_target_properties(nastran PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  Fortran_FLAGS "${FORTRAN_FLAGS}"
)

add_executable(nasthelp "um/nasthelp.f")
set_target_properties(nasthelp PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  Fortran_FLAGS "${FORTRAN_FLAGS}"
)

add_executable(nastplot "utility/nastplot.f")
set_target_properties(nastplot PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  Fortran_FLAGS "${FORTRAN_FLAGS}"
)

add_executable(ff "utility/ff.f")
target_link_libraries(ff PRIVATE nas)
set_target_properties(ff PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  Fortran_FLAGS "${FORTRAN_FLAGS}"
)

add_executable(chkfil "mds/chkfil.f")
set_target_properties(chkfil PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  Fortran_FLAGS "${FORTRAN_FLAGS}"
)

# Add custom commands for NASINFO and documentation files
add_custom_command(OUTPUT NASINFO
                   COMMAND ${CMAKE_COMMAND} -E create_symlink rf/NASINFO NASINFO
                   COMMENT "Creating symlink for NASINFO")

file(GLOB DOCTXT um/*.TXT)
foreach(FILE ${DOCTXT})
  get_filename_component(FILENAME ${FILE} NAME)
  add_custom_command(OUTPUT bin/${FILENAME}
                     COMMAND ${CMAKE_COMMAND} -E create_symlink ${FILE} bin/${FILENAME}
                     COMMENT "Creating symlink for ${FILENAME}")
endforeach()
