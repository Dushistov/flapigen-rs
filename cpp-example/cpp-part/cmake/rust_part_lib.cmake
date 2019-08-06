set(CARGO_CMD "cargo")
set(RUST_TARGET "")
set(RUSTC "rustc")
get_filename_component(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
set(RUST_BUILD_CWD "${ROOT_DIR}/rust-part")

function (find_cargo_target_directory CACHEVAR)
  if (DEFINED ${CACHEVAR})
    message(STATUS "Cashed target path: ${${CACHEVAR}}")
    return ()
  endif ()
  if(DEFINED ENV{CARGO_TARGET_DIR})
    message(STATUS "Found CARGO_TARGET_DIR: $ENV{CARGO_TARGET_DIR}")
    set(${CACHEVAR} "$ENV{CARGO_TARGET_DIR}" CACHE PATH "Path to cargo's target directory with build artifacts" FORCE)
    return ()
  endif ()
  set(CUR_DIR "${RUST_BUILD_CWD}/src")
  while (True)
    get_filename_component(NEW_DIR ${CUR_DIR}  DIRECTORY)
    set(TARGET_PATH "${NEW_DIR}/target")
    if (EXISTS "${TARGET_PATH}" AND IS_DIRECTORY "${TARGET_PATH}")
      message(STATUS "Found cargo's target directory: ${TARGET_PATH}")
      set(${CACHEVAR} "${TARGET_PATH}" CACHE PATH "Path to cargo's target directory with build artifacts" FORCE)
      break()
    endif ()

    if ("${NEW_DIR}" STREQUAL "${CUR_DIR}")
      message(FATAL_ERROR "Can not find cargo's target directory")
    endif ()

    set(CUR_DIR "${NEW_DIR}")
  endwhile()
endfunction(find_cargo_target_directory)

set(RUST_SWIG_SRCS "${RUST_BUILD_CWD}/src/cpp_glue.rs.in")
configure_file(cmake/rust_swig_gen.cmake.in ${CMAKE_BINARY_DIR}/rust_swig_gen.cmake @ONLY)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/rust_swig_gen)
if (NOT EXISTS ${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake)
  file(WRITE ${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake "")
endif()

execute_process(COMMAND ${CMAKE_COMMAND}
  -DSRCDIR=${CMAKE_SOURCE_DIR}
  -DRUST_BUILD_CWD=${RUST_BUILD_CWD}
    -DBINDIR=${CMAKE_BINARY_DIR}
    -P ${CMAKE_BINARY_DIR}/rust_swig_gen.cmake
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/rust_swig_gen
    RESULT_VARIABLE retcode)
if(NOT "${retcode}" STREQUAL "0")
  message(FATAL_ERROR "run of rust_swig_gen.cmake failed")
endif()

add_custom_target(rust_swig_gen_headers ${CMAKE_COMMAND}
    -DSRCDIR=${CMAKE_SOURCE_DIR}
    -DBINDIR=${CMAKE_BINARY_DIR}
    -DRUST_BUILD_CWD=${RUST_BUILD_CWD}
    -P ${CMAKE_BINARY_DIR}/rust_swig_gen.cmake
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/rust_swig_gen)
include(${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake)

find_cargo_target_directory(TARGET_PATH)

if (NOT RUST_DEBUG_BUILD)
  set(CARGO_BUILD ${CARGO_CMD} "build" "${RUST_TARGET}" "--release")
else()
  set(CARGO_BUILD ${CARGO_CMD} "build" "${RUST_TARGET}")
endif()

if (WIN32)
  set(RUST_PART_LIB_NAME "cpp_example_rust_part.dll.lib")
elseif (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set(RUST_PART_LIB_NAME "libcpp_example_rust_part.dylib")
else ()
  set(RUST_PART_LIB_NAME "libcpp_example_rust_part.so")
endif ()

if (NOT RUST_DEBUG_BUILD)
  set(RUST_PART_LIB_PATH "${TARGET_PATH}/release/${RUST_PART_LIB_NAME}")
else()
  set(RUST_PART_LIB_PATH "${TARGET_PATH}/debug/${RUST_PART_LIB_NAME}")
endif()

add_custom_command(OUTPUT ${RUST_PART_LIB_PATH}
  COMMAND ${CARGO_BUILD}
  WORKING_DIRECTORY "${RUST_BUILD_CWD}")
add_custom_target(rust_part_lib_target DEPENDS ${RUST_PART_LIB_PATH})
add_library(rust_part_lib UNKNOWN IMPORTED GLOBAL)
add_dependencies(rust_part_lib rust_part_lib_target)
add_dependencies(rust_part_lib rust_swig_gen_headers)
set_target_properties(rust_part_lib
    PROPERTIES
    IMPORTED_LOCATION ${RUST_PART_LIB_PATH}
    INTERFACE_INCLUDE_DIRECTORIES "${CMAKE_CURRENT_SOURCE_DIR}/rust-api")
