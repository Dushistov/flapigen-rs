cmake_minimum_required(VERSION 3.2)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest)
add_executable(c++-rust-swig-test
  #not use add_subdirectory(gtest/googletest)
  #because of strange compiler settings in google's cmake files
  gtest/googletest/src/gtest-all.cc
  main.cpp)

set(CARGO_CMD "cargo")
set(RUST_TARGET "")
set(RUSTC "rustc")

function (find_target_directory CACHEVAR)
  if (DEFINED ${CACHEVAR})
    message(STATUS "Cashed target path: ${${CACHEVAR}}")
    return ()
  endif ()
  set(CUR_DIR ${CMAKE_SOURCE_DIR})
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
endfunction()

function (required_libs_by_rust_library CACHEVAR)
  if (DEFINED ${CACHEVAR})
    message(STATUS "Cashed rust libraries: ${${CACHEVAR}}")
    return ()
  endif ()

  set(TEST_CRATE_ROOT "${CMAKE_BINARY_DIR}/rust_static_lib_cfg")
  file(MAKE_DIRECTORY "${TEST_CRATE_ROOT}")
  file(MAKE_DIRECTORY "${TEST_CRATE_ROOT}/src")
  file(WRITE "${TEST_CRATE_ROOT}/src/lib.rs" " ")
  file(WRITE "${TEST_CRATE_ROOT}/Cargo.toml" "[package]\nname = \"static_lib\"\nversion = \"0.0.1\"\nauthors = [\"\"]\n[lib]\nname = \"static_lib\"\ncrate-type = [\"staticlib\"]\n")

  execute_process(
    COMMAND ${RUSTC} ${RUST_TARGET} --crate-name static_lib src/lib.rs --crate-type staticlib  --print=native-static-libs
    OUTPUT_VARIABLE RUST_LINK_LIBRARIES_OUT
    ERROR_VARIABLE RUST_LINK_LIBRARIES_ERR
    RESULT_VARIABLE CARGO_RET
    WORKING_DIRECTORY "${TEST_CRATE_ROOT}"
    )
  if (NOT "${CARGO_RET}" STREQUAL "0")
    message(FATAL_ERROR "cargo build failed: ${RUST_LINK_LIBRARIES_ERR}")
  endif()
  set(RUST_LINK_LIBRARIES "${RUST_LINK_LIBRARIES_OUT} ${RUST_LINK_LIBRARIES_ERR}")
  string(REGEX MATCHALL "note: native-static-libs: ([\-a-zA-Z_0-9 \.]+)" RUST_LINK_LIBRARIES "${RUST_LINK_LIBRARIES}")
  string(REPLACE "note: native-static-libs: " "" RUST_LINK_LIBRARIES "${RUST_LINK_LIBRARIES}")
  if (WIN32)
    message(STATUS "Removing ms vcrt library from list")
    string(REPLACE "msvcrt.lib" "" RUST_LINK_LIBRARIES "${RUST_LINK_LIBRARIES}")
  endif ()
  separate_arguments(RUST_LINK_LIBRARIES)
  message(STATUS "rust libraries: ${RUST_LINK_LIBRARIES}")
  set(${CACHEVAR} "${RUST_LINK_LIBRARIES}" CACHE STRING "Required link libraries by rust" FORCE)
  file(REMOVE_RECURSE "${TEST_CRATE_ROOT}")
endfunction ()

get_filename_component(RUST_BUILD_CWD ${CMAKE_CURRENT_SOURCE_DIR}  DIRECTORY)

find_target_directory(TARGET_PATH)
required_libs_by_rust_library(RUST_LINK_LIBRARIES)

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Release" OR "${CMAKE_BUILD_TYPE}" STREQUAL "RelWithDebInfo")
  set(CARGO_BUILD "cargo" "build" "--release")
else()
  set(CARGO_BUILD "cargo" "build")
endif()

include(ExternalProject)

ExternalProject_Add(
    rust_swig_test
    DOWNLOAD_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ${CARGO_BUILD}
    BINARY_DIR "${RUST_BUILD_CWD}"
    INSTALL_COMMAND ""
    LOG_BUILD ON)

ExternalProject_Add_Step(
  rust_swig_test rebuild
  COMMAND ${CARGO_BUILD}
  ALWAYS 1
  WORKING_DIRECTORY "${RUST_BUILD_CWD}"
)

add_dependencies(c++-rust-swig-test rust_swig_test)

if (WIN32)
  set(LIBRUST_NAME "rust_swig_test.lib")
else ()
  set(LIBRUST_NAME "librust_swig_test.a")
endif ()

if ("${CMAKE_BUILD_TYPE}" STREQUAL "Release" OR "${CMAKE_BUILD_TYPE}" STREQUAL "RelWithDebInfo")
  target_link_libraries(c++-rust-swig-test "${TARGET_PATH}/release/${LIBRUST_NAME}" ${RUST_LINK_LIBRARIES})
else()
  target_link_libraries(c++-rust-swig-test "${TARGET_PATH}/debug/${LIBRUST_NAME}" ${RUST_LINK_LIBRARIES})
endif()

set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)
enable_testing()
add_test(NAME c++-rust-swig-test COMMAND c++-rust-swig-test)
