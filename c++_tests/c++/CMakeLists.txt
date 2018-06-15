cmake_minimum_required(VERSION 3.2)

include(CheckIncludeFileCXX)
include(ExternalProject)
include(CheckCXXCompilerFlag)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(compiler_extra_flags)

option(USE_BOOST "Use boost for types not avaible in c++11" OFF)

if (USE_BOOST)
  find_package(Boost 1.66.0 REQUIRED)
  message(STATUS "Boost_INCLUDE_DIRS: ${Boost_INCLUDE_DIRS}")
  include_directories(${Boost_INCLUDE_DIRS})
  set(CMAKE_REQUIRED_INCLUDES "${Boost_INCLUDE_DIRS}")
  check_include_file_cxx(boost/optional.hpp HAVE_BOOST_OPTIONAL_HPP)
  if (NOT HAVE_BOOST_OPTIONAL_HPP)
    message(FATAL_ERROR "Can not find boost/optional.hpp")
  endif ()
  check_include_file_cxx(boost/variant.hpp HAVE_BOOST_VARIANT_HPP)
  if (NOT HAVE_BOOST_VARIANT_HPP)
    message(FATAL_ERROR "Can not find boost/variant.hpp")
  endif ()
  check_include_file_cxx(boost/utility/string_view.hpp HAS_BOOST_STRING_VIEW_HPP)
  if (HAS_BOOST_STRING_VIEW_HPP)
    message(STATUS "Found boost/utility/string_view.hpp")
    add_definitions(-DHAS_BOOST_STRING_VIEW_HPP)
  endif ()
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest)

set(CARGO_CMD "cargo")
set(RUST_TARGET "")
set(RUSTC "rustc")
if (USE_BOOST)
  set(CARGO_ADDON_ARGS "--no-default-features" "--features" "boost")
  add_definitions(-DUSE_BOOST)
endif(USE_BOOST)

include(rust_cargo)

get_filename_component(RUST_BUILD_CWD ${CMAKE_CURRENT_SOURCE_DIR}  DIRECTORY)
#this should create "target" directory if it does not exists
set(RUST_SWIG_SRCS "${RUST_BUILD_CWD}/src/cpp_glue.rs.in")
configure_file(cmake/rust_swig_gen.cmake.in ${CMAKE_BINARY_DIR}/rust_swig_gen.cmake @ONLY)
file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/rust_swig_gen)
if (NOT EXISTS ${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake)
  file(WRITE ${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake "")
endif()
execute_process(COMMAND ${CMAKE_COMMAND}
    -DSRCDIR=${CMAKE_SOURCE_DIR}
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
    -P ${CMAKE_BINARY_DIR}/rust_swig_gen.cmake
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/rust_swig_gen)
include(${CMAKE_BINARY_DIR}/rust_swig_regen_headers.cmake)

find_target_directory(TARGET_PATH)
required_libs_by_rust_library(RUST_LINK_LIBRARIES)


if ("${CMAKE_BUILD_TYPE}" STREQUAL "Release" OR "${CMAKE_BUILD_TYPE}" STREQUAL "RelWithDebInfo")
  set(CARGO_BUILD ${CARGO_CMD} "build" ${CARGO_ADDON_ARGS} "${RUST_TARGET}" "--release")
else()
  set(CARGO_BUILD ${CARGO_CMD} "build" ${CARGO_ADDON_ARGS} "${RUST_TARGET}")
endif()


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
add_executable(c++-rust-swig-test
  #not use add_subdirectory(gtest/googletest)
  #because of strange compiler settings in google's cmake files
  gtest/googletest/src/gtest-all.cc
  ${RUST_SWIG_CPP_HEADERS}
  main.cpp
  )

add_dependencies(c++-rust-swig-test rust_swig_test rust_swig_gen_headers)
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

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  message(STATUS "Compiler is gcc or clang")
  check_cxx_compiler_flag("-std=c++17" HAS_STDCXX_17)
  if (UNIX AND NOT APPLE AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # clang can not handle libstd++ in c++17 mode
    # see https://bugs.llvm.org/show_bug.cgi?id=33222
    message(STATUS "clang is used on linux, disable c++17 mode")
    set(HAS_STDCXX_17 FALSE)
  endif ()
  if (HAS_STDCXX_17 AND (NOT USE_BOOST))
    set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 17 CXX_STANDARD_REQUIRED ON)
    add_definitions(-DHAS_STDCXX_17)
  else()
    set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)
  endif()
else ()
  set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)
endif()#compiler switch
enable_testing()
add_test(NAME c++-rust-swig-test COMMAND c++-rust-swig-test)
