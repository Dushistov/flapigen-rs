cmake_minimum_required(VERSION 3.9)
project(rust_swig_cpp_tests)

include(CheckIncludeFileCXX)
include(ExternalProject)
include(CheckCXXCompilerFlag)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(compiler_extra_flags)

option(USE_BOOST "Use boost for types not available in c++11" OFF)

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
if (NOT HAS_BOOST_STRING_VIEW_HPP)
  message(FATAL_ERROR "Can not find boost/utility/string_view.hpp")
else ()
  add_definitions(-DHAS_BOOST_STRING_VIEW_HPP)
endif ()

if (NOT USE_BOOST)
  if (NOT DEFINED CXX_17_STD)
    message(STATUS "Check if compiler supports C++17")
    if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
      message(STATUS "Compiler is gcc or clang")
      check_cxx_compiler_flag("-std=c++17" GCC_HAS_STDCXX_17)
      if (GCC_HAS_STDCXX_17)
	message(STATUS "Compiler has c++17 support")
	set(CXX_17_STD ON CACHE BOOL "Override option" FORCE)
      else ()
	message(STATUS "Compiler has NO c++17 support")
	set(CXX_17_STD OFF CACHE BOOL "Override option" FORCE)
      endif ()
    endif ()
    if (MSVC)
      message(STATUS "Compiler is VC++")
      check_cxx_compiler_flag("/std:c++17" VC_HAS_STDCXX_17)
      if (VC_HAS_STDCXX_17)
	message(STATUS "Compiler has c++17 support")
	set(CXX_17_STD ON CACHE BOOL "Override option" FORCE)
      else ()
	message(STATUS "Compiler has NO c++17 support")
	set(CXX_17_STD OFF CACHE BOOL "Override option" FORCE)
      endif ()
    endif ()
  endif (NOT DEFINED CXX_17_STD)

  if (CXX_17_STD)
    message(STATUS "add -DHAS_STDCXX_17")
    add_definitions(-DHAS_STDCXX_17)

    check_include_file_cxx(optional HAVE_STD17_OPTIONAL)
    if (NOT HAVE_STD17_OPTIONAL)
      message(STATUS "c++ compiler support c++17, but have no optional")
      add_definitions(-DNO_HAVE_STD17_OPTIONAL)
    endif ()
    check_include_file_cxx(variant HAVE_STD17_VARIANT)
    if (NOT HAVE_STD17_VARIANT)
      message(STATUS "c++ compiler support c++17, but have no variant")
      add_definitions(-DNO_HAVE_STD17_VARIANT)
    endif ()
    check_include_file_cxx(string_view HAVE_STD17_STRING_VIEW)
    if (NOT HAVE_STD17_STRING_VIEW)
      message(STATUS "c++ compiler support c++17, but have no string_view")
      add_definitions(-DNO_HAVE_STD17_STRING_VIEW)
    endif ()
  endif (CXX_17_STD)
endif (NOT USE_BOOST)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/gtest/googletest)

set(CARGO_CMD "cargo")
set(RUST_TARGET "")
set(RUSTC "rustc")
if (USE_BOOST)
  set(CARGO_ADDON_ARGS "--no-default-features" "--features" "boost")
  add_definitions(-DUSE_BOOST)
else ()
  if (HAVE_STD17_VARIANT)
    set(CARGO_ADDON_ARGS "cpp17_variant")
  endif ()
  if (HAVE_STD17_OPTIONAL)
     if ("${CARGO_ADDON_ARGS}" STREQUAL "")
       set(CARGO_ADDON_ARGS "cpp17_optional")
     else ()
       set(CARGO_ADDON_ARGS "${CARGO_ADDON_ARGS},cpp17_optional")
     endif ()
   endif ()
   if (HAVE_STD17_STRING_VIEW)
     if ("${CARGO_ADDON_ARGS}" STREQUAL "")
       set(CARGO_ADDON_ARGS "cpp17_string_view")
     else ()
       set(CARGO_ADDON_ARGS "${CARGO_ADDON_ARGS},cpp17_string_view")
     endif ()
   endif ()
  if (CARGO_ADDON_ARGS)
    set(CARGO_ADDON_ARGS "--features" "${CARGO_ADDON_ARGS}")
  endif ()
endif(USE_BOOST)

include(rust_cargo)

get_filename_component(RUST_BUILD_CWD ${CMAKE_CURRENT_SOURCE_DIR}  DIRECTORY)
#this should create "target" directory if it does not exists
set(RUST_SWIG_SRCS "${RUST_BUILD_CWD}/src/cpp_glue.rs.in")
# force regeneration of headers for the first build
execute_process(
  COMMAND ${CMAKE_COMMAND} -E touch_nocreate ${RUST_SWIG_SRCS}
  RESULT_VARIABLE touch_retcode
  )
if(NOT "${touch_retcode}" STREQUAL "0")
  message(FATAL_ERROR "touch of src/cpp_glue.rs.in failed")
endif()

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
  TestOptional.cpp
  TestWorkWithVec.cpp
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

if (CXX_17_STD)
  set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 17 CXX_STANDARD_REQUIRED ON)
else ()
  set_target_properties(c++-rust-swig-test PROPERTIES CXX_STANDARD 11 CXX_STANDARD_REQUIRED ON)
endif ()

enable_testing()
add_test(NAME c++-rust-swig-test COMMAND c++-rust-swig-test)
