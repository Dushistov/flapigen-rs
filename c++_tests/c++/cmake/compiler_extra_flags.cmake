if (CMAKE_COMPILER_IS_GNUCXX)
  include(CheckCXXCompilerFlag)
  set(CMAKE_CXX_FLAGS_WARN "-Wnon-virtual-dtor -Wno-long-long -Wcast-align -Wchar-subscripts -Wall -Wextra -Wpointer-arith -Wformat-security -Woverloaded-virtual -Wshadow -Wunused-parameter -fno-common")

  check_cxx_compiler_flag(-Werror=return-type HAVE_GCC_ERROR_RETURN_TYPE)
  if (HAVE_GCC_ERROR_RETURN_TYPE)
    set(CMAKE_CXX_FLAGS_ERROR "-Werror=return-type")
  endif()
  check_cxx_compiler_flag(-Werror=format HAVE_GCC_ERROR_FORMAT)
  if (HAVE_GCC_ERROR_FORMAT)
    set(CMAKE_CXX_FLAGS_ERROR "${CMAKE_CXX_FLAGS_ERROR} -Werror=format")
  endif()
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO
    "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} ${CMAKE_CXX_FLAGS_WARN} ${CMAKE_CXX_FLAGS_ERROR}")
  set(CMAKE_CXX_FLAGS_DEBUG
    "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_WARN} ${CMAKE_CXX_FLAGS_ERROR}")
  set(CMAKE_CXX_FLAGS_RELEASE
    "${CMAKE_CXX_FLAGS_RELEASE} ${CMAKE_CXX_FLAGS_WARN} ${CMAKE_CXX_FLAGS_ERROR}")
endif(CMAKE_COMPILER_IS_GNUCXX)
