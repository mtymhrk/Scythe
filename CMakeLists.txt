cmake_minimum_required(VERSION 2.6)

project(Scythe C)

include("${CMAKE_SOURCE_DIR}/cmake/common.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/dump_build_info.cmake")

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(test/ut)
