cmake_minimum_required(VERSION 2.6)

project(Scythe C)

include("${CMAKE_SOURCE_DIR}/cmake/common.cmake")

add_subdirectory(src)
add_subdirectory(test/ut)
