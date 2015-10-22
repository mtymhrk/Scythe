cmake_minimum_required(VERSION 2.6)

project(Scythe C)

include("${CMAKE_SOURCE_DIR}/cmake/common.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/dump_build_info.cmake")

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(test/ut)


##===============================================================
## Source Pacage 作成

add_custom_target(
  dist
  COMMAND ${CMAKE_SOURCE_DIR}/tools/gen-src-pkg.sh ${CMAKE_PROJECT_NAME} ${CMAKE_SOURCE_DIR} tip
  DEPENDS prepare
)
