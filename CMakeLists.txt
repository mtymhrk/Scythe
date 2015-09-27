cmake_minimum_required(VERSION 2.6)

project(Scythe C)

include("${CMAKE_SOURCE_DIR}/cmake/common.cmake")
include("${CMAKE_SOURCE_DIR}/cmake/dump_build_info.cmake")

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(test/ut)


##===============================================================
## Source Pacage 作成

execute_process(
  COMMAND hg log -r tip --template "{node|short}"
  OUTPUT_VARIABLE current_revision
)

execute_process(
  COMMAND date +%Y%m%d
  COMMAND tr -d '\n'
  OUTPUT_VARIABLE current_date
)

set(scythe_archive_prefix
    "${CMAKE_PROJECT_NAME}-${current_date}-${current_revision}")
set(scythe_archive_file "${scythe_archive_prefix}.tbz2")

add_custom_target(
  dist
  COMMAND hg archive -r tip -t tbz2 -p "${scythe_archive_prefix}" "${scythe_archive_file}" 
  COMMAND tar jxf ${scythe_archive_file}
  COMMAND cp -a "${CMAKE_SOURCE_DIR}/src/compile.c" "${scythe_archive_prefix}/src/"
  COMMAND tar jcf ${scythe_archive_file} "${scythe_archive_prefix}"
  COMMAND rm -rf ${scythe_archive_prefix}
  DEPENDS prepare
)
