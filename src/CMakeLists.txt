
##===============================================================
## 型サイズの取得

include(CheckTypeSize)

check_type_size("short" SIZEOF_SHORT)
check_type_size("int" SIZEOF_INT)
check_type_size("long" SIZEOF_LONG)
check_type_size("long long" SIZEOF_LLONG)
check_type_size("intptr_t" SIZEOF_INTPTR_T)


##===============================================================
## ヘッダーファイルの確認

include(CheckIncludeFile)

check_include_file(execinfo.h HAVE_EXECINFO_H)


##===============================================================
## config.h の生成

configure_file(include/scythe/config.h.in include/scythe/config.h)


##===============================================================
## ヘッダの依存関係、コンパイルオプションの設定

include_directories(${CMAKE_CURRENT_BINARY_DIR}/include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

if(CMAKE_BUILD_TYPE STREQUAL "Release")
  add_definitions(${scythe_compile_definitions_release})
else(CMAKE_BUILD_TYPE STREQUAL "Release")
  add_definitions(${scythe_compile_definitions_debug})
endif(CMAKE_BUILD_TYPE STREQUAL "Release")


##===============================================================
## compile.c 生成

if(preinstalled_scythe_bin_path)
  set(scm_compiler ${preinstalled_scythe_bin_path})
  set(scm_compiler_opt "")

  set(compiler_scm_files "scythe/internal/compile.scm"
                         "scythe/internal/dynamic-env.scm"
                         "scythe/internal/macro.scm"
                         "scythe/internal/record.scm"
                         "scythe/internal/multiple-val.scm")

  set(compiler_scm_paths "")
  foreach(file ${compiler_scm_files})
    list(APPEND compiler_scm_paths "${CMAKE_SOURCE_DIR}/lib/${file}")
  endforeach(file)

  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/compile.c
    COMMAND ${ruby_bin_path} ${CMAKE_CURRENT_SOURCE_DIR}/tools/scmcompile.rb "${scm_compiler}${scm_compiler_opt}" ${CMAKE_SOURCE_DIR}/lib ${CMAKE_CURRENT_SOURCE_DIR}/compile.c.in ${CMAKE_CURRENT_SOURCE_DIR}/compile.c
    DEPENDS compile.c.in ${scm_compiler} ${compiler_scm_paths}
  )

  add_custom_target(
    prepare-src
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/compile.c
  )

  add_custom_target(
    prepare
    DEPENDS prepare-src
  )
endif(preinstalled_scythe_bin_path)

## compile.c を make clean で削除しないようにするための設定
set_directory_properties(PROPERTIES CLEAN_NO_CUSTOM TRUE)


##===============================================================
## ターゲット設定

add_library(scythe-lib OBJECT ${scythe_lib_src})

add_executable(scythe
               main.c $<TARGET_OBJECTS:scythe-lib>)


##===============================================================
## install 設定

install(
  TARGETS scythe
  DESTINATION bin
)


##===============================================================
## コンパイルオプション等の情報をテキストファイルに吐き出す

dump_build_info(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
