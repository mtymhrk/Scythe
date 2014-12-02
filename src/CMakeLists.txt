
##===============================================================
## 型サイズの取得

include(CheckTypeSize)

check_type_size("short" SIZEOF_SHORT)
check_type_size("int" SIZEOF_INT)
check_type_size("long" SIZEOF_LONG)
check_type_size("long long" SIZEOF_LLONG)
check_type_size("intptr_t" SIZEOF_INTPTR_T)


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
## ターゲット設定

add_library(scythe-lib OBJECT ${scythe_lib_src})

add_executable(scythe
               main.c $<TARGET_OBJECTS:scythe-lib>)
add_executable(scythe-compile-file
               main_compile_file.c $<TARGET_OBJECTS:scythe-lib>)


##===============================================================
## src/tools 配下のツールを実行するための設定

set(scythe_compiler "${CMAKE_CURRENT_BINARY_DIR}/scythe-compile-file")

add_custom_target(
  src_tools
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/tools RUBY=${ruby_bin_path} COMPILER=${scythe_compiler} all
)


##===============================================================
## コンパイルオプション等の情報をテキストファイルに吐き出す

dump_build_info(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
