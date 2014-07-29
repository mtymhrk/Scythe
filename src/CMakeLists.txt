
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

configure_file(config.h.in config.h)


##===============================================================
## コンパイルオプション設定

add_definitions(
  -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2
  -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal
  -Wpointer-arith -Wswitch-enum -Wno-unused-parameter
)

include_directories(${CMAKE_CURRENT_BINARY_DIR})


##===============================================================
## ターゲット設定

add_executable(scythe main.c ${scythe_lib_src})


##===============================================================
## src/tools 配下のツールを実行するための設定

if(NOT scythe_sys_bin_path)
  SET(scythe_sys_bin_path ${CMAKE_CURRENT_BINARY_DIR}/scythe)
endif(NOT scythe_sys_bin_path)

add_custom_target(
  src_tools
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/tools RUBY=${ruby_bin_path} COMPILER=${scythe_sys_bin_path} all
)


##===============================================================
## src/test 配下にあるテストを実行するための設定

add_custom_target(
  old_test_depend
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test EX_INCLUDES='-iquote ${CMAKE_CURRENT_BINARY_DIR}' CUTTER_INCLUDE_PATH='${cutter_include_path}' depend
)

add_custom_target(
  old_test_all
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test EX_INCLUDES='-iquote ${CMAKE_CURRENT_BINARY_DIR}' CUTTER_INCLUDE_PATH='${cutter_include_path}' all
  DEPENDS old_test_depend
)

add_custom_target(
  old_test_clean
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test clean
)

add_custom_target(
  old_test
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test CUTTER='${cutter_bin_path}' run
  DEPENDS old_test_all
)


##===============================================================
## cmake で Makefile を生成した後に flymake 用に check-syntax ルールを追加す
## るスクリプトに必要な情報を設定
##
## <<FLYMAKE>> : scythe
