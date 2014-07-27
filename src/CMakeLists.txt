
##===============================================================
## 型サイズの取得

INCLUDE(CheckTypeSize)

CHECK_TYPE_SIZE("short" SIZEOF_SHORT)
CHECK_TYPE_SIZE("int" SIZEOF_INT)
CHECK_TYPE_SIZE("long" SIZEOF_LONG)
CHECK_TYPE_SIZE("long long" SIZEOF_LLONG)
CHECK_TYPE_SIZE("intptr_t" SIZEOF_INTPTR_T)


##===============================================================
## config.h の生成

configure_file(config.h.in config.h)


##===============================================================
## コンパイルオプション設定

ADD_DEFINITIONS(
  -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2
  -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal
  -Wpointer-arith -Wswitch-enum -Wno-unused-parameter
)

INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})


##===============================================================
## ターゲット設定

ADD_EXECUTABLE(scythe main.c ${scythe_lib_src})


##===============================================================
## src/tools 配下のツールを実行するための設定

IF(NOT scythe_sys_bin_path)
  SET(scythe_sys_bin_path ${CMAKE_CURRENT_BINARY_DIR}/scythe)
ENDIF(NOT scythe_sys_bin_path)

ADD_CUSTOM_TARGET(
  src_tools
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/tools RUBY=${ruby_bin_path} COMPILER=${scythe_sys_bin_path} all
)


##===============================================================
## src/test 配下にあるテストを実行するための設定

ADD_CUSTOM_TARGET(
  old_test_depend
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test EX_INCLUDES='-iquote ${CMAKE_CURRENT_BINARY_DIR}' CUTTER_INCLUDE_PATH='${cutter_include_path}' depend
)

ADD_CUSTOM_TARGET(
  old_test_all
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test EX_INCLUDES='-iquote ${CMAKE_CURRENT_BINARY_DIR}' CUTTER_INCLUDE_PATH='${cutter_include_path}' all
  DEPENDS old_test_depend
)

ADD_CUSTOM_TARGET(
  old_test_clean
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test clean
)

ADD_CUSTOM_TARGET(
  old_test
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test CUTTER='${cutter_bin_path}' run
  DEPENDS old_test_all
)


##===============================================================
## cmake で Makefile を生成した後に flymake 用に check-syntax ルールを追加す
## るスクリプトに必要な情報を設定
##
## <<FLYMAKE>> : scythe
