
##===============================================================
##

# configure_file(configure.h.in configure.h)


##===============================================================
## コンパイルオプション設定

ADD_DEFINITIONS(
  -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2
  -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal
  -Wpointer-arith -Wswitch-enum -Wno-unused-parameter
)

# INCLUDE_DIRECTORIES(${CMAKE_CURRENT_BINARY_DIR})


##===============================================================
## ターゲット設定

ADD_EXECUTABLE(scythe main.c ${scythe_lib_src})


##===============================================================
## src/test 配下にあるテストを実行するための設定

ADD_CUSTOM_TARGET(
  old_test_depend
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test depend
)

ADD_CUSTOM_TARGET(
  old_test_all
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test all
  DEPENDS old_test_depend
)

ADD_CUSTOM_TARGET(
  old_test_clean
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test clean
)

ADD_CUSTOM_TARGET(
  old_test
  COMMAND make -C ${CMAKE_CURRENT_SOURCE_DIR}/test run
  DEPENDS old_test_all
)


##===============================================================
## cmake で Makefile を生成した後に flymake 用に check-syntax ルールを追加す
## るスクリプトに必要な情報を設定
##
## <<FLYMAKE>> : scythe
