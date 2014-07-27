
SET(test_runner_src test_runner.c)
SET(test_runner_exe test.run)
SET(test_result_file result)
SET(runner_gen_script
    ${CMAKE_SOURCE_DIR}/${unity_dir}/auto/generate_unity_fixture_runner.rb)
SET(run_script ${CMAKE_SOURCE_DIR}/${unity_dir}/auto/invoke_runner.rb)

FIND_PROGRAM(ruby_bin_path ruby
             PATHS /usr/local/bin)

IF(NOT ruby_bin_path)
  MESSAGE(WARNING "Ruby script language is not found")
ENDIF(NOT ruby_bin_path)


##===============================================================
## コンパイルオプション設定

ADD_DEFINITIONS(
  -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2
  -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal
  -Wpointer-arith -Wswitch-enum -Wno-unused-parameter -DSCM_UNIT_TEST
)

INCLUDE_DIRECTORIES(${CMAKE_BINARY_DIR}/${scythe_src_path})
INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/${unity_src_path})

## string.h といった C 標準のインクルードファイルとバッティングするファイル
## 名を使用しているので、-iquote オプションでインクルードパスを指定したいが、
## cmake の INCLUDE_DIRECTORIES コマンドで指定したディレクトリは -iquote で
## はなく -I で指定されるため、CMAKE_C_FLAGS を変更して gcc のオプションを指
## 定する。
SET(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS} -iquote ${CMAKE_SOURCE_DIR}/${scythe_src_path}")


##===============================================================
## ターゲット設定

## unity_src 変数の各ソースファイル名を絶対パスに変換
STRING(REGEX REPLACE ";" ";${CMAKE_SOURCE_DIR}/${unity_src_path}/"
       unity_files ";${unity_src}")
STRING(REGEX REPLACE "^;" ""
        unity_files "${unity_files}")

## scythe_lib_src 変数の各ソースファイル名を絶対パスに変換
STRING(REGEX REPLACE ";" ";${CMAKE_SOURCE_DIR}/${scythe_src_path}/"
       scythe_files ";${scythe_lib_src}")
STRING(REGEX REPLACE "^;" ""
       scyte_files "${scythe_files}")

## unit_test_src 変数の各ソースファイル名を絶対パスに変換
STRING(REGEX REPLACE ";" ";${CMAKE_SOURCE_DIR}/${unit_test_src_path}/"
       unit_test_files ";${unit_test_src}")
STRING(REGEX REPLACE "^;" ""
       unit_test_files "${unit_test_files}")


## テストを走らせたときにできるログファイル ${test_result_file}.testpass、
## ${test_result_file}.testfail を削除するターゲット
ADD_CUSTOM_TARGET(
  delete_result_file
  COMMAND sh -c 'for f in ${test_result_file}.*\; do if [ -e $$f ]\; then rm $$f\; fi\; done'
)


## テストの実行ファイルのビルド、テストログファイルの削除を行い、テストを実
## 行するターゲット
ADD_CUSTOM_TARGET(
  unit
  COMMAND ${ruby_bin_path} ${run_script} ./${test_runner_exe} ${test_result_file}
  DEPENDS ${test_runner_exe} delete_result_file
)


## テストの実行ファイルのビルド
ADD_EXECUTABLE(${test_runner_exe} EXCLUDE_FROM_ALL
                                  ${test_runner_src}
                                  ${unit_test_src}
                                  ${unity_files}
                                  ${scythe_files})


## テストランナーを自動生成するための設定

## テストランナーのソースファイルがビルド中に生成されることを cmake に教える
SET_SOURCE_FILES_PROPERTIES(${test_runner_src}
                            PROPERTIES GENERATED TRUE)

## テストランナーのソースを生成するスクリプトの実行
ADD_CUSTOM_COMMAND(
   OUTPUT ${test_runner_src}
   COMMAND sh -c '${ruby_bin_path} ${runner_gen_script} ${unit_test_files} > ${test_runner_src}'
   DEPENDS ${runner_gen_script} ${unit_test_files}
)


##===============================================================
## cmake で Makefile を生成した後に flymake 用に check-syntax ルールを追加す
## るスクリプトに必要な情報を設定
##
## <<FLYMAKE>> : test.run
