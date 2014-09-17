
set(unit_test_aux_src test.c)
set(unit_test_src_apnd test_api_module.c test_api_pair_and_lists.c
                       test_api_strings.c test_api_characters.c
                       test_api_symbols.c test_api_equivalence.c
                       test_api_booleans.c test_api_fixnum.c test_api_bignum.c
                       test_api_vectors.c test_api_format.c
                       test_api_exceptions.c test_api_assembler.c
                       test_api_ports.c test_api_input.c test_api_output.c
                       test_api_nil.c test_api_undef.c
                       test_exec_compiler.c test_exec_syntax.c test_exec_proc.c)
string(REGEX REPLACE ";" ";test_" unit_test_src ";${scythe_lib_src}")
string(REGEX REPLACE "^;" "" unit_test_src "${unit_test_src}")
set(unit_test_src "${unit_test_src};${unit_test_src_apnd}")

set(unity_dir test/unity)
set(unity_src_path ${unity_dir}/src)
set(unity_src unity.c unity_fixture.c)

set(test_runner_src test_runner.c)
set(test_runner_exe test.run)
set(test_result_file result.txt)
set(runner_gen_script
    ${CMAKE_SOURCE_DIR}/${unity_dir}/auto/generate_unity_fixture_runner.rb)
set(run_script ${CMAKE_SOURCE_DIR}/${unity_dir}/auto/invoke_runner.rb)
set(summarize_script ${CMAKE_SOURCE_DIR}/${unity_dir}/auto/summarize_result.rb)


##===============================================================
## ヘッダの依存関係、コンパイルオプション設定

include_directories(${CMAKE_BINARY_DIR}/${scythe_src_path})
include_directories(${CMAKE_SOURCE_DIR}/${unity_src_path})

## string.h といった C 標準のインクルードファイルとバッティングするファイル
## 名を使用しているので、-iquote オプションでインクルードパスを指定したいが、
## cmake の INCLUDE_DIRECTORIES コマンドで指定したディレクトリは -iquote で
## はなく -I で指定されるため、CMAKE_C_FLAGS を変更して gcc のオプションを指
## 定する。
set(CMAKE_C_FLAGS
    "${CMAKE_C_FLAGS} -iquote ${CMAKE_SOURCE_DIR}/${scythe_src_path}")


##===============================================================
## ターゲット設定

## unity_src 変数の各ソースファイル名を絶対パスに変換
string(REGEX REPLACE ";" ";${CMAKE_SOURCE_DIR}/${unity_src_path}/"
       unity_files ";${unity_src}")
string(REGEX REPLACE "^;" ""
        unity_files "${unity_files}")

## unit_test_src 変数の各ソースファイル名を絶対パスに変換
string(REGEX REPLACE ";" ";${CMAKE_CURRENT_SOURCE_DIR}/"
       unit_test_files ";${unit_test_src}")
string(REGEX REPLACE "^;" ""
       unit_test_files "${unit_test_files}")

##
## テストを走らせたときにできるログファイルを削除するターゲット
##
add_custom_target(
  unit_delete_result_file
  COMMAND sh -c "if [ -f ${test_result_file} ]\;then rm ${test_result_file}\; fi"
)

##
## テストの実行ファイルのビルド、テストログファイルの削除を行い、テストを実
## 行するターゲット
##
add_custom_target(
  unit
  COMMAND ${ruby_bin_path} ${run_script} ${test_result_file} ./${test_runner_exe}
  DEPENDS ${test_runner_exe} unit_delete_result_file
)

##
## テストログファイルをもとに結果を表示するターゲット
##
add_custom_target(
  unit_summary
  COMMAND sh -c "${ruby_bin_path} ${summarize_script} ${test_result_file}"
)

##
## テストランナーのビルド
##
add_executable(${test_runner_exe} EXCLUDE_FROM_ALL
                                  ${test_runner_src}
                                  ${unit_test_aux_src}
                                  ${unit_test_src}
                                  ${unity_files})

## テストランナーのソースファイルがビルド中に生成されることを cmake に教える
set_source_files_properties(${test_runner_src}
                            PROPERTIES GENERATED TRUE)

## テストランナーのソースを生成するスクリプトの実行
add_custom_command(
   OUTPUT ${test_runner_src}
   COMMAND sh -c '${ruby_bin_path} ${runner_gen_script} ${unit_test_files} > ${test_runner_src}'
   DEPENDS ${runner_gen_script} ${unit_test_files}
)


##===============================================================
## cmake で Makefile を生成した後に flymake 用に check-syntax ルールを追加す
## るスクリプトに必要な情報を設定
##
## <<FLYMAKE>> : test.run
