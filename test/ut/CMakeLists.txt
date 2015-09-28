
set(unit_test_aux_src test.c)
set(unit_test_src_apnd test_eof.c
                       test_nil.c
                       test_port_input.c
                       test_port_output.c
                       test_undef.c

                       test_api_bignum.c
                       test_api_booleans.c
                       test_api_characters.c
                       test_api_equivalence.c
                       test_api_exceptions.c
                       test_api_fixnum.c
                       test_api_format.c
                       test_api_input.c
                       test_api_module.c
                       test_api_nil.c
                       test_api_output.c
                       test_api_pair_and_lists.c
                       test_api_ports.c
                       test_api_strings.c
                       test_api_symbols.c
                       test_api_vectors.c

                       test_exec_compiler.c
                       test_exec_proc.c
                       test_exec_syntax.c)

set(unit_test_src "")

foreach(file ${scythe_lib_src})
  list(APPEND unit_test_src test_${file})
endforeach(file)

foreach(file ${unit_test_src_apnd})
  list(APPEND unit_test_src ${file})
endforeach(file)

set(unity_dir test/unity)
set(unity_src_rpath ${unity_dir}/src)
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

include_directories(${CMAKE_SOURCE_DIR}/${unity_src_rpath})
include_directories(${CMAKE_BINARY_DIR}/${scythe_src_rpath}/include)
include_directories(${CMAKE_SOURCE_DIR}/${scythe_src_rpath}/include)
include_directories(${CMAKE_SOURCE_DIR}/${scythe_src_rpath})

add_definitions(${scythe_compile_definitions_debug})


##===============================================================
## ターゲット設定

## unity_src 変数の各ソースファイル名を絶対パスに変換
set(unity_files "")
foreach(file ${unity_src})
  list(APPEND unity_files ${CMAKE_SOURCE_DIR}/${unity_src_rpath}/${file})
endforeach(file)

## unit_test_src 変数の各ソースファイル名を絶対パスに変換
set(unit_test_files "")
foreach(file ${unit_test_src})
  list(APPEND unit_test_files ${CMAKE_CURRENT_SOURCE_DIR}/${file})
endforeach(file)

##
## テストを走らせたときにできるログファイルを削除するターゲット
##
add_custom_target(
  unit_delete_result_file
  COMMAND if [ -f ${test_result_file} ]\;then rm ${test_result_file}\; fi
)

##
## テストの実行ファイルのビルド、テストログファイルの削除を行い、テストを実
## 行するターゲット
##
add_custom_target(
  unit
  DEPENDS unit_delete_result_file ${test_result_file}
)

add_custom_command(
  OUTPUT ${test_result_file}
  COMMAND ${ruby_bin_path} ${run_script} ${test_result_file} ./${test_runner_exe}
  DEPENDS ${test_runner_exe}
)

##
## テストログファイルをもとに結果を表示するターゲット
##
add_custom_target(
  unit_summary
  COMMAND ${ruby_bin_path} ${summarize_script} ${test_result_file}
  DEPENDS ${test_result_file}
)

##
## テストランナーのビルド
##
add_executable(${test_runner_exe} EXCLUDE_FROM_ALL
                                  ${test_runner_src}
                                  ${unit_test_aux_src}
                                  ${unit_test_src}
                                  ${unity_files})

## テストランナーのソースを生成するスクリプトの実行
add_custom_command(
   OUTPUT ${test_runner_src}
   COMMAND ${ruby_bin_path} ${runner_gen_script} ${unit_test_files} > ${test_runner_src}
   DEPENDS ${runner_gen_script} ${unit_test_files}
)


##===============================================================
## コンパイルオプション等の情報をテキストファイルに吐き出す

dump_build_info(${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
