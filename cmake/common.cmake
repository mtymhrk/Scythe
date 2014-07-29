
##===============================================================
## ビルドやテストに必要となるソースファイルのパス

set(scythe_src_path src)
set(scythe_lib_src memory.c object.c pair.c string.c symbol.c vector.c
                   miscobjects.c char.c charconv.c encoding.c vm.c
                   vmstack.c procedure.c reference.c core_subr.c iseq.c
                   chashtbl.c api.c port.c parser.c assembler.c exception.c
                   syntax.c compiler.c module.c core_modules.c
                   fixnum.c bignum.c number_parser.c)

set(unit_test_src_path test/ut)
set(unit_test_src test_module.c test_pair_and_lists.c test_strings.c
                  test_characters.c test_symbols.c test_equivalence.c
                  test_booleans.c test_fixnum.c test_bignum.c test_vectors.c
                  test_format.c test_exceptions.c)

set(unity_dir test/unity)
set(unity_src_path ${unity_dir}/src)
set(unity_src unity.c unity_fixture.c)


##===============================================================
## テスト等で必要になる外部ツールの設定

####################
## Ruby

find_program(ruby_bin_path ruby
             PATHS /usr/local/bin)

if(NOT ruby_bin_path)
  message(WARNING "Ruby script language is not found.")
endif(NOT ruby_bin_path)

####################
## Cutter

find_path(cutter_include_path cutter.h
          PATHS /usr/local/include
          PATH_SUFFIXES cutter)

find_program(cutter_bin_path cutter
             PATHS /usr/local/bin)

if((NOT cutter_include_path) OR (NOT cutter_bin_path))
  message(WARNING "Cutter testing framework is not found.")
endif((NOT cutter_include_path) OR (NOT cutter_bin_path))

####################
## Scythe

find_program(scythe_sys_bin_path scythe
             PATHS /usr/local/bin)

if(NOT scythe_sys_bin_path)
  message(WARNING "System installed Scythe is not found.")
endif(NOT scythe_sys_bin_path)
