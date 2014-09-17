
##===============================================================
## ビルドやテストに必要となるソースファイルのパス

set(scythe_src_path src)
set(scythe_lib_src memory.c object.c pair.c string.c symbol.c vector.c
                   miscobjects.c char.c encoding.c vm.c vmstack.c vminst.c
                   procedure.c reference.c core_subr.c iseq.c chashtbl.c api.c
                   port.c parser.c assembler.c exception.c syntax.c compiler.c
                   module.c core_modules.c fixnum.c bignum.c number_parser.c)


##===============================================================
## コンパイラの設定

set(scythe_compile_options
  -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2
  -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal
  -Wpointer-arith -Wswitch-enum -Wno-unused-parameter)

find_program(gcc_bin_path gcc PATHS /usr/local/bin)

if(gcc_bin_path)
  set(CMAKE_C_COMPILER ${gcc_bin_path})
else(gcc_bin_path)
  message(WARNING "GCC is not found")
endif(gcc_bin_path)

foreach(flg ${scythe_compile_options})
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${flg}")
endforeach(flg)


##===============================================================
## テスト等で必要になる外部ツールの設定

####################
## Ruby

find_program(ruby_bin_path ruby
             PATHS /usr/local/bin)

if(NOT ruby_bin_path)
  message(WARNING "Ruby script language is not found.")
endif(NOT ruby_bin_path)
