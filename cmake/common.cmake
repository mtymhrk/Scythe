
##===============================================================
## ビルドやテストに必要となるソースファイルのパス

set(scythe_src_path src)
set(scythe_lib_src memory.c object.c pair.c string.c symbol.c vector.c
                   miscobjects.c char.c charconv.c encoding.c vm.c
                   vmstack.c procedure.c reference.c core_subr.c iseq.c
                   chashtbl.c api.c port.c parser.c assembler.c exception.c
                   syntax.c compiler.c module.c core_modules.c
                   fixnum.c bignum.c number_parser.c)


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
## Scythe

find_program(scythe_sys_bin_path scythe
             PATHS /usr/local/bin)

if(NOT scythe_sys_bin_path)
  message(WARNING "System installed Scythe is not found.")
endif(NOT scythe_sys_bin_path)
