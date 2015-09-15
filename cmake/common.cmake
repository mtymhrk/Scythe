
##===============================================================
## ビルドやテストに必要となるソースファイルのパス

set(scythe_src_path src)
set(scythe_lib_src api.c
                   assembler.c
                   bedrock.c
                   bignum.c
                   char.c
                   chashtbl.c
                   compiler.c
                   core_modules.c
                   core_subr.c
                   earray.c
                   encoding.c
                   equivalence.c
                   exception.c
                   file.c
                   fixnum.c
                   format.c
                   impl_utils.c
                   iseq.c
                   marshal.c
                   memory.c
                   miscobjects.c
                   module.c
                   number.c
                   number_parser.c
                   object.c
                   pair.c
                   parser.c
                   port.c
                   procedure.c
                   refstk.c
                   scythe.c
                   string.c
                   symbol.c
                   syntax.c
                   vector.c
                   vm.c
                   vminst.c
                   vmstack.c

                   compile_data.c
                   macro_data.c
                   repl_data.c
                   scheme_base_data.c)


##===============================================================
## コンパイラの設定

set(CMAKE_C_FLAGS "-g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2 -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal -Wpointer-arith -Wswitch-enum -Wno-unused-parameter")
set(CMAKE_C_FLAGS_DEBUG "-O0")
set(CMAKE_C_FLAGS_RELEASE "-O2")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2")

set(scythe_compile_definitions_debug -DSCM_DEBUG)
set(scythe_compile_definitions_release -DNDEBUG)

find_program(gcc_bin_path gcc PATHS /usr/local/bin)

if(gcc_bin_path)
  set(CMAKE_C_COMPILER ${gcc_bin_path})
else(gcc_bin_path)
  message(WARNING "GCC is not found")
endif(gcc_bin_path)


##===============================================================
## テスト等で必要になる外部ツールの設定

####################
## Ruby

find_program(ruby_bin_path ruby
             PATHS /usr/local/bin)

if(NOT ruby_bin_path)
  message(WARNING "Ruby script language is not found.")
endif(NOT ruby_bin_path)


##===============================================================
## デフォルトの build type の設定

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING
    "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel."
      FORCE)
endif(NOT CMAKE_BUILD_TYPE)
