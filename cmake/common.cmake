

SET(scythe_src_path src)
SET(scythe_lib_src memory.c object.c pair.c string.c symbol.c vector.c
                   miscobjects.c char.c charconv.c encoding.c vm.c
                   vmstack.c procedure.c reference.c core_subr.c iseq.c
                   chashtbl.c api.c port.c parser.c assembler.c exception.c
                   syntax.c compiler.c module.c core_modules.c
                   fixnum.c bignum.c number_parser.c)

SET(unit_test_src_path test/ut)
SET(unit_test_src test_module.c test_pair_and_lists.c test_strings.c
                  test_characters.c test_symbols.c test_equivalence.c
                  test_booleans.c test_fixnum.c test_bignum.c test_vectors.c
                  test_format.c)

SET(unity_dir test/unity)
SET(unity_src_path ${unity_dir}/src)
SET(unity_src unity.c unity_fixture.c)
