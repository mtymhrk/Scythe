CC       = gcc
LD       = ld
CFLAGS   = -O2 -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2 \
           -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal \
           -Wpointer-arith -Wswitch-enum -Wno-unused-parameter \
           -fPIC -g -DSCM_UNIT_TEST
INCLUDES = -I/usr/local/include/cutter -iquote ..
EX_INCLUDES = -iquote ../../build/src/
LDFLAGS  = -lcunit -g
CUTTER   = cutter
TEST_TARGET_DIR = ..
TEST_CASE_LIB_DIR = .lib

TEST_TARGET_SRC_ = memory.c object.c pair.c string.c symbol.c vector.c \
                   miscobjects.c char.c charconv.c \
                   encoding.c vm.c vmstack.c procedure.c reference.c \
                   core_subr.c iseq.c chashtbl.c api.c \
                   port.c parser.c assembler.c exception.c syntax.c \
                   compiler.c module.c core_modules.c\
                   number_common.c fixnum.c bignum.c number.c \

TEST_TARGET_SRC = $(addprefix $(TEST_TARGET_DIR)/, $(TEST_TARGET_SRC_))
TEST_CASE_SRC = test_pair.c test_string.c test_symbol.c test_nil.c \
                test_vector.c test_eof.c test_bool.c test_char.c \
                test_charconv.c test_string_utf8.c test_string_ucs4.c \
                test_string_ascii.c test_string_eucjp.c test_string_sjis.c \
                test_memory.c test_vm.c test_iseq.c \
                test_chashtbl.c test_fileport.c test_stringport.c \
                test_parser.c test_assembler.c test_undef.c test_bignum.c \
                test_module.c test_compiler.c test_eval.c test_eval_proc.c

TEST_TARGET_OBJ = $(TEST_TARGET_SRC:.c=.o)
TEST_CASE_OBJ = $(TEST_CASE_SRC:.c=.o)
TEST_CASE_LIB = $(TEST_CASE_SRC:.c=.so)



all: $(TEST_CASE_LIB)

test_pair.so : test_pair.o $(TEST_TARGET_OBJ)
test_string.so : test_string.o $(TEST_TARGET_OBJ)
test_symbol.so : test_symbol.o $(TEST_TARGET_OBJ)
test_nil.so : test_nil.o $(TEST_TARGET_OBJ)
test_pretty_print.so : test_pretty_print.o $(TEST_TARGET_OBJ)
test_ibuffer.so : test_ibuffer.o $(TEST_TARGET_OBJ)
test_lexer.so : test_lexer.o $(TEST_TARGET_OBJ)
test_parser.so : test_parser.o $(TEST_TARGET_OBJ)
test_vector.so : test_vector.o $(TEST_TARGET_OBJ)
test_eof.so : test_eof.o  $(TEST_TARGET_OBJ)
test_bool.so : test_bool.o  $(TEST_TARGET_OBJ)
test_char.so : test_char.o  $(TEST_TARGET_OBJ)
test_fileport.so : test_fileport.o  $(TEST_TARGET_OBJ)
test_stringport.so : test_stringport.o  $(TEST_TARGET_OBJ)
test_charconv.so : test_charconv.o  $(TEST_TARGET_OBJ)
test_string_utf8.so : test_string_utf8.o $(TEST_TARGET_OBJ)
test_string_ucs4.so : test_string_ucs4.o $(TEST_TARGET_OBJ)
test_string_ascii.so : test_string_ascii.o $(TEST_TARGET_OBJ)
test_string_eucjp.so : test_string_eucjp.o $(TEST_TARGET_OBJ)
test_string_sjis.so : test_string_sjis.o $(TEST_TARGET_OBJ)
test_memory.so : test_memory.o $(TEST_TARGET_OBJ)
test_binding.so : test_binding.o $(TEST_TARGET_OBJ)
test_vm.so : test_vm.o $(TEST_TARGET_OBJ)
test_iseq.so: test_iseq.o $(TEST_TARGET_OBJ)
test_chashtbl.so: test_chashtbl.o $(TEST_TARGET_OBJ)
test_assembler.so: test_assembler.o $(TEST_TARGET_OBJ)
test_undef.so: test_undef.o $(TEST_TARGET_OBJ)
test_bignum.so: test_bignum.o $(TEST_TARGET_OBJ)
test_module.so: test_module.o $(TEST_TARGET_OBJ)
test_compiler.so: test_compiler.o $(TEST_TARGET_OBJ)
test_eval.so: test_eval.o $(TEST_TARGET_OBJ)
test_eval_proc.so: test_eval_proc.o $(TEST_TARGET_OBJ)

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(EX_INCLUDES) $(CFLAGS) $<

%.so:
	-if [ ! -d $(TEST_CASE_LIB_DIR) ]; then mkdir $(TEST_CASE_LIB_DIR); fi
	$(CC) -shared -o $(TEST_CASE_LIB_DIR)/$@ $^

.SUFFIXES: .c .o .so
.PHONY: run clean depend check-syntax

run:
	$(CUTTER) $(TEST_CASE_LIB_DIR)/

clean:
	-rm $(TEST_TARGET_OBJ) $(TEST_CASE_OBJ)
	-cd $(TEST_CASE_LIB_DIR); rm $(TEST_CASE_LIB)

depend:
	$(CC) -MM $(INCLUDES) $(EX_INCLUDES) $(CFLAGS) $(TEST_TARGET_SRC)  \
	      | sed 's/^\([^ ]\)/$(TEST_TARGET_DIR)\/\1/' > Makefile.depend
	$(CC) -MM $(INCLUDES) $(EX_INCLUDES) $(CFLAGS) $(TEST_CASE_SRC) >> Makefile.depend

check-syntax:
	LANG=C $(CC) -fsyntax-only $(INCLUDES) $(EX_INCLUDES) $(CFLAGS) $(CHK_SOURCES)

include Makefile.depend
