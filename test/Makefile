CC       = gcc
LD       = ld
CFLAGS   = -O2 -Wall -fPIC -g
INCLUDES = -I/usr/local/include/cutter -iquote ..
LDFLAGS  = -lcunit -g
CUTTER   = cutter
TEST_TARGET_DIR = ..
TEST_CASE_LIB_DIR = .lib

include $(TEST_TARGET_DIR)/Makefile.sources

TEST_TARGET_SRC = $(addprefix $(TEST_TARGET_DIR)/, $(SOURCES))
TEST_CASE_SRC = test_pair.c test_string.c test_symbol.c test_nil.c \
                test_basichash.c test_integer.c test_pretty_print.c \
                test_basiclist.c test_ibuffer.c test_lexer.c test_parser.c \
                test_vector.c test_eof.c test_bool.c test_char.c \
                test_fileport.c test_stringport.c test_charconv.c test_ucs4.c \
                test_utf8.c test_string_utf8.c
TEST_TARGET_OBJ = $(TEST_TARGET_SRC:.c=.o) 
TEST_CASE_OBJ = $(TEST_CASE_SRC:.c=.o) 
TEST_CASE_LIB = $(TEST_CASE_SRC:.c=.so)



all: $(TEST_CASE_LIB)

test_pair.so : test_pair.o $(TEST_TARGET_OBJ)
test_string.so : test_string.o $(TEST_TARGET_OBJ)
test_symbol.so : test_symbol.o $(TEST_TARGET_OBJ)
test_nil.so : test_nil.o $(TEST_TARGET_OBJ)
test_basichash.so : test_basichash.o $(TEST_TARGET_OBJ)
test_integer.so : test_integer.o $(TEST_TARGET_OBJ)
test_pretty_print.so : test_pretty_print.o $(TEST_TARGET_OBJ)
test_basiclist.so : test_basiclist.o $(TEST_TARGET_OBJ)
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
test_ucs4.so : test_ucs4.o $(TEST_TARGET_DIR)/ucs4.o
test_utf8.so : test_utf8.o $(TEST_TARGET_DIR)/utf8.o
test_string_utf8.so : test_string_utf8.o $(TEST_TARGET_OBJ)

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $<

%.so:
	-if [ ! -d $(TEST_CASE_LIB_DIR) ]; then mkdir $(TEST_CASE_LIB_DIR); fi
	$(CC) -shared -o $(TEST_CASE_LIB_DIR)/$@ $^

.SUFFIXES: .c .o .so
.PHONY: run clean depend

run:
	$(CUTTER) $(TEST_CASE_LIB_DIR)/

clean:
	-rm $(TEST_TARGET_OBJ) $(TEST_CASE_OBJ)
	-cd $(TEST_CASE_LIB_DIR); rm $(TEST_CASE_LIB)

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(TEST_TARGET_SRC) $(TEST_CODE_SRC) > Makefile.depend

include Makefile.depend
