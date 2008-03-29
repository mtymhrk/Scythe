CC       = gcc
LD       = ld
CFLAGS   = -O2 -Wall
INCLUDES = -iquote ..
LDFLAGS  = -lcunit
TEST_TARGET_DIR = ..

include $(TEST_TARGET_DIR)/Makefile.sources

TEST_TARGET_SRC = $(addprefix $(TEST_TARGET_DIR)/, $(SOURCES))
TEST_CASE_SRC = test_pair.c test_string.c test_symbol.c test_nil.c test_hash.c \
                test_integer.c test_pretty_print.c test_basiclist.c
TEST_CODE_SRC = main.c $(TEST_CASE_SRC)
TEST_TARGET_OBJ = $(TEST_TARGET_SRC:.c=.o) 
TEST_CASE_OBJ = $(TEST_CASE_SRC:.c=.o) 
TEST_CODE_OBJ = $(TEST_CODE_SRC:.c=.o) 
TARGET = $(TEST_CASE_SRC:.c=)

all: $(TARGET)

test_pair : test_pair.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_string : test_string.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_symbol : test_symbol.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_nil : test_nil.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_hash : test_hash.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_integer : test_integer.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_pretty_print : test_pretty_print.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

test_basiclist : test_basiclist.o main.o $(TEST_TARGET_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $<


.PHONY: run clean depend

run:
	$(MAKE) clean depend
	$(MAKE)
	./run.sh $(TARGET)

clean:
	-rm $(TEST_TARGET_OBJ) $(TEST_CODE_OBJ) $(TARGET)

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(TEST_TARGET_SRC) $(TEST_CODE_SRC) > Makefile.depend

include Makefile.depend
