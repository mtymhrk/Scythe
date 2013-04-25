LD       = ld
CC       = gcc
CFLAGS   = -O2 -g -std=c99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2 \
           -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal \
           -Wpointer-arith -Wswitch-enum -Wno-unused-parameter
CMACROS  =
INCLUDES =
SOURCES  = 
OBJS     = $(SOURCES:.c=.o)
TARGET   = 
TEST_TARGET = $(OBJS)

include Makefile.sources

all: $(TARGET) $(OBJS)

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $(CMACROS) $<

.c.s:
	$(CC) -S -o $@ $(INCLUDES) $(CFLAGS) $(CMACROS) $<

$(TARGET): $(OBJS)
	$(CC) -o $@ $(OBJS)

.PHONY: clean depend test_build test_list_objs

clean:
	-rm $(TARGET) $(OBJS)

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(SOURCES) > Makefile.depend

test_build: $(TEST_TARGET)

test_list_objs:
	@echo $(TEST_TARGET)

include Makefile.depend
