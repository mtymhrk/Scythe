LD       = ld
CC       = gcc
CFLAGS   = -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2 \
           -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal \
           -Wpointer-arith -Wswitch-enum -Wno-unused-parameter
CMACROS  = -DSCM_UNIT_TEST
INCLUDES = -iquote ../unity/src -iquote ../../src

SOURCES  =
OBJS     = $(SOURCES:.c=.o)
TARGET   =

include Makefile.sources

all: $(OBJS)

$(TARGET) $(OBJS): Makefile

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $(CMACROS) $<

.c.s:
	$(CC) -S -o $@ $(INCLUDES) $(CFLAGS) $(CHACROS) $<

.PHONY: clean depend test_list_files test_list_objs test doxygen check-syntax

clean:
	-rm $(TARGET) $(OBJS)

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(SOURCES) > Makefile.depend

test_list_files:
	@echo $(SOURCES)

test_list_objs:
	@echo $(OBJS)

check-syntax:
	$(CC) -fsyntax-only $(INCLUDES) $(CFLAGS) $(CMACROS) $(CHK_SOURCES)

include Makefile.depend
