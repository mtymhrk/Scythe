CC       = gcc
CFLAGS   = -O2 -g -std=gnu99 -Wall -Wextra -Wformat=2 -Wstrict-aliasing=2 -Wcast-qual \
           -Wcast-align -Wwrite-strings -Wconversion -Wfloat-equal \
           -Wpointer-arith -Wswitch-enum -Wno-unused-parameter -Winline
INCLUDES =
SOURCES  = 
OBJS     = $(SOURCES:.c=.o)
TARGET   =
DOXYGEN  = doxygen
DOXYGEN_CONF = doxygen.conf

include Makefile.sources

all: $(TARGET) $(OBJS)

$(TARGET) $(OBJS): Makefile

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $<

.c.s:
	$(CC) -S -o $@ $(INCLUDES) $(CFLAGS) $<

.PHONY: clean depend test doxygen check-syntax

clean:
	-rm $(TARGET) $(OBJS)
	$(MAKE) -C ./test clean

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(SOURCES) > Makefile.depend

test:
#	$(MAKE)
	$(MAKE) -C ./test depend all run

doxygen:
	$(DOXYGEN) $(DOXYGEN_CONF)

check-syntax:
	LANG=C $(CC) -fsyntax-only $(INCLUDES) $(CFLAGS) $(CHK_SOURCES)

include Makefile.depend
