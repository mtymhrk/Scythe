CC       = gcc
CFLAGS   = -O2 -Wall
INCLUDES =
SOURCES  = 
OBJS     = $(SOURCES:.c=.o)
TARGET   =

include Makefile.sources

all: $(TARGET) $(OBJS)

$(TARGET) $(OBJS): Makefile

.c.o:
	$(CC) -c -o $@ $(INCLUDES) $(CFLAGS) $<


.PHONY: clean depend test check-syntax

clean:
	-rm $(TARGET) $(OBJS)
	$(MAKE) -C ./test clean

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(SOURCES) > Makefile.depend

test:
	$(MAKE) CFLAGS="-Wall -g"
	$(MAKE) -C ./test depend all run

check-syntax:
	$(CC) -fsyntax-only $(INCLUDES) $(CFLAGS) $(CHK_SOURCES)

include Makefile.depend
