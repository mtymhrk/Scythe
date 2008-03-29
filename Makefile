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


.PHONY: clean depend

clean:
	-rm $(TARGET) $(OBJS)

depend:
	$(CC) -MM $(INCLUDES) $(CFLAGS) $(SOURCES) > Makefile.depend

include Makefile.depend
