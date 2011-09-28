CC = gcc
CFLAGS = -Wall -g -O2 -fPIC
INCLUDES = $(shell sdl-config --cflags)
LDFLAGS = -fPIC -shared
LIBS =

SRCS = \
	lapis.c \
	sdl_graphics_context.c

OBJS = $(SRCS:.c=.o)

MAIN = liblapis.so


.SUFFIXES: .c .o .i

.c.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $< -o $@

.PHONY: depend clean

$(MAIN): $(OBJS)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(MAIN) $(OBJS) $(LDFLAGS) $(LIBS)

clean:
	rm -f *.o *~ $(MAIN)

depend: $(SRCS)
	$(CC) -M $(CFLAGS) $(INCLUDES) $^ > $@

include depend
