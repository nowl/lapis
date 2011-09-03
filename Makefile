CC = gcc
CFLAGS = -Wall -g -O2 -I/usr/local/include/SDL -fPIC
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

lapis_wrap.c: lapis.i
	swig -lua lapis.i

clean:
	rm -f *.o *~ $(MAIN) lapis_wrap.c

depend: $(SRCS)
	$(CC) -M $(CFLAGS) $(INCLUDES) $^ > $@

include depend
