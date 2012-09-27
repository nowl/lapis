CC = gcc
CFLAGS = -Wall -g -O2 -I/usr/local/include/SDL -fPIC
CFLAGS += -I/usr/include/lua5.1
INCLUDES = $(shell sdl-config --cflags)
LDFLAGS = -fPIC -shared

SRCS = \
	message.c \
	mempool.c \
	mempool_ext.c \
	hash.c \
	component.c \
	entity.c

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
