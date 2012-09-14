CC = gcc
CFLAGS = -Wall -g -O2 -I/usr/local/include/SDL -fPIC
CFLAGS += -I/usr/include/lua5.1
INCLUDES = $(shell sdl-config --cflags)
LDFLAGS = -fPIC -shared

SRCS = \
	engine.c \
	utils.c \
	lapis.c \
	game_state.c \
	sdl_graphics_context.c \
	sdl_font.c \
	game_object.c \
	message.c \
	mainloop.c \
	image_loader.c \
	image_render_set.c \
	collide.c \
	astar.c \
	random.c \
	aatree.c \
	list.c \
	ref.c \
	lapis_wrap.c \
	lua.c \
	shadow_los.c

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
