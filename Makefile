CC = gcc
CFLAGS = -Wall -g
INCLUDES = $(shell sdl-config --cflags)
LDFLAGS = -fPIC -shared
LIBS =

SRCS = \
	engine.c \
	utils.c \
	lapis.c \
	game_state.c \
	sdl_graphics_context.c \
	sdl_font.c \
	sdl_sound.c \
	game_object.c \
	message.c \
	mainloop.c \
	image_loader.c \
	image_render_set.c \
	collide.c \
	astar.c

OBJS = $(SRCS:.c=.o)

MAIN = liblapis.so


.SUFFIXES: .c .o

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
