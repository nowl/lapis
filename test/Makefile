CC = gcc
CFLAGS = -Wall -g
INCLUDES = $(shell sdl-config --cflags) -I../
LDFLAGS = -Wl,-rpath,'.'
LIBS = $(shell sdl-config --libs) -lSDL_image -lSDL_mixer -lSDL_ttf -L../ -llapis

SRCS =	test1.c

OBJS = $(SRCS:.c=.o)

MAIN = test1


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
