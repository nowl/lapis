CXX = g++
CXXFLAGS = -Wall -g -O2 -fPIC -std=c++0x -DNOCLEANUP
INCLUDES = $(shell sdl-config --cflags)
LDFLAGS = -shared

LIBS = $(shell sdl-config --libs) -lSDL_ttf -lSDL_image -lSDL_mixer -lGL -lGLU

LIBSRCS = \
	sdl_driver.cpp \
	component.cpp \
	engine.cpp \
	message.cpp \
	hash.cpp \
	image_loader.cpp

LIBOBJS = $(LIBSRCS:.cpp=.o)

MAINLIB = liblapis.so

all: $(MAINLIB)

.SUFFIXES: .cpp .o .i

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

.PHONY: depend clean all

$(MAINLIB): $(LIBOBJS)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAINLIB) $(LIBOBJS) $(LDFLAGS) $(LIBS)

#lapis_wrap.c: lapis.i
#	swig -lua lapis.i

clean:
	rm -f *.o *~ $(MAINLIB)
 #lapis_wrap.c

depend: $(LIBSRCS)
	$(CXX) -M $(CXXFLAGS) $(INCLUDES) $^ > $@

include depend
