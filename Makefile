CXX = g++
CXXFLAGS = -Wall -g -O2 -fPIC -std=c++0x
INCLUDES = $(shell sdl-config --cflags) $(shell lua-config --include)
LDFLAGS = -shared

LIBS = $(shell sdl-config --libs) $(shell lua-config --libs) -lSDL_ttf -lSDL_image -lSDL_mixer -lGL -lGLU

LIBSRCS = \
	entity.cpp \
	sdl_driver.cpp \
	component.cpp \
	engine.cpp \
	message.cpp \
	hash.cpp

EXECSRCS = \
	test.cpp

LIBOBJS = $(LIBSRCS:.cpp=.o)
EXECOBJS = $(EXECSRCS:.cpp=.o)

MAINLIB = liblapis.so
MAINEXEC = main

all: $(MAINLIB) $(MAINEXEC)

.SUFFIXES: .cpp .o .i

.cpp.o:
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

.PHONY: depend clean all

$(MAINLIB): $(LIBOBJS)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAINLIB) $(LIBOBJS) $(LDFLAGS) $(LIBS)

$(MAINEXEC): $(EXECOBJS)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAINEXEC) $(EXECOBJS) -L. -llapis -Wl,-rpath,'$$ORIGIN'

#lapis_wrap.c: lapis.i
#	swig -lua lapis.i

clean:
	rm -f *.o *~ $(MAINLIB) $(MAINEXEC)
 #lapis_wrap.c

depend: $(LIBSRCS) $(EXECSRCS)
	$(CXX) -M $(CXXFLAGS) $(INCLUDES) $^ > $@

include depend
