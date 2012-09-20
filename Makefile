CXX = g++
CXXFLAGS = -Wall -g -O2 -fPIC -std=c++0x
INCLUDES = $(shell pkg-config lua --cflags) $(shell sdl-config --cflags)
LDFLAGS = -fPIC -shared

LIBS = $(shell pkg-config lua --libs) $(shell sdl-config --libs)

LIBSRCS = \
	entity.cpp

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
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAINLIB) $(LIBOBJS) $(LDFLAGS)

$(MAINEXEC): $(EXECOBJS)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -o $(MAINEXEC) $(EXECOBJS) $(LIBS) -L. -llapis

#lapis_wrap.c: lapis.i
#	swig -lua lapis.i

clean:
	rm -f *.o *~ $(MAINLIB) $(MAINEXEC)
 #lapis_wrap.c

depend: $(LIBSRCS) $(EXECSRCS)
	$(CXX) -M $(CFLAGS) $(INCLUDES) $^ > $@

include depend
