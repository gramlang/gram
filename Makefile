# These options can be specified by the user, for example:
# $ make CC=gcc CXX=g++ NPROCS=4
# $ make install PREFIX=~/bin
CC=clang
CXX=clang++
CCFLAGS=-O3
CXXFLAGS=-O3
PREFIX=/usr/local/bin
NPROCS=1

# Try to determine how many cores are available.
override OS=$(shell uname -s)
ifeq ($(OS),Linux)
  NPROCS=$(shell grep -c ^processor /proc/cpuinfo)
endif
ifeq ($(OS),Darwin) # Assume OS X.
  NPROCS:=$(shell sysctl -n hw.ncpu)
endif

# The sources to compile relative to the src/ directory.
override SOURCES=main.cpp compiler.cpp error.cpp platform.cpp ../deps/whereami/whereami.cpp

# The targets will be placed in the bin/ directory.
override TARGETS=gram gram-llc

.PHONY: all clean install uninstall

all: $(addprefix bin/,$(TARGETS))

clean:
	rm -rf build

install: all
	cp $(addprefix bin/,$(TARGETS)) $(PREFIX)

uninstall:
	rm $(addprefix $(PREFIX)/,$(TARGETS))

bin/gram: $(addprefix src/,$(SOURCES))
	mkdir -p bin
	$(CXX) $(CXXFLAGS) $(addprefix src/,$(SOURCES)) -o bin/gram

bin/gram-llc: build/llvm/build/bin/llc
	mkdir -p bin
	cp build/llvm/build/bin/llc bin/gram-llc

build/llvm/build/bin/llc: deps/llvm-3.8.0.src.tar.xz
	mkdir -p build/llvm/llvm
	tar -xf deps/llvm-3.8.0.src.tar.xz -C build/llvm/llvm --strip-components=1
	mkdir -p build/llvm/build
	cd build/llvm/build && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm -DCMAKE_C_COMPILER=$(CC) -DCMAKE_CXX_COMPILER=$(CXX)
	cd build/llvm/build && make -j $(NPROCS)
