# These options can be specified by the user, for example:
# $ make NPROCS=4 BUILD_TYPE=Debug
# $ make install PREFIX=~/bin
PREFIX=/usr/local/bin
NPROCS=1
BUILD_TYPE=Release

# Try to determine how many cores are available.
override OS=$(shell uname -s)
ifeq ($(OS),Linux)
  NPROCS=$(shell grep -c ^processor /proc/cpuinfo)
endif
ifeq ($(OS),Darwin) # Assume OS X.
  NPROCS:=$(shell sysctl -n hw.ncpu)
endif

# Determine which compiler to use.
override CC=$(shell ./which-compiler.sh CC)
override CXX=$(shell ./which-compiler.sh CXX)

# The sources to compile relative to the src/ directory.
override SOURCES=main.cpp compiler.cpp error.cpp platform.cpp token.cpp ast.cpp

# The targets will be placed in the build/bin/ directory.
override TARGETS=gram

.PHONY: all clean install-deps install uninstall

all: $(addprefix build/bin/,$(TARGETS))

clean:
	rm -rf build

install-deps:
	./install-deps.sh

install: all
	cp $(addprefix build/bin/,$(TARGETS)) $(PREFIX)

uninstall:
	rm $(addprefix $(PREFIX)/,$(TARGETS))

build/bin/gram: $(addprefix src/,$(SOURCES)) build/llvm/build/bin/llvm-config
	mkdir -p build/bin
	$(CXX) $(addprefix src/,$(SOURCES)) $(shell build/llvm/build/bin/llvm-config --cxxflags --ldflags --libs --system-libs) -o build/bin/gram

build/llvm/build/bin/llvm-config: deps/llvm-3.8.0.src.tar.xz
	rm -rf build/llvm
	mkdir -p build/llvm/llvm
	tar -xf deps/llvm-3.8.0.src.tar.xz -C build/llvm/llvm --strip-components=1
	mkdir -p build/llvm/build
	cd build/llvm/build && cmake ../llvm -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=$(BUILD_TYPE) -DCMAKE_C_COMPILER=$(CC) -DCMAKE_CXX_COMPILER=$(CXX) -DLLVM_ENABLE_EH=ON -DLLVM_ENABLE_RTTI=ON
	cd build/llvm/build && make -j $(NPROCS)
