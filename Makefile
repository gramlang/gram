# These options can be overridden by the user, for example:
# $ make NPROCS=4 BUILD_TYPE=debug
# $ make install PREFIX=~/bin
PREFIX := /usr/local/bin
NPROCS := $(shell ./scripts/count-cores.sh)
BUILD_TYPE := release

# Determine which compiler to use.
override CC := $(shell ./scripts/get-compiler.sh CC)
override CXX := $(shell ./scripts/get-compiler.sh CXX)

# The sources to compile relative to the src/ directory.
override SOURCES := main.cpp compiler.cpp error.cpp platform.cpp token.cpp ast.cpp

# The targets will be placed in the $(BUILD_PREFIX)/bin/ directory.
override TARGETS := gram

# Determine build paths based on the build type.
ifeq ($(BUILD_TYPE),release)
  override CMAKE_BUILD_TYPE := Release
else ifeq ($(BUILD_TYPE),debug)
  override CMAKE_BUILD_TYPE := Debug
else
  $(error BUILD_TYPE must be 'release' or 'debug')
endif
override BUILD_PREFIX := build/$(BUILD_TYPE)

.PHONY: all clean install-deps install uninstall

all: $(addprefix $(BUILD_PREFIX)/bin/,$(TARGETS))

clean:
	rm -rf $(BUILD_PREFIX)

clean-all:
	rm -rf build

install-deps:
	./scripts/install-deps.sh

install: all
	cp $(addprefix $(BUILD_PREFIX)/bin/,$(TARGETS)) $(PREFIX)

uninstall:
	rm $(addprefix $(PREFIX)/,$(TARGETS))

$(BUILD_PREFIX)/bin/gram: $(addprefix src/,$(SOURCES)) $(BUILD_PREFIX)/llvm/build/bin/llvm-config
	mkdir -p $(BUILD_PREFIX)/gram
	./scripts/version.sh "$(BUILD_TYPE)" > $(BUILD_PREFIX)/gram/version.cpp
	mkdir -p $(BUILD_PREFIX)/bin
	$(CXX) $(addprefix src/,$(SOURCES)) $(BUILD_PREFIX)/gram/version.cpp \
		-o $(BUILD_PREFIX)/bin/gram \
		$(shell $(BUILD_PREFIX)/llvm/build/bin/llvm-config --cxxflags --ldflags --libs --system-libs)

$(BUILD_PREFIX)/llvm/build/bin/llvm-config: deps/llvm-3.8.0.src.tar.xz
	rm -rf $(BUILD_PREFIX)/llvm
	mkdir -p $(BUILD_PREFIX)/llvm/llvm
	tar -xf deps/llvm-3.8.0.src.tar.xz -C $(BUILD_PREFIX)/llvm/llvm --strip-components=1
	mkdir -p $(BUILD_PREFIX)/llvm/build
	cd $(BUILD_PREFIX)/llvm/build && cmake ../llvm \
		-G 'Unix Makefiles' \
		-DCMAKE_BUILD_TYPE=$(CMAKE_BUILD_TYPE) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DLLVM_ENABLE_EH=ON \
		-DLLVM_ENABLE_RTTI=ON
	cd $(BUILD_PREFIX)/llvm/build && make -j $(NPROCS)
