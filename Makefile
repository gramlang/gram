# This is where the final binary will be installed.
PREFIX := /usr/local

# Determine the build type (release or debug).
BUILD_TYPE := release
ifeq ($(BUILD_TYPE),release)
	override CMAKE_BUILD_TYPE := Release
else ifeq ($(BUILD_TYPE),debug)
	override CMAKE_BUILD_TYPE := Debug
else
	$(error BUILD_TYPE must be 'release' or 'debug')
endif
override BUILD_PREFIX := build/$(BUILD_TYPE)

# Determine which compilers to use.
CC := $(shell ./scripts/get-compiler.sh CC)
CXX := $(shell ./scripts/get-compiler.sh CXX)

# The headers and sources to compile relative to the src/ directory.
override HEADERS := compiler.h error.h lexer.h parser.h platform.h typer.h version.h
override SOURCES := compiler.cpp error.cpp lexer.cpp main.cpp parser.cpp platform.cpp typer.cpp

.PHONY: all clean clean-all lint install-deps install uninstall

all: $(BUILD_PREFIX)/bin/gram

clean:
	rm -rf $(BUILD_PREFIX)/bin $(BUILD_PREFIX)/gram

clean-deps:
	rm -rf $(BUILD_PREFIX)

clean-all:
	rm -rf build

docker-gram:
	mkdir -p build/release/bin
	export CONTAINER=$$(docker create gramlang/gram:build) && \
		docker cp $$CONTAINER:/usr/local/bin/gram build/gram-docker && \
		docker rm $$CONTAINER
	docker build -f Dockerfile-gram -t gramlang/gram .
	rm build/gram-docker

docker-gram-build:
	docker build -f Dockerfile-gram-build -t gramlang/gram:build .

docker-gram-deps:
	docker build -f Dockerfile-gram-deps -t gramlang/gram:deps .

lint: $(addprefix src/,$(HEADERS)) $(addprefix src/,$(SOURCES)) \
		$(BUILD_PREFIX)/llvm/install/bin/llvm-config
	[ -n "$(CC)" -a -n "$(CXX)" ] # Ensure we have sufficient C and C++ compilers.
	shellcheck scripts/*.sh
	cppcheck src --enable=all --force --error-exitcode=1 \
		-I $(BUILD_PREFIX)/llvm/install/include \
		--suppressions-list=cppcheck-suppressions.txt
	rm -f $(BUILD_PREFIX)/bin/gram && \
	scan-build \
		--status-bugs \
		--use-analyzer $$(which clang) \
		--use-cc $(CC) \
		--use-c++ $(CXX) \
		make $(BUILD_PREFIX)/bin/gram

install: all
	mkdir -p $(PREFIX)/bin
	cp $(BUILD_PREFIX)/bin/gram $(PREFIX)/bin

uninstall:
	rm $(PREFIX)/bin/gram

$(BUILD_PREFIX)/bin/gram: $(addprefix src/,$(HEADERS)) $(addprefix src/,$(SOURCES)) \
		$(BUILD_PREFIX)/llvm/install/bin/llvm-config
	[ -n "$(CXX)" ] # Ensure we have a sufficient C++ compiler.
	mkdir -p $(BUILD_PREFIX)/gram
	./scripts/version.sh $(BUILD_TYPE) > $(BUILD_PREFIX)/gram/version.cpp
	mkdir -p $(BUILD_PREFIX)/bin
	$(CXX) \
		$(addprefix src/,$(SOURCES)) $(BUILD_PREFIX)/gram/version.cpp \
		-flto -O3 -std=c++11 -Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter \
		-o $(BUILD_PREFIX)/bin/gram \
		-I $(BUILD_PREFIX)/llvm/install/include \
		-L $(BUILD_PREFIX)/llvm/install/lib \
		$$( $(BUILD_PREFIX)/llvm/install/bin/llvm-config --libs --system-libs ) \
		$$( (uname -s | grep -qi 'Darwin') || echo '-static' )

$(BUILD_PREFIX)/llvm/install/bin/llvm-config: deps/llvm-3.9.0.src.tar.xz
	[ -n "$(CC)" -a -n "$(CXX)" ] # Ensure we have sufficient C and C++ compilers.
	rm -rf $(BUILD_PREFIX)/llvm
	mkdir -p $(BUILD_PREFIX)/llvm/src
	tar -xf deps/llvm-3.9.0.src.tar.xz -C $(BUILD_PREFIX)/llvm/src --strip-components=1
	mkdir -p $(BUILD_PREFIX)/llvm/build
	cd $(BUILD_PREFIX)/llvm/build && cmake ../src \
		$$( which ninja > /dev/null 2>&1 && echo '-GNinja' || echo '' ) \
		-DCMAKE_INSTALL_PREFIX=../install \
		-DCMAKE_BUILD_TYPE=$(CMAKE_BUILD_TYPE) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DLLVM_ENABLE_EH=ON \
		-DLLVM_ENABLE_RTTI=ON \
		-DLLVM_ENABLE_TERMINFO=OFF \
		-DLLVM_ENABLE_ZLIB=OFF
	cd $(BUILD_PREFIX)/llvm/build && cmake --build . --target install
