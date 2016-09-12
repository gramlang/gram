# These options can be overridden by the user, for example:
# $ make NPROCS=4 BUILD_TYPE=debug
# $ make install PREFIX=~/bin
PREFIX := /usr/local/bin
NPROCS := $(shell ./scripts/count-cores.sh)
BUILD_TYPE := release

# Determine which compiler to use.
# The SCAN_BUILD option disables automatic compiler detection.
# This is needed by the Clang Static Analyzer (see the lint target).
SCAN_BUILD := no
ifeq ($(SCAN_BUILD),no)
	override CC := $(shell ./scripts/get-compiler.sh CC 2> /dev/null)
	override CXX := $(shell ./scripts/get-compiler.sh CXX 2> /dev/null)
endif

# The headers and sources to compile relative to the src/ directory.
override HEADERS := compiler.h error.h lexer.h parser.h platform.h typer.h version.h
override SOURCES := compiler.cpp error.cpp lexer.cpp main.cpp parser.cpp platform.cpp typer.cpp

# Determine build paths based on the build type.
ifeq ($(BUILD_TYPE),release)
	override CMAKE_BUILD_TYPE := Release
else ifeq ($(BUILD_TYPE),debug)
	override CMAKE_BUILD_TYPE := Debug
else
	$(error BUILD_TYPE must be 'release' or 'debug')
endif
override BUILD_PREFIX := build/$(BUILD_TYPE)

.PHONY: all clean clean-all lint install-deps install uninstall

all: $(BUILD_PREFIX)/bin/gram

clean:
	rm -rf $(BUILD_PREFIX)/bin $(BUILD_PREFIX)/gram

clean-all:
	rm -rf $(BUILD_PREFIX)

docker-gram:
	export CONTAINER=$$(docker create gramlang/gram:build) && \
		docker cp $$CONTAINER:/usr/local/bin/gram gram-docker && \
		docker rm $$CONTAINER
	docker build -f Dockerfile-gram -t gramlang/gram .
	rm gram-docker

docker-gram-build:
	docker build -f Dockerfile-gram-build -t gramlang/gram:build .

docker-gram-deps:
	docker build -f Dockerfile-gram-deps -t gramlang/gram:deps .

lint: $(addprefix src/,$(HEADERS)) $(addprefix src/,$(SOURCES)) \
		$(BUILD_PREFIX)/llvm/build/bin/llvm-config
	shellcheck scripts/*.sh
	cppcheck src --enable=all --force --error-exitcode=1 -j $(NPROCS) \
		-I $(BUILD_PREFIX)/llvm/llvm/include \
		-I $(BUILD_PREFIX)/llvm/build/include \
		--suppressions-list=cppcheck-suppressions.txt
	rm -f $(BUILD_PREFIX)/bin/gram && \
	scan-build \
		--status-bugs \
		--use-analyzer $$(which clang) \
		--use-cc $$(./scripts/get-compiler.sh CC) \
		--use-c++ $$(./scripts/get-compiler.sh CXX) \
		make $(BUILD_PREFIX)/bin/gram SCAN_BUILD=yes

install: all
	cp $(BUILD_PREFIX)/bin/gram $(PREFIX)

uninstall:
	rm $(PREFIX)/gram

$(BUILD_PREFIX)/bin/gram: $(addprefix src/,$(HEADERS)) $(addprefix src/,$(SOURCES)) \
		$(BUILD_PREFIX)/llvm/build/bin/llvm-config
	mkdir -p $(BUILD_PREFIX)/gram
	./scripts/version.sh $(BUILD_TYPE) > $(BUILD_PREFIX)/gram/version.cpp
	mkdir -p $(BUILD_PREFIX)/bin
	$(CXX) $(addprefix src/,$(SOURCES)) $(BUILD_PREFIX)/gram/version.cpp \
		-o $(BUILD_PREFIX)/bin/gram \
		$(shell $(BUILD_PREFIX)/llvm/build/bin/llvm-config --cxxflags --ldflags --libs --system-libs) \
		$$( (uname -s | grep -qi 'Darwin') || echo -static) -lncurses -static-libstdc++

$(BUILD_PREFIX)/llvm/build/bin/llvm-config: deps/llvm-3.9.0.src.tar.xz
	rm -rf $(BUILD_PREFIX)/llvm
	mkdir -p $(BUILD_PREFIX)/llvm/llvm
	tar -xf deps/llvm-3.9.0.src.tar.xz -C $(BUILD_PREFIX)/llvm/llvm --strip-components=1
	mkdir -p $(BUILD_PREFIX)/llvm/build
	cd $(BUILD_PREFIX)/llvm/build && cmake ../llvm \
		-G 'Unix Makefiles' \
		-DCMAKE_BUILD_TYPE=$(CMAKE_BUILD_TYPE) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DLLVM_ENABLE_EH=ON \
		-DLLVM_ENABLE_RTTI=ON
	cd $(BUILD_PREFIX)/llvm/build && make -j $(NPROCS)
