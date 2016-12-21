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
CC := $(shell ./scripts/get-compiler.sh CC 2> /dev/null)
CXX := $(shell ./scripts/get-compiler.sh CXX 2> /dev/null)

# The headers and sources to compile relative to the src directory.
# There is an additional source file not listed here called version.cpp,
# which is built from scripts/version.sh and included in the build.
override HEADERS := \
	ast.h \
	compiler.h \
	error.h \
	lexer.h \
	parser.h \
	platform.h \
	tokens.h \
	typechecker.h \
	version.h
override SOURCES := \
	ast.cpp \
	compiler.cpp \
	error.cpp \
	lexer.cpp \
	main.cpp \
	parser.cpp \
	platform.cpp \
	tokens.cpp \
	typechecker.cpp

# The default targets to build relative to the $(BUILD_PREFIX)/dist directory.
# These will be installed relative to the $(PREFIX) directory.
override TARGETS := bin/gram

# These targets do not name actual files.
# They are just recipes which may be executed by explicit request.
.PHONY: all \
	clean clean-deps clean-all clean-docs \
	docker-gram docker-gram-build docker-gram-deps \
	docs serve-docs
	lint install uninstall

# This is the default target.
# It builds all Gram artifacts.
all: $(addprefix $(BUILD_PREFIX)/dist/,$(TARGETS))

# This target removes Gram build artifacts, excluding dependencies,
# for the specified build type.
clean:
	rm -rf $(BUILD_PREFIX)/dist $(BUILD_PREFIX)/gram

# This target removes Gram build artifacts, including dependencies,
# for the specified build type.
clean-deps:
	rm -rf $(BUILD_PREFIX)

# This target removes Gram build artifacts, including dependencies,
# for all build types. This also removes build artifacts for the
# website.
clean-all:
	rm -rf build
	make clean-docs

# This target removes build artifacts for the website.
clean-docs:
	rm -rf docs/_site
	rm -rf .sass-cache
	rm -rf .jekyll-metadata

# This target produces the gramlang/gram Docker image.
# This image contains a complete Gram installation, and nothing more.
# It is suitable for distribution to end users.
# It requires the gramlang/gram:build Docker image, which is built by:
#   make docker-gram-build
docker-gram:
	CONTAINER="$$(docker create gramlang/gram:build)" && \
		GRAM_BINARY_PATH="$$(mktemp XXXXXXXXXX)" && \
		docker cp "$$CONTAINER:/root/gram/$(BUILD_PREFIX)/dist/bin/gram" \
			"$$GRAM_BINARY_PATH" && \
		docker rm "$$CONTAINER" && \
		docker build \
			-f docker/Dockerfile-gram \
			-t gramlang/gram \
			--build-arg "GRAM_BINARY_PATH=$$GRAM_BINARY_PATH" . && \
		rm "$$GRAM_BINARY_PATH"

# This target builds the gramlang/gram:build Docker container.
# Building this image amounts to building and linting Gram.
# It requires the gramlang/gram:deps Docker image, which is built by:
#   make docker-gram-deps
docker-gram-build:
	docker build -f docker/Dockerfile-gram-build -t gramlang/gram:build .

# This target builds the gramlang/gram:deps Docker image, which is based
# on Debian. It includes all the tools needed to build and lint Gram.
# This is the only target that requires a network connection.
docker-gram-deps:
	docker build -f docker/Dockerfile-gram-deps -t gramlang/gram:deps .

# This target builds the website.
# You must have github-pages installed.
docs:
	cd docs && jekyll build

# This target starts a local server for the website.
# You must have github-pages installed.
serve-docs:
	cd docs && jekyll serve

# This target runs the linters and static analyzers.
# The following must be installed:
# - Clang Static Analyzer
# - ShellCheck
lint: $(BUILD_PREFIX)/llvm/dist/bin/llvm-config
	# Make sure all lines conform to the line length limit.
	test $$( \
		awk '{print length}' \
			docker/* \
			.github/* \
			Makefile \
			scripts/* \
			src/* \
			.travis.yml \
			| sort -n \
			| tail -n 1 \
	) -lt 80

	# Ensure we have sufficient C and C++ compilers.
	[ -n "$(CC)" -a -n "$(CXX)" ]

	# Run ShellCheck on any shell scripts.
	shellcheck scripts/*.sh

	# Run Clang Static Analyzer on C++ source files.
	make clean && scan-build \
		--status-bugs \
		--use-analyzer "$$(which clang)" \
		--use-cc "$(CC)" \
		--use-c++ "$(CXX)" \
		make

# This target installs Gram to the $(PREFIX) directory.
# Artifacts will be placed in subdirectories such as $(PREFIX)/bin.
install: all
	mkdir -p "$(PREFIX)/bin"
	cp "$(BUILD_PREFIX)/dist/bin/"* "$(PREFIX)/bin"

# This target removes all installed artifacts from the $(PREFIX) directory.
uninstall:
	rm $(addprefix $(PREFIX)/,$(TARGETS))

# This target builds the main Gram binary.
$(BUILD_PREFIX)/dist/bin/gram: \
		$(addprefix src/,$(HEADERS)) \
		$(addprefix src/,$(SOURCES)) \
		$(BUILD_PREFIX)/llvm/dist/bin/llvm-config
	[ -n "$(CXX)" ] # Ensure we have a sufficient C++ compiler.
	mkdir -p $(BUILD_PREFIX)/gram/build
	./scripts/version.sh $(BUILD_TYPE) > $(BUILD_PREFIX)/gram/build/version.cpp
	mkdir -p $(BUILD_PREFIX)/dist/bin
	$(CXX) \
		$(addprefix src/,$(SOURCES)) $(BUILD_PREFIX)/gram/build/version.cpp \
		-flto -O3 -std=c++11 \
		-Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter \
		-o $(BUILD_PREFIX)/dist/bin/gram \
		-I $(BUILD_PREFIX)/llvm/dist/include \
		-L $(BUILD_PREFIX)/llvm/dist/lib \
		$$( $(BUILD_PREFIX)/llvm/dist/bin/llvm-config --libs --system-libs ) \
		$$( (uname -s | grep -qi 'Darwin') || echo '-static' )

# This target builds LLVM, which is a dependency for Gram.
$(BUILD_PREFIX)/llvm/dist/bin/llvm-config: deps/llvm-3.9.0.src.tar.xz
	[ -n "$(CC)" -a -n "$(CXX)" ] # Ensure we have sufficient C and C++ compilers.
	rm -rf $(BUILD_PREFIX)/llvm
	mkdir -p $(BUILD_PREFIX)/llvm/src
	tar -xf deps/llvm-3.9.0.src.tar.xz -C $(BUILD_PREFIX)/llvm/src \
		--strip-components=1
	mkdir -p $(BUILD_PREFIX)/llvm/build
	cd $(BUILD_PREFIX)/llvm/build && cmake ../src \
		$$( which ninja > /dev/null 2>&1 && echo '-GNinja' || echo '' ) \
		-DCMAKE_INSTALL_PREFIX=../dist \
		-DCMAKE_BUILD_TYPE=$(CMAKE_BUILD_TYPE) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DLLVM_ENABLE_EH=ON \
		-DLLVM_ENABLE_PIC=OFF \
		-DLLVM_ENABLE_RTTI=ON \
		-DLLVM_ENABLE_TERMINFO=OFF \
		-DLLVM_ENABLE_ZLIB=OFF
	cd $(BUILD_PREFIX)/llvm/build && cmake --build . --target install
