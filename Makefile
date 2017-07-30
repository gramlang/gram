# The final build artifacts will be installed in subdirectories of this path.
# For example, binaries will be located in "$PREFIX/bin".
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
override BUILD_PREFIX_COMMON := build/common

# Determine which compilers to use.
CC := $(shell ./scripts/get-compiler.sh CC 2> /dev/null)
CXX := $(shell ./scripts/get-compiler.sh CXX 2> /dev/null)

# The headers and sources to compile.
# There is an additional source file not listed here called version.cpp,
# which is built from scripts/version.sh and included in the static library.
override SOURCES := \
  $(shell find src/*.cpp -type f) \
  $(BUILD_PREFIX)/gram/src/version.cpp
override HEADERS := $(shell find include/*.h -type f)
override HEADERS_DIST := \
  $(patsubst include/%,$(BUILD_PREFIX)/dist/include/gram/%,$(HEADERS))
override OBJ := \
  $(BUILD_PREFIX)/gram/obj/$(BUILD_PREFIX)/gram/src/version.o \
  $(patsubst %.cpp,$(BUILD_PREFIX)/gram/obj/%.o,$(SOURCES))

# These targets do not name actual files.
# They are just recipes which may be executed by explicit request.
.PHONY: \
  all lib driver \
  clean clean-docs clean-spec clean-deps clean-all \
  docker-gram docker-gram-build docker-gram-deps \
  docs serve-docs spec \
  lint install uninstall \
  force

# This flag controls whether $(BUILD_PREFIX)/gram/src/version.cpp needs to be
# rebuilt. Specifically, if scripts/version.sh produces a different output
# than the existing version.cpp file (or the file does not exist), then we
# force a rebuild. We have this flag because scripts/version.sh is not
# idempotent (it depends on the git SHA).
override VERSION_CPP_DEPS := $(shell \
  if test "$$( \
    cksum '$(BUILD_PREFIX)/gram/src/version.cpp' 2> /dev/null \
      | cut -d ' ' -f 1 \
  )" != "$$( \
    ./scripts/version.sh $(BUILD_TYPE) | cksum 2> /dev/null \
      | cut -d ' ' -f 1 \
  )"; then echo 'force'; fi \
)

# This is the default target.
# It builds all artifacts for distribution.
all: lib driver

# This target builds the static library.
lib: $(BUILD_PREFIX)/dist/lib/gram.a $(HEADERS_DIST)

# This target builds the driver.
driver: $(BUILD_PREFIX)/dist/bin/gram lib

# This target removes Gram build artifacts, excluding dependencies,
# for the specified build type.
clean:
	rm -rf $(BUILD_PREFIX)/dist $(BUILD_PREFIX)/gram

# This target removes build artifacts for the website.
clean-docs:
	jekyll clean --source docs --destination $(BUILD_PREFIX_COMMON)/docs

	# This directory is created by Jekyll and needs to be deleted.
	# Ideally `jekyll clean` would remove it, but that's not currently the case
	# due to a bug in Jekyll. See the discussion here:
	# https://github.com/jekyll/jekyll/pull/5701
	rm -rf .sass-cache

# This target removes build artifacts for the formal specification.
clean-spec:
	rm -rf $(BUILD_PREFIX_COMMON)/spec

# This target removes Gram build artifacts, including dependencies,
# for the specified build type (and common).
clean-deps:
	rm -rf $(BUILD_PREFIX)
	rm -rf $(BUILD_PREFIX_COMMON)

# This target removes Gram build artifacts, including dependencies,
# for all build types. This also removes build artifacts for the
# website.
clean-all:
	rm -rf build

	# This directory is created by Jekyll and needs to be deleted.
	# Ideally `jekyll clean` would remove it, but that's not currently the case
	# due to a bug in Jekyll. See the discussion here:
	# https://github.com/jekyll/jekyll/pull/5701
	rm -rf .sass-cache

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
docs: $(BUILD_PREFIX_COMMON)/docs

# This target starts a local server for the website.
# You must have github-pages installed.
serve-docs:
	jekyll serve --source docs --destination $(BUILD_PREFIX_COMMON)/docs

# This target builds the formal specification.
# You must have pdflatex installed.
spec: $(BUILD_PREFIX_COMMON)/spec/gram.pdf

# This target runs the linters and static analyzers.
# The following must be installed:
# - Clang Static Analyzer
# - ShellCheck
lint: $(BUILD_PREFIX)/llvm
	# Make sure all lines conform to the line length limit.
	scripts/check-line-lengths.sh \
	  .travis.yml \
	  Makefile \
	  docker/* \
	  driver/* \
	  include/* \
	  scripts/* \
	  src/*

	# Run ShellCheck on any shell scripts.
	shellcheck scripts/*.sh

	# Ensure we have sufficient compilers.
	[ -n "$(CC)" -a -n "$(CXX)" ]

	# Run the Clang Static Analyzer on source files. Afterward, we
	# run `make clean` because we don't trust the binary produced
	# during the analysis.
	make clean && \
	  scan-build \
	    --status-bugs \
	    --use-analyzer "$$(which clang)" \
	    --use-cc "$(CC)" \
	    --use-c++ "$(CXX)" \
	    make && \
	  make clean

# This target installs build artifacts to the $(PREFIX) directory.
# The artifacts will be placed in subdirectories such as $(PREFIX)/bin.
install: all uninstall
	mkdir -p "$(PREFIX)/bin"
	mkdir -p "$(PREFIX)/include/gram"
	mkdir -p "$(PREFIX)/lib"
	cp $(BUILD_PREFIX)/dist/bin/gram "$(PREFIX)/bin/gram"
	cp $(BUILD_PREFIX)/dist/lib/gram.a "$(PREFIX)/lib/gram.a"
	cp $(HEADERS_DIST) "$(PREFIX)/include/gram"

# This target removes all installed artifacts from the $(PREFIX) directory.
uninstall:
	rm -f "$(PREFIX)/bin/gram"
	rm -f "$(PREFIX)/lib/gram.a"
	rm -rf "$(PREFIX)/include/gram"

# This is a phony target that any other target can depend on to force a build.
force:

# This target uses Jekyll to compile the website.
# You probably want to run `make docs` instead of using this directly.
$(BUILD_PREFIX_COMMON)/docs: $(shell find docs -type f)
	jekyll build --source docs --destination $(BUILD_PREFIX_COMMON)/docs

# This target builds the specification.
# You probably want to run `make spec` instead of using this directly.
$(BUILD_PREFIX_COMMON)/spec/gram.pdf: spec/gram.tex
	mkdir -p $(BUILD_PREFIX_COMMON)/spec
	pdflatex -output-directory $(BUILD_PREFIX_COMMON)/spec spec/gram.tex
	while ( \
	  grep -qi \
	    '^LaTeX Warning: Label(s) may have changed' \
	    $(BUILD_PREFIX_COMMON)/spec/gram.log \
	) do \
	  pdflatex -output-directory $(BUILD_PREFIX_COMMON)/spec spec/gram.tex; \
	done

# This target copies the headers into the $(BUILD_PREFIX)/dist/include/gram
# directory for distribution.
$(HEADERS_DIST): $(HEADERS)
	mkdir -p $$(dirname $@)
	cp $(HEADERS) $(BUILD_PREFIX)/dist/include/gram

# This target generates version.cpp using the scripts/version.sh script.
$(BUILD_PREFIX)/gram/src/version.cpp: $(VERSION_CPP_DEPS)
	mkdir -p $$(dirname $@)
	./scripts/version.sh $(BUILD_TYPE) > \
	  $(BUILD_PREFIX)/gram/src/version.cpp

# This target compiles a source file.
$(BUILD_PREFIX)/gram/obj/%.o: %.cpp $(HEADERS_DIST) $(BUILD_PREFIX)/llvm
	mkdir -p $$(dirname $@)
	$(CXX) -c $< \
	  -I $(BUILD_PREFIX)/dist/include \
	  -I $(BUILD_PREFIX)/llvm/dist/include \
	  $$( \
	    echo $(BUILD_TYPE) | grep -qi 'debug' && \
	    echo '-g -O0' || \
	    echo '-O3' \
	  ) \
	  -std=c++11 \
	  -Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter \
	  -o $@

# This target builds the static library.
$(BUILD_PREFIX)/dist/lib/gram.a: $(OBJ)
	mkdir -p $$(dirname $@)
	rm -f $@
	ar rcs $@ $(OBJ)

# This target builds the driver.
$(BUILD_PREFIX)/dist/bin/gram: \
  $(BUILD_PREFIX)/dist/lib/gram.a \
  $(BUILD_PREFIX)/gram/obj/driver/main.o \
  $(HEADERS_DIST)
	mkdir -p $$(dirname $@)
	$(CXX) \
	    $(BUILD_PREFIX)/gram/obj/driver/main.o \
	    $(BUILD_PREFIX)/dist/lib/gram.a \
	  -I $(BUILD_PREFIX)/dist/include/gram \
	  $$( \
	    echo $(BUILD_TYPE) | grep -qi 'debug' && \
	    echo '-g -O0' || \
	    echo '-flto -O3' \
	  ) \
	  -std=c++11 \
	  -Wall -Wextra -Wpedantic -Werror -Wno-unused-parameter \
	  -o $@ \
	  -L $(BUILD_PREFIX)/llvm/dist/lib \
	  $$( $(BUILD_PREFIX)/llvm/dist/bin/llvm-config --libs --system-libs ) \
	  $$( (uname -s | grep -qi 'Darwin') || echo '-static' )

# This target builds LLVM, which is a dependency for Gram.
$(BUILD_PREFIX)/llvm: deps/llvm-4.0.1.src.tar.xz
	[ -n "$(CC)" -a -n "$(CXX)" ] # Ensure we have sufficient compilers.
	rm -rf $(BUILD_PREFIX)/llvm
	mkdir -p $(BUILD_PREFIX)/llvm/src
	tar -xf deps/llvm-4.0.1.src.tar.xz -C $(BUILD_PREFIX)/llvm/src \
	  --strip-components=1
	mkdir -p $(BUILD_PREFIX)/llvm/build
	cd $(BUILD_PREFIX)/llvm/build && cmake ../src \
	  $$( which ninja > /dev/null 2>&1 && echo '-GNinja' || echo '' ) \
	  -DCMAKE_INSTALL_PREFIX=../dist \
	  -DCMAKE_BUILD_TYPE=$(CMAKE_BUILD_TYPE) \
	  -DCMAKE_C_COMPILER="$(CC)" \
	  -DCMAKE_CXX_COMPILER="$(CXX)" \
	  -DLLVM_ENABLE_EH=ON \
	  -DLLVM_ENABLE_PIC=OFF \
	  -DLLVM_ENABLE_RTTI=ON \
	  -DLLVM_ENABLE_TERMINFO=OFF \
	  -DLLVM_ENABLE_ZLIB=OFF
	cd $(BUILD_PREFIX)/llvm/build && cmake --build . --target install
