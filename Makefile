CC=clang++
SOURCES=src/main.cpp src/compiler.cpp src/error.cpp src/platform.cpp deps/whereami/whereami.cpp
LLVM_TOOLS=llc

.PHONY: all clean

all: bin/gram

clean:
	rm -rf build

bin/gram: $(SOURCES) $(addprefix bin/llvm/,$(LLVM_TOOLS))
	mkdir -p bin
	$(CC) $(SOURCES) -o bin/gram

$(addprefix bin/llvm/,$(LLVM_TOOLS)): $(addprefix build/llvm/build/bin/,$(LLVM_TOOLS))
	mkdir -p bin/llvm
	cp $(addprefix build/llvm/build/bin/,$(LLVM_TOOLS)) bin/llvm/

$(addprefix build/llvm/build/bin/,$(LLVM_TOOLS)): build/llvm/llvm-3.8.0.src.tar.xz
	mkdir -p build/llvm/llvm
	tar -xf build/llvm/llvm-3.8.0.src.tar.xz -C build/llvm/llvm --strip-components=1
	mkdir -p build/llvm/build
	cd build/llvm/build && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm
	cd build/llvm/build && make

build/llvm/llvm-3.8.0.src.tar.xz:
	mkdir -p build/llvm
	curl -o build/llvm/llvm-3.8.0-untrusted.src.tar.xz http://llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz
	if [ "$$(openssl sha1 build/llvm/llvm-3.8.0-untrusted.src.tar.xz)" = \
			 "SHA1(build/llvm/llvm-3.8.0-untrusted.src.tar.xz)= 723ac918979255706434a05f5af34b71c49c9971" ]; \
		then mv build/llvm/llvm-3.8.0-untrusted.src.tar.xz build/llvm/llvm-3.8.0.src.tar.xz; \
	else \
		echo "LLVM integrity check failed."; \
		exit 1; \
	fi
