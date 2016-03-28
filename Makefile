CC=g++
SOURCES=src/main.cpp src/platform.cpp deps/whereami/whereami.cpp

.PHONY: all clean llvm

all: llvm build/bin/gram

clean:
	rm -rf build

build/bin/gram: $(SOURCES)
	mkdir -p build/bin
	$(CC) $(SOURCES) -o build/bin/gram

llvm: build/llvm/build/bin/llc

build/llvm/build/bin/llc: build/llvm/llvm-3.8.0.src.tar.xz
	cd build/llvm && tar -xf llvm-3.8.0.src.tar.xz
	mkdir -p build/llvm/build
	cd build/llvm/build && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm-3.8.0.src
	cd build/llvm/build && make

build/llvm/llvm-3.8.0.src.tar.xz:
	mkdir -p build/llvm
	curl -o build/llvm/llvm-3.8.0-untrusted.src.tar.xz http://llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz
	if [ "$$(openssl sha1 build/llvm/llvm-3.8.0-untrusted.src.tar.xz)" = "SHA1(build/llvm/llvm-3.8.0-untrusted.src.tar.xz)= 723ac918979255706434a05f5af34b71c49c9971" ]; then mv build/llvm/llvm-3.8.0-untrusted.src.tar.xz build/llvm/llvm-3.8.0.src.tar.xz; else echo "LLVM integrity check failed."; exit 1; fi
