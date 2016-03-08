.PHONY: all clean llvm

all: llvm

clean:
	rm -rf build

llvm: build/llvm/build/bin/llc

build/llvm/build/bin/llc: build/llvm/llvm-3.7.1.src.tar.xz
	cd build/llvm && tar -xf llvm-3.7.1.src.tar.xz
	mkdir build/llvm/build
	cd build/llvm/build && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm-3.7.1.src && make

build/llvm/llvm-3.7.1.src.tar.xz:
	mkdir -p build/llvm
	curl -o build/llvm/llvm-3.7.1.src.tar.xz http://llvm.org/releases/3.7.1/llvm-3.7.1.src.tar.xz
