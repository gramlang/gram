.PHONY: all clean llvm unicodedata

all: llvm unicodedata

clean:
	rm -rf build

llvm: build/llvm/build/bin/llc

build/llvm/build/bin/llc: build/llvm/llvm-3.8.0.src.tar.xz
	cd build/llvm && tar -xf llvm-3.8.0.src.tar.xz
	mkdir build/llvm/build
	cd build/llvm/build && cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release ../llvm-3.8.0.src
	cd build/llvm/build && make

build/llvm/llvm-3.8.0.src.tar.xz:
	mkdir -p build/llvm
	curl -o build/llvm/llvm-3.8.0.src.tar.xz http://llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz

unicodedata: build/unicodedata/UnicodeData.txt

build/unicodedata/UnicodeData.txt:
	mkdir -p build/unicodedata
	curl -o build/unicodedata/UnicodeData.txt http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
