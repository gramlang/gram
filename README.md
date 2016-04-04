# The Gram Programming Language

[Gram](https://www.gram.org) is a high-level programming language with a small, extensible core and a strong, static type system.

[![Build Status](https://travis-ci.org/gramlang/gram.svg?branch=master)](https://travis-ci.org/gramlang/gram)

## Getting started

1. Ensure you have the dependencies listed below.
2. Run `make` to build.
3. Run `sudo make install` to install.

If all goes well, you should be able to run `gram` from the command line.

### Other build and installation options

Normally, all of your CPU cores will be used to build Gram. You can override this behavior with the `NPROCS` option. For example, you can run `make NPROCS=1` to build Gram with only one core.

The default installation directory is `/usr/local/bin`. You can run `make install PREFIX=path` to install to a different directory.

The build system creates a directory called `build` for intermediate build artifacts. Normally, these files are retained so future builds can use them instead of starting from scratch. You can remove them with `make clean`.

If Gram was installed to the default location, you can uninstall it with `sudo make uninstall`. You can uninstall from a different directory with `make uninstall PREFIX=path`.

The build script uses Clang by default. You can optionally specify a different C and C++ compiler. For example, you can run `make CC=gcc CXX=g++` to build Gram with [GCC](https://gcc.gnu.org/). Gram is known to build with GCC >= 4.9.

### Build-time dependencies

Building Gram requires the following:

* [Clang](http://clang.llvm.org/) >= 3.1
* [CMake](https://cmake.org/) >= 2.8.12.2
* [Make](http://savannah.gnu.org/projects/make) >= 3.79

Additionally, the build process assumes the existence of common Unix utilities like `cp`, `grep`, etc.

#### Ubuntu

On a recent Ubuntu distribution, the following should be sufficient to install the dependencies:

```bash
sudo apt-get install clang cmake
```

#### OS X

On OS X, install the Command Line Tools for Xcode to get Clang and Make:

```bash
xcode-select --install
```

You will also need CMake, which can be downloaded [here](https://cmake.org/download/) or installed via [Homebrew](http://brew.sh/).


### Run-time dependencies

Gram itself requires either Clang >= 3.1 or GCC >= 4.7 to assemble and link executables.

Executables produced by Gram have no intrinsic run-time dependencies above the architecture they were built for.

## How to contribute

See the file [CONTRIBUTING.md](https://github.com/gramlang/gram/blob/master/CONTRIBUTING.md).

## License

See the file [LICENSE.md](https://github.com/gramlang/gram/blob/master/LICENSE.md).
