# The Gram Programming Language

[Gram](https://www.gram.org) is a high-level programming language with a small, extensible core and a strong, static type system.

[![Build Status](https://travis-ci.org/gramlang/gram.svg?branch=master)](https://travis-ci.org/gramlang/gram)

## Getting started

Gram is supported on popular Unix-like operating systems, such as Ubuntu and OS X. To install Gram, follow these steps from the root of this repository:

1. Make sure you have the dependencies listed below.
2. Run `make` to build.
3. Run `sudo make install` to install.

If all goes well, you should be able to run `gram` from the command line.

### Dependencies

To build Gram, you need the following:

- [GNU Make](https://www.gnu.org/software/make/) >= 3.79.1
- [Clang](http://clang.llvm.org/) >= 3.1 or [GCC](https://gcc.gnu.org/) >= 4.7.0
- [CMake](https://cmake.org/) >= 3.4.3

You also need the usual set of Unix tools, such as `echo`, `grep`, etc.

## How to contribute

See the file [CONTRIBUTING.md](https://github.com/gramlang/gram/blob/master/CONTRIBUTING.md).

## License

See the file [LICENSE.md](https://github.com/gramlang/gram/blob/master/LICENSE.md).
