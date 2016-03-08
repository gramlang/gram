# The Gram Programming Language

Gram is a high-level programming language with a small, extensible core and a strong, static type system.

# How to build Gram

To build Gram, run `make` at the root of this repository.

## Dependencies

Building Gram requires the following:

* [GNU Make](http://savannah.gnu.org/projects/make)
* [GCC](https://gcc.gnu.org/)

Gram uses [LLVM](http://llvm.org/) for code generation. When compiled for the first time, Gram will automatically download and compile LLVM. LLVM has some additional dependencies which are documented [here](http://llvm.org/docs/GettingStarted.html#requirements).

# Contributing

See the file [.github/CONTRIBUTING.md](https://github.com/boyers/gram/blob/master/.github/CONTRIBUTING.md).

# License

See the file [LICENSE.md](https://github.com/boyers/gram/blob/master/LICENSE.md).
