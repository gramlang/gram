# The Gram Programming Language

Gram is a high-level programming language with a small, extensible core and a strong, static type system.

## How to build

To build Gram, run `make` at the root of this repository.

### Dependencies

Building Gram requires the following:

* [GNU Make](http://savannah.gnu.org/projects/make)
* [GCC](https://gcc.gnu.org/)

Gram uses [LLVM](http://llvm.org/) for code generation. When compiled for the first time, Gram will automatically download and compile LLVM. LLVM has some additional dependencies which are documented [here](http://llvm.org/docs/GettingStarted.html#requirements).

## How to contribute

See the file [CONTRIBUTING.md](https://github.com/gramlang/gram/blob/master/CONTRIBUTING.md).

## License

See the file [LICENSE.md](https://github.com/gramlang/gram/blob/master/LICENSE.md).
