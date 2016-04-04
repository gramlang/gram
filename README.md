# The Gram Programming Language

[Gram](https://www.gram.org) is a high-level programming language with a small, extensible core and a strong, static type system.

## How to build and install

1. Ensure you have the dependencies listed below.
2. Run `make` at the root of this repository to build.
3. Run `sudo make install` at the root of this repository to install.

You can also run `make install PREFIX=path` to install to a specific directory (the default is `/usr/local/bin`).

If you want to uninstall Gram, run `sudo make uninstall` from the root of this repository. You can also uninstall from a specific directory with `make uninstall PREFIX=path` (the default is `/usr/local/bin`).

### Dependencies

Building Gram requires the following:

* [Clang](http://clang.llvm.org/) >= 3.1
* [CMake](https://cmake.org/) >= 2.8.8
* [GNU Make](http://savannah.gnu.org/projects/make) >= 3.79

Additionally, the build process assumes the existence of common Unix utilities like `cp`, `grep`, etc.

#### Ubuntu

On Ubuntu, the following should be sufficient to install the dependencies:

```bash
sudo apt-get install clang cmake
```

#### OS X

On OS X, install the Command Line Tools for Xcode:

```bash
xcode-select --install
```

You will also need CMake, which can be downloaded [here](https://cmake.org/download/) or installed via [Homebrew](http://brew.sh/).

## How to contribute

See the file [CONTRIBUTING.md](https://github.com/gramlang/gram/blob/master/CONTRIBUTING.md).

## License

See the file [LICENSE.md](https://github.com/gramlang/gram/blob/master/LICENSE.md).
