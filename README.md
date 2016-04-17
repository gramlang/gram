# The Gram Programming Language

[Gram](https://www.gram.org) is a high-level programming language with a small, extensible core and a strong, static type system.

[![Build Status](https://travis-ci.org/gramlang/gram.svg?branch=master)](https://travis-ci.org/gramlang/gram)

## Getting started

Gram is supported on popular Unix-like operating systems, such as Ubuntu and OS X. To install Gram, follow these steps from the root of this repository:

1. Run `sudo make install-deps` to ensure you have the dependencies installed.
2. Run `make` to build.
3. Run `sudo make install` to install.

If all goes well, you should be able to run `gram` from the command line.

### Other build and installation options

Normally, all of your CPU cores will be used to build Gram. You can override this behavior with the `NPROCS` option. For example, you can run `make NPROCS=1` to build Gram with only one core.

The default installation directory is `/usr/local/bin`. You can run `make install PREFIX=path` to install to a different directory.

The build system creates a directory called `build` for intermediate build artifacts. Normally, these files are retained so future builds can use them instead of starting from scratch. You can remove them with `make clean`.

If Gram was installed to the default location, you can uninstall it with `sudo make uninstall`. You can uninstall from a different directory with `make uninstall PREFIX=path`.

## How to contribute

See the file [CONTRIBUTING.md](https://github.com/gramlang/gram/blob/master/CONTRIBUTING.md).

## License

See the file [LICENSE.md](https://github.com/gramlang/gram/blob/master/LICENSE.md).
