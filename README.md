# The Gram Programming Language

[![Build status](https://github.com/gramlang/gram/workflows/Continuous%20integration/badge.svg?branch=main)](https://github.com/gramlang/gram/actions?query=branch%3Amain)

[Gram](https://www.gram.org) is a programming language for distributed systems.

## Installation instructions

### Installation on macOS or Linux (AArch64 or x86-64)

If you're running macOS or Linux (AArch64 or x86-64), you can install Gram with this command:

```sh
curl https://raw.githubusercontent.com/gramlang/gram/main/install.sh -LSfs | sh
```

The same command can be used again to update to the latest version.

The installation script supports the following optional environment variables:

- `VERSION=x.y.z` (defaults to the latest version)
- `PREFIX=/path/to/install` (defaults to `/usr/local/bin`)

For example, the following will install Gram into the working directory:

```sh
curl https://raw.githubusercontent.com/gramlang/gram/main/install.sh -LSfs | PREFIX=. sh
```

If you prefer not to use this installation method, you can download the binary from the [releases page](https://github.com/gramlang/gram/releases), make it executable (e.g., with `chmod`), and place it in some directory in your [`PATH`](https://en.wikipedia.org/wiki/PATH_\(variable\)) (e.g., `/usr/local/bin`).

### Installation on Windows (AArch64 or x86-64)

If you're running Windows (AArch64 or x86-64), download the latest binary from the [releases page](https://github.com/gramlang/gram/releases) and rename it to `gram` (or `gram.exe` if you have file extensions visible). Create a directory called `Gram` in your `%PROGRAMFILES%` directory (e.g., `C:\Program Files\Gram`), and place the renamed binary in there. Then, in the "Advanced" tab of the "System Properties" section of Control Panel, click on "Environment Variables..." and add the full path to the new `Gram` directory to the `PATH` variable under "System variables". Note that the `Program Files` directory might have a different name if Windows is configured for a language other than English.

To update an existing installation, simply replace the existing binary.

### Installation with Cargo

If you have [Cargo](https://doc.rust-lang.org/cargo/), you can install Gram as follows:

```sh
cargo install gram
```

You can run that command with `--force` to update an existing installation.
