# The Gram Programming Language

[![Build status](https://github.com/gramlang/gram/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/gramlang/gram/actions?query=branch%3Amain)

*Gram* is a programming language for distributed systems.

## Usage

Once Gram is [installed](#installation-instructions), you can run it from the command line as
follows:

```sh
gram program.g
```

Here are the supported command-line options:

```
Usage: gram [PATH] [COMMAND]

Commands:
  check             Check a program
  run               Run a program
  shell-completion  Print a shell completion script. Supports Bash, Fish, Zsh, PowerShell, and
                    Elvish.
  help              Print this message or the help of the given subcommand(s)

Arguments:
  [PATH]  Set the path to the program entrypoint

Options:
  -v, --version  Print version
  -h, --help     Print help
```

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
