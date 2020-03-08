# The Gram Programming Language

[Gram](https://www.gram.org) is a programming language for distributed systems.

## Installation

### Easy installation

If you are running macOS or a GNU-based Linux on an x86-64 CPU, you can install Gram with this command:

```sh
curl https://raw.githubusercontent.com/gramlang/gram/master/install.sh -LSfs | sh
```

The same command can be used again to update Gram to the latest version.

**NOTE:** Piping `curl` to `sh` is dangerous since the server might be compromised. If you're concerned about this, you can download and inspect the installation script or choose one of the other installation methods.

#### Customizing the installation

The installation script supports the following environment variables:

- `VERSION=x.y.z` (defaults to the latest version)
- `PREFIX=/path/to/install` (defaults to `/usr/local/bin`)

For example, the following will install Gram into the working directory:

```sh
curl https://raw.githubusercontent.com/gramlang/gram/master/install.sh -LSfs | PREFIX=. sh
```

### Manual installation

The [releases page](https://github.com/gramlang/gram/releases) has precompiled binaries for macOS or Linux systems running on an x86-64 CPU. You can download one of them and place it in a directory listed in your [`PATH`](https://en.wikipedia.org/wiki/PATH_\(variable\)).

### Installation with Cargo

If you have [Cargo](https://doc.rust-lang.org/cargo/), you can install Gram as follows:

```sh
cargo install gram
```

You can run that command with `--force` to update an existing installation.
