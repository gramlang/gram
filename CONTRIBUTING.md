# Contributing to Gram

Thank you for your interest in contributing! You can contribute by filing [issues](https://github.com/gramlang/gram/issues) and submitting [pull requests](https://github.com/gramlang/gram/pulls).

All code must strictly adhere to the appropriate style guide. If a style guide is changed, then any applicable code must be changed accordingly. For any matters not prescribed by a style guide, be consistent with existing code.

All code must pass the linters in order to be accepted:

- [Clang Static Analyzer](http://clang-analyzer.llvm.org/)
- [ShellCheck](http://www.shellcheck.net/)

If you have these tools installed, you can run them with `make lint`.

## C++ style guide

### Formatting

Lines should be less than or equal to 80 bytes, including whitespace and newline characters.

**Motivation:** We don't want lines to grow without bound.

The unit for indentation is two spaces.

**Motivation:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

Opening curly braces `{`, square brackets `[`, and parentheses `(` should be placed on the same line as the opening construct, preceded by a single space. The closing and opening braces surrounding an `else` should be on the same line. For example:

```C++
if (condition) {
  doSomething();
} else {
  doSomethingElse();
}
```

**Motivation:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Header files

Header files should be self-contained, meaning that they can be included without requiring the user to define any symbols, include any other header files beforehand, or perform any other ceremony.

**Motivation:** This allows users to include the header without adhering to any special conditions.

Header files should have `#define` guards to prevent multiple inclusion. The format should be `GRAM_<NAME>_H` where `<NAME>` is the name of the file, in uppercase, without the `.h` extension.

**Motivation:** This ensures that including a header file multiple times (directly or indirectly) is safe.

Header files should be included in lexicographical order, including the opening `"` or `<` delimiters. That means local header files come before system header files. Header files should be included at the top of the file, outside any namespaces, but inside the include guard if applicable.

**Motivation:** This convention makes it easy to determine if a particular header is included.

### Namespaces

Define everything in the `gram` namespace, except the `main` function and other code in its translation unit.

**Motivation:** This is useful for preventing name collisions with other identifiers in the global scope.

Do not use `using` directives, such as `using namespace std`.

**Motivation:** This directive pollutes the global namespace. It is easier to relax this rule later on than to introduce it retroactively, so it is safer to start with it.

### Portability

Put all platform-specific code in the [`src/platform.cpp`](https://github.com/gramlang/gram/blob/master/src/platform.cpp) file.

**Motivation:** If we want to port the code to a new platform, then we only have to port one file.

## Bash style guide

### Formatting

Lines should be less than or equal to 80 bytes, including whitespace and newline characters.

**Motivation:** We don't want lines to grow without bound.

The unit for indentation is two spaces.

**Motivation:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Safety

Scripts should include the following prefix at the top of the file:

```bash
#!/usr/bin/env bash
set -eu -o pipefail
```

**Motivation:** The first line is a shebang which locates a user's preferred Bash, rather than assuming it is the one located at `/bin/bash`. The second line causes the script to fail fast in case of an error, and to treat unset variables as an error.
