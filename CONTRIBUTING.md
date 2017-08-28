# Contributing to Gram

Thank you for your interest in contributing! You can contribute by filing [issues](https://github.com/gramlang/gram/issues) and submitting [pull requests](https://github.com/gramlang/gram/pulls). You can also contribute by discussing the language on the [gram-dev@googlegroups.com](https://groups.google.com/forum/#!forum/gram-dev) mailing list.

All code must strictly adhere to the appropriate style guide. If a style guide is changed, then any applicable code must be changed accordingly. For any matters not prescribed by a style guide, be consistent with existing code.

All code must pass the following lint checks in order to be accepted:

- C++ source and header files must pass the [Clang Static Analyzer](http://clang-analyzer.llvm.org/).
- Bash scripts must pass the [ShellCheck](http://www.shellcheck.net/) linter.
- Lines should be less than or equal to 80 bytes, including whitespace and newline characters.
- Lines should not have trailing whitespace.
- Tabs should not be used anywhere, except in the `Makefile`.
- Every non-empty file should be terminated with exactly one blank line.

The continuous integration system will run these checks on pull requests automatically. If you have these tools installed, you can run them yourself with `make lint`.

## C++ style guide

### Comments

- Comments need not be complete sentences. Comments which are complete sentences should be punctuated as such. Either way, comments should be written in sentence case. For example:

  ```c++
  // These bounds define the source region spanned by the token.
  std::size_t start_pos; // Inclusive
  std::size_t end_pos; // Exclusive
  ```

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Identifiers

- Variables and functions should be written in `snake_case`. Types (classes, structs, etc.) should be written in `UpperCamelCase`. Constants should be written in `ALL_CAPS`.

  **Rationale:** This is consistent with many C++ codebases.

### Formatting

- Lines should be less than or equal to 80 bytes, including whitespace and newline characters.

  **Rationale:** We don't want lines to grow without bound.

- The unit for indentation is two spaces.

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

- Opening curly braces `{`, square brackets `[`, and parentheses `(` should be placed on the same line as the opening construct, preceded by a single space. The closing and opening braces surrounding an `else` should be on the same line. For example:

  ```c++
  if (condition) {
    doSomething();
  } else {
    doSomethingElse();
  }
  ```

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Header files

- Header files should be self-contained, meaning that they can be included without requiring the user to define any symbols, include any other header files beforehand, or perform any other ceremony.

  **Rationale:** This allows users to include the header without adhering to any special conditions.

- Header files should have `#define` guards to prevent multiple inclusion. The format should be `GRAM_<NAME>_H` where `<NAME>` is the name of the file, in uppercase, without the `.h` extension.

  **Rationale:** This ensures that including a header file multiple times (directly or indirectly) is safe.

- Header files should be included in lexicographical order, including the opening `"` or `<` delimiters. Header files should be included at the top of the file, outside any namespaces, but inside the include guard if applicable.

  **Rationale:** This convention makes it easy to determine if a particular header is included.

### Namespaces

- Define everything in the `gram` namespace, except the `main` function and other code in its translation unit.

  **Rationale:** This is useful for preventing name collisions with other identifiers in the global scope.

- Do not use `using` directives, such as `using namespace std`.

  **Rationale:** This directive pollutes the global namespace. It is easier to relax this rule later on than to introduce it retroactively, so it is safer to start with it.

### Immutability

- All public member variables should be declared `const`.

  **Rationale:** We want to discourage mutation to make the code easier to reason about.

- All public member methods should be declared `const` when possible.

  **Rationale:** We want to discourage mutation to make the code easier to reason about.

### Portability

- Put all platform-specific code in the [`src/platform.cpp`](https://github.com/gramlang/gram/blob/master/src/platform.cpp) file.

  **Rationale:** If we want to port the code to a new platform, then we only have to port one file.

### Converting constructors

- All constructors must be marked `explicit`.

  **Rationale:** Converting constructors are a form of weak typing, and we want to encourage a strong typing discipline to catch as many errors as possible at compile time.

### Visibility

- `protected` members of a class should be declared after all the `public` members, and `private` members should be declared after all the `protected` members.

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

## Bash style guide

### Comments

- Comments need not be complete sentences. Comments which are complete sentences should be punctuated as such. Either way, comments should be written in sentence case. For example:

  ```bash
  # These bounds define the source region spanned by the token.
  export START_POS # Inclusive
  export END_POS # Exclusive
  ```

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Identifiers

- Functions should be written in `snake_case`. Variables should be written in `ALL_CAPS`.

  **Rationale:** This is consistent with many Bash codebases.

### Formatting

- Lines should be less than or equal to 80 bytes, including whitespace and newline characters.

  **Rationale:** We don't want lines to grow without bound.

- The unit for indentation is two spaces.

  **Rationale:** This is somewhat arbitrary, but we want to choose a convention and be consistent everywhere.

### Safety

- Scripts should include the following prefix at the top of the file:

  ```bash
  #!/usr/bin/env bash
  set -eu -o pipefail
  ```

  **Rationale:** The first line is a shebang which locates a user's preferred Bash, rather than assuming it is the one located at `/bin/bash`. The second line causes the script to fail fast in case of an error, and to treat unset variables as an error.

## Git style guide

### Commit messages

- The commit message should consist of a subject and an optional body, separated by an empty line.

  **Rationale:** The subject gives the reader a brief summary of the change. The body provides additional details, if necessary.

- The subject should be written in sentence case, in the imperative mood, and with no trailing punctuation.

  **Rationale:** This is consistent with commit messages generated by Git and GitHub. For example:

  ```
  Merge pull request #81 from gram/git-style-guide
  ```

- The body should be written like prose, using proper capitalization and punctuation.

  **Rationale:** This is consistent with many Git repositories.
