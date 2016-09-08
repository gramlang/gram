#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to find a version of Clang >= 3.1 or GCC >= 4.9.

# Usage:
#   ./get-compiler.sh CC
#   ./get-compiler.sh CXX

if ! echo "$1" | grep -qi 'CC\|CXX'; then
  echo 'The argument must be CC or CXX.' >&2
  exit 1
fi

# Clang >= 3.1
if (clang --version 2> /dev/null | grep -qi ' 3\.[1-9]') &&
  clang++ --version 2> /dev/null | grep -qi ' 3\.[1-9]'; then
  if echo "$1" | grep -qi 'CC'; then echo clang; exit; fi
  if echo "$1" | grep -qi 'CXX'; then echo clang++; exit; fi
fi

# Clang that ships with Xcode >= 5.0, based on Clang >= 3.3
if (clang --version 2> /dev/null | grep -qi 'apple llvm version [5-9]\.') &&
  clang++ --version 2> /dev/null | grep -qi 'apple llvm version [5-9]\.'; then
  if echo "$1" | grep -qi 'CC'; then echo clang; exit; fi
  if echo "$1" | grep -qi 'CXX'; then echo clang++; exit; fi
fi

# GCC >= 4.9
if (gcc --version 2> /dev/null | grep -qi ' \(4\.9\.\)\|\([5-9]\.\)') &&
  (gcc++ --version 2> /dev/null | grep -qi ' \(4\.9\.\)\|\([5-9]\.\)'); then
  if echo "$1" | grep -qi 'CC'; then echo gcc; exit; fi
  if echo "$1" | grep -qi 'CXX'; then echo g++; exit; fi
fi

# GCC = 4.9, sometimes installed by the install-deps.sh script
if which gcc-4.9 >/dev/null 2>&1 && which g++-4.9 >/dev/null 2>&1; then
  if echo "$1" | grep -qi 'CC'; then echo gcc-4.9; exit; fi
  if echo "$1" | grep -qi 'CXX'; then echo g++-4.9; exit; fi
fi

# If we made it this far, we were unable to determine the compiler.
echo 'No suitable compiler found. You must have Clang >= 3.1 or GCC >= 4.9.' >&2
exit 1
