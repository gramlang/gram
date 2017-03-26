#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to find a version of Clang >= 3.1 or GCC >= 4.7.
# It returns the absolute path to the compiler.

# Usage:
#   ./get-compiler.sh CC
#   ./get-compiler.sh CXX

if ! (echo "$1" | grep -qi '^\(CC\|CXX\)$'); then
  echo 'The argument must be CC or CXX.' >&2
  exit 1
fi

# Clang >= 3.1
if (clang --version 2> /dev/null | grep -qi ' \(3\.[1-9]\|[4-9]\.\)') &&
  clang++ --version 2> /dev/null | grep -qi ' \(3\.[1-9]\|[4-9]\.\)'; then
  if echo "$1" | grep -qi 'CC'; then which clang; exit; fi
  if echo "$1" | grep -qi 'CXX'; then which clang++; exit; fi
fi

# Clang that ships with Xcode >= 5.0, based on Clang >= 3.3
if (clang --version 2> /dev/null | grep -qi 'apple llvm version [5-9]\.') &&
  clang++ --version 2> /dev/null | grep -qi 'apple llvm version [5-9]\.'; then
  if echo "$1" | grep -qi 'CC'; then which clang; exit; fi
  if echo "$1" | grep -qi 'CXX'; then which clang++; exit; fi
fi

# GCC >= 4.7
if (gcc --version 2> /dev/null | grep -qi ' \(4\.7\.\|[5-9]\.\)') &&
  (g++ --version 2> /dev/null | grep -qi ' \(4\.7\.\|[5-9]\.\)'); then
  if echo "$1" | grep -qi 'CC'; then which gcc; exit; fi
  if echo "$1" | grep -qi 'CXX'; then which g++; exit; fi
fi

# If we made it this far, we were unable to determine the compiler.
echo \
  'No suitable compiler found. You must have Clang >= 3.1 or GCC >= 4.7.' >&2
exit 1
