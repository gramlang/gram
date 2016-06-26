#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to find a version of GCC >= 4.9 or Clang >= 3.1.

# Usage:
#   ./which-compiler.sh CC
#   ./which-compiler.sh CXX

# These variables will store the compiler names.
CC=NONE
CXX=NONE

# GCC >= 4.9
if (gcc --version 2>/dev/null | grep -qi '\( 4\.9\.\)\|\( [5-9]\.\)') &&
  (gcc++ --version 2>/dev/null | grep -qi '\( 4\.9\.\)\|\( [5-9]\.\)'); then
  CC=gcc
  CXX=g++
fi

# GCC = 4.9, sometimes installed by the install-deps.sh script.
if which gcc-4.9 >/dev/null 2>&1 && which g++-4.9 >/dev/null 2>&1; then
  CC=gcc-4.9
  CXX=g++-4.9
fi

# Clang >= 3.1
if (clang --version 2>/dev/null | grep -qi ' 3\.[1-9]') &&
  clang++ --version 2>/dev/null | grep -qi ' 3\.[1-9]'; then
  CC=clang
  CXX=clang++
fi

# Clang that ships with Xcode >= 5.0, based on Clang >= 3.3
if (clang --version 2>/dev/null | grep -qi 'apple llvm version [5-9]\.') &&
  clang++ --version 2>/dev/null | grep -qi 'apple llvm version [5-9]\.'; then
  CC=clang
  CXX=clang++
fi

# Print the compiler that the user asked for.
if [ "$1" = "CC" ]; then echo "$CC"; fi
if [ "$1" = "CXX" ]; then echo "$CXX"; fi
