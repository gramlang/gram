#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to determine how many CPU cores we have.

# Usage:
#   ./count-cores.sh

# The `uname` program is assumed to exist.
OS="$(uname -s)"

# Linux
if echo "$OS" | grep -qi 'Linux'; then
  grep -c ^processor /proc/cpuinfo; exit
fi

# OS X
if echo "$OS" | grep -qi 'Darwin'; then
  sysctl -n hw.ncpu; exit
fi
