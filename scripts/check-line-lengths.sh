#!/usr/bin/env bash
set -eu -o pipefail

# This script checks that lines in the given file(s) are at most 80 bytes,
# including newline characters. It prints violations to standard output.

# Usage:
#   ./check-line-lengths.sh file1 file2 ...

export LINE_LENGTH_VIOLATIONS

LINE_LENGTH_VIOLATIONS="$( \
  awk '{ if (length > 79) { print length, FILENAME, "@", FNR }}' "$@" \
)"

echo "$LINE_LENGTH_VIOLATIONS"

if ! test -z "$LINE_LENGTH_VIOLATIONS"; then
  exit 1
fi
