#!/usr/bin/env bash
set -eu -o pipefail

# This script checks for three things:
# 1. Lines should not have trailing whitespace.
# 2. Tabs should not be used anywhere.
# 3. Every non-empty file should be terminated with exactly one blank line.
# Violations are printed to standard output.

# Usage:
#   SKIP_TABS_CHECK=FALSE ./catch-trailing-whitespace.sh file1 file2 ...

# Lines should not have trailing whitespace.
TRAILING_WHITESPACE_VIOLATIONS="$( \
  awk '/ $/ { print FILENAME, "@", FNR }' "$@" \
)"

# Tabs should not be used anywhere.
TAB_VIOLATIONS=""
if [ "$SKIP_TABS_CHECK" != 'TRUE' ]; then
  TAB_VIOLATIONS="$( \
    awk '/\t/ { print FILENAME, "@", FNR }' "$@" \
  )"
fi

# Every non-empty file should be terminated with exactly one blank line.
BLANK_LINE_VIOLATIONS=""
for file in "$@"; do
  if test -n "$(tail -c 1 "$file")"; then
    BLANK_LINE_VIOLATIONS="$file $BLANK_LINE_VIOLATIONS"
  fi
  if test -z "$(tail -c 2 "$file")"; then
    BLANK_LINE_VIOLATIONS="$file $BLANK_LINE_VIOLATIONS"
  fi
done

# Print the violations.
echo 'Checking for trailing whitespace...'
if test -n "$TRAILING_WHITESPACE_VIOLATIONS"; then
  echo "$TRAILING_WHITESPACE_VIOLATIONS"
fi
if [ "$SKIP_TABS_CHECK" != 'TRUE' ]; then
  echo 'Checking for tabs...'
fi
if test -n "$TAB_VIOLATIONS"; then
  echo "$TAB_VIOLATIONS"
fi
echo 'Checking that each file ends with exactly one blank line...'
if test -n "$BLANK_LINE_VIOLATIONS"; then
  echo "$BLANK_LINE_VIOLATIONS"
fi

# Fail if any of the checks failed.
if test -n "$TRAILING_WHITESPACE_VIOLATIONS"; then
  exit 1
fi
if test -n "$TAB_VIOLATIONS"; then
  exit 1
fi
if test -n "$BLANK_LINE_VIOLATIONS"; then
  exit 1
fi
