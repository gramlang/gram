#!/usr/bin/env bash
set -eu -o pipefail

# This script creates a C++ source file with information about the version.
# The output is printed to stdout; no actual files are written.

# Usage:
#   ./make-version.sh

# The source of truth for the version number.
VERSION="0.0.1"

# Fetch the git commit hash.
if git diff --quiet HEAD >/dev/null 2>&1; then
  COMMIT_HASH="$(git rev-parse --verify HEAD^{commit})"
else
  echo "The working tree does not match any commit." >&2
  exit 1
fi

# Print the version information as a C++ source file.
cat <<-ENDOFMESSAGE
#include "../../src/version.h"

namespace gram {
  const char *VERSION = "$VERSION";
  const char *COMMIT_HASH = "$COMMIT_HASH";
}
ENDOFMESSAGE
