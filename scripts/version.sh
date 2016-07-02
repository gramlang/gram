#!/usr/bin/env bash
set -eu -o pipefail

# This script creates a C++ source file with information about the version.
# The output is printed to stdout; no actual files are written.

# Usage:
#   ./make-version.sh BUILD_TYPE

# The source of truth for the version number.
VERSION="0.0.1"

# Get the build type (release or debug).
BUILD_TYPE="$1"

# Fetch the git commit hash.
if git diff --quiet HEAD >/dev/null 2>&1; then
  COMMIT_HASH="$(git rev-parse --verify HEAD^{commit})"
else
  # Unless this is a debug build, fail if the working tree doesn't match HEAD.
  if echo "$BUILD_TYPE" | grep -qi 'debug'; then
    COMMIT_HASH=""
  else
    echo "The working tree does not match any commit." >&2
    exit 1
  fi
fi

# Print the version information as a C++ source file.
cat <<-ENDOFMESSAGE
#include "../../../src/version.h"

namespace gram {
  const char *VERSION = "$VERSION";
  const char *COMMIT_HASH = "$COMMIT_HASH";
}
ENDOFMESSAGE
