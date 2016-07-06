#!/usr/bin/env bash
set -eu -o pipefail

# This script creates a C++ source file with information about the version.
# The output is printed to stdout; no actual files are written.

# Usage:
#   ./version.sh BUILD_TYPE

# The source of truth for the version number.
VERSION='0.0.1'

# Fetch the git commit hash.
if git diff --quiet HEAD >/dev/null 2>&1; then
  COMMIT_HASH="$(git rev-parse --verify 'HEAD^{commit}')"
else
  COMMIT_HASH=''
fi

# Get the build type (release or debug).
if test "$#" -ne 1; then
  echo 'Missing build type.'
  exit 1
else
  if test "$1" = 'release' || test "$1" = 'debug'; then
    BUILD_TYPE="$1"
  else
    echo "BUILD_TYPE must be 'release' or 'debug'"
  fi
fi

# Print the version information as a C++ source file.
cat <<-ENDOFMESSAGE
#include "../../../src/version.h"

namespace gram {

  const char * const VERSION = "$VERSION";
  const char * const COMMIT_HASH = $(test -z "$COMMIT_HASH" && echo '0' || echo "\"$COMMIT_HASH\"");
  const char * const BUILD_TYPE = "$BUILD_TYPE";

}
ENDOFMESSAGE
