#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to download and install the tools needed to lint:
# - ShellCheck
# - Cppcheck

# Usage:
#   ./install-lint-deps.sh

# Install ShellCheck if necessary.
echo 'Looking for shellcheck...'
if ! (which shellcheck >/dev/null 2>&1); then
  if (uname | grep -qi 'darwin') && which brew >/dev/null 2>&1; then # OS X + Homebrew
    # Install via Homebrew.
    echo 'Installing shellcheck via Homebrew...'
    brew update
    brew install shellcheck
  else
    if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
      # Install via apt-get.
      echo 'Installing shellcheck via apt-get...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install shellcheck
    else
      echo 'No sufficient shellcheck found.'
      echo 'Please install shellcheck.'
      exit 1
    fi
  fi
fi
echo "Found shellcheck: $(shellcheck -V | grep '^version:')"

# Install Cppcheck if necessary.
echo 'Looking for cppcheck...'
if ! (which cppcheck >/dev/null 2>&1); then
  if (uname | grep -qi 'darwin') && which brew >/dev/null 2>&1; then # OS X + Homebrew
    # Install via Homebrew.
    echo 'Installing cppcheck via Homebrew...'
    brew update
    brew install cppcheck
  else
    if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
      # Install via apt-get.
      echo 'Installing cppcheck via apt-get...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install cppcheck
    else
      echo 'No sufficient cppcheck found.'
      echo 'Please install cppcheck.'
      exit 1
    fi
  fi
fi
echo "Found cppcheck: $(cppcheck --version)"

echo 'Ready to lint.'
