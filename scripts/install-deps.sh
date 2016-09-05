#!/usr/bin/env bash
set -eu -o pipefail

# This script tries to download and install the build tools needed to build Gram:
# - GNU Make >= 3.79.1
# - GCC >= 4.9 or Clang >= 3.1
# - CMake >= 3.4.3

# Usage:
#   ./install-deps.sh

# Install make if necessary.
# Gram requires GNU Make >= 3.79.1.
echo 'Looking for sufficient make...'
if ! (which make >/dev/null 2>&1 &&
  (make --version | grep -qi 'make \(3\.79\)\|\(3\.8[0-2]\)\|\(4\.\)')); then
  if (uname | grep -qi 'darwin') && which xcode-select; then # OS X
    # Install the Command Line Tools for Xcode.
    echo 'Installing the Command Line Tools for Xcode...'
    xcode-select --install || true # Fails if already installed
  else
    if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
      echo 'Installing build-essential via apt-get...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install build-essential
    else
      echo 'No sufficient make found.'
      echo 'Please install GNU Make >= 3.79.1.'
      exit 1
    fi
  fi
fi
echo "Found make: $(make --version | head -n 1)"

# Install compilers if necessary.
# Gram requires GCC >= 4.9 or Clang >= 3.1.
echo 'Looking for sufficient C and C++ compilers...'
if (! "${BASH_SOURCE%/*}/get-compiler.sh" CC >/dev/null 2>&1) ||
  (! "${BASH_SOURCE%/*}/get-compiler.sh" CXX >/dev/null 2>&1); then
  echo 'No sufficient C and C++ compilers found.'
  if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
    # Update package index.
    echo 'Updating apt-get index...'
    sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
  fi
  if (uname -a | grep -qi 'ubuntu\|debian') &&
    (DEBIAN_FRONTEND=noninteractive apt-get -Vs install build-essential |
      grep -qi '\(gcc (4\.9\.\)\|\(gcc ([5-9]\.\)'); then # Ubuntu
    # Install build-essential.
    echo 'Installing build-essential via apt-get...'
    sudo DEBIAN_FRONTEND=noninteractive apt-get -y install build-essential
  else
    if uname -a | grep -qi 'ubuntu'; then # Ubuntu
      # Install gcc-4.9 and g++-4.9.
      echo 'Installing gcc-4.9 and g++-4.9 via apt-get...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install \
        software-properties-common python-software-properties # For add-apt-repository
      sudo DEBIAN_FRONTEND=noninteractive add-apt-repository -y \
        ppa:ubuntu-toolchain-r/test # For gcc-4.9 and g++-4.9
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install gcc-4.9 g++-4.9
    else
      if (uname | grep -qi 'darwin') && which xcode-select; then # OS X
        # Install the Command Line Tools for Xcode.
        echo 'Installing the Command Line Tools for Xcode...'
        xcode-select --install || true # Fails if already installed
      else
        echo 'Unable to install sufficient C and C++ compilers.'
        echo 'Please install GCC >= 4.9 or Clang >= 3.1.'
        exit 1
      fi
    fi
  fi
fi
CC="$("${BASH_SOURCE%/*}/get-compiler.sh" CC)"
CXX="$("${BASH_SOURCE%/*}/get-compiler.sh" CXX)"
echo "Found $CC: $($CC --version | head -n 1)"
echo "Found $CXX: $($CXX --version | head -n 1)"

# Install CMake if necessary.
# Gram requires CMake >= 3.4.3.
echo 'Looking for sufficient cmake...'
if ! (which cmake >/dev/null 2>&1 &&
  (cmake --version | grep -qi 'cmake version 3\.\(\(4\.[3-9]\)\|[5-9]\)')); then
  echo 'No sufficient cmake found.'
  if (uname | grep -qi 'darwin') && which brew >/dev/null 2>&1; then # OS X + Homebrew
    # Install via Homebrew.
    echo 'Installing cmake via Homebrew...'
    brew update
    brew install cmake
  else
    if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
    # Update package index.
      echo 'Updating apt-get index...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
    fi
    if (uname -a | grep -qi 'ubuntu\|debian') &&
      (DEBIAN_FRONTEND=noninteractive apt-get -Vs install cmake |
        grep -qi 'cmake (3\.\(\(4\.[3-9]\)\|[5-9]\)'); then # Ubuntu
      # Install via apt-get.
      echo 'Installing cmake via apt-get...'
      sudo DEBIAN_FRONTEND=noninteractive apt-get -y install cmake
    else # Other platforms
      # Build and install from source.
      echo 'Building and installing cmake from source...'
      mkdir -p build/cmake-3.5.2
      tar -xf deps/cmake-3.5.2.tar.gz -C build/cmake-3.5.2 --strip-components=1
      cd build/cmake-3.5.2
      CC="$CC" CXX="$CXX" ./bootstrap
      make
      sudo make install
      cd ../..
      rm -rf build/cmake-3.5.2
    fi
  fi
fi
echo "Found cmake: $(cmake --version | head -n 1)"

echo 'Ready to build Gram.'
