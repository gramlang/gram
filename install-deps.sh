#!/bin/sh

# Exit immediately if any command fails.
set -e

# Install compilers if necessary.
CC="$(./which-compiler.sh CC)"
CXX="$(./which-compiler.sh CXX)"
if (echo "$CC" | grep -q "NONE") || (echo "$CXX" | grep -q "NONE"); then
  if uname -a | grep -q "Ubuntu"; then # Ubuntu
    # Install gcc-4.9 and g++-4.9.
    DEBIAN_FRONTEND=noninteractive apt-get -y update
    DEBIAN_FRONTEND=noninteractive apt-get -y install software-properties-common python-software-properties # For add-apt-repository
    DEBIAN_FRONTEND=noninteractive add-apt-repository -y ppa:ubuntu-toolchain-r/test # For gcc-4.9 and g++-4.9
    DEBIAN_FRONTEND=noninteractive apt-get -y update
    DEBIAN_FRONTEND=noninteractive apt-get -y install gcc-4.9 g++-4.9
  else
    if (uname | grep -q "Darwin") && which xcode-select; then # OS X
      # Install the Command Line Tools for Xcode.
      sudo -u $SUDO_USER xcode-select --install || true # Fails if already installed
    else
      echo "Unable to install sufficient C and C++ compilers."
      exit 1
    fi
  fi
  CC="$(./which-compiler.sh CC)"
  CXX="$(./which-compiler.sh CXX)"
fi

# Install CMake if necessary.
if ! (which cmake && (cmake --version | grep -q "cmake version 3")); then
  if (uname | grep -q "Darwin") && which brew; then # OS X + Homebrew
    # Install via Homebrew.
    sudo -u $SUDO_USER brew update
    sudo -u $SUDO_USER brew install cmake
  else
    if (uname -a | grep -q "Ubuntu") && (DEBIAN_FRONTEND=noninteractive apt-get -Vs install cmake | grep -q "cmake (3\."); then # Ubuntu + apt-get
      # Install via apt-get.
      DEBIAN_FRONTEND=noninteractive apt-get -y install cmake
    else # Other platforms
      # Build and install from source.
      mkdir -p build/cmake-3.5.2
      tar -xf deps/cmake-3.5.2.tar.gz -C build/cmake-3.5.2 --strip-components=1
      cd build/cmake-3.5.2 && CC="$CC" CXX="$CXX" ./bootstrap
      cd build/cmake-3.5.2 && make
      cd build/cmake-3.5.2 && make install
      rm -rf build/cmake-3.5.2
    fi
  fi
fi

echo "Ready to build Gram."
