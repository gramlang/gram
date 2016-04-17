#!/bin/sh

# Exit immediately if any command fails.
set -e

# Install compilers if necessary.
if (./which-compiler.sh CC | grep -q "NONE") || (./which-compiler.sh CXX | grep -q "NONE"); then
  if uname -a | grep -q "Ubuntu"; then # Ubuntu
    # Install gcc-4.9 and g++-4.9.
    export DEBIAN_FRONTEND=noninteractive
    apt-get -y update
    apt-get -y install software-properties-common python-software-properties # for add-apt-repository
    add-apt-repository -y ppa:ubuntu-toolchain-r/test # for gcc-4.9 and g++-4.9
    apt-get -y update
    apt-get -y install gcc-4.9 g++-4.9
  else
    if (uname | grep -q "Darwin") && which xcode-select; then # OS X
      # Install the Command Line Tools for Xcode.
      sudo -u $SUDO_USER xcode-select --install || true # Fails if already installed
    else
      echo "Unable to install sufficient C and C++ compilers."
      exit 1
    fi
  fi
fi

# Install CMake if necessary.
if ! (cmake --version | grep -q "cmake version 3"); then
  if (uname | grep -q "Darwin") && which brew; then # OS X + Homebrew
    # Install via Homebrew.
    sudo -u $SUDO_USER brew update
    sudo -u $SUDO_USER brew install cmake
  else # Other platforms
    # Build and install from source.
    mkdir -p build/cmake-3.5.2
    tar -xf deps/cmake-3.5.2.tar.gz -C build/cmake-3.5.2 --strip-components=1
    cd build/cmake-3.5.2 && ./bootstrap
    cd build/cmake-3.5.2 && make
    cd build/cmake-3.5.2 && make install
    rm -rf build/cmake-3.5.2
  fi
fi

echo "Ready to build Gram."
