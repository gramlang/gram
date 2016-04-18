#!/bin/sh

# Exit immediately if any command fails.
set -e

# Update package index.
if uname -a | grep -qi 'ubuntu\|debian'; then # Ubuntu or Debian
  echo 'Updating apt-get index...'
  sudo DEBIAN_FRONTEND=noninteractive apt-get -y update
fi
if (uname | grep -qi 'darwin') && which brew >/dev/null 2>&1; then # OS X + Homebrew
  echo 'Updating Homebrew index...'
  brew update
fi

# Install compilers if necessary.
# Gram requires GCC >= 4.9 or Clang >= 3.1.
echo 'Looking for sufficient C and C++ compilers...'
CC="$(./which-compiler.sh CC)"
CXX="$(./which-compiler.sh CXX)"
if (echo "$CC" | grep -qi 'none') || (echo "$CXX" | grep -qi 'none'); then
  echo 'No sufficient C and C++ compilers found.'
  if uname -a | grep -qi 'ubuntu'; then # Ubuntu
    # Install gcc-4.9 and g++-4.9.
    echo 'Installing gcc-4.9 and g++-4.9 via apt-get...'
    sudo DEBIAN_FRONTEND=noninteractive apt-get -y install software-properties-common python-software-properties # For add-apt-repository
    sudo DEBIAN_FRONTEND=noninteractive add-apt-repository -y ppa:ubuntu-toolchain-r/test # For gcc-4.9 and g++-4.9
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
  CC="$(./which-compiler.sh CC)"
  CXX="$(./which-compiler.sh CXX)"
fi
echo "Found C compiler: $CC"
echo "Found C++ compiler: $CXX"

# Install CMake if necessary.
# Gram requires CMake >= 2.8.12.2.
echo 'Looking for sufficient cmake...'
if ! (which cmake >/dev/null 2>&1 && (cmake --version | grep -qi 'cmake version 3')); then
  echo 'No sufficient cmake found.'
  if (uname | grep -qi 'darwin') && which brew >/dev/null 2>&1; then # OS X + Homebrew
    # Install via Homebrew.
    echo 'Installing cmake via Homebrew...'
    brew install cmake
  else
    if (uname -a | grep -qi 'ubuntu\|debian') && (DEBIAN_FRONTEND=noninteractive apt-get -Vs install cmake | grep -qi 'cmake (3\.'); then # Ubuntu + apt-get
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
echo "Found cmake: $(cmake --version | head -n 1 | awk '{print $3}')"

echo 'Ready to build Gram.'
