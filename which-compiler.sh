# Usage:
#   ./get-compiler.sh CC
#   ./get-compiler.sh CXX

export CC=NONE
export CXX=NONE
if (gcc --version 2>/dev/null | grep -q "( 4\.9\.)|( 5\.)|( 6\.)") && (gcc++ --version 2>/dev/null | grep -q "( 4\.9\.)|( 5\.)|( 6\.)"); then
  export CC=gcc
  export CXX=g++
fi
if which gcc-4.9 >/dev/null 2>&1 && which g++-4.9 >/dev/null 2>&1; then
  export CC=gcc-4.9
  export CXX=g++-4.9
fi
if (clang --version 2>/dev/null | grep -q " 3\.[1-9]") && clang++ --version 2>/dev/null | grep -q " 3\.[1-9]"; then
  export CC=clang
  export CXX=clang++
fi
if (clang --version 2>/dev/null | grep -q "Apple LLVM version 7\.") && clang++ --version 2>/dev/null | grep -q "Apple LLVM version 7\."; then
  export CC=clang
  export CXX=clang++
fi

if [ "$1" = "CC" ]; then
  echo "$CC"
fi
if [ "$1" = "CXX" ]; then
  echo "$CXX"
fi
