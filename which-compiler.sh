# Usage:
#   ./get-compiler.sh CC
#   ./get-compiler.sh CXX

CC=NONE
CXX=NONE
if (gcc --version 2>/dev/null | grep -q "( 4\.9\.)|( 5\.)|( 6\.)") && (gcc++ --version 2>/dev/null | grep -q "( 4\.9\.)|( 5\.)|( 6\.)"); then
  CC=gcc
  CXX=g++
fi
if which gcc-4.9 >/dev/null 2>&1 && which g++-4.9 >/dev/null 2>&1; then
  CC=gcc-4.9
  CXX=g++-4.9
fi
if (clang --version 2>/dev/null | grep -q " 3\.[1-9]") && clang++ --version 2>/dev/null | grep -q " 3\.[1-9]"; then
  CC=clang
  CXX=clang++
fi
if (clang --version 2>/dev/null | grep -q "Apple LLVM version 7\.") && clang++ --version 2>/dev/null | grep -q "Apple LLVM version 7\."; then
  CC=clang
  CXX=clang++
fi

if [ "$1" = "CC" ]; then
  echo "$CC"
fi
if [ "$1" = "CXX" ]; then
  echo "$CXX"
fi
