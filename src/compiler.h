/*
  This header declares the main compiler interface.
*/

#ifndef GRAM_COMPILER_H
#define GRAM_COMPILER_H

#include <string>

namespace gram {

  enum class OutputType {
    ASM,
    BINARY,
    LLVM_ASM,
    LLVM_BITCODE
  };

  void compile(std::string input_path, std::string output_path, gram::OutputType output_type);

}

#endif
