/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#ifndef GRAM_PLATFORM_H
#define GRAM_PLATFORM_H

#include "compiler.h"
#include <llvm/IR/Module.h>
#include <string>
#include <vector>

namespace gram {

  // Execute a program and return its exit status.
  // This function will use the PATH environment variable to find the program.
  // Raises a std::runtime_error if there was a problem starting the program.
  int execute_program(
    const std::string &path,
    const std::vector<std::string> &args,
    const std::string &stdin,
    std::string &stdout,
    std::string &stderr
  );

  // Compile an LLVM module into a native binary.
  // Raises a std::runtime_error if compilation fails.
  void llc(const std::string &output_path, llvm::Module &module, gram::OutputType output_type);

}

#endif
