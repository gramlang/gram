/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#ifndef GRAM_PLATFORM_H
#define GRAM_PLATFORM_H

#include <string>
#include <vector>
#include <llvm/IR/Module.h>

// Execute a program. Returns the stdout from the program.
// Raises a std::runtime_error if the program does not exit successfully.
std::string execute_file(
  const std::string &path,
  const std::vector<std::string> &args,
  const std::string &stdin
);

// Compile an LLVM module into a native binary.
// Raises a std::runtime_error if compilation fails.
void llc(const std::string output_path, llvm::Module &module);

#endif
