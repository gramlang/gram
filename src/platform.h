/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#ifndef GRAM_PLATFORM_H
#define GRAM_PLATFORM_H

#include <string>
#include <vector>

// Return whether a file exists.
bool file_exists(const std::string& path);

// Return the path to this executable.
std::string get_executable_path();

// Execute a program. Returns the stdout from the program.
// Raises a std::runtime_error if the program does not exit successfully.
std::string execute_file(
  const std::string &path,
  const std::vector<std::string> &args,
  const std::string &stdin
);

// Compile LLVM assembly into a native binary.
// Returns the stdout from llc.
std::string llc(const std::string output_path, const std::string llvm_asm);

#endif
