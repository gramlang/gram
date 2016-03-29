/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#include <string>
#include <vector>

// Return the path to this executable.
std::string get_executable_path();

// Execute a program. Returns the output of the program.
// Raises a std::runtime_error if the program does not exit successfully.
std::string execute_file(
  const std::string &file,
  const std::vector<std::string> &args,
  const std::string &stdin
);

// Compile LLVM assembly into a native binary.
void llc(const std::string filename, const std::string llvm_asm);
