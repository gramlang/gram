/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#include <string>
#include <vector>

// Invoke an LLVM command. For example:
//   vector<string> args;
//   args.push_back("--version");
//   invoke_llvm("llc", args);
void invoke_llvm(const std::string &command, const std::vector<std::string> &args);
