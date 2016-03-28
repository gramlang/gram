/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in the implementations of these functions.
*/

#include <string>
#include <vector>

void invoke_llvm(const std::string &command, const std::vector<std::string> &args);
