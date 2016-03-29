#include "platform.h"
#include <iostream>

using namespace std;

int main() {
  // Get the path to the LLVM compiler.
  string executable_path = get_executable_path();
  string llc_path = executable_path.substr(0, executable_path.size() - 4) + "llvm/llc";

  // This is the LLVM source.
  string llvm_asm = "define i64 @main() {\n  ret i64 0\n}\n";

  // Compile the LLVM to assembly.
  vector<string> llvm_args;
  llvm_args.push_back("-O=3");
  string native_asm = execute_file(llc_path, llvm_args, llvm_asm);

  // Compile the assembly with GCC.
  vector<string> gcc_args;
  gcc_args.push_back("-o");
  gcc_args.push_back("test");
  gcc_args.push_back("-x");
  gcc_args.push_back("assembler");
  gcc_args.push_back("-");
  string native_exe = execute_file("/usr/bin/gcc", gcc_args, native_asm);

  return 0;
}
