#include "compiler.h"
#include "error.h"
#include "version.h"
#include <iostream>

int main(int argc, char *argv[]) {
  // Print this message if we are unable to parse the input.
  const std::string parse_error = "Try gram --help for more information.\n";

  // Display the help message.
  if (
    argc == 1 ||
    (argc == 2 && (
      std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help")
    )
  ) {
    std::cout <<
      "Gram (https://www.gram.org/)\n"
      "----------------------------\n"
      "Usage:\n"
      "  gram -h\n"
      "  gram --help\n"
      "  gram -v\n"
      "  gram --version\n"
      "  gram source dest\n"
      "  gram --emit-tokens source dest\n"
      "  gram --emit-ast source dest\n"
      "  gram --emit-types source dest\n"
      "  gram --emit-llvm-asm source dest\n"
      "  gram --emit-llvm-bitcode source dest\n"
      "  gram --emit-asm source dest\n"
      "  gram --emit-binary source dest\n";
    return 0;
  }

  // Display the version information.
  if (argc == 2 && (
    std::string(argv[1]) == "-v" || std::string(argv[1]) == "--version")
  ) {
    std::cout << "Version: " << gram::VERSION << "\n";
    if (gram::COMMIT_HASH) {
      std::cout << "Commit: " << gram::COMMIT_HASH << "\n";
    }
    std::cout << "Build type: " << gram::BUILD_TYPE << "\n";
    return 0;
  }

  // Invoke the compiler.
  if (argc == 3 || argc == 4) {
    try {
      auto output_type = gram::OutputType::AST;
      auto input_path = argv[1];
      auto output_path = argv[2];
      if (argc == 4) {
        if (std::string(argv[1]) == "--emit-tokens") {
          output_type = gram::OutputType::TOKENS;
        } else if (std::string(argv[1]) == "--emit-ast") {
          output_type = gram::OutputType::AST;
        } else if (std::string(argv[1]) == "--emit-types") {
          output_type = gram::OutputType::TYPES;
        } else if (std::string(argv[1]) == "--emit-llvm-asm") {
          output_type = gram::OutputType::LLVM_ASM;
        } else if (std::string(argv[1]) == "--emit-llvm-bitcode") {
          output_type = gram::OutputType::LLVM_BITCODE;
        } else if (std::string(argv[1]) == "--emit-asm") {
          output_type = gram::OutputType::ASM;
        } else if (std::string(argv[1]) == "--emit-binary") {
          output_type = gram::OutputType::BINARY;
        } else {
          // We didn't recognize the syntax.
          std::cout << parse_error;
          return 1;
        }
        input_path = argv[2];
        output_path = argv[3];
      }
      gram::compile(input_path, output_path, output_type);
    } catch(gram::Error &e) {
      std::cout << "Error: " << e.what() << "\n";
      return 1;
    }
    return 0;
  }

  // We didn't recognize the syntax.
  std::cout << parse_error;
  return 1;
}
