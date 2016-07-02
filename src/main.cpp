#include "compiler.h"
#include "error.h"
#include "version.h"
#include <iostream>

int main(int argc, char *argv[]) {
  // Get the help message.
  if (
    argc == 1 ||
    (argc == 2 && (std::string(argv[1]) == "-h" || std::string(argv[1]) == "--help"))
  ) {
    std::cout << "Gram (https://www.gram.org/)\n";
    std::cout << "----------------------------\n";
    std::cout << "Usage:\n";
    std::cout << "  gram -h\n";
    std::cout << "  gram --help\n";
    std::cout << "  gram -v\n";
    std::cout << "  gram --version\n";
    std::cout << "  gram source dest\n";
    return 0;
  }

  // Get the version information.
  if (argc == 2 && (std::string(argv[1]) == "-v" || std::string(argv[1]) == "--version")) {
    std::cout << "Version: " << gram::VERSION << "\n";
    if (gram::COMMIT_HASH) {
      std::cout << "Commit: " << gram::COMMIT_HASH << "\n";
    }
    std::cout << "Build type: " << gram::BUILD_TYPE << "\n";
    return 0;
  }

  // Invoke the compiler.
  if (argc == 3) {
    try {
      gram::compile(argv[1], argv[2]);
    } catch(gram::error &e) {
      std::cout << "Error: " << e.what() << "\n";
      return 1;
    }
    return 0;
  }

  // We didn't recognize the syntax.
  std::cout << "Try gram --help for more information.\n";
  return 1;
}
