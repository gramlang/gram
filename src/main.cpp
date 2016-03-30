#include "compiler.h"
#include <iostream>

using namespace std;

int main(int argc, char *argv[]) {
  // Get the help message.
  if (argc == 2 && (string(argv[1]) == "-h" || string(argv[1]) == "--help")) {
    cout << "Gram (https://www.gram.org/)" << endl;
    cout << "----------------------------" << endl;
    cout << "Usage:" << endl;
    cout << "  gram -h" << endl;
    cout << "  gram --help" << endl;
    cout << "  gram -v" << endl;
    cout << "  gram --version" << endl;
    cout << "  gram input output" << endl;
    return 0;
  }

  // Get the version.
  if (argc == 2 && (string(argv[1]) == "-v" || string(argv[1]) == "--version")) {
    cout << "Gram 0.0.1" << endl;
    return 0;
  }

  // Invoke the compiler.
  if (argc == 3) {
    try {
      compile(argv[1], argv[2]);
    } catch(error &e) {
      cout << "Error: " << e.what() << endl;
      return 1;
    }
    return 0;
  }

  // We didn't recognize the syntax.
  cout << "Try gram --help for more information." << endl;
  return 1;
}
