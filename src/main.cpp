#include "platform.h"
#include <iostream>
#include <stdexcept>

using namespace std;

int main() {
  try {
    llc("output", "define i64 @main() {\n  ret i64 0\n}\n");
  } catch(std::runtime_error &e) {
    cout << "Error: " << e.what() << endl;
  }
  return 0;
}
