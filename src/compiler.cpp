#include "compiler.h"
#include "platform.h"
#include <stdexcept>

using namespace std;

void compile(string input_path, string output_path) {
  try {
    llc(output_path, "define i64 @main() {\n  ret i64 0\n}\n");
  } catch(runtime_error &e) {
    throw error(e.what());
  }
}
