#include "platform.h"

using namespace std;

int main() {
  vector<string> args;
  args.push_back("--version");
  invoke_llvm("llc", args);
  return 0;
}
