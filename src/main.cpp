#include "platform.h"

using namespace std;

int main() {
  vector<string> llc_args;
  llc_args.push_back("--version");
  invoke_llvm("llc", llc_args);
  return 0;
}
