#include "platform.h"
#include <iostream>

using namespace std;

int main() {
  cout << llc("/dev/stdout", "define i64 @main() {\n  ret i64 0\n}\n") << endl;
  return 0;
}
