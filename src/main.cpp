#include "platform.h"
#include <iostream>

using namespace std;

int main() {
  llc("test", "define i64 @main() {\n  ret i64 0\n}\n");
  return 0;
}
