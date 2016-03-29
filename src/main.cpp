#include <iostream>
#include "platform.h"

using namespace std;

int main() {
  llc("test", "define i64 @main() {\n  ret i64 0\n}\n");
  return 0;
}
