#include "error.h"

using namespace std;

error::error(string message) {
  this->message = message;
}

string error::what() {
  return message;
}
