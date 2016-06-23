#include "error.h"

gram::error::error(std::string message) {
  this->message = message;
}

std::string gram::error::what() {
  return message;
}
