#include "error.h"

gram::error::error(std::string message) : message(message) {
}

std::string gram::error::what() {
  return message;
}
