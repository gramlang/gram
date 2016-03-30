/*
  This header declares a class for errors reported by the compiler.
*/

#ifndef GRAM_ERROR_H
#define GRAM_ERROR_H

#include <string>

class error {
private:
  std::string message;

public:
  error(std::string message);
  std::string what();
};

#endif
