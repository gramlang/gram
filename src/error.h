/*
  This header declares a class for errors reported by the compiler.
*/

#ifndef GRAM_ERROR_H
#define GRAM_ERROR_H

#include <string>

namespace gram {

  class Error {
  private:
    std::string message;

  public:
    explicit Error(std::string message);
    explicit Error(
      std::string message, // No trailing line break
      const std::string &source,
      std::string source_name,
      size_t start_pos, // Inclusive
      size_t end_pos // Exclusive
    );
    std::string what(); // No trailing line break
  };

}

#endif
