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
      std::string message,
      const std::string &source, std::string source_name,
      size_t start_line, size_t start_col, // Zero-indexed, inclusive
      size_t end_line, size_t end_col // Zero-indexed, exclusive
    );
    std::string what();
  };

}

#endif
