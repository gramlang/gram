/*
  This header declares a class for errors reported by the compiler.
*/

#ifndef GRAM_ERROR_H
#define GRAM_ERROR_H

#include <string>

namespace gram {

  // Notes about line and column numbering (also noted in lexer.h):
  // - Line feeds exist on the lines they are terminating,
  //   not the following line.
  // - All indices must point to a valid character, with one exception:
  //   the end of the file is represented by the last column number + 1,
  //   with no change to the line number.

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
