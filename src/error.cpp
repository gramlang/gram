#include "error.h"

gram::Error::Error(std::string message) : message(message) {
}

gram::Error::Error(std::string message, std::string &source, std::string &source_name,
      size_t start_line, size_t start_col, size_t end_line, size_t end_col) {
  // The linter gives a nonsensical warning about the line below, so we suppress it.
  // cppcheck-suppress useInitializationList
  this->message = source_name +
    ":" + std::to_string(start_line + 1) +
    ":" + std::to_string(start_col + 1) + "\n" + message;

  // Try to find the line(s) from the source from the line numbers.
  size_t line = 0;
  size_t start_pos;
  for (size_t pos = 0; pos < source.size(); ++pos) {
    if (source[pos] == '\n') {
      ++line;
      if (line == start_line) {
        start_pos = pos + 1;
      }
      if (line == end_line) {
        // Output the line.
        this->message += "\n\n" + source.substr(start_pos, pos - start_pos) + "\n";

        // If there was only one line, highlight the offending section with carets.
        if (end_line - start_line == 1 && end_col - start_col > 0) {
          for (size_t i = 0; i < start_col; ++i) {
            if (source[start_pos + i] == '\t') {
              // Assume a tab takes up 8 spaces in whatever program
              // this error message is being displayed in. Not ideal,
              // but what else can we do?
              this->message += "        ";
            } else {
              this->message += " ";
            }
          }
          for (size_t j = 0; j < end_col - start_col; ++j) {
            if (source[start_pos + start_col + j] == '\t') {
              // Assume a tab takes up 8 carets in whatever program
              // this error message is being displayed in. Not ideal,
              // but what else can we do?
              this->message += "^^^^^^^^";
            } else {
              this->message += "^";
            }
          }
          this->message += "\n";
        }
        break;
      }
    }
  }
}

std::string gram::Error::what() {
  return message;
}
