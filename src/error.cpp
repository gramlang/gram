#include "error.h"

gram::Error::Error(std::string message) : message(message) {
}

gram::Error::Error(
  std::string message,
  const std::string &source, std::string source_name,
  size_t start_line, size_t start_col,
  size_t end_line, size_t end_col
) {
  this->message = source_name +
    ":" + std::to_string(start_line + 1) +
    ":" + std::to_string(start_col + 1) + "\n" + message;

  // Try to find the line(s) from the source by the line numbers.
  size_t line_number = 0;
  size_t start_pos = 0;
  size_t end_pos = source.size();
  for (size_t pos = 0; pos < source.size(); ++pos) {
    if (source[pos] == '\n') {
      ++line_number;
      if (line_number == start_line) {
        start_pos = pos + 1;
      }
      if (line_number == end_line + 1) {
        end_pos = pos;
        break;
      }
    }
  }
  auto context = source.substr(start_pos, end_pos - start_pos);

  // Check if the context has only whitespace.
  auto only_whitespace = true;
  for (auto c : context) {
    if (c != ' ' && c != '\t' && c != '\r' && c != '\n') {
      only_whitespace = false;
      break;
    }
  }

  // If there is nothing but whitespace, don't bother.
  if (!only_whitespace) {
    // Output the context.
    this->message += "\n\n" + context + "\n";

    // If there was only one line, highlight the offending section with carets.
    if (end_line == start_line) {
      // Before printing the carets, indent until we are at the right column.
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

      // Add the carets to highlight the problematic code.
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

      // We have a line feed before the context, so we add one after for symmetry.
      this->message += "\n";
    }
  }
}

std::string gram::Error::what() {
  return message;
}
