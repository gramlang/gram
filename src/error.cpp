#include "error.h"

gram::Error::Error(std::string message) : message(message) {
}

gram::Error::Error(
  std::string message,
  const std::string &source, std::string source_name,
  size_t start_pos, size_t end_pos
) {
  // Compute line numbers, column numbers, and context boundaries.
  size_t start_line = 0;
  size_t start_col = 0;
  size_t end_line = 0;
  size_t end_col = 0;
  size_t context_start_pos = 0;
  size_t context_end_pos = source.size();
  size_t line_number = 0;
  size_t col_number = 0;
  bool found_start = false;
  bool found_end = false;
  for (size_t pos = 0; pos <= source.size(); ++pos) {
    if (pos == start_pos) {
      start_line = line_number;
      start_col = col_number;
      found_start = true;
    }
    if (pos == end_pos) {
      end_line = line_number;
      end_col = col_number;
      found_end = true;
    }
    if (pos == source.size() || source[pos] == '\n') {
      ++line_number;
      col_number = 0;
      if (!found_start) {
        context_start_pos = pos + 1;
      }
      if (found_end) {
        context_end_pos = pos;
        break;
      }
    } else {
      ++col_number;
    }
  }
  auto context = source.substr(
    context_start_pos,
    context_end_pos - context_start_pos
  );

  // Output the source name, position, and message.
  if (end_pos == start_pos || end_pos == start_pos + 1) {
    this->message = source_name +
      " @ " + std::to_string(start_line + 1) +
      ":" + std::to_string(start_col + 1) +
      "\n" + message;
  } else {
    this->message = source_name +
      " @ " + std::to_string(start_line + 1) +
      ":" + std::to_string(start_col + 1) +
      " - " + std::to_string(end_line + 1) +
      ":" + std::to_string(end_col) +
      "\n" + message;
  }

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

      // We have a line feed before the context,
      // so we add one after for symmetry.
      this->message += "\n";
    }
  }
}

std::string gram::Error::what() {
  return message;
}
