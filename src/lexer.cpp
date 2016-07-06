#include "error.h"
#include "lexer.h"

gram::Token::Token(
  gram::TokenType type,
  std::string literal,
  size_t start_line,
  size_t start_col,
  size_t end_line,
  size_t end_col) :
  type(type), literal(literal),
  start_line(start_line), start_col(start_col),
  end_line(end_line), end_col(end_col) {
}

std::string gram::Token::show() {
  return std::string(
    TokenTypeName[static_cast<typename std::underlying_type<TokenType>::type>(type)]
  ) + ": '" + literal + "'";
}

void gram::lex(std::vector<gram::Token> &tokens, std::string &source, std::string &source_name) {
  size_t pos = 0;
  size_t start_line = 0;
  size_t start_col = 0;
  size_t paren_depth = 0;
  std::vector<size_t> indentations;
  while (pos < source.size()) {
    // If we are at the beginning of a line, consume any indentation.
    if (start_col == 0) {
      while (source[pos] == ' ') {
        ++pos;
        ++start_col;
        continue;
      }

      // If line wasn't empty and we aren't inside any parentheses, record the indentation.
      if (source[pos] != '\n' && source[pos] != '#' && paren_depth == 0) {
        if (start_col > 0) {
          // There is some indentation. Match it up with the previous line.
          if (indentations.empty() || start_col > indentations.back()) {
            // The indentation increased. Open a new block.
            indentations.push_back(start_col);
            tokens.push_back(Token(TokenType::BEGIN,
              source.substr(pos - start_col, start_col),
              start_line, 0, start_line + 1, start_col));
          } else if (start_col < indentations.back()) {
            // The indentation decreased. Close blocks as appropriate.
            while (!indentations.empty() && start_col < indentations.back()) {
              indentations.pop_back();
              tokens.push_back(Token(TokenType::END,
                "", start_line, 0, start_line + 1, 0));
            }

            // Make sure we ended up at a previous indentation level.
            if (indentations.empty() || start_col != indentations.back()) {
              throw Error("Unmatched outdent.",
                source, source_name, start_line, start_col, start_line + 1, start_col);
            }
          } else if (start_col == indentations.back() && !tokens.empty()) {
            // Same indentation as before. Just insert a sequencer.
            tokens.push_back(Token(TokenType::SEQUENCER, "",
              start_line, start_col, start_line + 1, start_col));
          }
          continue;
        } else {
          // There is no indentation on this line.
          if (indentations.empty() && !tokens.empty()) {
            // Same indentation as before. Just insert a sequencer.
            tokens.push_back(Token(TokenType::SEQUENCER, "",
              start_line, start_col, start_line + 1, start_col));
          } else {
            // We were indented before. Close any indentation blocks.
            while (!indentations.empty()) {
              indentations.pop_back();
              tokens.push_back(Token(TokenType::END,
                "", start_line, 0, start_line + 1, 0));
            }
          }
        }
      }
    }

    // Opening parenthesis
    if (source[pos] == '(') {
      tokens.push_back(Token(TokenType::BEGIN, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      ++paren_depth;
      continue;
    }

    // Type annotation
    if (source[pos] == ':') {
      tokens.push_back(Token(TokenType::COLON, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }

    // Closing parenthesis
    if (source[pos] == ')') {
      tokens.push_back(Token(TokenType::END, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      if (paren_depth == 0) {
        throw Error("Unmatched ')'.",
          source, source_name, start_line, start_col, start_line + 1, start_col + 1);
      }
      ++pos;
      ++start_col;
      --paren_depth;
      continue;
    }

    // Equals sign
    if (source[pos] == '=' && !(pos < source.size() - 1 && source.substr(pos, 2) == "=>")) {
      tokens.push_back(Token(TokenType::EQUALS, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }

    // Identifiers consist of ASCII letters, digits, and underscores, and must not start
    // with a letter. We also accept any bytes >= 0x80, which allows for Unicode symbols.
    if (source[pos] == '_' ||
      (source[pos] >= 'A' && source[pos] <= 'Z') ||
      (source[pos] >= 'a' && source[pos] <= 'z') ||
      (source[pos] & 0x80) != 0) {
      size_t end_pos = pos + 1;
      while (end_pos < source.size() && (source[end_pos] == '_' ||
        (source[end_pos] >= 'A' && source[end_pos] <= 'Z') ||
        (source[end_pos] >= 'a' && source[end_pos] <= 'z') ||
        (source[end_pos] >= '0' && source[end_pos] <= '9') ||
        (source[pos] & 0x80) != 0)) {
        ++end_pos;
      }
      size_t length = end_pos - pos;
      tokens.push_back(Token(TokenType::IDENTIFIER, source.substr(pos, length),
        start_line, start_col, start_line + 1, start_col + length));
      start_col += length;
      pos = end_pos;
      continue;
    }

    // Integers are composed of digits optionally preceded by a '-' sign.
    if ((pos < source.size() - 1 && source[pos] == '-' &&
      source[pos + 1] >= '0' && source[pos + 1] <= '9') ||
      (source[pos] >= '0' && source[pos] <= '9')) {
      size_t end_pos = pos + 1;
      while (end_pos < source.size() && source[end_pos] >= '0' && source[end_pos] <= '9') {
        ++end_pos;
      }
      size_t length = end_pos - pos;
      tokens.push_back(Token(TokenType::INTEGER, source.substr(pos, length),
        start_line, start_col, start_line + 1, start_col + length));
      start_col += length;
      pos = end_pos;
      continue;
    }

    // Expression sequencer
    if (source[pos] == ';') {
      tokens.push_back(Token(TokenType::SEQUENCER, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }

    // Dependent product type
    if (pos < source.size() - 1 && source.substr(pos, 2) == "=>") {
      tokens.push_back(Token(TokenType::THICK_ARROW, source.substr(pos, 2),
        start_line, start_col, start_line + 1, start_col + 2));
      pos += 2;
      start_col += 2;
      continue;
    }

    // Abstraction
    if (pos < source.size() - 1 && source.substr(pos, 2) == "->") {
      tokens.push_back(Token(TokenType::THIN_ARROW, source.substr(pos, 2),
        start_line, start_col, start_line + 1, start_col + 2));
      pos += 2;
      start_col += 2;
      continue;
    }

    // Ignore non-indentation spaces; they are only used to separate other tokens.
    if (source[pos] == ' ') {
      ++pos;
      ++start_col;
      continue;
    }

    // Comments begin with '#' and continue to the end of the line.
    if (source[pos] == '#') {
      while (pos < source.size() && source[pos] != '\n') {
        ++pos;
        ++start_col;
        continue;
      }
    }

    // Ignore newlines, but keep track of which line and column we are on.
    if (source[pos] == '\n') {
      ++pos;
      ++start_line;
      start_col = 0;
      continue;
    }

    // If we made it this far, the input wasn't recognized and should be rejected.
    throw Error("Unexpected character '" + source.substr(pos, 1) + "'.",
      source, source_name, start_line, start_col, start_line + 1, start_col + 1);
  }

  // Make sure all parentheses have been closed.
  if (paren_depth > 0) {
    throw Error("Missing ')'.",
      source, source_name, start_line, start_col, start_line + 1, start_col + 1);
  }

  // Close any indentation blocks.
  while (!indentations.empty()) {
    indentations.pop_back();
    tokens.push_back(Token(TokenType::END,
      "", start_line, start_col, start_line + 1, start_col));
  }
}
