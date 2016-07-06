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
  std::vector<std::string> indentations = {""};
  while (pos < source.size()) {
    // If we are at the beginning of a line, consume any indentation.
    if (start_col == 0) {
      while (source[pos] == ' ' || source[pos] == '\t') {
        ++pos;
        ++start_col;
      }
      std::string indentation = source.substr(pos - start_col, start_col);

      // If line wasn't empty and we aren't inside any parentheses, interpret the indentation.
      if (source[pos] != '\n' && source[pos] != '#' && paren_depth == 0) {
        if (indentation == indentations.back()) {
          // Same indentation as before. Just insert a sequencer.
          if (!tokens.empty()) {
            tokens.push_back(Token(TokenType::SEQUENCER, "",
              start_line, start_col, start_line + 1, start_col));
          }
        } else if (indentation.find(indentations.back()) != std::string::npos) {
          // The indentation increased. Open a new block.
          indentations.push_back(indentation);
          tokens.push_back(Token(TokenType::BEGIN,
            source.substr(pos - start_col, start_col),
            start_line, 0, start_line + 1, start_col));
        } else if (indentations.back().find(indentation) != std::string::npos) {
          // The indentation decreased. Close blocks as appropriate.
          while (!indentations.empty() && indentation != indentations.back()) {
            indentations.pop_back();
            tokens.push_back(Token(TokenType::END,
              "", start_line, 0, start_line + 1, start_col));
          }

          // Make sure we ended up at a previous indentation level.
          if (indentations.empty()) {
            throw Error("Unmatched outdent.",
              source, source_name, start_line, 0, start_line + 1, start_col);
          }
        } else {
          // A strange possibility. The new indentation is neither a substring
          // nor a superstring of the old indentation. This can happen if spaces
          // and tabs are mixed.
          throw Error(
            "Unable to compare this indentation to that of previous lines.\n"
            "This can happen if you are mixing tabs and spaces.",
            source, source_name, start_line, 0, start_line + 1, start_col
          );
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

    // Ignore non-indentation whitespace; it is only used to separate other tokens.
    if (source[pos] == ' ' || source[pos] == '\t') {
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

    // Ignore line feeds, but keep track of which line and column we are on.
    if (source[pos] == '\n') {
      ++pos;
      ++start_line;
      start_col = 0;
      continue;
    }

    // Some platforms use carriage returns in combination with line feeds to break lines.
    // Line feeds are handled above. Just ignore carriage returns.
    if (source[pos] == '\r') {
      ++pos;
      ++start_col;
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
  while (indentations.size() > 1) {
    indentations.pop_back();
    tokens.push_back(Token(TokenType::END,
      "", start_line, start_col, start_line + 1, start_col));
  }
}
