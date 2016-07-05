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

std::vector<gram::Token> gram::lex(std::string &source, std::string &source_name) {
  std::vector<Token> tokens;
  size_t pos = 0;
  size_t start_line = 0;
  size_t start_col = 0;
  size_t paren_depth = 0;
  std::vector<size_t> indentations;
  while (pos < source.size()) {
    if (start_col == 0) {
      while (source[pos] == ' ') {
        ++pos;
        ++start_col;
        continue;
      }
      if (source[pos] != '\n' && source[pos] != '#' && paren_depth == 0) {
        if (start_col > 0) {
          if (indentations.empty() || start_col > indentations.back()) {
            indentations.push_back(start_col);
            tokens.push_back(Token(TokenType::BEGIN,
              source.substr(pos - start_col, start_col),
              start_line, 0, start_line + 1, start_col));
          } else if (start_col < indentations.back()) {
            while (!indentations.empty() && start_col < indentations.back()) {
              indentations.pop_back();
              tokens.push_back(Token(TokenType::END,
                "", start_line, 0, start_line + 1, 0));
            }
            if (indentations.empty() || start_col != indentations.back()) {
              throw error("Unmatched outdent.",
                source, source_name, start_line, start_col, start_line + 1, start_col);
            }
          } else if (start_col == indentations.back() && !tokens.empty()) {
            tokens.push_back(Token(TokenType::SEQUENCER, "",
              start_line, start_col, start_line + 1, start_col));
          }
          continue;
        } else {
          if (indentations.empty() && !tokens.empty()) {
            tokens.push_back(Token(TokenType::SEQUENCER, "",
              start_line, start_col, start_line + 1, start_col));
          } else {
            while (!indentations.empty()) {
              indentations.pop_back();
              tokens.push_back(Token(TokenType::END,
                "", start_line, 0, start_line + 1, 0));
            }
          }
        }
      }
    }
    if (source[pos] == '(') {
      tokens.push_back(Token(TokenType::BEGIN, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      ++paren_depth;
      continue;
    }
    if (source[pos] == ')') {
      tokens.push_back(Token(TokenType::END, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      if (paren_depth == 0) {
        throw error("Unmatched ')'.",
          source, source_name, start_line, start_col, start_line + 1, start_col + 1);
      }
      ++pos;
      ++start_col;
      --paren_depth;
      continue;
    }
    if (source[pos] == ':') {
      tokens.push_back(Token(TokenType::COLON, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }
    if (source[pos] == ';') {
      tokens.push_back(Token(TokenType::SEQUENCER, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }
    if (pos < source.size() - 1 && source.substr(pos, 2) == "=>") {
      tokens.push_back(Token(TokenType::THICK_ARROW, source.substr(pos, 2),
        start_line, start_col, start_line + 1, start_col + 2));
      pos += 2;
      start_col += 2;
      continue;
    }
    if (pos < source.size() - 1 && source.substr(pos, 2) == "->") {
      tokens.push_back(Token(TokenType::THIN_ARROW, source.substr(pos, 2),
        start_line, start_col, start_line + 1, start_col + 2));
      pos += 2;
      start_col += 2;
      continue;
    }
    if (source[pos] == '=') {
      tokens.push_back(Token(TokenType::EQUALS, source.substr(pos, 1),
        start_line, start_col, start_line + 1, start_col + 1));
      ++pos;
      ++start_col;
      continue;
    }
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
    if (source[pos] == ' ') {
      ++pos;
      ++start_col;
      continue;
    }
    if (source[pos] == '#') {
      while (pos < source.size() && source[pos] != '\n') {
        ++pos;
        ++start_col;
        continue;
      }
    }
    if (source[pos] == '\n') {
      ++pos;
      ++start_line;
      start_col = 0;
      continue;
    }
    throw error("Unexpected character '" + source.substr(pos, 1) + "'.",
      source, source_name, start_line, start_col, start_line + 1, start_col + 1);
  }
  if (paren_depth > 0) {
    throw error("Missing ')'.",
      source, source_name, start_line, start_col, start_line + 1, start_col + 1);
  }
  while (!indentations.empty()) {
    indentations.pop_back();
    tokens.push_back(Token(TokenType::END,
      "", start_line, start_col, start_line + 1, start_col));
  }
  return tokens;
}
