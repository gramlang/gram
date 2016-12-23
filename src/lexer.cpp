#include "error.h"
#include "lexer.h"

std::unique_ptr<std::vector<gram::Token>> gram::lex(
  std::shared_ptr<std::string> source_name,
  std::shared_ptr<std::string> source
) {
  auto tokens = std::unique_ptr<std::vector<Token>>(new std::vector<Token>);
  size_t pos = 0;
  size_t start_col = 0;
  std::vector<Token> grouping_stack;
  size_t line_continuation_marker_pos = source->size(); // Sentinel value
  while (pos < source->size()) {
    // Comments begin with '#' and continue to the end of the line.
    if ((*source)[pos] == '#') {
      while (pos < source->size() && (*source)[pos] != '\n') {
        ++pos;
        ++start_col;
        continue;
      }
    }

    // Ignore whitespace except for line feeds;
    // it is only used to separate other tokens.
    if (
      (*source)[pos] == ' ' ||
      (*source)[pos] == '\t' ||
      (*source)[pos] == '\r'
    ) {
      ++pos;
      ++start_col;
      continue;
    }

    // Parse line continuation markers.
    if ((*source)[pos] == '\\') {
      line_continuation_marker_pos = pos;
      ++pos;
      ++start_col;
      continue;
    }

    // Ignore line feeds, but keep track of which line and column we are on.
    if ((*source)[pos] == '\n') {
      if (line_continuation_marker_pos == source->size()) {
        tokens->push_back(Token(
          TokenType::SEQUENCER, "",
          source_name, source,
          pos - start_col, pos
        ));
      }

      ++pos;
      start_col = 0;
      line_continuation_marker_pos = source->size();
      continue;
    }

    // If we parsed a line continuation marker, there
    // should have been a subsequent line feed.
    if (line_continuation_marker_pos != source->size()) {
      throw Error(
        "Unexpected '\\'.",
        *source, *source_name,
        line_continuation_marker_pos, line_continuation_marker_pos + 1
      );
    }

    // COLON
    if ((*source)[pos] == ':') {
      tokens->push_back(Token(
        TokenType::COLON, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      ++pos;
      ++start_col;
      continue;
    }

    // EQUALS
    if ((*source)[pos] == '=' && !(
      pos < source->size() - 1 && source->substr(pos, 2) == "=>"
    )) {
      tokens->push_back(Token(
        TokenType::EQUALS, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      ++pos;
      ++start_col;
      continue;
    }

    // IDENTIFIER
    // Identifiers consist of ASCII letters, digits, and underscores, and must
    // not start with a letter. We also accept any bytes >= 0x80, which allows
    // for Unicode symbols.
    if ((*source)[pos] == '_' ||
      ((*source)[pos] >= 'A' && (*source)[pos] <= 'Z') ||
      ((*source)[pos] >= 'a' && (*source)[pos] <= 'z') ||
      ((*source)[pos] & 0x80) != 0) {
      size_t end_pos = pos + 1;
      while (end_pos < source->size() && ((*source)[end_pos] == '_' ||
        ((*source)[end_pos] >= 'A' && (*source)[end_pos] <= 'Z') ||
        ((*source)[end_pos] >= 'a' && (*source)[end_pos] <= 'z') ||
        ((*source)[end_pos] >= '0' && (*source)[end_pos] <= '9') ||
        ((*source)[end_pos] & 0x80) != 0)) {
        ++end_pos;
      }
      size_t length = end_pos - pos;
      tokens->push_back(Token(
        TokenType::IDENTIFIER, source->substr(pos, length),
        source_name, source,
        pos, end_pos
      ));
      start_col += length;
      pos = end_pos;
      continue;
    }

    // LEFT_CURLY
    if ((*source)[pos] == '{') {
      Token token(
        TokenType::LEFT_CURLY, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      );
      tokens->push_back(token);
      grouping_stack.push_back(token);
      ++pos;
      ++start_col;
      continue;
    }

    // LEFT_PAREN
    if ((*source)[pos] == '(') {
      Token token(
        TokenType::LEFT_PAREN, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      );
      tokens->push_back(token);
      grouping_stack.push_back(token);
      ++pos;
      ++start_col;
      continue;
    }

    // LEFT_SQUARE
    if ((*source)[pos] == '[') {
      Token token(
        TokenType::LEFT_SQUARE, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      );
      tokens->push_back(token);
      grouping_stack.push_back(token);
      ++pos;
      ++start_col;
      continue;
    }

    // RIGHT_CURLY
    if ((*source)[pos] == '}') {
      if (grouping_stack.empty()) {
        throw Error(
          "Unmatched '}'.",
          *source, *source_name,
          pos, pos + 1
        );
      } else if (grouping_stack.back().type != TokenType::LEFT_CURLY) {
        throw Error(
          "Unmatched '" + grouping_stack.back().literal + "'.",
          *source, *source_name,
          grouping_stack.back().start_pos, grouping_stack.back().end_pos
        );
      }
      tokens->push_back(Token(
        TokenType::RIGHT_CURLY, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      grouping_stack.pop_back();
      ++pos;
      ++start_col;
      continue;
    }

    // RIGHT_PAREN
    if ((*source)[pos] == ')') {
      if (grouping_stack.empty()) {
        throw Error(
          "Unmatched ')'.",
          *source, *source_name,
          pos, pos + 1
        );
      } else if (grouping_stack.back().type != TokenType::LEFT_PAREN) {
        throw Error(
          "Unmatched '" + grouping_stack.back().literal + "'.",
          *source, *source_name,
          grouping_stack.back().start_pos, grouping_stack.back().end_pos
        );
      }
      tokens->push_back(Token(
        TokenType::RIGHT_PAREN, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      grouping_stack.pop_back();
      ++pos;
      ++start_col;
      continue;
    }

    // RIGHT_SQUARE
    if ((*source)[pos] == ']') {
      if (grouping_stack.empty()) {
        throw Error(
          "Unmatched ']'.",
          *source, *source_name,
          pos, pos + 1
        );
      } else if (grouping_stack.back().type != TokenType::LEFT_SQUARE) {
        throw Error(
          "Unmatched '" + grouping_stack.back().literal + "'.",
          *source, *source_name,
          grouping_stack.back().start_pos, grouping_stack.back().end_pos
        );
      }
      tokens->push_back(Token(
        TokenType::RIGHT_SQUARE, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      grouping_stack.pop_back();
      ++pos;
      ++start_col;
      continue;
    }

    // SEQUENCER
    if ((*source)[pos] == ';') {
      tokens->push_back(Token(
        TokenType::SEQUENCER, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      ++pos;
      ++start_col;
      continue;
    }

    // THICK_ARROW
    if (pos < source->size() - 1 && source->substr(pos, 2) == "=>") {
      tokens->push_back(Token(
        TokenType::THICK_ARROW, source->substr(pos, 2),
        source_name, source,
        pos, pos + 2
      ));
      pos += 2;
      start_col += 2;
      continue;
    }

    // THIN_ARROW
    if (pos < source->size() - 1 && source->substr(pos, 2) == "->") {
      tokens->push_back(Token(
        TokenType::THIN_ARROW, source->substr(pos, 2),
        source_name, source,
        pos, pos + 2
      ));
      pos += 2;
      start_col += 2;
      continue;
    }

    // If we made it this far, the input wasn't recognized and
    // should be rejected.
    throw Error(
      "Unexpected character '" + source->substr(pos, 1) + "'.",
      *source, *source_name,
      pos, pos + 1
    );
  }

  // Make sure all braces/brackets have been closed.
  if (!grouping_stack.empty()) {
    throw Error("Unmatched '" + grouping_stack.back().literal + "'.",
      *source, *source_name,
      grouping_stack.back().start_pos, grouping_stack.back().end_pos
    );
  }

  // Return an std::unique_ptr to the vector.
  return tokens;
}
