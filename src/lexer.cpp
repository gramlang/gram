#include "error.h"
#include "lexer.h"

enum class LineContinuationStatus {
  LCS_DEFAULT,
  LCS_WAIT_FOR_NEWLINE,
  LCS_WAIT_FOR_TOKEN
};

std::unique_ptr<std::vector<gram::Token>> gram::lex(
  std::shared_ptr<std::string> source_name,
  std::shared_ptr<std::string> source
) {
  auto tokens = std::unique_ptr<std::vector<Token>>(new std::vector<Token>);
  size_t pos = 0;
  std::vector<Token> grouping_stack;
  LineContinuationStatus line_continuation_status =
    LineContinuationStatus::LCS_DEFAULT;
  size_t line_continuation_marker_pos = 0;

  while (pos < source->size()) {
    // Comments begin with '#' and continue to the end of the line.
    if ((*source)[pos] == '#') {
      while (pos < source->size() && (*source)[pos] != '\n') {
        ++pos;
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
      continue;
    }

    // Parse line continuation markers.
    if ((*source)[pos] == '\\') {
      if (line_continuation_status != LineContinuationStatus::LCS_DEFAULT) {
        throw Error(
          "Duplicate '\\'.",
          *source, *source_name,
          pos, pos + 1
        );
      }
      line_continuation_status = LineContinuationStatus::LCS_WAIT_FOR_NEWLINE;
      line_continuation_marker_pos = pos;
      ++pos;
      continue;
    }

    // For line feeds, insert a SEQUENCER unless
    // there was a line continuation marker.
    if ((*source)[pos] == '\n') {
      if (line_continuation_status == LineContinuationStatus::LCS_DEFAULT) {
        tokens->push_back(Token(
          TokenType::SEQUENCER, "",
          source_name, source,
          pos, pos
        ));
      }
      if (
        line_continuation_status ==
        LineContinuationStatus::LCS_WAIT_FOR_NEWLINE
      ) {
        line_continuation_status = LineContinuationStatus::LCS_WAIT_FOR_TOKEN;
      }

      line_continuation_marker_pos = source->size();
      ++pos;
      continue;
    }

    // If we parsed a line continuation marker, there
    // should have been a subsequent line feed.
    if (
      line_continuation_status ==
      LineContinuationStatus::LCS_WAIT_FOR_NEWLINE
    ) {
      throw Error(
        "Unexpected '\\'.",
        *source, *source_name,
        line_continuation_marker_pos, line_continuation_marker_pos + 1
      );
    }

    if (
      line_continuation_status ==
      LineContinuationStatus::LCS_WAIT_FOR_TOKEN
    ) {
      line_continuation_status = LineContinuationStatus::LCS_DEFAULT;
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
      pos = end_pos;
      continue;
    }

    auto parse_symbol = [&](
      TokenType type,
      std::string literal,
      bool opener,
      bool closer,
      TokenType opener_type
    ) {
      if (
        pos + literal.size() <= source->size() &&
        source->substr(pos, literal.size()) == literal
      ) {
        if (closer) {
          if (grouping_stack.empty()) {
            throw Error(
              "Unmatched '" + literal + "'.",
              *source, *source_name,
              pos, pos + 1
            );
          } else if (grouping_stack.back().type != opener_type) {
            throw Error(
              "Unmatched '" + grouping_stack.back().literal + "'.",
              *source, *source_name,
              grouping_stack.back().start_pos, grouping_stack.back().end_pos
            );
          }
          grouping_stack.pop_back();
        }
        Token token(
          type, literal,
          source_name, source,
          pos, pos + literal.size()
        );
        if (opener) {
          grouping_stack.push_back(token);
        }
        tokens->push_back(token);
        pos += literal.size();
        return true;
      }
      return false;
    };

    // Parse two-character symbols first to prevent them from
    // being parsed as one-character symbols.

    // THICK_ARROW
    if (parse_symbol(
      TokenType::THICK_ARROW, "=>", false, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // THIN_ARROW
    if (parse_symbol(
      TokenType::THIN_ARROW, "->", false, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // Parse one-character symbols.

    // COLON
    if (parse_symbol(
      TokenType::COLON, ":", false, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // EQUALS
    if (parse_symbol(
      TokenType::EQUALS, "=", false, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // FULL_STOP
    if (parse_symbol(
      TokenType::FULL_STOP, ".", false, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // LEFT_CURLY
    if (parse_symbol(
      TokenType::LEFT_CURLY, "{", true, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // LEFT_PAREN
    if (parse_symbol(
      TokenType::LEFT_PAREN, "(", true, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // LEFT_SQUARE
    if (parse_symbol(
      TokenType::LEFT_SQUARE, "[", true, false, static_cast<TokenType>(0)
    )) {
      continue;
    }

    // RIGHT_CURLY
    if (parse_symbol(
      TokenType::RIGHT_CURLY, "}", false, true, TokenType::LEFT_CURLY
    )) {
      continue;
    }

    // RIGHT_PAREN
    if (parse_symbol(
      TokenType::RIGHT_PAREN, ")", false, true, TokenType::LEFT_PAREN
    )) {
      continue;
    }

    // RIGHT_SQUARE
    if (parse_symbol(
      TokenType::RIGHT_SQUARE, "]", false, true, TokenType::LEFT_SQUARE
    )) {
      continue;
    }

    // SEQUENCER
    if (parse_symbol(
      TokenType::SEQUENCER, ";", false, false, static_cast<TokenType>(0)
    )) {
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

  // Remove extra SEQUENCER tokens from the beginning.
  auto prefix_end = tokens->begin();
  for (
    ;
    prefix_end != tokens->end() &&
      prefix_end->type == TokenType::SEQUENCER;
    ++prefix_end
  );
  tokens->erase(tokens->begin(), prefix_end);

  // Remove extra SEQUENCER tokens from the end.
  auto suffix_begin = tokens->end();
  for (
    ;
    suffix_begin != tokens->begin() &&
      (suffix_begin - 1)->type == TokenType::SEQUENCER;
    --suffix_begin
  );
  tokens->erase(suffix_begin, tokens->end());

  // Remove extra SEQUENCER tokens from the middle.
  for (auto iter = tokens->begin(); iter != tokens->end(); ++iter) {
    // Remove extra SEQUENCER tokens after a group opener.
    // Also remove redundant SEQUENCER tokens.
    if (
      iter->type == TokenType::LEFT_CURLY ||
      iter->type == TokenType::LEFT_PAREN ||
      iter->type == TokenType::LEFT_SQUARE ||
      iter->type == TokenType::SEQUENCER
    ) {
      auto section_end = iter + 1;
      for (
        ;
        section_end != tokens->end() &&
          section_end->type == TokenType::SEQUENCER;
        ++section_end
      );
      iter = tokens->erase(iter + 1, section_end) - 1;
      continue;
    }

    // Remove extra SEQUENCER tokens before a group closer.
    if (
      iter->type == TokenType::RIGHT_CURLY ||
      iter->type == TokenType::RIGHT_PAREN ||
      iter->type == TokenType::RIGHT_SQUARE
    ) {
      auto section_begin = iter;
      for (
        ;
        section_begin != tokens->begin() &&
          (section_begin - 1)->type == TokenType::SEQUENCER;
        --section_begin
      );
      iter = tokens->erase(section_begin, iter);
      continue;
    }
  }

  // Return an std::unique_ptr to the vector.
  return tokens;
}
