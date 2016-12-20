#include "error.h"
#include "lexer.h"

std::unique_ptr<std::vector<gram::Token>> gram::lex(
  std::shared_ptr<std::string> source_name,
  std::shared_ptr<std::string> source
) {
  auto tokens = std::unique_ptr<std::vector<Token>>(new std::vector<Token>);
  size_t pos = 0;
  size_t start_col = 0;
  std::vector<Token> opening_parens;
  std::vector<std::string> indentations = {""};
  while (pos < source->size()) {
    // If we are at the beginning of a line, process any indentation.
    if (start_col == 0) {
      // Consume it.
      while (pos < source->size() && (
        (*source)[pos] == ' ' || (*source)[pos] == '\t'
      )) {
        ++pos;
        ++start_col;
      }
      std::string indentation = source->substr(pos - start_col, start_col);

      // If line wasn't empty, interpret the indentation (or lack thereof).
      if (
        pos < source->size() &&
        (*source)[pos] != '\n' &&
        (*source)[pos] != '#'
      ) {
        if (indentation == indentations.back()) {
          // Same indentation as before. Just insert a sequencer.
          if (!tokens->empty()) {
            tokens->push_back(Token(
              TokenType::SEQUENCER, "",
              source_name, source,
              pos - start_col, pos
            ));
          }
        } else if (
          indentation.find(indentations.back()) != std::string::npos
        ) {
          // The indentation increased. Open a new block.
          indentations.push_back(indentation);
          tokens->push_back(Token(
            TokenType::BEGIN, source->substr(pos - start_col, start_col),
            source_name, source,
            pos - start_col, pos
          ));
        } else if (
          indentations.back().find(indentation) != std::string::npos
        ) {
          // The indentation decreased.
          // Close blocks and add sequencers as appropriate.
          while (
            !indentations.empty() && indentation != indentations.back()
          ) {
            indentations.pop_back();
            tokens->push_back(Token(
              TokenType::END, "",
              source_name, source,
              pos - start_col, pos
            ));
            tokens->push_back(Token(
              TokenType::SEQUENCER, "",
              source_name, source,
              pos - start_col, pos
            ));
          }

          // Make sure we ended up at a previous indentation level.
          if (indentations.empty()) {
            throw Error(
              "Unmatched outdent.",
              *source, *source_name,
              pos - start_col, pos
            );
          }
        } else {
          // A strange possibility. The new indentation is neither a substring
          // nor a superstring of the old indentation. This can happen if
          // spaces and tabs are mixed.
          throw Error(
            "Unable to compare this indentation to that of previous lines. "
            "This can happen if you are mixing tabs and spaces.",
            *source, *source_name,
            pos - start_col, pos
          );
        }
      }

      // If we consumed any characters, start again. We might be at the end of
      // the stream now.
      if (start_col > 0) {
        continue;
      }
    }

    // A '~' can be used to continue a line.
    if ((*source)[pos] == '~') {
      // Consume the symbol.
      if (tokens->empty() || tokens->back().type != TokenType::SEQUENCER) {
        throw Error(
          "Unexpected '~'.",
          *source, *source_name,
          pos, pos + 1
        );
      }
      tokens->pop_back();
      ++pos;
      ++start_col;
      continue;
    }

    // BEGIN
    if ((*source)[pos] == '(') {
      Token paren(
        TokenType::BEGIN, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      );
      tokens->push_back(paren);
      opening_parens.push_back(paren);
      ++pos;
      ++start_col;
      continue;
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

    // END
    if ((*source)[pos] == ')') {
      if (opening_parens.empty()) {
        throw Error(
          "Unmatched ')'.",
          *source, *source_name,
          pos, pos + 1
        );
      }
      tokens->push_back(Token(
        TokenType::END, source->substr(pos, 1),
        source_name, source,
        pos, pos + 1
      ));
      opening_parens.pop_back();
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

    // INTEGER
    // Integers are composed of digits optionally preceded by a '-' sign.
    if ((pos < source->size() - 1 && (*source)[pos] == '-' &&
      (*source)[pos + 1] >= '0' && (*source)[pos + 1] <= '9') ||
      ((*source)[pos] >= '0' && (*source)[pos] <= '9')) {
      size_t end_pos = pos + 1;
      while (
        end_pos < source->size() &&
        (*source)[end_pos] >= '0' &&
        (*source)[end_pos] <= '9'
      ) {
        ++end_pos;
      }
      size_t length = end_pos - pos;
      tokens->push_back(Token(
        TokenType::INTEGER, source->substr(pos, length),
        source_name, source,
        pos, end_pos
      ));
      start_col += length;
      pos = end_pos;
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

    // Ignore non-indentation whitespace;
    // it is only used to separate other tokens.
    if ((*source)[pos] == ' ' || (*source)[pos] == '\t') {
      ++pos;
      ++start_col;
      continue;
    }

    // Comments begin with '#' and continue to the end of the line.
    if ((*source)[pos] == '#') {
      while (pos < source->size() && (*source)[pos] != '\n') {
        ++pos;
        ++start_col;
        continue;
      }
    }

    // Ignore line feeds, but keep track of which line and column we are on.
    if ((*source)[pos] == '\n') {
      // Don't let parentheses groups span multiple lines.
      // We have indentation for that!
      if (!opening_parens.empty()) {
        throw Error(
          "Unmatched '('. Note that parentheses groups may "
            "not span multiple lines.",
          *source, *source_name,
          opening_parens.back().start_pos, opening_parens.back().end_pos
        );
      }

      ++pos;
      start_col = 0;
      continue;
    }

    // Some platforms use carriage returns in combination with line feeds to
    // break lines. Line feeds are handled above. Just ignore carriage returns.
    if ((*source)[pos] == '\r') {
      ++pos;
      ++start_col;
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

  // Make sure all parentheses have been closed.
  if (!opening_parens.empty()) {
    throw Error("Unmatched '('.",
      *source, *source_name,
      opening_parens.back().start_pos, opening_parens.back().end_pos
    );
  }

  // Close any indentation blocks.
  while (indentations.size() > 1) {
    indentations.pop_back();
    tokens->push_back(Token(
      TokenType::END, "",
      source_name, source,
      pos, pos
    ));
  }

  // Return an std::unique_ptr to the vector.
  return tokens;
}
