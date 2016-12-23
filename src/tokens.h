/*
  This header declares the lexical tokens.
*/

#ifndef GRAM_TOKENS_H
#define GRAM_TOKENS_H

#include <memory>
#include <string>

namespace gram {

  enum class TokenType {
    COLON,
    EQUALS,
    FULL_STOP,
    IDENTIFIER,
    LEFT_CURLY,
    LEFT_PAREN,
    LEFT_SQUARE,
    RIGHT_CURLY,
    RIGHT_PAREN,
    RIGHT_SQUARE,
    SEQUENCER,
    THICK_ARROW,
    THIN_ARROW
  };

  const char * const TokenTypeName[] = {
    "COLON",
    "EQUALS",
    "FULL_STOP",
    "IDENTIFIER",
    "LEFT_CURLY",
    "LEFT_PAREN",
    "LEFT_SQUARE",
    "RIGHT_CURLY",
    "RIGHT_PAREN",
    "RIGHT_SQUARE",
    "SEQUENCER",
    "THICK_ARROW",
    "THIN_ARROW"
  };

  class Token {
  public:
    gram::TokenType type;
    std::string literal;
    std::shared_ptr<std::string> source_name;
    std::shared_ptr<std::string> source;
    size_t start_pos; // Inclusive
    size_t end_pos; // Exclusive

    Token(
      gram::TokenType type, const std::string &literal,
      std::shared_ptr<std::string> source_name,
      std::shared_ptr<std::string> source,
      size_t start_pos, size_t end_pos
    );
    std::string show();
  };

}

#endif
