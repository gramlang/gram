/*
  This header declares the interface to the lexical analyzer.
*/

#ifndef GRAM_LEXER_H
#define GRAM_LEXER_H

#include <string>
#include <vector>

namespace gram {

  enum class TokenType {
    BEGIN,
    COLON,
    END,
    EQUALS,
    IDENTIFIER,
    INTEGER,
    SEQUENCER,
    THICK_ARROW,
    THIN_ARROW
  };

  const char * const TokenTypeName[] = {
    "BEGIN",
    "COLON",
    "END",
    "EQUALS",
    "IDENTIFIER",
    "INTEGER",
    "SEQUENCER",
    "THICK_ARROW",
    "THIN_ARROW"
  };


  class Token {
  public:
    gram::TokenType type;
    std::string literal;
    size_t start_line, start_col, end_line, end_col;
    Token(gram::TokenType type, std::string literal,
      size_t start_line, size_t start_col, // Zero-indexed, inclusive
      size_t end_line, size_t end_col); // Zero-indexed, exclusive
    std::string show();
  };

  // Perform lexical analysis.
  void lex(std::vector<gram::Token> &tokens, std::string &source, std::string &source_name);

}

#endif
