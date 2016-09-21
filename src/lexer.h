/*
  This header declares the interface to the lexical analyzer.
*/

#ifndef GRAM_LEXER_H
#define GRAM_LEXER_H

#include <string>
#include <vector>
#include <memory>

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

  // Notes about line and column numbering (also noted in error.h):
  // - Line feeds exist on the lines they are terminating, not the following line.
  // - All indices must point to a valid character, with one exception: the end of the file is
  //   represented by the last column number + 1, with no change to the line number.

  class Token {
  public:
    gram::TokenType type;
    std::string literal;
    std::shared_ptr<std::string> source_name;
    std::shared_ptr<std::string> source;
    size_t start_line, start_col, // Zero-indexed, inclusive
      end_line, end_col; // Zero-indexed, exclusive

    Token(
      gram::TokenType type, const std::string &literal,
      std::shared_ptr<std::string> source_name,
      std::shared_ptr<std::string> source,
      size_t start_line, size_t start_col,
      size_t end_line, size_t end_col
    );
    std::string show();
  };

  // Perform lexical analysis.
  // The lexer guarantees that all BEGIN/END tokens will be matched in the returned stream.
  std::unique_ptr<std::vector<gram::Token>> lex(
    std::shared_ptr<std::string> source_name,
    std::shared_ptr<std::string> source
  );

}

#endif
