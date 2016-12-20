/*
  This header declares the interface to the lexical analyzer.
*/

#ifndef GRAM_LEXER_H
#define GRAM_LEXER_H

#include "tokens.h"
#include <memory>
#include <string>
#include <vector>

namespace gram {

  // Perform lexical analysis.
  // The lexer guarantees that all BEGIN/END tokens will be matched
  // in the returned stream.
  std::unique_ptr<std::vector<gram::Token>> lex(
    std::shared_ptr<std::string> source_name,
    std::shared_ptr<std::string> source
  );

}

#endif
