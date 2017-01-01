/*
  This header declares the interface to the lexical analyzer.
*/

#ifndef GRAM_TOKENIZER_H
#define GRAM_TOKENIZER_H

#include "tokens.h"
#include <memory>
#include <string>
#include <vector>

namespace gram {

  // Perform lexical analysis.
  // The tokenizer guarantees that all LEFT_*/RIGHT_* tokens will be matched
  // in the returned stream.
  std::unique_ptr<std::vector<gram::Token>> tokenize(
    std::shared_ptr<std::string> source_name,
    std::shared_ptr<std::string> source
  );

}

#endif
