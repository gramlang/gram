/*
  This header declares the interface to the parser.
*/

#ifndef GRAM_PARSER_H
#define GRAM_PARSER_H

#include "ast.h"
#include "lexer.h"
#include <memory>

namespace gram {

  // Parse a stream of tokens. Nodes may be shared (i.e., the result is a DAG,
  // but not necessarily a tree).
  std::shared_ptr<gram::Node> parse(std::vector<gram::Token> &tokens);

}

#endif
