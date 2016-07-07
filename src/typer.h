/*
  This header declares the interface to the type checker and inferencer.
*/

#ifndef GRAM_TYPER_H
#define GRAM_TYPER_H

#include "parser.h"

namespace gram {

  // Parse a stream of tokens.
  void type(gram::Node &node);

}

#endif
