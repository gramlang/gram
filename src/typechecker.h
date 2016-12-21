/*
  This header declares the interface to the type checker and inferencer.
*/

#ifndef GRAM_TYPECHECKER_H
#define GRAM_TYPECHECKER_H

#include "parser.h"

namespace gram {

  // Perform type inference and checking.
  void typecheck(gram::Node &node);

}

#endif
