/*
  This header declares the main compiler interface.
*/

#ifndef GRAM_COMPILER_H
#define GRAM_COMPILER_H

#include "error.h"
#include <string>

void compile(std::string input_path, std::string output_path);

#endif
