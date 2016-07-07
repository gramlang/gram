#include "compiler.h"
#include "error.h"
#include "lexer.h"
#include "parser.h"
#include "platform.h"
#include "typer.h"
#include <fstream>
#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <sstream>
#include <stdexcept>

void gram::compile(std::string input_path, std::string output_path, gram::OutputType output_type) {
  // Read the source file.
  std::ifstream file(input_path);
  if (!file.is_open()) {
    throw Error("Unable to open file '" + input_path + "'.");
  }
  std::stringstream file_buffer;
  file_buffer << file.rdbuf();
  file.close();
  auto source = std::shared_ptr<std::string>(new std::string(file_buffer.str()));
  auto source_name = std::shared_ptr<std::string>(new std::string(input_path));

  // Perform lexical analysis.
  auto tokens = lex(source_name, source);

  // Parse the tokens into an AST.
  auto node = parse(*tokens);

  // Perform type checking and inference.
  if (node) {
    type(*node);
  }

  // If we got a node, print it!
  if (node) {
    std::cout << node->show() << "\n";
  }

  // Temporary stuff to demonstrate LLVM code generation.
  try {
    llvm::LLVMContext context;
    llvm::Module module("test", context);
    auto main_fn_type = llvm::FunctionType::get(llvm::Type::getInt64Ty(context), false);
    auto main_fn = llvm::Function::Create(main_fn_type,
      llvm::Function::ExternalLinkage, "main", &module);
    auto basic_block = llvm::BasicBlock::Create(context, "", main_fn);
    basic_block->getInstList().push_back(
      llvm::ReturnInst::Create(context, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), 0))
    );
    llc(output_path, module, output_type);
  } catch(std::runtime_error &e) {
    throw Error(e.what());
  }
}
