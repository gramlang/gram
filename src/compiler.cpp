#include "compiler.h"
#include "error.h"
#include "lexer.h"
#include "parser.h"
#include "platform.h"
#include "typer.h"
#include <fstream>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <memory>
#include <sstream>
#include <stdexcept>

void gram::compile(std::string input_path, std::string output_path, gram::OutputType output_type) {
  // Read the source file.
  std::ifstream input_file(input_path);
  if (!input_file.is_open()) {
    throw Error("Unable to open file '" + input_path + "'.");
  }
  std::stringstream file_buffer;
  file_buffer << input_file.rdbuf();
  input_file.close();
  auto source = std::make_shared<std::string>(file_buffer.str());
  auto source_name = std::make_shared<std::string>(input_path);

  // Perform lexical analysis.
  auto tokens = lex(source_name, source);

  // Output the tokens, if requested.
  if (output_type == OutputType::TOKENS) {
    std::ofstream output_file(output_path);
    if (!output_file.is_open()) {
      throw Error("Unable to write to file '" + output_path + "'.");
    }
    for (auto token : *tokens) {
      output_file << token.show() << "\n";
    }
    output_file.close();
    return;
  }

  // Parse the tokens into an AST.
  auto node = parse(*tokens);

  // Output the AST, if requested.
  if (output_type == OutputType::AST) {
    std::ofstream output_file(output_path);
    if (!output_file.is_open()) {
      throw Error("Unable to write to file '" + output_path + "'.");
    }
    if (node) {
      output_file << node->show() << "\n";
    }
    output_file.close();
    return;
  }

  // Perform type checking and inference.
  if (node) {
    type(*node);
  }

  // Output the types, if requested.
  if (output_type == OutputType::TYPES) {
    std::ofstream output_file(output_path);
    if (!output_file.is_open()) {
      throw Error("Unable to write to file '" + output_path + "'.");
    }
    if (node) {
      output_file << node->show() << "\n";
    }
    output_file.close();
    return;
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
