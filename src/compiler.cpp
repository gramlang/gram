#include "compiler.h"
#include "error.h"
#include "platform.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <stdexcept>

void gram::compile(std::string input_path, std::string output_path) {
  try {
    llvm::Module module("test", llvm::getGlobalContext());
    llvm::Function* main_fn = llvm::cast<llvm::Function>(
      module.getOrInsertFunction(
        "main",
        llvm::IntegerType::getInt64Ty(llvm::getGlobalContext()),
        NULL
      )
    );
    llvm::BasicBlock* block = llvm::BasicBlock::Create(llvm::getGlobalContext(), "", main_fn);
    llvm::IRBuilder<> builder(block);
    builder.CreateRet(
      llvm::ConstantInt::getSigned(llvm::IntegerType::getInt64Ty(llvm::getGlobalContext()), 0)
    );

    llc(output_path, module);
  } catch(std::runtime_error &e) {
    throw error(e.what());
  }
}
