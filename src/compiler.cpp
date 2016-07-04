#include "compiler.h"
#include "error.h"
#include "platform.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <stdexcept>

void gram::compile(std::string input_path, std::string output_path, gram::OutputType output_type) {
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
    throw error(e.what());
  }
}
