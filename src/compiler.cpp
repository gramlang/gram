#include "compiler.h"
#include "platform.h"

#include <stdexcept>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>

using namespace std;
using namespace llvm;

void compile(string input_path, string output_path) {
  try {
    Module module("test", getGlobalContext());
    Function* main_fn = cast<Function>(module.getOrInsertFunction("main", IntegerType::getInt64Ty(getGlobalContext()), NULL));
    BasicBlock* block = BasicBlock::Create(getGlobalContext(), "", main_fn);
    IRBuilder<> builder(block);
    builder.CreateRet(ConstantInt::getSigned(IntegerType::getInt64Ty(getGlobalContext()), 0));

    llc(output_path, module);
  } catch(runtime_error &e) {
    throw error(e.what());
  }
}
