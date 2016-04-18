#include "compiler.h"
#include "platform.h"
#include <stdexcept>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>

using namespace std;
using namespace llvm;

void compile(string input_path, string output_path) {
  try {
    Module module("test", getGlobalContext());
    Function* main_fn = cast<Function>(module.getOrInsertFunction("main", IntegerType::getInt64Ty(getGlobalContext()), NULL));
    main_fn->setCallingConv(CallingConv::C);
    BasicBlock* block = BasicBlock::Create(getGlobalContext(), "", main_fn);
    IRBuilder<> builder(block);
    builder.CreateRet(ConstantInt::getSigned(IntegerType::getInt64Ty(getGlobalContext()), 0));

    llc(output_path, module);
  } catch(runtime_error &e) {
    throw error(e.what());
  }
}
