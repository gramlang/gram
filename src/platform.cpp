#include "platform.h"
#include <errno.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <stdexcept>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// This object just calls llvm_shutdown() when it is destroyed.
llvm::llvm_shutdown_obj Y;

std::string gram::execute_program(
  const std::string &path,
  const std::vector<std::string> &args,
  const std::string &stdin
) {
  // Set up a pipe to send data to the stdin of the child.
  int parent_to_child[2];
  if (pipe(parent_to_child) == -1) {
    throw std::runtime_error("An unexpected error occurred.");
  }

  // Set up a pipe to capture the stdout of the child.
  int child_to_parent[2];
  if (pipe(child_to_parent) == -1) {
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    throw std::runtime_error("An unexpected error occurred.");
  }

  // Fork a child process to run the command.
  pid_t pid = fork();
  if (pid == -1) {
    // Something went wrong.
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    close(child_to_parent[1]);
    close(child_to_parent[0]);
    throw std::runtime_error("An unexpected error occurred.");
  } else if (pid == 0) {
    // Set up stdin and stdout.
    while (dup2(parent_to_child[0], STDIN_FILENO) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        throw std::runtime_error("An unexpected error occurred.");
      }
    }
    while (dup2(child_to_parent[1], STDOUT_FILENO) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        close(parent_to_child[1]);
        close(parent_to_child[0]);
        throw std::runtime_error("An unexpected error occurred.");
      }
    }

    // We don't need these descriptors anymore.
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    close(child_to_parent[1]);
    close(child_to_parent[0]);

    // Put the args into an array.
    char **argv = new char *[args.size() + 2];
    argv[0] = const_cast<char *>(path.c_str());
    for (size_t i = 0; i < args.size(); i++) {
      argv[i + 1] = const_cast<char *>(args[i].c_str());
    }
    argv[args.size() + 1] = 0;

    // Run the command.
    execvp(path.c_str(), argv);

    // If we got this far, execv failed.
    delete [] argv;
    throw std::runtime_error("An unexpected error occurred.");
  } else {
    // We don't need these descriptors anymore.
    close(parent_to_child[0]);
    close(child_to_parent[1]);

    // Send the data to the child and close the pipe.
    while (write(parent_to_child[1], stdin.c_str(), stdin.size()) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        close(parent_to_child[1]);
        close(child_to_parent[0]);
        throw std::runtime_error("An unexpected error occurred.");
      }
    }
    close(parent_to_child[1]);

    // Read the output of the child and close the pipe.
    std::string stdout;
    char buffer[4096];
    while (true) {
      ssize_t count = read(child_to_parent[0], buffer, sizeof(buffer));
      if (count == -1) {
        if (errno == EINTR) {
          continue;
        } else {
          close(child_to_parent[0]);
          throw std::runtime_error("An unexpected error occurred.");
        }
      } else if (count == 0) {
        break;
      } else {
        stdout.append(buffer, count);
      }
    }
    close(child_to_parent[0]);

    // Wait for the child process to terminate.
    int status = 0;
    while (waitpid(pid, &status, 0) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        throw std::runtime_error("An unexpected error occurred.");
      }
    }

    // Make sure the process exited successfully.
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
      throw std::runtime_error("The child process did not exit successfully.");
    }

    return stdout;
  }
}

void gram::llc(
  const std::string &output_path,
  llvm::Module &module,
  gram::OutputType output_type
) {
  // Verify the module.
  llvm::SmallString<0> module_error;
  llvm::raw_svector_ostream module_error_ostream(module_error);
  if (llvm::verifyModule(module, &module_error_ostream)) {
    throw std::runtime_error(("LLVM module verification failed: " + module_error).str());
  }

  // The native assembly will be written to this string.
  llvm::SmallString<0> native_asm;

  // Initialize targets.
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();

  // Initialize passes.
  llvm::PassRegistry &pass_registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeCore(pass_registry);
  llvm::initializeScalarOpts(pass_registry);
  llvm::initializeObjCARCOpts(pass_registry);
  llvm::initializeVectorization(pass_registry);
  llvm::initializeIPO(pass_registry);
  llvm::initializeAnalysis(pass_registry);
  llvm::initializeTransformUtils(pass_registry);
  llvm::initializeInstCombine(pass_registry);
  llvm::initializeInstrumentation(pass_registry);
  llvm::initializeTarget(pass_registry);
  llvm::initializeCodeGenPreparePass(pass_registry);
  llvm::initializeAtomicExpandPass(pass_registry);
  llvm::initializeRewriteSymbolsPass(pass_registry);
  llvm::initializeWinEHPreparePass(pass_registry);
  llvm::initializeDwarfEHPreparePass(pass_registry);
  llvm::initializeSjLjEHPreparePass(pass_registry);

  // Get the target triple for this machine.
  llvm::Triple triple;
  triple.setTriple(llvm::sys::getDefaultTargetTriple());
  module.setTargetTriple(llvm::sys::getDefaultTargetTriple());

  // Match the triple to a target.
  std::string target_error;
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
    triple.getTriple(),
    target_error
  );
  if (!target) {
    throw std::runtime_error("Unable to find LLVM target for triple " + triple.getTriple() + ".");
  }

  // Set up the target machine.
  llvm::TargetOptions options;
  std::unique_ptr<llvm::TargetMachine> target_machine(target->createTargetMachine(
    triple.getTriple(),
    "",
    "",
    options,
    llvm::Reloc::Default,
    llvm::CodeModel::Default,
    llvm::CodeGenOpt::Aggressive
  ));
  module.setDataLayout(target_machine->createDataLayout());

  // Set up a pass manager to schedule the optimizations.
  llvm::legacy::PassManager pass_manager;
  llvm::legacy::FunctionPassManager function_pass_manager(&module);

  // Add information about which built-in functions (from the C standard library)
  // are supported for optimization purposes.
  pass_manager.add(new llvm::TargetLibraryInfoWrapperPass(triple));

  // Add internal analysis passes from the target machine.
  pass_manager.add(
    llvm::createTargetTransformInfoWrapperPass(target_machine->getTargetIRAnalysis())
  );
  function_pass_manager.add(
    llvm::createTargetTransformInfoWrapperPass(target_machine->getTargetIRAnalysis())
  );

  // Add optimization passes.
  llvm::PassManagerBuilder pass_manager_builder;
  pass_manager_builder.OptLevel = 3;
  pass_manager_builder.SizeLevel = 1;
  pass_manager_builder.Inliner = llvm::createFunctionInliningPass(
    pass_manager_builder.OptLevel,
    pass_manager_builder.SizeLevel
  );
  pass_manager_builder.DisableTailCalls = false;
  pass_manager_builder.DisableUnitAtATime = false;
  pass_manager_builder.DisableUnrollLoops = false;
  pass_manager_builder.BBVectorize = true;
  pass_manager_builder.SLPVectorize = true;
  pass_manager_builder.LoopVectorize = true;
  pass_manager_builder.RerollLoops = false;
  pass_manager_builder.LoadCombine = true;
  pass_manager_builder.DisableGVNLoadPRE = false;
  pass_manager_builder.VerifyInput = false;
  pass_manager_builder.VerifyOutput = false;
  pass_manager_builder.MergeFunctions = true;
  pass_manager_builder.PrepareForLTO = true;
  pass_manager_builder.populateFunctionPassManager(function_pass_manager);
  pass_manager_builder.populateModulePassManager(pass_manager);
  pass_manager_builder.populateLTOPassManager(pass_manager);

  // Add passes to emit native assembly if needed.
  llvm::raw_svector_ostream native_asm_ostream(native_asm);
  if (output_type == gram::OutputType::ASM || output_type == gram::OutputType::BINARY) {
    target_machine->addPassesToEmitFile(
      pass_manager,
      native_asm_ostream,
      llvm::TargetMachine::CGFT_AssemblyFile
    );
  }

  // Run all function passes.
  function_pass_manager.doInitialization();
  for (llvm::Function &func : module) {
    function_pass_manager.run(func);
  }
  function_pass_manager.doFinalization();

  // Run all the module passes.
  pass_manager.run(module);

  // Output LLVM bitcode if requested.
  if (output_type == gram::OutputType::LLVM_BITCODE) {
    std::error_code ec;
    llvm::raw_fd_ostream out(output_path, ec, llvm::sys::fs::F_None);
    if (ec) {
      throw std::runtime_error("Unable to write to file: " + output_path);
    }
    llvm::WriteBitcodeToFile(&module, out);
    return;
  }

  // Output LLVM assembly if requested.
  if (output_type == gram::OutputType::LLVM_ASM) {
    std::error_code ec;
    llvm::raw_fd_ostream out(output_path, ec, llvm::sys::fs::F_None);
    if (ec) {
      throw std::runtime_error("Unable to write to file: " + output_path);
    }
    module.print(out, 0);
    return;
  }

  // Output native assembly if requested.
  if (output_type == gram::OutputType::ASM) {
    std::error_code ec;
    llvm::raw_fd_ostream out(output_path, ec, llvm::sys::fs::F_None);
    if (ec) {
      throw std::runtime_error("Unable to write to file: " + output_path);
    }
    out << native_asm;
    return;
  }

  // Output native code if requested.
  if (output_type == gram::OutputType::BINARY) {
    // Assemble and link with Clang or GCC (whichever is available).
    std::vector<std::string> cc_args;
    cc_args.push_back("-o");
    cc_args.push_back(output_path);
    cc_args.push_back("-x");
    cc_args.push_back("assembler");
    cc_args.push_back("-");
    try {
      gram::execute_program("clang", cc_args, native_asm.str());
    } catch(std::runtime_error &e) {
      try {
        gram::execute_program("gcc", cc_args, native_asm.str());
      } catch(std::runtime_error &e) {
        throw std::runtime_error(
          "Unable to invoke Clang or GCC. Ensure that at least one of these is installed."
        );
      }
    }
    return;
  }
}
