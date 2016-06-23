#include "platform.h"
#include <errno.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <stdexcept>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// This object just calls llvm_shutdown() when it is destroyed.
llvm::llvm_shutdown_obj Y;

// Execute a program. Returns the output of the program.
// This function will use the PATH environment variable to find the program.
// Raises a std::runtime_error if the program does not exit successfully.
std::string gram::execute_file(
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

// Compile an LLVM module into a native binary.
void gram::llc(const std::string output_path, llvm::Module &module) {
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
  llvm::InitializeAllAsmParsers();

  // Get the target triple for this machine.
  llvm::Triple triple;
  triple.setTriple(llvm::sys::getDefaultTargetTriple());

  // Match the triple to a target.
  std::string target_error;
  const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
    triple.getTriple(),
    target_error
  );
  if (!target) {
    throw std::runtime_error("Unable to find LLVM target for triple " + triple.getTriple() + ".");
  }

  // Set up a pass manager to schedule the optimizations.
  llvm::legacy::PassManager pass_manager;

  // Add information about which built-in functions (from the C standard library)
  // are supported for optimization purposes.
  llvm::TargetLibraryInfoImpl target_library_info_impl(triple);
  pass_manager.add(new llvm::TargetLibraryInfoWrapperPass(target_library_info_impl));

  // Add a passes to optimize the code and emit native assembly.
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
  llvm::raw_svector_ostream native_asm_ostream(native_asm);
  target_machine->addPassesToEmitFile(
    pass_manager,
    native_asm_ostream,
    llvm::TargetMachine::CGFT_AssemblyFile
  );
  module.setDataLayout(target_machine->createDataLayout());

  // Run all the passes.
  pass_manager.run(module);

  // Assemble and link with Clang or GCC (whichever is available).
  std::vector<std::string> cc_args;
  cc_args.push_back("-o");
  cc_args.push_back(output_path);
  cc_args.push_back("-x");
  cc_args.push_back("assembler");
  cc_args.push_back("-");
  try {
    gram::execute_file("clang", cc_args, native_asm.str());
  } catch(std::runtime_error &e) {
    try {
      gram::execute_file("gcc", cc_args, native_asm.str());
    } catch(std::runtime_error &e) {
      throw std::runtime_error(
        "Unable to invoke Clang or GCC. Ensure that at least one of these is installed."
      );
    }
  }
}
