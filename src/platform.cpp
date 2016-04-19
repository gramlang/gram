#include "platform.h"

#include <errno.h>
#include <stdexcept>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <llvm/ADT/SmallString.h>
#include <llvm/ADT/Triple.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

using namespace std;
using namespace llvm;

// This object just calls llvm_shutdown() when it is destroyed.
llvm_shutdown_obj Y;

// Execute a program. Returns the output of the program.
// This function will use the PATH environment variable to find the program.
// Raises a std::runtime_error if the program does not exit successfully.
string execute_file(const string &path, const vector<string> &args, const string &stdin) {
  // Set up a pipe to send data to the stdin of the child.
  int parent_to_child[2];
  if (pipe(parent_to_child) == -1) {
    throw runtime_error("An unexpected error occurred.");
  }

  // Set up a pipe to capture the stdout of the child.
  int child_to_parent[2];
  if (pipe(child_to_parent) == -1) {
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    throw runtime_error("An unexpected error occurred.");
  }

  // Fork a child process to run the command.
  pid_t pid = fork();
  if (pid == -1) {
    // Something went wrong.
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    close(child_to_parent[1]);
    close(child_to_parent[0]);
    throw runtime_error("An unexpected error occurred.");
  } else if (pid == 0) {
    // Set up stdin and stdout.
    while (dup2(parent_to_child[0], STDIN_FILENO) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        throw runtime_error("An unexpected error occurred.");
      }
    }
    while (dup2(child_to_parent[1], STDOUT_FILENO) == -1) {
      if (errno == EINTR) {
        continue;
      } else {
        close(parent_to_child[1]);
        close(parent_to_child[0]);
        throw runtime_error("An unexpected error occurred.");
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
    throw runtime_error("An unexpected error occurred.");
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
        throw runtime_error("An unexpected error occurred.");
      }
    }
    close(parent_to_child[1]);

    // Read the output of the child and close the pipe.
    string stdout;
    char buffer[4096];
    while (true) {
      ssize_t count = read(child_to_parent[0], buffer, sizeof(buffer));
      if (count == -1) {
        if (errno == EINTR) {
          continue;
        } else {
          close(child_to_parent[0]);
          throw runtime_error("An unexpected error occurred.");
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
        throw runtime_error("An unexpected error occurred.");
      }
    }

    // Make sure the process exited successfully.
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
      throw runtime_error("The child process did not exit successfully.");
    }

    return stdout;
  }
}

// Compile an LLVM module into a native binary.
string llc(const string output_path, Module &module) {
  // The native assembly will be written to this string.
  SmallString<1024> native_asm;

  // Initialize targets.
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  // Initialize passes.
  PassRegistry *registry = PassRegistry::getPassRegistry();
  initializeCore(*registry);
  initializeCodeGen(*registry);
  initializeLoopStrengthReducePass(*registry);
  initializeLowerIntrinsicsPass(*registry);
  initializeUnreachableBlockElimPass(*registry);

  // Get info about the current target triple.
  Triple triple;
  triple.setTriple(sys::getDefaultTargetTriple());

  // Get info about the current target.
  std::string error;
  const Target *target = TargetRegistry::lookupTarget(triple.getTriple(), error);
  if (!target) {
    throw runtime_error("Unable to find LLVM target for triple " + triple.getTriple() + ".");
  }

  // Set up a pass manager.
  legacy::PassManager pass_manager;

  // Add the optimization passes supported for this target.
  TargetLibraryInfoImpl target_library_info_impl(triple);
  pass_manager.add(new TargetLibraryInfoWrapperPass(target_library_info_impl));

  // Add a pass to emit the native assembly.
  TargetOptions options;
  TargetMachine *target_machine = target->createTargetMachine(
    triple.getTriple(),
    "",
    "",
    options,
    Reloc::Default,
    CodeModel::Default,
    CodeGenOpt::Aggressive
  );
  raw_svector_ostream native_asm_ostream(native_asm);
  target_machine->addPassesToEmitFile(pass_manager, native_asm_ostream, TargetMachine::CGFT_AssemblyFile);
  module.setDataLayout(target_machine->createDataLayout());

  // Run all the passes.
  pass_manager.run(module);

  // Assemble and link with Clang or GCC (whichever is available).
  vector<string> cc_args;
  cc_args.push_back("-o");
  cc_args.push_back(output_path);
  cc_args.push_back("-x");
  cc_args.push_back("assembler");
  cc_args.push_back("-");
  try {
    return execute_file("clang", cc_args, native_asm.str());
  } catch(runtime_error &e) {
    try {
      return execute_file("gcc", cc_args, native_asm.str());
    } catch(runtime_error &e) {
      throw runtime_error("Unable to invoke Clang or GCC. Ensure that at least one of these is installed.");
    }
  }
}
