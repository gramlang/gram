/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#include "platform.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "../deps/whereami/whereami.h"

using namespace std;

// Return the path to this executable.
string get_executable_path() {
  // Use whereami to get the length of the path.
  int length = wai_getExecutablePath(NULL, 0, NULL);
  if (length == -1) {
    throw runtime_error("An unexpected error occurred.");
  }

  // Use whereami to get the path.
  char *path = new char[length + 1];
  if (wai_getExecutablePath(path, length, NULL) == -1) {
    throw runtime_error("An unexpected error occurred.");
  }
  path[length] = '\0';
  string pathStr(path);
  delete [] path;

  return pathStr;
}

// Execute a program. Returns the output of the program.
// Raises a std::runtime_error if the program does not exit successfully.
string execute_file(const string &file, const vector<string> &args, const string &stdin) {
  // Set up a pipe to send data to the stdin of the child.
  int parent_to_child[2];
  if (pipe(parent_to_child) == -1) {
    throw runtime_error("An unexpected error occurred.");
  }

  // Set up a pipe to capture the stdout of the child.
  int child_to_parent[2];
  if (pipe(child_to_parent) == -1) {
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
    while ((dup2(parent_to_child[0], STDIN_FILENO) == -1) && (errno == EINTR));
    while ((dup2(child_to_parent[1], STDOUT_FILENO) == -1) && (errno == EINTR));

    // We don't need these descriptors anymore.
    close(parent_to_child[1]);
    close(parent_to_child[0]);
    close(child_to_parent[1]);
    close(child_to_parent[0]);

    // Put the args into an array.
    char **argv = new char *[args.size() + 2];
    argv[0] = const_cast<char *>(file.c_str());
    for (size_t i = 0; i < args.size(); i++) {
      argv[i + 1] = const_cast<char *>(args[i].c_str());
    }
    argv[args.size() + 1] = 0;

    // Run the command.
    execv(file.c_str(), argv);

    // If we got this far, execv failed.
    delete [] argv;
    throw runtime_error("An unexpected error occurred.");
  } else {
    // We don't need these descriptors anymore.
    close(parent_to_child[0]);
    close(child_to_parent[1]);

    // Send the data to the child and close the pipe.
    write(parent_to_child[1], stdin.c_str(), stdin.size());
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
    if (waitpid(pid, &status, 0) == -1) {
      throw runtime_error("An unexpected error occurred.");
    }

    // Make sure the process exited successfully.
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
      throw runtime_error("The child process did not exit successfully.");
    }

    return stdout;
  }
}

// Compile LLVM assembly into a native binary.
void llc(const string filename, const string llvm_asm) {
  // Get the path to the LLVM compiler.
  string executable_path = get_executable_path();
  string llc_path = executable_path.substr(0, executable_path.size() - 4) + "llvm/llc";

  // Compile the LLVM to assembly.
  vector<string> llvm_args;
  llvm_args.push_back("-O=3");
  string native_asm = execute_file(llc_path, llvm_args, llvm_asm);

  // Compile the assembly with GCC.
  vector<string> gcc_args;
  gcc_args.push_back("-o");
  gcc_args.push_back(filename);
  gcc_args.push_back("-x");
  gcc_args.push_back("assembler");
  gcc_args.push_back("-");
  execute_file("/usr/bin/gcc", gcc_args, native_asm);
}
