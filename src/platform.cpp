/*
  This header abstracts away details about the operating system and environment.
  All platform-specific code lives in these functions.
*/

#include "platform.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <iostream>
#include <sstream>
#include "../deps/whereami/whereami.h"

using namespace std;

// Use this macro to print the file and line number, then kill the program.
#define panic() (cout << "Internal error at " << __FILE__ << ":" << __LINE__ << endl, exit(1))

// Returns the path to this executable.
string get_executable_path() {
  // Use whereami to get the length of the path.
  int length = wai_getExecutablePath(NULL, 0, NULL);
  if (length == -1) {
    panic();
  }

  // Use whereami to get the path.
  char *path = new char[length + 1];
  if (wai_getExecutablePath(path, length, NULL) == -1) {
    panic();
  }
  path[length] = '\0';
  std::string pathStr(path);
  delete [] path;
  return pathStr;
}

// Invokes an LLVM command. For example:
//   vector<string> args;
//   args.push_back("--version");
//   invoke_llvm("llc", args);
void invoke_llvm(const string &command, const vector<string> &args) {
  // Fork a child process to run the command.
  pid_t pid = fork();
  if (pid == -1) {
    // Something went wrong.
    panic();
  } else if (pid == 0) {
    // Get the path to the command.
    string executable_path = get_executable_path();
    string command_path = executable_path.substr(0, executable_path.size() - 4) + "llvm/" + command;

    // Put the args into an array.
    char **argv = new char *[args.size() + 2];
    argv[0] = const_cast<char *>(command_path.c_str());
    for (size_t i = 0; i < args.size(); i++) {
      argv[i + 1] = const_cast<char *>(args[i].c_str());
    }
    argv[args.size() + 1] = 0;

    // Redirect standard output to /dev/null.
    FILE *out = fopen("/dev/null", "w");
    dup2(fileno(out), 1);

    // Run the command.
    execv(command_path.c_str(), argv);

    // If we got this far, execv failed.
    delete [] argv;
    panic();
  } else {
    // Wait for the child process to terminate.
    int status = 0;
    if (waitpid(pid, &status, 0) == -1) {
      panic();
    }

    // Make sure the command succeeded.
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
      panic();
    }
  }
}
