#include "platform.h"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include "../deps/whereami/whereami.h"

using namespace std;

string get_executable_path() {
  int length = wai_getExecutablePath(NULL, 0, NULL);
  if (length == -1) {
    exit(1);
  }
  char *path = (char*)malloc(length + 1);
  if (wai_getExecutablePath(path, length, NULL) == -1) {
    exit(1);
  }
  path[length] = '\0';
  return path;
}

void invoke_llvm(const string &command, const vector<string> &args) {
  pid_t pid = fork();
  if (pid == -1) {
    exit(1);
  } else if (pid == 0) {
    string executable_path = get_executable_path();
    string command_path = executable_path.substr(0, executable_path.size() - 4) +
      "../llvm/build/bin/" + command;
    char **argv = new char *[args.size() + 2];
    argv[0] = const_cast<char *>(command_path.c_str());
    for (size_t i = 0; i < args.size(); i++) {
      argv[i + 1] = const_cast<char *>(args[i].c_str());
    }
    argv[args.size() + 1] = 0;
    execv(command_path.c_str(), argv);
    exit(1);
  } else {
    if (waitpid(pid, NULL, 0) == -1) {
      exit(1);
    }
  }
}
