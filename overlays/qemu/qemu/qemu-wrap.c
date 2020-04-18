#include <alloca.h>
#include <malloc.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#if !defined(QEMU_ARM_BIN)
  #define QEMU_ARM_BIN "qemu-arm"
#endif

const char * qemu_arm_bin = QEMU_ARM_BIN;

// This program takes arguments according to the behavior of binfmt_misc with
// the preserve-argv[0] flag set.
//
// The first value in argv is the name of this executable, uninteresting.
// The second value is the full path of the executable to run with the
// alternate interpreter.
// The third value is the name that executable was called with.
//
// This program passes the third value in to qemu-arm after the -0 flag.
int main(int argc, char const* argv[]) {
  // Abort if we don't have sufficient arguments
  if(argc < 3){
    fprintf( stderr, "qemu-arm wrapper called with too few arguments.\nEnsure that the 'P' flag is set in binfmt_misc.\n");
    return -1;
  }

  char *qemu;
  asprintf(&qemu, "%s/%s", dirname(argv[0]), qemu_arm_bin);

  // Allocate the new argc array to pass to qemu-arm
  const int new_argc = argc + 1;
  char** const new_argv = alloca((new_argc + 1) * sizeof(void *));

  // Fill this new array
  new_argv[0] = qemu;
  new_argv[1] = strdup("-0");
  new_argv[2] = strdup(argv[2]);
  new_argv[3] = strdup(argv[1]);
  for(int i = 4; i < new_argc; ++i){
    new_argv[i] = strdup(argv[i-1]);
  }
  new_argv[new_argc] = NULL;

  // Run qemu with the new arguments
  execvp(new_argv[0], new_argv);
  const int ret = errno;

  // Clean up, haha C
  for(int i = 0; i < new_argc; ++i){
    free(new_argv[i]);
  }

  return ret;
};
