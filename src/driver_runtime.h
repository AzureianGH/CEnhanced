#ifndef CHANCE_DRIVER_RUNTIME_H
#define CHANCE_DRIVER_RUNTIME_H

typedef struct
{
  const char *exe_dir;
  int no_link;

  int *freestanding_requested;
  int *freestanding;

  const char *default_stdlib_path;
  const char *default_runtime_path;

  const char ***cclib_inputs;
  int *cclib_count;
  int *cclib_cap;
  char ***owned_cclib_inputs;
  int *owned_cclib_count;
  int *owned_cclib_cap;
} DriverRuntimeLibState;


int maybe_inject_default_runtime_libs(const DriverRuntimeLibState *state);

#endif