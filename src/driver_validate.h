#ifndef CHANCE_DRIVER_VALIDATE_H
#define CHANCE_DRIVER_VALIDATE_H

#include "ast.h"
#include "driver_types.h"

typedef struct
{
  const char *prog_name;
  const char **out;

  int *output_overridden;
  int *stop_after_asm;
  int *stop_after_ccb;
  int *no_link;
  int *emit_library;
  int *export_executable;
  int *freestanding;
  int *m32;
  int *debug_symbols;
  int *strip_metadata;
  int *implicit_void_function;
  int *language_standard;
  int *request_ast;
  int *diagnostics_only;

  TargetArch *target_arch;

  int *ce_count;
  int *ccb_count;
  int *cclib_count;
  int *obj_count;
  int *asm_count;
} DriverValidationState;


int validate_driver_options(const DriverValidationState *state);

#endif