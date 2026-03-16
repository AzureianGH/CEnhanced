#ifndef CHANCE_DRIVER_OPTIONS_H
#define CHANCE_DRIVER_OPTIONS_H

#include "ast.h"
#include "driver_overrides.h"
#include "driver_project.h"

typedef struct
{
  const char *prog_name;
  const char *cwd;

  const char **out;
  const char **obj_override;
  int *output_overridden;

  int *stop_after_asm;
  int *stop_after_ccb;
  int *no_link;
  int *emit_library;
  int *freestanding;
  int *freestanding_requested;
  int *m32;
  int *opt_level;
  int *debug_symbols;
  int *strip_metadata;
  int *strip_hard;
  int *obfuscate;
  int *implicit_voidp;
  int *implicit_void_function;
  int *implicit_sizeof;
  int *request_ast;
  int *diagnostics_only;
  int *toolchain_debug_mode;
  int *toolchain_debug_deep;
  int *language_standard;
  int *verbose_use_ansi;

  AsmSyntax *asm_syntax;
  TargetArch *target_arch;
  TargetOS *target_os;

  const char **chancecode_backend;
  const char **chancecodec_cmd_override;
  const char **chs_cmd_override;
  const char **host_cc_cmd_override;

  const char ***ce_inputs;
  int *ce_count;
  int *ce_cap;
  char ***ce_obj_outputs;

  const char ***ccb_inputs;
  int *ccb_count;
  int *ccb_cap;

  const char ***cclib_inputs;
  int *cclib_count;
  int *cclib_cap;

  const char ***obj_inputs;
  int *obj_count;
  int *obj_cap;

  const char ***asm_inputs;
  int *asm_count;
  int *asm_cap;

  const char ***symbol_ref_ce_inputs;
  int *symbol_ref_ce_count;
  int *symbol_ref_ce_cap;

  const char ***symbol_ref_cclib_inputs;
  int *symbol_ref_cclib_count;
  int *symbol_ref_cclib_cap;

  char ***owned_ce_inputs;
  int *owned_ce_count;
  int *owned_ce_cap;

  char ***owned_ccb_inputs;
  int *owned_ccb_count;
  int *owned_ccb_cap;

  char ***owned_cclib_inputs;
  int *owned_cclib_count;
  int *owned_cclib_cap;

  char ***owned_obj_inputs;
  int *owned_obj_count;
  int *owned_obj_cap;

  char ***owned_asm_inputs;
  int *owned_asm_count;
  int *owned_asm_cap;

  char ***include_dirs;
  int *include_dir_count;

  OverrideFile **override_files;
  int *override_file_count;
  int *override_file_cap;

  char **project_output_alloc;
  char **project_after_cmd;
} DriverOptionsState;

// Returns 0 for successful parse, 1 when --help/--version was handled,
// 2 for user-facing argument errors, and -1 for internal/oom failures.
int parse_driver_options_argv(int argc, char **argv, DriverOptionsState *state);

#endif