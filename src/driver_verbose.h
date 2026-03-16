#ifndef CHANCE_DRIVER_VERBOSE_H
#define CHANCE_DRIVER_VERBOSE_H

#include "ast.h"
#include "driver_types.h"

typedef struct
{
  const char *output_path;
  int opt_level;
  TargetArch target_arch;
  const char *target_os_name;
  int stop_after_ccb;
  int stop_after_asm;
  int no_link;
  int emit_library;
  int freestanding;
  AsmSyntax asm_syntax;
  int include_dir_count;
  int ce_count;
  int ccb_count;
  int cclib_count;
  int obj_count;
  int symbol_ref_ce_count;
  int symbol_ref_cclib_count;
  const char *host_cc_cmd;
  int host_cc_has_override;
  const char *chancecodec_cmd;
  int chancecodec_has_override;
  int needs_chancecodec;
  const char *chancecode_backend;
  int chancecodec_uses_fallback;
  int verbose_active;
  int verbose_deep;
} DriverVerboseConfigState;

typedef struct
{
  int include_dir_count;
  char **include_dirs;
  int ce_count;
  const char **ce_inputs;
  int symbol_ref_ce_count;
  const char **symbol_ref_ce_inputs;
  int ccb_count;
  const char **ccb_inputs;
  int cclib_count;
  const char **cclib_inputs;
  int symbol_ref_cclib_count;
  const char **symbol_ref_cclib_inputs;
  int obj_count;
  const char **obj_inputs;
  int asm_count;
  const char **asm_inputs;
} DriverVerboseInputsState;

void driver_verbose_set_use_ansi(int enabled);
void driver_verbose_section(const char *title);
void driver_verbose_table_row(const char *label, const char *value);
void driver_verbose_progress(const char *tag, int current, int total);
void driver_verbose_print_config(const DriverVerboseConfigState *state);
void driver_verbose_print_inputs(const DriverVerboseInputsState *state);

#endif
