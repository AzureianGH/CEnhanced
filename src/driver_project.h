#ifndef CHANCE_DRIVER_PROJECT_H
#define CHANCE_DRIVER_PROJECT_H

#include "ast.h"
#include "driver_overrides.h"
#include "driver_types.h"

typedef struct
{
  const char ***items;
  int *count;
  int *cap;
  char ***owned_items;
  int *owned_count;
  int *owned_cap;
  char ***parallel_strings;
} ProjectInputList;

int push_input_entry(const char *value, ProjectInputList list, int make_copy);

int parse_ceproj_file(
    const char *proj_path, ProjectInputList ce_list, ProjectInputList ccb_list,
    ProjectInputList cclib_list, ProjectInputList obj_list,
    ProjectInputList asm_list,
    char ***include_dirs, int *include_dir_count, int *output_overridden,
    const char **out, char **project_output_alloc, TargetArch *target_arch,
    const char **chancecode_backend, int *stop_after_ccb, int *stop_after_asm,
    int *emit_library, int *no_link, int *freestanding, TargetOS *target_os,
    int *freestanding_requested, int *m32, int *opt_level, int *debug_symbols,
    int *strip_metadata, int *strip_hard, int *obfuscate,
    AsmSyntax *asm_syntax, const char **chancecodec_cmd_override,
    const char **chs_cmd_override, const char **host_cc_cmd_override,
    const char **obj_override, int *implicit_voidp, int *implicit_void_function,
    int *implicit_sizeof, int *request_ast, int *language_standard,
    int *diagnostics_only, int *toolchain_debug_mode,
    int *toolchain_debug_deep, int *verbose_use_ansi,
    OverrideFile **override_files, int *override_file_count,
    int *override_file_cap, ProjectInputList symbol_ref_ce_list,
    ProjectInputList symbol_ref_cclib_list, char **after_cmd);

#endif
