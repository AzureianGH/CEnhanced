#include "driver_options.h"

#include "chance_version.h"
#include "driver_cli.h"
#include "includes.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int append_const_input(const char *value, const char ***items,
                              int *count, int *cap)
{
  if (!value || !items || !count || !cap)
    return -1;
  if (*count == *cap)
  {
    int new_cap = *cap ? (*cap * 2) : 8;
    const char **grown =
        (const char **)realloc((void *)*items, sizeof(char *) * (size_t)new_cap);
    if (!grown)
      return -1;
    *items = grown;
    *cap = new_cap;
  }
  (*items)[(*count)++] = value;
  return 0;
}

int parse_driver_options_argv(int argc, char **argv, DriverOptionsState *state)
{
  if (!argv || !state || argc <= 0)
    return 2;

  const char *pending_output = NULL;
  int pending_ce_output_index = -1;

  ProjectInputList ce_cli_list = {state->ce_inputs, state->ce_count,
                                  state->ce_cap, state->owned_ce_inputs,
                                  state->owned_ce_count, state->owned_ce_cap,
                                  state->ce_obj_outputs};

  for (int i = 1; i < argc; i++)
  {
    if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0)
    {
      usage(state->prog_name ? state->prog_name : argv[0]);
      return 1;
    }
    if (strcmp(argv[i], "--version") == 0)
    {
      printf("chancec: CHance Compiler version %s\n",
             CHANCEC_VERSION_STRING);
      printf("chancec: CE language standard default: %s\n",
             chance_standard_name(CHANCEC_DEFAULT_STANDARD));
      printf("chancec: License: OpenAzure License\n");
      printf("chancec: Compiled on %s %s\n", __DATE__, __TIME__);
      printf("chancec: Created by Nathan Hornby (AzureianGH)\n");
      return 1;
    }
    if (strcmp(argv[i], "-o") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: -o expects an output path\n");
        return 2;
      }
      const char *dest = argv[++i];
      if (pending_ce_output_index >= 0 && *state->no_link)
      {
        if (!*state->ce_obj_outputs)
        {
          fprintf(stderr, "error: internal state mismatch for -o handling\n");
          return -1;
        }
        char *dup = xstrdup(dest);
        if (!dup)
        {
          fprintf(stderr, "error: out of memory while assigning -o path\n");
          return -1;
        }
        if ((*state->ce_obj_outputs)[pending_ce_output_index])
          free((*state->ce_obj_outputs)[pending_ce_output_index]);
        (*state->ce_obj_outputs)[pending_ce_output_index] = dup;
        pending_ce_output_index = -1;
      }
      else
      {
        pending_output = dest;
        *state->output_overridden = 1;
      }
      continue;
    }
    if (strcmp(argv[i], "-S") == 0)
    {
      *state->stop_after_asm = 1;
      continue;
    }
    if (strcmp(argv[i], "-Sccb") == 0)
    {
      *state->stop_after_ccb = 1;
      continue;
    }
    if (strcmp(argv[i], "-g") == 0)
    {
      *state->debug_symbols = 1;
      continue;
    }
    if (strcmp(argv[i], "--strip") == 0)
    {
      *state->strip_metadata = 1;
      continue;
    }
    if (strcmp(argv[i], "--strip-hard") == 0)
    {
      *state->strip_metadata = 1;
      *state->strip_hard = 1;
      continue;
    }
    if (strcmp(argv[i], "--obfuscate") == 0)
    {
      *state->strip_metadata = 1;
      *state->strip_hard = 1;
      *state->obfuscate = 1;
      continue;
    }
    if (strncmp(argv[i], "-O", 2) == 0)
    {
      const char *level_str = argv[i] + 2;
      int level = 1;
      if (*level_str != '\0')
      {
        char *endptr = NULL;
        long parsed = strtol(level_str, &endptr, 10);
        if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 3)
        {
          fprintf(stderr,
                  "invalid optimization level '%s' (use -O0|-O1|-O2|-O3)\n",
                  argv[i]);
          return 2;
        }
        level = (int)parsed;
      }
      *state->opt_level = level;
      continue;
    }
    if (strcmp(argv[i], "--no-link") == 0)
    {
      *state->no_link = 1;
      if (i + 1 < argc && argv[i + 1][0] != '-')
      {
        const char *cand = argv[i + 1];
        if (is_object_file_arg(cand))
        {
          *state->obj_override = cand;
          i++;
        }
      }
      continue;
    }
    if (strcmp(argv[i], "-c") == 0)
    {
      *state->no_link = 1;
      if (i + 1 < argc && argv[i + 1][0] != '-')
      {
        const char *cand = argv[i + 1];
        if (is_ce_source_arg(cand))
        {
          if (push_input_entry(cand, ce_cli_list, 0) != 0)
            return -1;
          pending_ce_output_index = *state->ce_count - 1;
          i++;
          continue;
        }
        if (is_object_file_arg(cand))
        {
          *state->obj_override = cand;
          i++;
          continue;
        }
      }
      continue;
    }
    if (strcmp(argv[i], "--asm-syntax") == 0 && i + 1 < argc)
    {
      const char *arg = argv[++i];
      if (strcmp(arg, "intel") == 0)
        *state->asm_syntax = ASM_INTEL;
      else if (strcmp(arg, "att") == 0)
        *state->asm_syntax = ASM_ATT;
      else if (strcmp(arg, "nasm") == 0)
        *state->asm_syntax = ASM_NASM;
      else
      {
        fprintf(stderr, "Unknown asm syntax '%s' (use intel|att|nasm)\n", arg);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "--chancecodec") == 0 && i + 1 < argc)
    {
      *state->chancecodec_cmd_override = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "--chs") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: --chs expects an assembler path\n");
        return 2;
      }
      *state->chs_cmd_override = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "--entry") == 0 || strcmp(argv[i], "-e") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: %s expects an entry symbol\n", argv[i]);
        return 2;
      }
      *state->entry_symbol = argv[++i];
      continue;
    }
    if (strncmp(argv[i], "--entry=", 8) == 0)
    {
      const char *sym = argv[i] + 8;
      if (!sym || !*sym)
      {
        fprintf(stderr, "error: --entry expects an entry symbol\n");
        return 2;
      }
      *state->entry_symbol = sym;
      continue;
    }
    if (strcmp(argv[i], "--library") == 0)
    {
      *state->emit_library = 1;
      continue;
    }
    if (strcmp(argv[i], "--export-exe") == 0)
    {
      if (state->export_executable)
        *state->export_executable = 1;
      if (state->emit_library)
        *state->emit_library = 1;
      continue;
    }
    if (strcmp(argv[i], "--static") == 0)
    {
      *state->static_link = 1;
      continue;
    }
    if (strcmp(argv[i], "--freestanding") == 0)
    {
      *state->freestanding = 1;
      *state->freestanding_requested = 1;
      continue;
    }
    if (strcmp(argv[i], "-x86") == 0)
    {
      *state->target_arch = ARCH_X86;
      *state->chancecode_backend = "x86-gas";
      continue;
    }
    if (strcmp(argv[i], "-arm64") == 0)
    {
      *state->target_arch = ARCH_ARM64;
      *state->chancecode_backend = NULL;
      continue;
    }
    if (strcmp(argv[i], "-bslash") == 0)
    {
      *state->target_arch = ARCH_BSLASH;
      *state->chancecode_backend = "bslash";
      continue;
    }
    if (strcmp(argv[i], "-m32") == 0)
    {
      *state->m32 = 1;
      continue;
    }
    if (strcmp(argv[i], "-m64") == 0)
    {
      *state->m32 = 0;
      continue;
    }
    if (strcmp(argv[i], "--target-os") == 0 && i + 1 < argc)
    {
      const char *os = argv[++i];
      if (strcmp(os, "windows") == 0)
        *state->target_os = OS_WINDOWS;
      else if (strcmp(os, "linux") == 0)
        *state->target_os = OS_LINUX;
      else if (strcmp(os, "macos") == 0)
        *state->target_os = OS_MACOS;
      else
      {
        fprintf(stderr,
                "Unknown --target-os '%s' (use windows|linux|macos)\n", os);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "-I") == 0 && i + 1 < argc)
    {
      chance_add_include_dir(state->include_dirs, state->include_dir_count,
                             argv[++i]);
      continue;
    }
    if (strcmp(argv[i], "-Nno-formatting") == 0)
    {
      parser_set_disable_formatting_notes(1);
      continue;
    }
    if (strcmp(argv[i], "-vd") == 0 || strcmp(argv[i], "--verbose-deep") == 0)
    {
      compiler_verbose_set_mode(1);
      compiler_verbose_set_deep(1);
      *state->toolchain_debug_mode = 1;
      *state->toolchain_debug_deep = 1;
      continue;
    }
    if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--debug") == 0)
    {
      *state->toolchain_debug_mode = 1;
      continue;
    }
    if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0)
    {
      compiler_verbose_set_mode(1);
      continue;
    }
    if (strcmp(argv[i], "--no-ansi") == 0)
    {
      diag_set_use_ansi(0);
      *state->verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(argv[i], "--data-log") == 0)
    {
      diag_set_data_log(1);
      diag_set_use_ansi(0);
      *state->verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(argv[i], "--request-ast") == 0)
    {
      *state->request_ast = 1;
      continue;
    }
    if (strcmp(argv[i], "--diagnostics-only") == 0 || strcmp(argv[i], "--diag-only") == 0)
    {
      *state->diagnostics_only = 1;
      continue;
    }
    if (strcmp(argv[i], "--override-file") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: --override-file expects <original>=<override>\n");
        return 2;
      }
      const char *spec = argv[++i];
      if (add_override_file(spec, state->cwd, state->override_files,
                            state->override_file_count,
                            state->override_file_cap) != 0)
      {
        fprintf(stderr, "error: invalid --override-file '%s'\n", spec);
        return 2;
      }
      continue;
    }
    if (strncmp(argv[i], "--override-file=", 16) == 0)
    {
      const char *spec = argv[i] + 16;
      if (add_override_file(spec, state->cwd, state->override_files,
                            state->override_file_count,
                            state->override_file_cap) != 0)
      {
        fprintf(stderr, "error: invalid --override-file '%s'\n", spec);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "--implicit-voidp") == 0)
    {
      *state->implicit_voidp = 1;
      continue;
    }
    if (strcmp(argv[i], "--implicit-void-function") == 0)
    {
      *state->implicit_void_function = 1;
      continue;
    }
    if (strcmp(argv[i], "--implicit-sizeof") == 0)
    {
      *state->implicit_sizeof = 1;
      continue;
    }
    if (strcmp(argv[i], "-H26") == 0)
    {
      *state->language_standard = CHANCE_STD_H26;
      continue;
    }
    if (strcmp(argv[i], "-H27") == 0)
    {
      *state->language_standard = CHANCE_STD_H27;
      continue;
    }
    if (strncmp(argv[i], "-sr:", 4) == 0)
    {
      const char *sr_path = argv[i] + 4;
      if (!sr_path || !*sr_path)
      {
        fprintf(stderr,
                "error: -sr: requires a .ce, .cin, or .cclib path immediately following\n");
        return 2;
      }
      if (ends_with_icase(sr_path, ".ce") || ends_with_icase(sr_path, ".cin"))
      {
        if (*state->symbol_ref_ce_count == *state->symbol_ref_ce_cap)
        {
          *state->symbol_ref_ce_cap =
              *state->symbol_ref_ce_cap ? (*state->symbol_ref_ce_cap * 2) : 4;
          *state->symbol_ref_ce_inputs = (const char **)realloc(
              (void *)*state->symbol_ref_ce_inputs,
              sizeof(char *) * (size_t)*state->symbol_ref_ce_cap);
          if (!*state->symbol_ref_ce_inputs)
            return -1;
        }
        (*state->symbol_ref_ce_inputs)[(*state->symbol_ref_ce_count)++] = sr_path;
      }
      else if (ends_with_icase(sr_path, ".cclib"))
      {
        if (*state->symbol_ref_cclib_count == *state->symbol_ref_cclib_cap)
        {
          *state->symbol_ref_cclib_cap = *state->symbol_ref_cclib_cap
                                             ? (*state->symbol_ref_cclib_cap * 2)
                                             : 4;
          *state->symbol_ref_cclib_inputs = (const char **)realloc(
              (void *)*state->symbol_ref_cclib_inputs,
              sizeof(char *) * (size_t)*state->symbol_ref_cclib_cap);
          if (!*state->symbol_ref_cclib_inputs)
            return -1;
        }
        (*state->symbol_ref_cclib_inputs)[(*state->symbol_ref_cclib_count)++] = sr_path;
      }
      else
      {
        fprintf(stderr,
                "error: -sr: path '%s' must be a .ce, .cin, or .cclib file\n",
                sr_path);
        return 2;
      }
      continue;
    }
    if (argv[i][0] == '-')
    {
      usage(state->prog_name ? state->prog_name : argv[0]);
      return 2;
    }

    const char *arg = argv[i];
    if (ends_with_icase(arg, ".ceproj"))
    {
      ProjectInputList ce_list = {state->ce_inputs, state->ce_count,
                                  state->ce_cap, state->owned_ce_inputs,
                                  state->owned_ce_count, state->owned_ce_cap,
                                  state->ce_obj_outputs};
      ProjectInputList ccb_list = {state->ccb_inputs, state->ccb_count,
                                   state->ccb_cap, state->owned_ccb_inputs,
                                   state->owned_ccb_count,
                                   state->owned_ccb_cap, NULL};
      ProjectInputList cclib_list = {state->cclib_inputs, state->cclib_count,
                                     state->cclib_cap,
                                     state->owned_cclib_inputs,
                                     state->owned_cclib_count,
                                     state->owned_cclib_cap, NULL};
      ProjectInputList obj_list = {state->obj_inputs, state->obj_count,
                                   state->obj_cap, state->owned_obj_inputs,
                                   state->owned_obj_count,
                                   state->owned_obj_cap, NULL};
      ProjectInputList asm_list = {state->asm_inputs, state->asm_count,
                                   state->asm_cap, state->owned_asm_inputs,
                                   state->owned_asm_count,
                                   state->owned_asm_cap, NULL};
      int freestanding_before = *state->freestanding;
      int perr = parse_ceproj_file(
          arg, ce_list, ccb_list, cclib_list, obj_list, asm_list,
          state->include_dirs, state->include_dir_count, state->output_overridden,
          state->out, state->project_output_alloc, state->target_arch,
          state->chancecode_backend, state->stop_after_ccb, state->stop_after_asm,
          state->emit_library, state->no_link, state->freestanding,
          state->target_os, state->export_executable, state->freestanding_requested, state->m32,
          state->opt_level, state->debug_symbols, state->strip_metadata,
          state->strip_hard, state->obfuscate, state->asm_syntax,
          state->chancecodec_cmd_override, state->chs_cmd_override,
          state->entry_symbol, state->obj_override, state->implicit_voidp,
          state->implicit_void_function, state->implicit_sizeof,
          state->request_ast, state->language_standard, state->diagnostics_only,
          state->toolchain_debug_mode, state->toolchain_debug_deep,
          state->verbose_use_ansi, state->override_files,
          state->override_file_count, state->override_file_cap,
          (ProjectInputList){state->symbol_ref_ce_inputs,
                             state->symbol_ref_ce_count,
                             state->symbol_ref_ce_cap, state->owned_ce_inputs,
                             state->owned_ce_count, state->owned_ce_cap, NULL},
          (ProjectInputList){state->symbol_ref_cclib_inputs,
                             state->symbol_ref_cclib_count,
                             state->symbol_ref_cclib_cap,
                             state->owned_cclib_inputs,
                             state->owned_cclib_count,
                             state->owned_cclib_cap, NULL},
          state->project_after_cmd);
      if (perr != 0)
        return 2;
      if (!*state->freestanding_requested && !freestanding_before &&
          *state->freestanding)
        *state->freestanding_requested = 1;
      continue;
    }

    if (ends_with_icase(arg, ".ce") || ends_with_icase(arg, ".cin"))
    {
      if (push_input_entry(arg, ce_cli_list, 0) != 0)
        return -1;
      pending_ce_output_index = -1;
    }
    else if (ends_with_icase(arg, ".ccb"))
    {
      if (append_const_input(arg, state->ccb_inputs, state->ccb_count,
                             state->ccb_cap) != 0)
        return -1;
    }
    else if (ends_with_icase(arg, ".cclib"))
    {
      if (append_const_input(arg, state->cclib_inputs, state->cclib_count,
                             state->cclib_cap) != 0)
        return -1;
    }
    else if (ends_with_icase(arg, ".o") || ends_with_icase(arg, ".obj"))
    {
      if (append_const_input(arg, state->obj_inputs, state->obj_count,
                             state->obj_cap) != 0)
        return -1;
    }
    else
    {
      fprintf(stderr,
              "error: unknown input type '%s' (expected .ce/.cin or .o/.obj)\n",
              arg);
      return 2;
    }
  }

  if (pending_output)
  {
    if (*state->no_link)
      *state->obj_override = pending_output;
    else
      *state->out = pending_output;
  }

  return 0;
}