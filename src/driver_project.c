#include "driver_project.h"

#include "cclib.h"
#include "chance_version.h"
#include "driver_cli.h"
#include "driver_paths.h"
#include "frontend.h"
#include "includes.h"
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
  const char **out;
  char **project_output_alloc;
  int *output_overridden;
  int *stop_after_ccb;
  int *stop_after_asm;
  int *no_link;
  int *emit_library;
  int *export_executable;
  int *freestanding;
  int *freestanding_requested;
  int *m32;
  int *opt_level;
  int *debug_symbols;
  int *strip_metadata;
  int *strip_hard;
  int *obfuscate;
  AsmSyntax *asm_syntax;
  TargetArch *target_arch;
  const char **chancecode_backend;
  const char **chancecodec_cmd_override;
  const char **chs_cmd_override;
  const char **entry_symbol;
  TargetOS *target_os;
  char ***include_dirs;
  int *include_dir_count;
  const char **obj_override;
  int *implicit_voidp;
  int *implicit_void_function;
  int *implicit_sizeof;
  int *language_standard;
  int *c_dialect;
  int *request_ast;
  int *diagnostics_only;
  int *toolchain_debug_mode;
  int *toolchain_debug_deep;
  int *verbose_use_ansi;
  OverrideFile **override_files;
  int *override_file_count;
  int *override_file_cap;
  ProjectInputList symbol_ref_ce_list;
  ProjectInputList symbol_ref_cclib_list;
} ProjectArgState;

static int parse_c_dialect_value(const char *value, int *out)
{
  if (!value || !out)
    return -1;
  if (equals_icase(value, "c23"))
  {
    *out = CHANCE_C_DIALECT_C23;
    return 0;
  }
  if (equals_icase(value, "gnu23"))
  {
    *out = CHANCE_C_DIALECT_GNU23;
    return 0;
  }
  return -1;
}

int push_input_entry(const char *value, ProjectInputList list, int make_copy)
{
  if (!value || !list.items || !list.count || !list.cap || !list.owned_items ||
      !list.owned_count || !list.owned_cap)
    return -1;
  const char **arr = *list.items;
  if (*list.count == *list.cap)
  {
    int new_cap = *list.cap ? (*list.cap * 2) : 8;
    const char **new_arr =
        (const char **)realloc((void *)arr, sizeof(char *) * (size_t)new_cap);
    if (!new_arr)
    {
      fprintf(stderr,
              "error: out of memory while growing project input list\n");
      return -1;
    }
    if (list.parallel_strings)
    {
      char **parallel = *list.parallel_strings;
      char **new_parallel =
          (char **)realloc(parallel, sizeof(char *) * (size_t)new_cap);
      if (!new_parallel)
      {
        fprintf(stderr,
                "error: out of memory while growing parallel input metadata\n");
        return -1;
      }
      for (int idx = *list.cap; idx < new_cap; ++idx)
        new_parallel[idx] = NULL;
      *list.parallel_strings = new_parallel;
    }
    *list.items = new_arr;
    arr = new_arr;
    *list.cap = new_cap;
  }
  const char *stored = value;
  if (make_copy)
  {
    char *dup = xstrdup(value);
    char **owned = *list.owned_items;
    if (*list.owned_count == *list.owned_cap)
    {
      int new_cap = *list.owned_cap ? (*list.owned_cap * 2) : 8;
      char **new_owned =
          (char **)realloc(owned, sizeof(char *) * (size_t)new_cap);
      if (!new_owned)
      {
        fprintf(stderr,
                "error: out of memory while growing project-owned strings\n");
        free(dup);
        return -1;
      }
      *list.owned_items = new_owned;
      owned = new_owned;
      *list.owned_cap = new_cap;
    }
    owned[*list.owned_count] = dup;
    (*list.owned_count)++;
    stored = dup;
  }
  arr[*list.count] = stored;
  if (list.parallel_strings && *list.parallel_strings)
    (*list.parallel_strings)[*list.count] = NULL;
  (*list.count)++;
  return 0;
}

static int add_file_list_entries(const char *value, const char *project_dir,
                                 ProjectInputList list)
{
  if (!value)
    return 0;
  const char *cursor = value;
  while (*cursor)
  {
    while (*cursor == ';' || *cursor == ',' || isspace((unsigned char)*cursor))
      ++cursor;
    if (!*cursor)
      break;
    const char *start = cursor;
    while (*cursor && *cursor != ';' && *cursor != ',')
      ++cursor;
    const char *end = cursor;
    while (end > start && isspace((unsigned char)*(end - 1)))
      --end;
    while (start < end && isspace((unsigned char)*start))
      ++start;
    if (end > start)
    {
      size_t len = (size_t)(end - start);
      char token[1024];
      if (len >= sizeof(token))
        len = sizeof(token) - 1;
      memcpy(token, start, len);
      token[len] = '\0';
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    token);
      if (push_input_entry(resolved, list, 1) != 0)
        return -1;
    }
    if (*cursor == ';' || *cursor == ',')
      ++cursor;
  }
  return 0;
}

static int add_include_dir_list(const char *value, const char *project_dir,
                                char ***include_dirs, int *include_dir_count)
{
  if (!value || !include_dirs || !include_dir_count)
    return 0;
  const char *cursor = value;
  while (*cursor)
  {
    while (*cursor == ';' || *cursor == ',' || isspace((unsigned char)*cursor))
      ++cursor;
    if (!*cursor)
      break;
    const char *start = cursor;
    while (*cursor && *cursor != ';' && *cursor != ',')
      ++cursor;
    const char *end = cursor;
    while (end > start && isspace((unsigned char)*(end - 1)))
      --end;
    while (start < end && isspace((unsigned char)*start))
      ++start;
    if (end > start)
    {
      size_t len = (size_t)(end - start);
      char token[1024];
      if (len >= sizeof(token))
        len = sizeof(token) - 1;
      memcpy(token, start, len);
      token[len] = '\0';
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    token);
      chance_add_include_dir(include_dirs, include_dir_count, resolved);
    }
    if (*cursor == ';' || *cursor == ',')
      ++cursor;
  }
  return 0;
}

static void strip_utf8_bom(char *text)
{
  if (!text)
    return;
  unsigned char *u = (unsigned char *)text;
  if (u[0] == 0xEF && u[1] == 0xBB && u[2] == 0xBF)
    memmove(text, text + 3, strlen(text + 3) + 1);
}

static int parse_project_block_value(FILE *f, int *lineno, const char *value,
                                     char *out, size_t outsz)
{
  if (!f || !out || outsz == 0)
    return -1;
  out[0] = '\0';

  const char *start = value ? value : "";
  while (*start && isspace((unsigned char)*start))
    ++start;

  size_t len = 0;
  if (*start == '{')
  {
    ++start;
    while (*start && isspace((unsigned char)*start))
      ++start;
    const char *end = strrchr(start, '}');
    if (end)
    {
      const char *after_end = end + 1;
      while (*after_end && isspace((unsigned char)*after_end))
        ++after_end;
      if (*after_end == '\0')
      {
        size_t chunk = (size_t)(end - start);
        if (chunk >= outsz)
          chunk = outsz - 1;
        memcpy(out, start, chunk);
        out[chunk] = '\0';
        return 0;
      }
    }
  }

  if (*start)
  {
    len = strnlen(start, outsz - 1);
    memcpy(out, start, len);
    out[len] = '\0';
  }

  char line[2048];
  while (fgets(line, sizeof(line), f))
  {
    if (lineno)
      (*lineno)++;
    char work[2048];
    snprintf(work, sizeof(work), "%s", line);
    char trimmed[2048];
    snprintf(trimmed, sizeof(trimmed), "%s", work);
    trim_whitespace_inplace(trimmed);
    if (strcmp(trimmed, "}") == 0)
      return 0;

    size_t seg_len = strlen(work);
    while (seg_len > 0 && (work[seg_len - 1] == '\n' || work[seg_len - 1] == '\r'))
      --seg_len;
    if (seg_len > 0)
    {
      if (len + 1 < outsz)
      {
        if (len > 0)
          out[len++] = '\n';
        size_t copy = seg_len;
        if (copy > outsz - 1 - len)
          copy = outsz - 1 - len;
        memcpy(out + len, work, copy);
        len += copy;
        out[len] = '\0';
      }
    }
  }
  return -1;
}

static int project_args_push(char ***items, int *count, int *cap,
                             const char *value)
{
  if (!items || !count || !cap || !value)
    return -1;
  if (*count == *cap)
  {
    int new_cap = *cap ? (*cap * 2) : 8;
    char **grown = (char **)realloc(*items, sizeof(char *) * (size_t)new_cap);
    if (!grown)
      return -1;
    *items = grown;
    *cap = new_cap;
  }
  (*items)[*count] = xstrdup(value);
  if (!(*items)[*count])
    return -1;
  (*count)++;
  return 0;
}

static int project_args_tokenize(const char *text, char ***out_args,
                                 int *out_count)
{
  if (!out_args || !out_count)
    return -1;
  *out_args = NULL;
  *out_count = 0;
  if (!text)
    return 0;
  int cap = 0;
  const char *p = text;
  while (*p)
  {
    while (*p && (isspace((unsigned char)*p) || *p == ','))
      ++p;
    if (!*p)
      break;
    char token[1024];
    size_t len = 0;
    char quote = 0;
    if (*p == '"' || *p == '\'')
    {
      quote = *p++;
    }
    while (*p)
    {
      if (quote)
      {
        if (*p == quote)
        {
          ++p;
          break;
        }
      }
      else if (isspace((unsigned char)*p) || *p == ',')
      {
        break;
      }
      if (*p == '\\' && p[1])
      {
        if (len + 1 < sizeof(token))
          token[len++] = p[1];
        p += 2;
        continue;
      }
      if (len + 1 < sizeof(token))
        token[len++] = *p;
      ++p;
    }
    token[len] = '\0';
    if (len > 0)
    {
      if (project_args_push(out_args, out_count, &cap, token) != 0)
        return -1;
    }
  }
  return 0;
}

static void project_args_free(char **args, int count)
{
  if (!args)
    return;
  for (int i = 0; i < count; ++i)
    free(args[i]);
  free(args);
}

static int project_args_apply(const char *proj_path, int lineno,
                              const char *project_dir, char **args, int count,
                              ProjectArgState *state)
{
  if (!state)
    return -1;

  const char *pending_output = NULL;
  for (int i = 0; i < count; ++i)
  {
    const char *arg = args[i];
    if (!arg || !*arg)
      continue;
    if (strcmp(arg, "-o") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: -o expects a path in '%s' (line %d)\n", proj_path,
                lineno);
        return -1;
      }
      const char *dest = args[++i];
      if (!dest || !*dest)
      {
        fprintf(stderr, "error: -o expects a path in '%s' (line %d)\n", proj_path,
                lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    dest);
      if (pending_output)
        free((char *)pending_output);
      pending_output = xstrdup(resolved);
      continue;
    }
    if (strcmp(arg, "-S") == 0)
    {
      if (state->stop_after_asm)
        *state->stop_after_asm = 1;
      continue;
    }
    if (strcmp(arg, "-Sccb") == 0)
    {
      if (state->stop_after_ccb)
        *state->stop_after_ccb = 1;
      continue;
    }
    if (strcmp(arg, "-g") == 0)
    {
      if (state->debug_symbols)
        *state->debug_symbols = 1;
      continue;
    }
    if (strcmp(arg, "--strip") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      continue;
    }
    if (strcmp(arg, "--strip-hard") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      if (state->strip_hard)
        *state->strip_hard = 1;
      continue;
    }
    if (strcmp(arg, "--obfuscate") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      if (state->strip_hard)
        *state->strip_hard = 1;
      if (state->obfuscate)
        *state->obfuscate = 1;
      continue;
    }
    if (strncmp(arg, "-O", 2) == 0)
    {
      const char *level_str = arg + 2;
      int level = 1;
      if (*level_str != '\0')
      {
        char *endptr = NULL;
        long parsed = strtol(level_str, &endptr, 10);
        if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 3)
        {
          fprintf(stderr,
                  "invalid optimization level '%s' in '%s' (line %d)\n", arg,
                  proj_path, lineno);
          return -1;
        }
        level = (int)parsed;
      }
      if (state->opt_level)
        *state->opt_level = level;
      continue;
    }
    if (strncmp(arg, "-std=", 5) == 0)
    {
      if (!state->c_dialect ||
          parse_c_dialect_value(arg + 5, state->c_dialect) != 0)
      {
        fprintf(stderr,
                "error: unsupported C dialect '%s' in '%s' (line %d); use c23|gnu23\n",
                arg + 5, proj_path, lineno);
        return -1;
      }
      continue;
    }
    if (strcmp(arg, "--std") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --std expects a dialect in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (!state->c_dialect ||
          parse_c_dialect_value(args[i + 1], state->c_dialect) != 0)
      {
        fprintf(stderr,
                "error: unsupported C dialect '%s' in '%s' (line %d); use c23|gnu23\n",
                args[i + 1], proj_path, lineno);
        return -1;
      }
      ++i;
      continue;
    }
    if (strncmp(arg, "--std=", 6) == 0)
    {
      if (!state->c_dialect ||
          parse_c_dialect_value(arg + 6, state->c_dialect) != 0)
      {
        fprintf(stderr,
                "error: unsupported C dialect '%s' in '%s' (line %d); use c23|gnu23\n",
                arg + 6, proj_path, lineno);
        return -1;
      }
      continue;
    }
    if (strcmp(arg, "--no-link") == 0)
    {
      if (state->no_link)
        *state->no_link = 1;
      if (i + 1 < count && args[i + 1][0] != '-')
      {
        const char *cand = args[i + 1];
        if (is_object_file_arg(cand))
        {
          if (state->obj_override)
            *state->obj_override = xstrdup(cand);
          i++;
        }
      }
      continue;
    }
    if (strcmp(arg, "-c") == 0)
    {
      if (state->no_link)
        *state->no_link = 1;
      if (i + 1 < count && args[i + 1][0] != '-')
      {
        const char *cand = args[i + 1];
        if (is_object_file_arg(cand))
        {
          if (state->obj_override)
            *state->obj_override = xstrdup(cand);
          i++;
        }
        else if (is_ce_source_arg(cand))
        {
          fprintf(stderr,
                  "error: -c in project args cannot list source inputs; use ce= instead (line %d)\n",
                  lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "--asm-syntax") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --asm-syntax expects a value in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *val = args[++i];
      if (state->asm_syntax)
      {
        if (strcmp(val, "intel") == 0)
          *state->asm_syntax = ASM_INTEL;
        else if (strcmp(val, "att") == 0)
          *state->asm_syntax = ASM_ATT;
        else if (strcmp(val, "nasm") == 0)
          *state->asm_syntax = ASM_NASM;
        else
        {
          fprintf(stderr, "Unknown asm syntax '%s' in '%s' (line %d)\n", val,
                  proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "--chancecodec") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --chancecodec expects a path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (state->chancecodec_cmd_override)
        *state->chancecodec_cmd_override = xstrdup(args[++i]);
      else
        i++;
      continue;
    }
    if (strcmp(arg, "--entry") == 0 || strcmp(arg, "-e") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: %s expects a symbol in '%s' (line %d)\n",
                arg, proj_path, lineno);
        return -1;
      }
      if (state->entry_symbol)
        *state->entry_symbol = xstrdup(args[++i]);
      else
        i++;
      continue;
    }
    if (strncmp(arg, "--entry=", 8) == 0)
    {
      const char *sym = arg + 8;
      if (!sym || !*sym)
      {
        fprintf(stderr, "error: --entry expects a symbol in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (state->entry_symbol)
        *state->entry_symbol = xstrdup(sym);
      continue;
    }
    if (strcmp(arg, "--chs") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: --chs expects a path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (state->chs_cmd_override)
        *state->chs_cmd_override = xstrdup(args[++i]);
      else
        i++;
      continue;
    }
    if (strcmp(arg, "--library") == 0)
    {
      if (state->emit_library)
        *state->emit_library = 1;
      continue;
    }
    if (strcmp(arg, "--export-exe") == 0)
    {
      if (state->export_executable)
        *state->export_executable = 1;
      if (state->emit_library)
        *state->emit_library = 1;
      continue;
    }
    if (strcmp(arg, "--freestanding") == 0)
    {
      if (state->freestanding)
        *state->freestanding = 1;
      if (state->freestanding_requested)
        *state->freestanding_requested = 1;
      continue;
    }
    if (strcmp(arg, "-x86") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_X86;
      if (state->chancecode_backend)
        *state->chancecode_backend = "x86-gas";
      continue;
    }
    if (strcmp(arg, "-arm64") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_ARM64;
      if (state->chancecode_backend)
        *state->chancecode_backend = NULL;
      continue;
    }
    if (strcmp(arg, "-bslash") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_BSLASH;
      if (state->chancecode_backend)
        *state->chancecode_backend = "bslash";
      continue;
    }
    if (strcmp(arg, "-m32") == 0)
    {
      if (state->m32)
        *state->m32 = 1;
      continue;
    }
    if (strcmp(arg, "-m64") == 0)
    {
      if (state->m32)
        *state->m32 = 0;
      continue;
    }
    if (strcmp(arg, "--target-os") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --target-os expects a value in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *os = args[++i];
      if (state->target_os)
      {
        if (strcmp(os, "windows") == 0)
          *state->target_os = OS_WINDOWS;
        else if (strcmp(os, "linux") == 0)
          *state->target_os = OS_LINUX;
        else if (strcmp(os, "macos") == 0)
          *state->target_os = OS_MACOS;
        else
        {
          fprintf(stderr, "Unknown --target-os '%s' in '%s' (line %d)\n", os,
                  proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "-I") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: -I expects a directory in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    args[++i]);
      chance_add_include_dir(state->include_dirs, state->include_dir_count,
                             resolved);
      continue;
    }
    if (strcmp(arg, "-Nno-formatting") == 0)
    {
      parser_set_disable_formatting_notes(1);
      continue;
    }
    if (strcmp(arg, "-vd") == 0 || strcmp(arg, "--verbose-deep") == 0)
    {
      compiler_verbose_set_mode(1);
      compiler_verbose_set_deep(1);
      if (state->toolchain_debug_mode)
        *state->toolchain_debug_mode = 1;
      if (state->toolchain_debug_deep)
        *state->toolchain_debug_deep = 1;
      continue;
    }
    if (strcmp(arg, "-d") == 0 || strcmp(arg, "--debug") == 0)
    {
      if (state->toolchain_debug_mode)
        *state->toolchain_debug_mode = 1;
      continue;
    }
    if (strcmp(arg, "-v") == 0 || strcmp(arg, "--verbose") == 0)
    {
      compiler_verbose_set_mode(1);
      continue;
    }
    if (strcmp(arg, "--no-ansi") == 0)
    {
      diag_set_use_ansi(0);
      if (state->verbose_use_ansi)
        *state->verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(arg, "--data-log") == 0)
    {
      diag_set_data_log(1);
      diag_set_use_ansi(0);
      if (state->verbose_use_ansi)
        *state->verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(arg, "--request-ast") == 0)
    {
      if (state->request_ast)
        *state->request_ast = 1;
      continue;
    }
    if (strcmp(arg, "--diagnostics-only") == 0 ||
        strcmp(arg, "--diag-only") == 0)
    {
      if (state->diagnostics_only)
        *state->diagnostics_only = 1;
      continue;
    }
    if (strcmp(arg, "--override-file") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --override-file expects <original>=<override> in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *spec = args[++i];
      if (state->override_files && state->override_file_count &&
          state->override_file_cap)
      {
        if (add_override_file(spec, project_dir, state->override_files,
                              state->override_file_count,
                              state->override_file_cap) != 0)
        {
          fprintf(stderr,
                  "error: invalid --override-file '%s' in '%s' (line %d)\n",
                  spec, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strncmp(arg, "--override-file=", 16) == 0)
    {
      const char *spec = arg + 16;
      if (state->override_files && state->override_file_count &&
          state->override_file_cap)
      {
        if (add_override_file(spec, project_dir, state->override_files,
                              state->override_file_count,
                              state->override_file_cap) != 0)
        {
          fprintf(stderr,
                  "error: invalid --override-file '%s' in '%s' (line %d)\n",
                  spec, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strncmp(arg, "-sr:", 4) == 0)
    {
      const char *sr_path = arg + 4;
      if (!sr_path || !*sr_path)
      {
        fprintf(stderr,
                "error: -sr: requires a .ce, .cin, or .cclib path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    sr_path);
      if (ends_with_icase(sr_path, ".ce") || ends_with_icase(sr_path, ".cin"))
      {
        if (push_input_entry(resolved, state->symbol_ref_ce_list, 1) != 0)
          return -1;
      }
      else if (ends_with_icase(sr_path, ".cclib"))
      {
        if (push_input_entry(resolved, state->symbol_ref_cclib_list, 1) != 0)
          return -1;
      }
      else
      {
        fprintf(stderr,
                "error: -sr: path '%s' must be a .ce, .cin, or .cclib file (line %d)\n",
                sr_path, lineno);
        return -1;
      }
      continue;
    }
    if (strcmp(arg, "--implicit-voidp") == 0)
    {
      if (state->implicit_voidp)
        *state->implicit_voidp = 1;
      continue;
    }
    if (strcmp(arg, "--implicit-void-function") == 0)
    {
      if (state->implicit_void_function)
        *state->implicit_void_function = 1;
      continue;
    }
    if (strcmp(arg, "--implicit-sizeof") == 0)
    {
      if (state->implicit_sizeof)
        *state->implicit_sizeof = 1;
      continue;
    }
    if (strcmp(arg, "-H26") == 0)
    {
      if (state->language_standard)
        *state->language_standard = CHANCE_STD_H26;
      continue;
    }
    if (strcmp(arg, "-H27") == 0)
    {
      if (state->language_standard)
        *state->language_standard = CHANCE_STD_H27;
      continue;
    }
    if (strcmp(arg, "-H28") == 0)
    {
      if (state->language_standard)
        *state->language_standard = CHANCE_STD_H28;
      continue;
    }
    if (arg[0] == '-')
    {
      fprintf(stderr, "error: unknown project arg '%s' in '%s' (line %d)\n", arg,
              proj_path, lineno);
      return -1;
    }
    fprintf(stderr,
            "error: unexpected token '%s' in project args for '%s' (line %d)\n",
            arg, proj_path, lineno);
    return -1;
  }

  if (pending_output)
  {
    if (state->output_overridden)
      *state->output_overridden = 1;
    if (state->project_output_alloc && state->out)
    {
      if (*state->project_output_alloc)
        free(*state->project_output_alloc);
      *state->project_output_alloc = (char *)pending_output;
      if (state->no_link && *state->no_link && state->obj_override)
        *state->obj_override = *state->project_output_alloc;
      else
        *state->out = *state->project_output_alloc;
    }
    else
    {
      free((char *)pending_output);
    }
  }
  return 0;
}

int parse_ceproj_file(
    const char *proj_path, ProjectInputList ce_list, ProjectInputList ccb_list,
    ProjectInputList cclib_list, ProjectInputList obj_list,
    ProjectInputList asm_list,
    char ***include_dirs, int *include_dir_count, int *output_overridden,
    const char **out, char **project_output_alloc, TargetArch *target_arch,
    const char **chancecode_backend, int *stop_after_ccb, int *stop_after_asm,
    int *emit_library, int *no_link, int *freestanding, TargetOS *target_os,
    int *export_executable,
    int *freestanding_requested, int *m32, int *opt_level, int *debug_symbols,
    int *strip_metadata, int *strip_hard, int *obfuscate,
    AsmSyntax *asm_syntax, const char **chancecodec_cmd_override,
    const char **chs_cmd_override,
    const char **entry_symbol,
    const char **obj_override, int *implicit_voidp, int *implicit_void_function,
    int *implicit_sizeof, int *request_ast, int *language_standard,
    int *c_dialect,
    int *diagnostics_only, int *toolchain_debug_mode,
    int *toolchain_debug_deep, int *verbose_use_ansi,
    OverrideFile **override_files, int *override_file_count,
    int *override_file_cap, ProjectInputList symbol_ref_ce_list,
    ProjectInputList symbol_ref_cclib_list, char **after_cmd)
{
  FILE *f = fopen(proj_path, "rb");
  if (!f)
  {
    fprintf(stderr, "error: failed to open project file '%s': %s\n", proj_path,
            strerror(errno));
    return -1;
  }
  char project_dir[1024] = {0};
  if (parent_directory(proj_path, project_dir, sizeof(project_dir)) != 0)
    project_dir[0] = '\0';

  char line[2048];
  int lineno = 0;
  int rc = 0;
  while (fgets(line, sizeof(line), f))
  {
    lineno++;
    char work[2048];
    snprintf(work, sizeof(work), "%s", line);
    trim_whitespace_inplace(work);
    if (lineno == 1)
      strip_utf8_bom(work);
    if (!work[0])
      continue;
    if (work[0] == '#' || work[0] == ';')
      continue;
    if (work[0] == '/' && work[1] == '/')
      continue;
    if (work[0] == '[')
      continue;
    char *eq = strchr(work, '=');
    if (!eq)
    {
      fprintf(stderr, "error: expected key=value in '%s' (line %d)\n", proj_path,
              lineno);
      rc = -1;
      break;
    }
    *eq = '\0';
    char *key = work;
    char *value = eq + 1;
    trim_whitespace_inplace(key);
    trim_whitespace_inplace(value);
    if (!key[0])
      continue;
    for (char *p = key; *p; ++p)
      *p = (char)tolower((unsigned char)*p);
    char value_buf[2048];
    strip_wrapping_quotes(value, value_buf, sizeof(value_buf));
    trim_whitespace_inplace(value_buf);
    if (strcmp(key, "ce") == 0 || strcmp(key, "source") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, ce_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "ccb") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, ccb_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "cclib") == 0 || strcmp(key, "library") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, cclib_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "obj") == 0 || strcmp(key, "object") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, obj_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "asm") == 0 || strcmp(key, "assembly") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, asm_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "include") == 0 || strcmp(key, "includedir") == 0)
    {
      if (add_include_dir_list(value_buf, project_dir, include_dirs,
                               include_dir_count) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "output") == 0)
    {
      if (!out || !project_output_alloc || !output_overridden)
        continue;
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    value_buf);
      if (*project_output_alloc)
      {
        free(*project_output_alloc);
        *project_output_alloc = NULL;
      }
      *project_output_alloc = xstrdup(resolved);
      *out = *project_output_alloc;
      *output_overridden = 1;
    }
    else if (strcmp(key, "backend") == 0)
    {
      if (equals_icase(value_buf, "x86"))
      {
        if (target_arch)
          *target_arch = ARCH_X86;
        if (chancecode_backend)
          *chancecode_backend = "x86-gas";
      }
      else if (equals_icase(value_buf, "arm64"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = NULL;
      }
      else if (equals_icase(value_buf, "arm64-macos"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = "arm64-macos";
        if (target_os)
          *target_os = OS_MACOS;
      }
      else if (equals_icase(value_buf, "arm64-elf") ||
               equals_icase(value_buf, "arm64-linux"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = "arm64-elf";
        if (target_os)
          *target_os = OS_LINUX;
      }
      else if (equals_icase(value_buf, "arm64-windows") ||
               equals_icase(value_buf, "arm64-coff"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = "arm64-windows";
        if (target_os)
          *target_os = OS_WINDOWS;
      }
      else if (equals_icase(value_buf, "bslash") ||
               equals_icase(value_buf, "bslash"))
      {
        if (target_arch)
          *target_arch = ARCH_BSLASH;
        if (chancecode_backend)
          *chancecode_backend = "bslash";
      }
      else if (equals_icase(value_buf, "none"))
      {
        if (target_arch)
          *target_arch = ARCH_NONE;
        if (chancecode_backend)
          *chancecode_backend = NULL;
      }
      else
      {
        fprintf(stderr, "error: unknown backend '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "type") == 0)
    {
      if (equals_icase(value_buf, "library"))
      {
        if (emit_library)
          *emit_library = 1;
      }
      else if (equals_icase(value_buf, "object") || equals_icase(value_buf, "obj"))
      {
        if (no_link)
          *no_link = 1;
      }
    }
    else if (strcmp(key, "after") == 0)
    {
      if (after_cmd)
      {
        char block[4096];
        if (parse_project_block_value(f, &lineno, value_buf, block,
                                      sizeof(block)) != 0)
        {
          fprintf(stderr, "error: unterminated after block in '%s' (line %d)\n",
                  proj_path, lineno);
          rc = -1;
          break;
        }
        if (*after_cmd)
        {
          free(*after_cmd);
          *after_cmd = NULL;
        }
        trim_whitespace_inplace(block);
        *after_cmd = block[0] ? xstrdup(block) : NULL;
      }
    }
    else if (strcmp(key, "args") == 0 || strcmp(key, "control_args") == 0 ||
             strcmp(key, "control-args") == 0)
    {
      ProjectArgState state = {
          .out = out,
          .project_output_alloc = project_output_alloc,
          .output_overridden = output_overridden,
          .stop_after_ccb = stop_after_ccb,
          .stop_after_asm = stop_after_asm,
          .no_link = no_link,
          .emit_library = emit_library,
          .export_executable = export_executable,
          .freestanding = freestanding,
          .freestanding_requested = freestanding_requested,
          .m32 = m32,
          .opt_level = opt_level,
          .debug_symbols = debug_symbols,
          .strip_metadata = strip_metadata,
          .strip_hard = strip_hard,
          .obfuscate = obfuscate,
          .asm_syntax = asm_syntax,
          .target_arch = target_arch,
          .chancecode_backend = chancecode_backend,
          .chancecodec_cmd_override = chancecodec_cmd_override,
          .chs_cmd_override = chs_cmd_override,
          .entry_symbol = entry_symbol,
          .target_os = target_os,
          .include_dirs = include_dirs,
          .include_dir_count = include_dir_count,
          .obj_override = obj_override,
          .implicit_voidp = implicit_voidp,
          .implicit_void_function = implicit_void_function,
          .implicit_sizeof = implicit_sizeof,
          .language_standard = language_standard,
          .c_dialect = c_dialect,
          .request_ast = request_ast,
          .diagnostics_only = diagnostics_only,
          .toolchain_debug_mode = toolchain_debug_mode,
          .toolchain_debug_deep = toolchain_debug_deep,
          .verbose_use_ansi = verbose_use_ansi,
          .override_files = override_files,
          .override_file_count = override_file_count,
          .override_file_cap = override_file_cap,
          .symbol_ref_ce_list = symbol_ref_ce_list,
          .symbol_ref_cclib_list = symbol_ref_cclib_list};

      char **args = NULL;
      int arg_count = 0;
      if (project_args_tokenize(value_buf, &args, &arg_count) != 0)
      {
        fprintf(stderr, "error: failed to parse args in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (project_args_apply(proj_path, lineno, project_dir, args, arg_count,
                             &state) != 0)
      {
        project_args_free(args, arg_count);
        rc = -1;
        break;
      }
      project_args_free(args, arg_count);
    }
    else if (strcmp(key, "stop_after") == 0 || strcmp(key, "stopafter") == 0)
    {
      if ((!stop_after_asm || !*stop_after_asm) &&
          (!stop_after_ccb || !*stop_after_ccb))
      {
        if (equals_icase(value_buf, "ccb"))
        {
          if (stop_after_ccb)
            *stop_after_ccb = 1;
        }
        else if (equals_icase(value_buf, "asm"))
        {
          if (stop_after_asm)
            *stop_after_asm = 1;
        }
      }
    }
    else if (strcmp(key, "no_link") == 0 || strcmp(key, "nolink") == 0)
    {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0)
      {
        fprintf(stderr,
                "error: expected boolean for no_link in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (no_link)
        *no_link = val;
    }
    else if (strcmp(key, "entry") == 0 || strcmp(key, "entry_symbol") == 0 ||
             strcmp(key, "entrysymbol") == 0)
    {
      if (entry_symbol)
      {
        *entry_symbol = value_buf[0] ? xstrdup(value_buf) : NULL;
      }
    }
    else if (strcmp(key, "freestanding") == 0)
    {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0)
      {
        fprintf(stderr,
                "error: expected boolean for freestanding in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (freestanding)
        *freestanding = val;
    }
    else if (strcmp(key, "target_os") == 0 || strcmp(key, "targetos") == 0)
    {
      if (equals_icase(value_buf, "windows"))
      {
        if (target_os)
          *target_os = OS_WINDOWS;
      }
      else if (equals_icase(value_buf, "linux"))
      {
        if (target_os)
          *target_os = OS_LINUX;
      }
      else if (equals_icase(value_buf, "macos"))
      {
        if (target_os)
          *target_os = OS_MACOS;
      }
      else
      {
        fprintf(stderr, "error: unknown target_os '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    }
    else
    {
      fprintf(stderr,
              "warning: ignoring unrecognized key '%s' in '%s' (line %d)\n", key,
              proj_path, lineno);
    }
  }
  fclose(f);
  if (no_link && *no_link && output_overridden && *output_overridden &&
      obj_override && out && *out)
  {
    if (!*obj_override)
      *obj_override = *out;
  }
  return rc;
}
