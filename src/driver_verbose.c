#include "driver_verbose.h"

#include "ast.h"

#include <stdio.h>
#include <string.h>

static int driver_verbose_use_ansi = 1;

static const char *ansi_reset(void)
{
  return driver_verbose_use_ansi ? "\x1b[0m" : "";
}

static const char *ansi_bold(void)
{
  return driver_verbose_use_ansi ? "\x1b[1m" : "";
}

static const char *ansi_cyan(void)
{
  return driver_verbose_use_ansi ? "\x1b[36m" : "";
}

static const char *ansi_green(void)
{
  return driver_verbose_use_ansi ? "\x1b[32m" : "";
}

static const char *ansi_magenta(void)
{
  return driver_verbose_use_ansi ? "\x1b[35m" : "";
}

static const char *bool_str(int value)
{
  return value ? "yes" : "no";
}

static const char *asm_syntax_to_option(AsmSyntax syntax)
{
  switch (syntax)
  {
  case ASM_INTEL:
    return "intel";
  case ASM_ATT:
    return "att";
  case ASM_NASM:
    return "nasm";
  default:
    return "unknown";
  }
}

static const char *target_arch_name(TargetArch arch)
{
  if (arch == ARCH_X86)
    return "x86-64";
  if (arch == ARCH_ARM64)
    return "arm64";
  if (arch == ARCH_BSLASH)
    return "bslash";
  return "none";
}

static void print_indexed_inputs(const char *title, const char *prefix, int count,
                                 const char **values)
{
  if (!compiler_verbose_enabled() || !title || !prefix || count <= 0 ||
      !values)
    return;
  driver_verbose_section(title);
  for (int idx = 0; idx < count; ++idx)
  {
    char label[32];
    snprintf(label, sizeof(label), "%s[%d]", prefix, idx);
    driver_verbose_table_row(label, values[idx]);
  }
}

void driver_verbose_set_use_ansi(int enabled)
{
  driver_verbose_use_ansi = enabled ? 1 : 0;
}

void driver_verbose_section(const char *title)
{
  if (!compiler_verbose_enabled() || !title)
    return;
  fprintf(stderr, "\n%s%s%s\n", ansi_bold(), title, ansi_reset());
  size_t len = strlen(title);
  for (size_t i = 0; i < len; ++i)
    fputc('-', stderr);
  fputc('\n', stderr);
}

void driver_verbose_table_row(const char *label, const char *value)
{
  if (!compiler_verbose_enabled() || !label)
    return;
  fprintf(stderr, "  %s%-18s%s | %s\n", ansi_cyan(), label, ansi_reset(),
          value ? value : "-");
}

void driver_verbose_progress(const char *tag, int current, int total)
{
  enum
  {
    BAR_WIDTH = 28
  };
  if (!compiler_verbose_enabled() || !tag || total <= 0)
    return;
  if (current < 0)
    current = 0;
  if (current > total)
    current = total;
  int filled = (int)((long long)current * BAR_WIDTH / (total ? total : 1));
  if (filled < 0)
    filled = 0;
  if (filled > BAR_WIDTH)
    filled = BAR_WIDTH;
  char bar[BAR_WIDTH + 1];
  for (int i = 0; i < BAR_WIDTH; ++i)
    bar[i] = (i < filled) ? '=' : ' ';
  bar[BAR_WIDTH] = '\0';
  int pct = total ? (int)((long long)current * 100 / total) : 100;
  fprintf(stderr, "%s%-12s%s [%s%s%s] %3d%% (%d/%d)\n", ansi_magenta(), tag,
          ansi_reset(), ansi_green(), bar, ansi_reset(), pct, current, total);
  fflush(stderr);
}

void driver_verbose_print_config(const DriverVerboseConfigState *state)
{
  if (!compiler_verbose_enabled() || !state)
    return;

  driver_verbose_section("Configuration");
  driver_verbose_table_row("Output",
                           state->output_path ? state->output_path : "(default)");
  char opt_buf[16];
  snprintf(opt_buf, sizeof(opt_buf), "-O%d", state->opt_level);
  driver_verbose_table_row("Optimization", opt_buf);
  driver_verbose_table_row("Backend", target_arch_name(state->target_arch));
  driver_verbose_table_row("Target OS",
                           state->target_os_name ? state->target_os_name : "-");
  driver_verbose_table_row("Stop after .ccb", bool_str(state->stop_after_ccb));
  driver_verbose_table_row("Stop after asm", bool_str(state->stop_after_asm));
  driver_verbose_table_row("No link", bool_str(state->no_link));
  driver_verbose_table_row("Emit library", bool_str(state->emit_library));
  driver_verbose_table_row("Freestanding", bool_str(state->freestanding));
  driver_verbose_table_row("Entry symbol",
                           (state->entry_symbol && *state->entry_symbol)
                               ? state->entry_symbol
                               : "(default)");
  driver_verbose_table_row("Asm syntax", asm_syntax_to_option(state->asm_syntax));
  driver_verbose_table_row("Verbose mode", bool_str(state->verbose_active));
  driver_verbose_table_row("Verbose deep", bool_str(state->verbose_deep));

  char count_buf[32];
  snprintf(count_buf, sizeof(count_buf), "%d", state->include_dir_count);
  driver_verbose_table_row("Include dirs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->ce_count);
  driver_verbose_table_row("CE inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->ccb_count);
  driver_verbose_table_row("CCB inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->cclib_count);
  driver_verbose_table_row("CCLib inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->obj_count);
  driver_verbose_table_row("OBJ inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->symbol_ref_ce_count);
  driver_verbose_table_row("Symbol-ref CE inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", state->symbol_ref_cclib_count);
  driver_verbose_table_row("Symbol-ref CCLib inputs", count_buf);

  driver_verbose_table_row("ChanceCode backend",
                           state->chancecode_backend
                               ? state->chancecode_backend
                               : "(auto)");
  driver_verbose_table_row("Host CC",
                           (state->host_cc_cmd && *state->host_cc_cmd)
                               ? state->host_cc_cmd
                               : "cc");
  driver_verbose_table_row("Host CC override",
                           bool_str(state->host_cc_has_override));

  if (state->needs_chancecodec)
  {
    driver_verbose_table_row(
        "Chancecodec cmd",
        (state->chancecodec_cmd && *state->chancecodec_cmd)
            ? state->chancecodec_cmd
            : "(unresolved)");
    driver_verbose_table_row("Chancecodec override",
                             bool_str(state->chancecodec_has_override));
    driver_verbose_table_row("Chancecodec fallback",
                             bool_str(state->chancecodec_uses_fallback));
  }
  else
  {
    driver_verbose_table_row("Chancecodec cmd", "(not required)");
  }
}

void driver_verbose_print_inputs(const DriverVerboseInputsState *state)
{
  if (!compiler_verbose_enabled() || !state)
    return;

  print_indexed_inputs("Include Directories", "dir", state->include_dir_count,
                       (const char **)state->include_dirs);
  print_indexed_inputs("CE Inputs", "ce", state->ce_count, state->ce_inputs);
  print_indexed_inputs("Symbol-ref CE Inputs", "sr-ce", state->symbol_ref_ce_count,
                       state->symbol_ref_ce_inputs);
  print_indexed_inputs("CCB Inputs", "ccb", state->ccb_count, state->ccb_inputs);
  print_indexed_inputs("CCLib Inputs", "cclib", state->cclib_count,
                       state->cclib_inputs);
  print_indexed_inputs("Symbol-ref CCLib Inputs", "sr-cclib",
                       state->symbol_ref_cclib_count,
                       state->symbol_ref_cclib_inputs);
  print_indexed_inputs("Object Inputs", "obj", state->obj_count,
                       state->obj_inputs);
  print_indexed_inputs("Assembly Inputs", "asm", state->asm_count,
                       state->asm_inputs);
}
