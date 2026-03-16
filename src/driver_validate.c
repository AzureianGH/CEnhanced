#include "driver_validate.h"

#include "driver_cli.h"

#include <stdio.h>

int validate_driver_options(const DriverValidationState *state)
{
  if (!state || !state->out || !state->output_overridden ||
      !state->stop_after_asm || !state->stop_after_ccb || !state->no_link ||
      !state->emit_library || !state->freestanding || !state->m32 ||
      !state->debug_symbols || !state->strip_metadata ||
      !state->implicit_void_function || !state->language_standard ||
      !state->request_ast || !state->diagnostics_only || !state->target_arch ||
      !state->ce_count || !state->ccb_count || !state->cclib_count ||
      !state->obj_count || !state->asm_count)
    return 2;

  if (*state->strip_metadata)
    *state->debug_symbols = 0;

  if (*state->stop_after_asm && *state->stop_after_ccb)
  {
    fprintf(stderr, "error: -S and -Sccb cannot be used together\n");
    return 2;
  }
  if (*state->stop_after_asm && *state->target_arch == ARCH_NONE &&
      !*state->request_ast && !*state->diagnostics_only)
  {
    fprintf(stderr,
            "error: -S requires a backend selection (e.g., -x86 or -arm64)\n");
    return 2;
  }
  if (!*state->emit_library && *state->target_arch == ARCH_NONE &&
      !*state->request_ast && !*state->diagnostics_only)
  {
    if (!*state->stop_after_ccb)
    {
      fprintf(stderr,
              "error: selecting a backend (e.g., -x86 or -arm64) is required "
              "unless stopping at bytecode with -Sccb\n");
      return 2;
    }
    if (*state->no_link)
    {
      fprintf(stderr, "error: -c/--no-link is incompatible with -Sccb (no "
                      "object emission when stopping at bytecode)\n");
      return 2;
    }
    if (*state->obj_count > 0 || *state->ccb_count > 0 || *state->asm_count > 0)
    {
      fprintf(stderr, "error: providing .ccb/.o/.obj inputs requires selecting "
                      "a backend (e.g., -x86 or -arm64)\n");
      return 2;
    }
  }
  if (*state->ce_count == 0 && *state->obj_count == 0 && *state->ccb_count == 0 &&
      *state->asm_count == 0)
  {
    usage(state->prog_name);
    return 2;
  }
  if (*state->emit_library)
  {
    if (*state->stop_after_asm)
    {
      fprintf(stderr, "error: --library cannot be combined with -S\n");
      return 2;
    }
    if (*state->stop_after_ccb)
    {
      fprintf(stderr, "error: --library already stops after bytecode output\n");
      return 2;
    }
    if (*state->no_link)
    {
      fprintf(stderr, "error: --library is incompatible with -c/--no-link\n");
      return 2;
    }
    if (*state->target_arch != ARCH_NONE)
    {
      fprintf(stderr, "error: --library cannot be combined with backend "
                      "selection (e.g., -x86 or -arm64)\n");
      return 2;
    }
    if (*state->ccb_count > 0 || *state->obj_count > 0 || *state->asm_count > 0 ||
        *state->cclib_count > 0)
    {
      fprintf(stderr, "error: --library currently only accepts .ce inputs\n");
      return 2;
    }
    if (*state->ce_count == 0)
    {
      fprintf(stderr, "error: --library requires at least one .ce input\n");
      return 2;
    }
    if (!*state->output_overridden)
    {
      *state->out = "a.cclib";
    }
    if (!ends_with_icase(*state->out, ".cclib"))
    {
      fprintf(stderr, "error: --library output must end with .cclib\n");
      return 2;
    }
  }
  if (*state->m32)
  {
    fprintf(stderr, "Error: -m32 not implemented yet\n");
    return 2;
  }
  if (*state->language_standard == CHANCE_STD_H26 && *state->implicit_void_function)
  {
    fprintf(stderr,
            "error: --implicit-void-function is only available in H27 mode\n");
    return 2;
  }

  return 0;
}