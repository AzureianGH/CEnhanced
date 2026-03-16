#include "driver_toolchain.h"

#include "driver_paths.h"

#include <stdio.h>
#include <string.h>

static const char *default_chancecodec_name =
#ifdef _WIN32
    "chancecodec.exe"
#else
    "chancecodec"
#endif
    ;

static const char *default_cld_name =
#ifdef _WIN32
    "cld.exe"
#else
    "cld"
#endif
    ;

static const char *default_chs_name =
#ifdef _WIN32
    "chs.exe"
#else
    "chs"
#endif
    ;

static const char *cld_target_name_for_link(TargetArch arch, TargetOS os)
{
  switch (arch)
  {
  case ARCH_ARM64:
    return os == OS_MACOS ? "macos-arm64" : NULL;
  case ARCH_X86:
    return os == OS_LINUX ? "x86_64-elf" : NULL;
  case ARCH_BSLASH:
    return "bslash";
  default:
    return NULL;
  }
}

int resolve_driver_toolchain(const DriverToolchainInputs *inputs,
                             DriverToolchainSelection *selection)
{
  if (!inputs || !selection)
    return 2;
  memset(selection, 0, sizeof(*selection));

  selection->host_cc_cmd_to_use = "cc";
  if (inputs->host_cc_cmd_override && *inputs->host_cc_cmd_override)
  {
    strip_wrapping_quotes(inputs->host_cc_cmd_override,
                          selection->host_cc_override_buf,
                          sizeof(selection->host_cc_override_buf));
    if (!selection->host_cc_override_buf[0])
    {
      fprintf(stderr,
              "error: --cc path is empty after trimming quotes/whitespace\n");
      return 2;
    }
    selection->host_cc_cmd_to_use = selection->host_cc_override_buf;
    selection->host_cc_has_override = 1;
  }

  selection->needs_chancecodec =
      (inputs->target_arch != ARCH_NONE) || inputs->emit_library;
  if (selection->needs_chancecodec)
  {
    if (inputs->chancecodec_cmd_override && *inputs->chancecodec_cmd_override)
    {
      strip_wrapping_quotes(inputs->chancecodec_cmd_override,
                            selection->chancecodec_override_buf,
                            sizeof(selection->chancecodec_override_buf));
      if (!selection->chancecodec_override_buf[0])
      {
        fprintf(stderr, "error: --chancecodec path is empty after trimming "
                        "quotes/whitespace\n");
        return 2;
      }
      selection->chancecodec_cmd_to_use = selection->chancecodec_override_buf;
      selection->chancecodec_has_override = 1;
    }
    else if (locate_chancecodec(selection->chancecodec_exec_buf,
                                sizeof(selection->chancecodec_exec_buf),
                                inputs->exe_dir) == 0 &&
             selection->chancecodec_exec_buf[0])
    {
      selection->chancecodec_cmd_to_use = selection->chancecodec_exec_buf;
    }
    else
    {
      selection->chancecodec_cmd_to_use = default_chancecodec_name;
      selection->chancecodec_uses_fallback = 1;
    }
  }

  selection->needs_chs = !inputs->emit_library && !inputs->stop_after_ccb &&
                         !inputs->stop_after_asm &&
                         (inputs->target_arch == ARCH_ARM64 ||
                          inputs->target_arch == ARCH_BSLASH);
  if (selection->needs_chs)
  {
    if (inputs->chs_cmd_override && *inputs->chs_cmd_override)
    {
      strip_wrapping_quotes(inputs->chs_cmd_override,
                            selection->chs_override_buf,
                            sizeof(selection->chs_override_buf));
      if (!selection->chs_override_buf[0])
      {
        fprintf(stderr,
                "error: --chs path is empty after trimming quotes/whitespace\n");
        return 2;
      }
      selection->chs_cmd_to_use = selection->chs_override_buf;
      selection->chs_has_override = 1;
    }
    else if (locate_chs(selection->chs_exec_buf, sizeof(selection->chs_exec_buf),
                        inputs->exe_dir) == 0 &&
             selection->chs_exec_buf[0])
    {
      selection->chs_cmd_to_use = selection->chs_exec_buf;
    }
    else
    {
      selection->chs_cmd_to_use = default_chs_name;
      selection->chs_uses_fallback = 1;
    }
  }

  selection->cld_target_to_use =
      cld_target_name_for_link(inputs->target_arch, inputs->target_os);
  selection->cld_supported_link_target =
      selection->cld_target_to_use && *selection->cld_target_to_use;
  if (!inputs->emit_library && selection->cld_supported_link_target)
  {
    if (locate_cld(selection->cld_exec_buf, sizeof(selection->cld_exec_buf),
                   inputs->exe_dir) == 0 &&
        selection->cld_exec_buf[0])
    {
      selection->cld_cmd_to_use = selection->cld_exec_buf;
    }
    else
    {
      selection->cld_cmd_to_use = default_cld_name;
      selection->cld_uses_fallback = 1;
    }
  }

  return 0;
}