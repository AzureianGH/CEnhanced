#ifndef CHANCE_DRIVER_TOOLCHAIN_H
#define CHANCE_DRIVER_TOOLCHAIN_H

#include "ast.h"
#include "driver_types.h"

typedef struct
{
  const char *exe_dir;
  const char *host_cc_cmd_override;
  const char *chancecodec_cmd_override;
  const char *chs_cmd_override;

  TargetArch target_arch;
  TargetOS target_os;

  int emit_library;
  int stop_after_ccb;
  int stop_after_asm;
} DriverToolchainInputs;

typedef struct
{
  const char *host_cc_cmd_to_use;
  int host_cc_has_override;

  const char *chancecodec_cmd_to_use;
  int chancecodec_uses_fallback;
  int chancecodec_has_override;
  int needs_chancecodec;

  const char *chs_cmd_to_use;
  int chs_uses_fallback;
  int chs_has_override;
  int needs_chs;

  const char *cld_cmd_to_use;
  int cld_uses_fallback;
  const char *cld_target_to_use;
  int cld_supported_link_target;

  char host_cc_override_buf[1024];
  char chancecodec_exec_buf[1024];
  char chancecodec_override_buf[1024];
  char chs_exec_buf[1024];
  char chs_override_buf[1024];
  char cld_exec_buf[1024];
} DriverToolchainSelection;


int resolve_driver_toolchain(const DriverToolchainInputs *inputs,
                             DriverToolchainSelection *selection);

#endif