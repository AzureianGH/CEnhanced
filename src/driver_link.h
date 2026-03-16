#ifndef CHANCE_DRIVER_LINK_H
#define CHANCE_DRIVER_LINK_H

#include "ast.h"

typedef struct
{
  int multi_link;
  int have_single_obj;
  int single_obj_is_temp;
  int no_link;
  const char *obj_override;

  int cld_supported_link_target;
  const char *cld_cmd_to_use;
  const char *cld_target_to_use;
  int cld_uses_fallback;

  int freestanding;
  int toolchain_debug_mode;
  int toolchain_debug_deep;
  int debug_symbols;
  TargetOS target_os;

  const char *out;
  const char **obj_inputs;
  int obj_count;
  char *single_obj_path;

  char ***temp_objs;
  int *to_cnt;
  int *to_cap;
} DriverLinkPhaseState;

int run_driver_link_phase(DriverLinkPhaseState *state);

#endif
