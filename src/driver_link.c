#include "driver_link.h"

#include "driver_verbose.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <process.h>
#else
#include <spawn.h>
#include <sys/wait.h>
extern char **environ;
#endif

static void maybe_generate_dsym(const char *binary_path, int debug_symbols,
                                TargetOS target_os)
{
#if defined(__APPLE__)
  if (!binary_path || !debug_symbols || target_os != OS_MACOS)
    return;
  char ds_cmd[4096];
  snprintf(ds_cmd, sizeof(ds_cmd), "dsymutil \"%s\"", binary_path);
  int ds_rc = system(ds_cmd);
  if (ds_rc != 0)
  {
    fprintf(stderr,
            "warning: dsymutil failed (rc=%d) while processing '%s'\n",
            ds_rc, binary_path);
  }
#else
  (void)binary_path;
  (void)debug_symbols;
  (void)target_os;
#endif
}

static int run_cld_link_process(const char *cmd, const char *target_name,
                                const char *output_kind,
                                const char *output_path, int no_stdlib,
                                const char **inputs, int input_count,
                                const char *entry_symbol,
                                int toolchain_debug_mode,
                                int toolchain_debug_deep,
                                int *spawn_errno_out)
{
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  if (!cmd || !*cmd || !output_kind || !*output_kind || !output_path ||
      !*output_path || !inputs || input_count <= 0)
  {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }

  size_t arg_cap =
      (size_t)input_count + 12u + (target_name && *target_name ? 2u : 0u) +
      (no_stdlib ? 1u : 0u) +
      ((entry_symbol && *entry_symbol && strcmp(output_kind, "executable") == 0) ? 2u : 0u);
#ifdef _WIN32
  const char **args = (const char **)calloc(arg_cap, sizeof(*args));
#else
  char **args = (char **)calloc(arg_cap, sizeof(*args));
#endif
  if (!args)
  {
    if (spawn_errno_out)
      *spawn_errno_out = ENOMEM;
    return -1;
  }

  size_t idx = 0;
  args[idx++] =
#ifdef _WIN32
      cmd;
#else
      (char *)cmd;
#endif
  args[idx++] =
#ifdef _WIN32
      "link";
#else
      (char *)"link";
#endif
  args[idx++] =
#ifdef _WIN32
      "-o";
#else
      (char *)"-o";
#endif
  args[idx++] =
#ifdef _WIN32
      output_path;
#else
      (char *)output_path;
#endif
  if (target_name && *target_name)
  {
    args[idx++] =
#ifdef _WIN32
        "--target";
#else
        (char *)"--target";
#endif
    args[idx++] =
#ifdef _WIN32
        target_name;
#else
        (char *)target_name;
#endif
  }
  args[idx++] =
#ifdef _WIN32
      "--output-kind";
#else
      (char *)"--output-kind";
#endif
  args[idx++] =
#ifdef _WIN32
      output_kind;
#else
      (char *)output_kind;
#endif
    if (entry_symbol && *entry_symbol && strcmp(output_kind, "executable") == 0)
    {
      args[idx++] =
  #ifdef _WIN32
    "--entry";
  #else
    (char *)"--entry";
  #endif
      args[idx++] =
  #ifdef _WIN32
    entry_symbol;
  #else
    (char *)entry_symbol;
  #endif
    }
  if (no_stdlib)
    args[idx++] =
#ifdef _WIN32
        "-nostdlib";
#else
        (char *)"-nostdlib";
#endif
  if (toolchain_debug_deep)
    args[idx++] =
#ifdef _WIN32
        "-vd";
#else
        (char *)"-vd";
#endif
  else if (toolchain_debug_mode)
    args[idx++] =
#ifdef _WIN32
        "-d";
#else
        (char *)"-d";
#endif
  for (int i = 0; i < input_count; ++i)
    args[idx++] =
#ifdef _WIN32
        inputs[i];
#else
        (char *)inputs[i];
#endif
  args[idx] = NULL;

#ifdef _WIN32
  intptr_t rc = _spawnvp(_P_WAIT, cmd, args);
  free(args);
  if (rc == -1)
  {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  return (int)rc;
#else
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, cmd, NULL, NULL, args, environ);
  free(args);
  if (rc != 0)
  {
    if (spawn_errno_out)
      *spawn_errno_out = rc;
    errno = rc;
    return -1;
  }
  int status = 0;
  if (waitpid(pid, &status, 0) == -1)
  {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  if (WIFEXITED(status))
    return WEXITSTATUS(status);
  if (WIFSIGNALED(status))
    return 128 + WTERMSIG(status);
  return -1;
#endif
}

static void cleanup_temp_objects(char ***temp_objs, int *to_cnt, int *to_cap)
{
  if (!temp_objs || !*temp_objs || !to_cnt || !to_cap)
    return;
  for (int i = 0; i < *to_cnt; ++i)
  {
    remove((*temp_objs)[i]);
    free((*temp_objs)[i]);
  }
  free(*temp_objs);
  *temp_objs = NULL;
  *to_cnt = 0;
  *to_cap = 0;
}

int run_driver_link_phase(DriverLinkPhaseState *state)
{
  if (!state)
    return 1;

  if (state->multi_link)
  {
    if (state->cld_supported_link_target)
    {
      int input_count = *state->to_cnt + state->obj_count;
      const char **link_inputs =
          (const char **)xcalloc((size_t)input_count, sizeof(const char *));
      if (!link_inputs)
      {
        fprintf(stderr, "error: out of memory allocating linker inputs\n");
        cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
        return 1;
      }

      int input_index = 0;
      for (int i = 0; i < *state->to_cnt; ++i)
        link_inputs[input_index++] = (*state->temp_objs)[i];
      for (int i = 0; i < state->obj_count; ++i)
        link_inputs[input_index++] = state->obj_inputs[i];

      if (compiler_verbose_enabled())
      {
        driver_verbose_section("Linking executable");
        driver_verbose_table_row("Linker", state->cld_cmd_to_use);
        driver_verbose_table_row("Target", state->cld_target_to_use);
        driver_verbose_table_row("Entry", state->entry_symbol ? state->entry_symbol : "(default)");
      }

      int spawn_errno = 0;
      int lrc = run_cld_link_process(
          state->cld_cmd_to_use, state->cld_target_to_use, "executable",
          state->out, state->freestanding, link_inputs, input_count,
          state->entry_symbol,
          state->toolchain_debug_mode, state->toolchain_debug_deep,
          &spawn_errno);
      free(link_inputs);
      if (lrc != 0)
      {
        if (lrc < 0)
        {
          fprintf(stderr, "failed to launch cld '%s': %s\n",
                  state->cld_cmd_to_use, strerror(spawn_errno));
          if (state->cld_uses_fallback)
            fprintf(stderr, "hint: install cld or set CLD_CMD to point at the CLD executable\n");
        }
        else
        {
          fprintf(stderr, "cld link failed (rc=%d)\n", lrc);
        }
        cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
        return 1;
      }
      maybe_generate_dsym(state->out, state->debug_symbols, state->target_os);
    }
    else
    {
      fprintf(stderr,
              "error: self-hosted linking is required; no CLD target is available for this build/target\n");
      fprintf(stderr,
              "hint: use a CLD-supported target or extend CLD for this architecture/object format\n");
      cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
      return 1;
    }

    cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
    return 0;
  }

  if (state->have_single_obj)
  {
    if (state->cld_supported_link_target)
    {
      const char *link_inputs[1] = {state->single_obj_path};
      if (compiler_verbose_enabled())
      {
        driver_verbose_section("Linking single object");
        driver_verbose_table_row("Linker", state->cld_cmd_to_use);
        driver_verbose_table_row("Target", state->cld_target_to_use);
        driver_verbose_table_row("Entry", state->entry_symbol ? state->entry_symbol : "(default)");
      }
      int spawn_errno = 0;
      int lrc = run_cld_link_process(
          state->cld_cmd_to_use, state->cld_target_to_use, "executable",
          state->out, state->freestanding, link_inputs, 1,
          state->entry_symbol,
          state->toolchain_debug_mode, state->toolchain_debug_deep,
          &spawn_errno);
      if (lrc != 0)
      {
        if (lrc < 0)
        {
          fprintf(stderr, "failed to launch cld '%s': %s\n",
                  state->cld_cmd_to_use, strerror(spawn_errno));
          if (state->cld_uses_fallback)
            fprintf(stderr, "hint: install cld or set CLD_CMD to point at the CLD executable\n");
        }
        else
        {
          fprintf(stderr, "cld link failed (rc=%d)\n", lrc);
        }
        if (state->single_obj_is_temp)
          remove(state->single_obj_path);
        return 1;
      }
      maybe_generate_dsym(state->out, state->debug_symbols, state->target_os);
    }
    else
    {
      fprintf(stderr,
              "error: self-hosted linking is required; no CLD target is available for this build/target\n");
      fprintf(stderr,
              "hint: use a CLD-supported target or extend CLD for this architecture/object format\n");
      if (state->single_obj_is_temp)
        remove(state->single_obj_path);
      return 1;
    }

    if (state->single_obj_is_temp)
      remove(state->single_obj_path);
    return 0;
  }

  if (state->no_link && state->obj_override)
  {
    if (state->cld_supported_link_target)
    {
      int input_count = *state->to_cnt + state->obj_count;
      const char **link_inputs =
          (const char **)xcalloc((size_t)input_count, sizeof(const char *));
      if (!link_inputs)
      {
        fprintf(stderr, "error: out of memory allocating linker inputs\n");
        cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
        return 1;
      }

      int input_index = 0;
      for (int i = 0; i < *state->to_cnt; ++i)
        link_inputs[input_index++] = (*state->temp_objs)[i];
      for (int i = 0; i < state->obj_count; ++i)
        link_inputs[input_index++] = state->obj_inputs[i];

      if (compiler_verbose_enabled())
      {
        driver_verbose_section("Merging objects");
        driver_verbose_table_row("Linker", state->cld_cmd_to_use);
        driver_verbose_table_row("Target", state->cld_target_to_use);
      }

      int spawn_errno = 0;
      int lrc = run_cld_link_process(
          state->cld_cmd_to_use, state->cld_target_to_use, "relocatable",
          state->obj_override, state->freestanding, link_inputs, input_count,
          NULL,
          state->toolchain_debug_mode, state->toolchain_debug_deep,
          &spawn_errno);
      free(link_inputs);
      if (lrc != 0)
      {
        if (lrc < 0)
        {
          fprintf(stderr, "failed to launch cld '%s': %s\n",
                  state->cld_cmd_to_use, strerror(spawn_errno));
          if (state->cld_uses_fallback)
            fprintf(stderr, "hint: install cld or set CLD_CMD to point at the CLD executable\n");
        }
        else
        {
          fprintf(stderr, "cld relocatable link failed (rc=%d)\n", lrc);
        }
        cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
        return 1;
      }
    }
    else
    {
      fprintf(stderr,
              "error: self-hosted relocatable linking is required; no CLD target is available for this build/target\n");
      fprintf(stderr,
              "hint: use a CLD-supported target or extend CLD relocatable output for this architecture/object format\n");
      cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
      return 1;
    }

    cleanup_temp_objects(state->temp_objs, state->to_cnt, state->to_cap);
  }

  return 0;
}
