#include "driver_runtime.h"

#include "driver_paths.h"
#include "driver_project.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#define CHANCE_PATH_SEP '\\'
#else
#define CHANCE_PATH_SEP '/'
#endif

static int locate_cclib_path(const char *exe_dir, const char *subdir,
                             const char *filename, const char *default_path,
                             char *out, size_t outsz)
{
  if (!out || outsz == 0 || !subdir || !filename)
    return 0;
  out[0] = '\0';

  if (exe_dir && exe_dir[0])
  {
    snprintf(out, outsz, "%s%c%s%c%s", exe_dir, CHANCE_PATH_SEP, subdir,
             CHANCE_PATH_SEP, filename);
    if (is_regular_file(out))
      return 1;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0])
    {
      snprintf(out, outsz, "%s%c%s%c%s", parent, CHANCE_PATH_SEP, subdir,
               CHANCE_PATH_SEP, filename);
      if (is_regular_file(out))
        return 1;
    }
  }

  if (default_path && default_path[0])
  {
    snprintf(out, outsz, "%s", default_path);
    if (is_regular_file(out))
      return 1;
  }

  const char *home_env = getenv("CHANCE_HOME");
  if (!home_env || !home_env[0])
    home_env = getenv("CHANCEC_HOME");
  if (!home_env || !home_env[0])
    home_env = getenv("CHANCE_ROOT");
  if (home_env && home_env[0])
  {
    snprintf(out, outsz, "%s%c%s%c%s", home_env, CHANCE_PATH_SEP, subdir,
             CHANCE_PATH_SEP, filename);
    if (is_regular_file(out))
      return 1;
  }

  const char *path_env = getenv("PATH");
  if (path_env)
  {
    const char *p = path_env;
    while (*p)
    {
      const char *s = p;
      while (*p && *p != ':')
        p++;
      size_t seg_len = (size_t)(p - s);
      if (seg_len > 0)
      {
        char candidate[1024];
        if (snprintf(candidate, sizeof(candidate), "%.*s/%s/%s", (int)seg_len,
                     s, subdir, filename) > 0)
        {
          if (is_regular_file(candidate))
          {
            snprintf(out, outsz, "%s", candidate);
            return 1;
          }
        }
      }
      if (*p)
        p++;
    }
  }

  return 0;
}

int maybe_inject_default_runtime_libs(const DriverRuntimeLibState *state)
{
  if (!state || !state->freestanding_requested || !state->freestanding ||
      !state->cclib_inputs || !state->cclib_count || !state->cclib_cap ||
      !state->owned_cclib_inputs || !state->owned_cclib_count ||
      !state->owned_cclib_cap)
    return -1;

  if (state->emit_library)
    return 0;

  if (state->no_link)
    return 0;

  ProjectInputList cclib_list = {
      state->cclib_inputs,       state->cclib_count,      state->cclib_cap,
      state->owned_cclib_inputs, state->owned_cclib_count,
      state->owned_cclib_cap,    NULL};

  if (!*state->freestanding_requested)
  {
    char stdlib_path[1024];
    if (!locate_cclib_path(state->exe_dir, "stdlib", "stdlib.cclib",
                           state->default_stdlib_path, stdlib_path,
                           sizeof(stdlib_path)))
    {
      (void)locate_cclib_path(state->exe_dir, "src/stdlib", "stdlib.cclib",
                              state->default_stdlib_path, stdlib_path,
                              sizeof(stdlib_path));
    }

    if (stdlib_path[0] && is_regular_file(stdlib_path))
    {
      if (push_input_entry(stdlib_path, cclib_list, 1) != 0)
        return -1;
    }
    else
    {
      *state->freestanding = 1;
      fprintf(stderr,
              "warning: stdlib not found at '%s'; enabling freestanding (--nostdlib)\n",
              stdlib_path[0] ? stdlib_path : "<unknown>");
    }
  }

  if (!*state->freestanding)
  {
    char runtime_path[1024];
    if (!locate_cclib_path(state->exe_dir, "runtime", "runtime.cclib",
                           state->default_runtime_path, runtime_path,
                           sizeof(runtime_path)))
    {
      (void)locate_cclib_path(state->exe_dir, "stdlib", "runtime.cclib",
                              state->default_runtime_path, runtime_path,
                              sizeof(runtime_path));
    }

    if (runtime_path[0] && is_regular_file(runtime_path))
    {
      if (push_input_entry(runtime_path, cclib_list, 1) != 0)
        return -1;
    }
    else
    {
      fprintf(stderr, "error: runtime.cclib not found at '%s'\n",
              runtime_path[0] ? runtime_path : "<unknown>");
      return -1;
    }
  }

  return 0;
}