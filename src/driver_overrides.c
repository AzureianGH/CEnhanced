#include "driver_overrides.h"

#include "ast.h"
#include "driver_paths.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int parse_override_spec(const char *spec, const char *base_dir,
                               char *orig, size_t origsz,
                               char *repl, size_t replsz)
{
  if (!spec || !orig || !repl)
    return -1;
  const char *eq = strchr(spec, '=');
  if (!eq || eq == spec || !eq[1])
    return -1;
  char left[1024];
  char right[1024];
  size_t left_len = (size_t)(eq - spec);
  if (left_len >= sizeof(left))
    left_len = sizeof(left) - 1;
  memcpy(left, spec, left_len);
  left[left_len] = '\0';
  snprintf(right, sizeof(right), "%s", eq + 1);
  char resolved_left[1024];
  char resolved_right[1024];
  resolve_project_relative_path(resolved_left, sizeof(resolved_left), base_dir,
                                left);
  resolve_project_relative_path(resolved_right, sizeof(resolved_right), base_dir,
                                right);
  normalize_path_simple(orig, origsz, resolved_left);
  normalize_path_simple(repl, replsz, resolved_right);
  if (!orig[0] || !repl[0])
    return -1;
  return 0;
}

int add_override_file(const char *spec, const char *base_dir,
                      OverrideFile **list, int *count, int *cap)
{
  if (!spec || !list || !count || !cap)
    return -1;
  char orig[1024];
  char repl[1024];
  if (parse_override_spec(spec, base_dir, orig, sizeof(orig), repl,
                          sizeof(repl)) != 0)
    return -1;
  if (*count == *cap)
  {
    int new_cap = *cap ? (*cap * 2) : 4;
    OverrideFile *grown =
        (OverrideFile *)realloc(*list, sizeof(OverrideFile) * (size_t)new_cap);
    if (!grown)
      return -1;
    for (int i = *cap; i < new_cap; ++i)
    {
      grown[i].original = NULL;
      grown[i].replacement = NULL;
    }
    *list = grown;
    *cap = new_cap;
  }
  OverrideFile *slot = &(*list)[*count];
  slot->original = xstrdup(orig);
  slot->replacement = xstrdup(repl);
  if (!slot->original || !slot->replacement)
    return -1;
  (*count)++;
  return 0;
}

const char *find_override_path(const char *input,
                               const OverrideFile *list, int count)
{
  if (!input || !list || count <= 0)
    return NULL;
  char normalized[1024];
  normalize_path_simple(normalized, sizeof(normalized), input);
  for (int i = 0; i < count; ++i)
  {
    const OverrideFile *entry = &list[i];
    if (!entry->original || !entry->replacement)
      continue;
    if ((normalized[0] && strcmp(normalized, entry->original) == 0) ||
        strcmp(input, entry->original) == 0)
      return entry->replacement;
  }
  return NULL;
}
