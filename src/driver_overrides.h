#ifndef CHANCE_DRIVER_OVERRIDES_H
#define CHANCE_DRIVER_OVERRIDES_H

#include <stddef.h>

typedef struct
{
  char *original;
  char *replacement;
} OverrideFile;

int add_override_file(const char *spec, const char *base_dir,
                      OverrideFile **list, int *count, int *cap);

const char *find_override_path(const char *input,
                               const OverrideFile *list, int count);

#endif
