#ifndef CHANCE_INCLUDES_H
#define CHANCE_INCLUDES_H

#include "ast.h"

// Manage include directories
void chance_add_include_dir(char ***dirs, int *count, const char *dir);
void chance_add_default_include_dirs(char ***dirs, int *count);

// Process #include lines in the given source (for path resolution only) and
// recursively scan included C headers for simple function prototypes, adding
// extern symbols to 'syms'. Returns 0 on success.
int chance_process_includes_and_scan(const char *source_path,
                                     const char *source_buf, int source_len,
                                     char **include_dirs, int dir_count,
                                     SymTable *syms);

// Produce a copy of source with lines that start with optional spaces then '#'
// removed. Caller must free the returned buffer.
char *chance_strip_preprocessor_lines(const char *src, int len, int *out_len);

#endif
