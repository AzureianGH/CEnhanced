#ifndef CHANCE_INCLUDES_H
#define CHANCE_INCLUDES_H

#include "ast.h"


void chance_add_include_dir(char ***dirs, int *count, const char *dir);
void chance_add_default_include_dirs(char ***dirs, int *count);




int chance_process_includes_and_scan(const char *source_path,
                                     const char *source_buf, int source_len,
                                     char **include_dirs, int dir_count,
                                     SymTable *syms);

#endif
