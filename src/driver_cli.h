#ifndef CHANCE_DRIVER_CLI_H
#define CHANCE_DRIVER_CLI_H

void usage(const char *prog);
int ends_with_icase(const char *s, const char *suf);
int is_ce_source_arg(const char *path);
int is_object_file_arg(const char *path);
int equals_icase(const char *a, const char *b);

#endif