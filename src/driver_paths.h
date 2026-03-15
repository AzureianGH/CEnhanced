#ifndef CHANCE_DRIVER_PATHS_H
#define CHANCE_DRIVER_PATHS_H

#include <stddef.h>

void split_path(const char *path, char *dir, size_t dsz,
                char *base_noext, size_t bsz);
void build_path_with_ext(const char *dir, const char *base,
                         const char *ext, char *buffer, size_t bufsz);
int is_regular_file(const char *path);
int parent_directory(const char *path, char *out, size_t outsz);
void strip_wrapping_quotes(const char *in, char *out, size_t outsz);
int get_executable_dir(char *dir, size_t dirsz, const char *argv0);
int locate_chancecodec(char *out, size_t outsz, const char *exe_dir);
int locate_cld(char *out, size_t outsz, const char *exe_dir);
int locate_chs(char *out, size_t outsz, const char *exe_dir);
void trim_whitespace_inplace(char *text);
int parse_bool_value(const char *text, int *out);
int is_path_absolute_simple(const char *path);
void resolve_project_relative_path(char *dst, size_t dstsz,
                                   const char *base_dir,
                                   const char *rel);
void normalize_path_simple(char *dst, size_t dstsz, const char *src);
const char *get_cwd_path(char *buf, size_t bufsz);

#endif
