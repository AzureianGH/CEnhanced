#include "ast.h"
#include "cclib.h"
#include "includes.h"
#include "module_registry.h"
#include "preproc.h"
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#ifdef _WIN32
#include <direct.h>
#include <process.h>
#else
#include <spawn.h>
#include <sys/wait.h>
extern char **environ;
#endif
#if !defined(_WIN32)
#include <limits.h>
#include <unistd.h>
#endif

#define CHANCECODEC_BASE "chancecodec"
#ifdef _WIN32
#define CHANCE_PATH_SEP '\\'
#define CHANCECODEC_EXT ".exe"
#else
#define CHANCE_PATH_SEP '/'
#define CHANCECODEC_EXT ""
#endif

typedef enum { ARCH_NONE = 0, ARCH_X86 } TargetArch;

static const char *default_chancecodec_name =
#ifdef _WIN32
    "chancecodec.exe"
#else
    "chancecodec"
#endif
    ;

static int verbose_use_ansi = 1;
static const char *target_os_to_option(TargetOS os);

static const char *ansi_reset(void)
{
  return verbose_use_ansi ? "\x1b[0m" : "";
}

static const char *ansi_bold(void)
{
  return verbose_use_ansi ? "\x1b[1m" : "";
}

static const char *ansi_cyan(void)
{
  return verbose_use_ansi ? "\x1b[36m" : "";
}

static const char *ansi_green(void)
{
  return verbose_use_ansi ? "\x1b[32m" : "";
}

static const char *ansi_magenta(void)
{
  return verbose_use_ansi ? "\x1b[35m" : "";
}

static const char *bool_str(int value)
{
  return value ? "yes" : "no";
}

static void verbose_section(const char *title)
{
  if (!compiler_verbose_enabled() || !title)
    return;
  fprintf(stderr, "\n%s%s%s\n", ansi_bold(), title, ansi_reset());
  size_t len = strlen(title);
  for (size_t i = 0; i < len; ++i)
    fputc('-', stderr);
  fputc('\n', stderr);
}

static void verbose_table_row(const char *label, const char *value)
{
  if (!compiler_verbose_enabled() || !label)
    return;
  fprintf(stderr, "  %s%-18s%s | %s\n", ansi_cyan(), label, ansi_reset(),
          value ? value : "-");
}

static void verbose_progress(const char *tag, int current, int total)
{
  if (!compiler_verbose_enabled() || !tag || total <= 0)
    return;
  if (current < 0)
    current = 0;
  if (current > total)
    current = total;
  const int bar_width = 28;
  int filled = (int)((long long)current * bar_width / (total ? total : 1));
  if (filled < 0)
    filled = 0;
  if (filled > bar_width)
    filled = bar_width;
  char bar[bar_width + 1];
  for (int i = 0; i < bar_width; ++i)
    bar[i] = (i < filled) ? '=' : ' ';
  bar[bar_width] = '\0';
  int pct = total ? (int)((long long)current * 100 / total) : 100;
  fprintf(stderr, "%s%-12s%s [%s%s%s] %3d%% (%d/%d)\n", ansi_magenta(), tag,
          ansi_reset(), ansi_green(), bar, ansi_reset(), pct, current, total);
  fflush(stderr);
}

static const char *asm_syntax_to_option(AsmSyntax syntax)
{
  switch (syntax) {
  case ASM_INTEL:
    return "intel";
  case ASM_ATT:
    return "att";
  case ASM_NASM:
    return "nasm";
  default:
    return "unknown";
  }
}

static const char *target_arch_to_macro(TargetArch arch)
{
  switch (arch) {
  case ARCH_X86:
    return "x86";
  case ARCH_NONE:
  default:
    return NULL;
  }
}

static void verbose_print_config(
    const char *output_path, int opt_level, TargetArch target_arch,
    TargetOS target_os, int stop_after_ccb, int stop_after_asm, int no_link,
    int emit_library, int freestanding, AsmSyntax asm_syntax,
    int include_dir_count, int ce_count, int ccb_count, int cclib_count,
    int obj_count, const char *chancecodec_cmd, int chancecodec_has_override,
  int needs_chancecodec, const char *chancecode_backend,
  int chancecodec_uses_fallback, int verbose_active, int verbose_deep)
{
  if (!compiler_verbose_enabled())
    return;
  verbose_section("Configuration");
  verbose_table_row("Output", output_path ? output_path : "(default)");
  char opt_buf[16];
  snprintf(opt_buf, sizeof(opt_buf), "-O%d", opt_level);
  verbose_table_row("Optimization", opt_buf);
  const char *arch = target_arch == ARCH_X86 ? "x86-64" : "none";
  verbose_table_row("Backend", arch);
  verbose_table_row("Target OS", target_os_to_option(target_os));
  verbose_table_row("Stop after .ccb", bool_str(stop_after_ccb));
  verbose_table_row("Stop after asm", bool_str(stop_after_asm));
  verbose_table_row("No link", bool_str(no_link));
  verbose_table_row("Emit library", bool_str(emit_library));
  verbose_table_row("Freestanding", bool_str(freestanding));
  verbose_table_row("Asm syntax", asm_syntax_to_option(asm_syntax));
  verbose_table_row("Verbose mode", bool_str(verbose_active));
  verbose_table_row("Verbose deep", bool_str(verbose_deep));
  char count_buf[32];
  snprintf(count_buf, sizeof(count_buf), "%d", include_dir_count);
  verbose_table_row("Include dirs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", ce_count);
  verbose_table_row("CE inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", ccb_count);
  verbose_table_row("CCB inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", cclib_count);
  verbose_table_row("CCLib inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", obj_count);
  verbose_table_row("OBJ inputs", count_buf);
  verbose_table_row("ChanceCode backend",
                    chancecode_backend ? chancecode_backend : "(auto)");
  if (needs_chancecodec) {
    verbose_table_row("Chancecodec cmd",
                      chancecodec_cmd && *chancecodec_cmd ? chancecodec_cmd
                                                           : "(unresolved)");
    verbose_table_row("Chancecodec override",
                      bool_str(chancecodec_has_override));
    verbose_table_row("Chancecodec fallback",
                      bool_str(chancecodec_uses_fallback));
  } else {
    verbose_table_row("Chancecodec cmd", "(not required)");
  }
}

typedef struct {
  char *input_path;
  char *src;
  char *stripped;
  Node *unit;
  SemaContext *sc;
  Parser *parser;
} UnitCompile;

// sema
int sema_eval_const_i32(Node *expr);
SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
int sema_check_unit(SemaContext *sc, Node *unit);

#if defined(__linux__) || defined(__APPLE__)
#define _stricmp strcasecmp
#endif

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s [options] input.ce [more.ce ...]\n", prog);
  fprintf(stderr, "       %s [options] project.ceproj\n", prog);
  fprintf(stderr, "       %s new <template> [name]\n", prog);
  fprintf(stderr, "Options:\n");
  fprintf(stderr,
          "  -o <file>         Output executable path (default a.exe)\n");
  fprintf(stderr, "  -S                Emit pseudo-asm alongside exe (.S)\n");
  fprintf(stderr,
          "  -Sccb             Stop after emitting Chance bytecode (.ccb)\n");
  fprintf(stderr,
          "  -O0|-O1|-O2|-O3   Select optimization level (default -O0)\n");
  fprintf(stderr, "  -c [obj] | --no-link [obj]\n");
  fprintf(stderr, "                    Compile only; do not link (emit "
                  "object). Optional obj output path.\n");
  fprintf(stderr, "  --freestanding    Freestanding mode (no default libs)\n");
  fprintf(stderr, "  -m32|-m64         Target bitness (currently -m64 only)\n");
  fprintf(stderr, "  -x86              Select x86-64 backend for "
                  "assembly/object/executable output\n");
  fprintf(stderr, "  --target-os <os>  Backend target OS: windows|linux\n");
  fprintf(
      stderr,
      "  --asm-syntax <s>  Assembly syntax: intel|att|nasm (default intel)\n");
  fprintf(stderr, "  --chancecodec <path>\n");
  fprintf(stderr, "                    Override ChanceCode CLI executable path "
                  "(default: auto-detect or PATH)\n");
  fprintf(stderr,
          "  -I <dir>          Add include search directory for #include <>\n");
  fprintf(stderr, "  -Nno-formatting  Disable formatting guidance notes\n");
  fprintf(stderr,
    "  -v, --verbose    Enable verbose diagnostics and progress\n");
  fprintf(stderr,
    "  -vd             Verbose + optimizer deep-dive visuals\n");
  fprintf(stderr,
    "  --no-ansi        Disable colored diagnostics (verbose too)\n");
  fprintf(stderr, "  --library         Emit a .cclib library instead of "
      "compiling/linking\n");
}

static char *read_all(const char *path, int *out_len) {
  FILE *f = fopen(path, "rb");
  if (!f) {
    perror("fopen");
    return NULL;
  }
  fseek(f, 0, SEEK_END);
  long sz = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *buf = (char *)xmalloc((size_t)sz + 1);
  if (fread(buf, 1, sz, f) != (size_t)sz) {
    perror("fread");
    fclose(f);
    free(buf);
    return NULL;
  }
  fclose(f);
  buf[sz] = '\0';
  if (out_len)
    *out_len = (int)sz;
  return buf;
}

static int ends_with_icase(const char *s, const char *suf) {
  if (!s || !suf)
    return 0;
  size_t ls = strlen(s), lu = strlen(suf);
  if (lu > ls)
    return 0;
  const char *p = s + (ls - lu);
#if defined(_WIN32)
  return _stricmp(p, suf) == 0;
#else
  for (size_t i = 0; i < lu; ++i) {
    char a = p[i], b = suf[i];
    if (a >= 'A' && a <= 'Z')
      a = (char)(a - 'A' + 'a');
    if (b >= 'A' && b <= 'Z')
      b = (char)(b - 'A' + 'a');
    if (a != b)
      return 0;
  }
  return 1;
#endif
}

static void split_path(const char *path, char *dir, size_t dsz,
                       char *base_noext, size_t bsz) {
  if (dir && dsz)
    dir[0] = '\0';
  if (base_noext && bsz)
    base_noext[0] = '\0';
  if (!path)
    return;
  const char *slash1 = strrchr(path, '/');
  const char *slash2 = strrchr(path, '\\');
  const char *base = path;
  const char *dirend = path;
  if (slash1 && slash2) {
    const char *mx = (slash1 > slash2 ? slash1 : slash2);
    base = mx + 1;
    dirend = mx;
  } else if (slash1) {
    base = slash1 + 1;
    dirend = slash1;
  } else if (slash2) {
    base = slash2 + 1;
    dirend = slash2;
  }
  if (dir && dsz) {
    size_t n = (size_t)(dirend - path);
    if (n >= dsz)
      n = dsz - 1;
    memcpy(dir, path, n);
    dir[n] = '\0';
  }
  const char *dot = strrchr(base, '.');
  if (base_noext && bsz) {
    size_t n = dot ? (size_t)(dot - base) : strlen(base);
    if (n >= bsz)
      n = bsz - 1;
    memcpy(base_noext, base, n);
    base_noext[n] = '\0';
  }
}

static void build_path_with_ext(const char *dir, const char *base,
                                const char *ext, char *buffer, size_t bufsz) {
#ifdef _WIN32
  const char sep = '\\';
#else
  const char sep = '/';
#endif
  if (!buffer || bufsz == 0)
    return;
  if (!ext)
    ext = "";
  if (dir && dir[0])
    snprintf(buffer, bufsz, "%s%c%s%s", dir, sep, base, ext);
  else
    snprintf(buffer, bufsz, "%s%s", base, ext);
}

static int is_regular_file(const char *path) {
  if (!path || !*path)
    return 0;
  struct stat st;
  if (stat(path, &st) != 0)
    return 0;
#if defined(S_ISREG)
  return S_ISREG(st.st_mode) != 0;
#else
  return (st.st_mode & _S_IFREG) != 0;
#endif
}

static int parent_directory(const char *path, char *out, size_t outsz) {
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  if (!path || !*path)
    return 1;
  size_t len = strlen(path);
  if (len == 0)
    return 1;
  while (len > 0 && (path[len - 1] == '/' || path[len - 1] == '\\'))
    len--;
  while (len > 0 && path[len - 1] != '/' && path[len - 1] != '\\')
    len--;
  while (len > 0 && (path[len - 1] == '/' || path[len - 1] == '\\'))
    len--;
  if (len == 0) {
#ifdef _WIN32
    if (path[0] && path[1] == ':') {
      out[0] = path[0];
      out[1] = ':';
      out[2] = '\0';
      return 0;
    }
#endif
    return 1;
  }
  if (len >= outsz)
    len = outsz - 1;
  memcpy(out, path, len);
  out[len] = '\0';
  return 0;
}

static void strip_wrapping_quotes(const char *in, char *out, size_t outsz) {
  if (!out || outsz == 0)
    return;
  out[0] = '\0';
  if (!in)
    return;
  while (*in == ' ' || *in == '\t')
    ++in;
  size_t len = strlen(in);
  while (len > 0 && (in[len - 1] == ' ' || in[len - 1] == '\t' ||
                     in[len - 1] == '\n' || in[len - 1] == '\r'))
    --len;
  if (len >= 2) {
    if ((in[0] == '"' && in[len - 1] == '"') ||
        (in[0] == '\'' && in[len - 1] == '\'')) {
      ++in;
      len -= 2;
    }
  }
  if (len >= outsz)
    len = outsz - 1;
  memcpy(out, in, len);
  out[len] = '\0';
}

static int get_executable_dir(char *dir, size_t dirsz, const char *argv0) {
  if (!dir || dirsz == 0)
    return 1;
  dir[0] = '\0';
#ifdef _WIN32
  char resolved[1024];
  if (_fullpath(resolved, argv0 ? argv0 : "", sizeof(resolved)) == NULL)
    return 1;
#else
  char resolved[PATH_MAX];
  if (!argv0)
    argv0 = "";
  if (!realpath(argv0, resolved)) {
#if defined(__linux__)
    ssize_t len = readlink("/proc/self/exe", resolved, sizeof(resolved) - 1);
    if (len <= 0)
      return 1;
    resolved[len] = '\0';
#else
    return 1;
#endif
  }
#endif
  split_path(resolved, dir, dirsz, NULL, 0);
  return dir[0] ? 0 : 1;
}

static int locate_chancecodec(char *out, size_t outsz, const char *exe_dir) {
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  const char *env_cmd = getenv("CHANCECODEC_CMD");
  if (!env_cmd || !*env_cmd)
    env_cmd = getenv("CHANCECODEC");
  if (env_cmd && *env_cmd) {
    strip_wrapping_quotes(env_cmd, out, outsz);
    if (out[0])
      return 0;
  }
  const char *env_home = getenv("CHANCECODE_HOME");
  if (env_home && *env_home) {
    char home_build[1024];
    snprintf(home_build, sizeof(home_build), "%s%cbuild", env_home,
             CHANCE_PATH_SEP);
    build_path_with_ext(home_build, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                        outsz);
    if (is_regular_file(out))
      return 0;
    build_path_with_ext(env_home, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                        outsz);
    if (is_regular_file(out))
      return 0;
  }
  if (exe_dir && *exe_dir) {
    build_path_with_ext(exe_dir, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0]) {
      char sibling[1024];
      snprintf(sibling, sizeof(sibling), "%s%cChanceCode", parent,
               CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                          outsz);
      if (is_regular_file(out))
        return 0;
      snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", parent,
               CHANCE_PATH_SEP, CHANCE_PATH_SEP);
      build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                          outsz);
      if (is_regular_file(out))
        return 0;
      char grandparent[1024];
      if (parent_directory(parent, grandparent, sizeof(grandparent)) == 0 &&
          grandparent[0]) {
        snprintf(sibling, sizeof(sibling), "%s%cChanceCode", grandparent,
                 CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                            outsz);
        if (is_regular_file(out))
          return 0;
        snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", grandparent,
                 CHANCE_PATH_SEP, CHANCE_PATH_SEP);
        build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out,
                            outsz);
        if (is_regular_file(out))
          return 0;
      }
    }
  }
  out[0] = '\0';
  return 1;
}

static const char *target_os_to_option(TargetOS os) {
  switch (os) {
  case OS_WINDOWS:
    return "windows";
  case OS_LINUX:
    return "linux";
  default:
    return NULL;
  }
}

static int equals_icase(const char *a, const char *b) {
  if (!a || !b)
    return 0;
  while (*a && *b) {
    unsigned char ca = (unsigned char)*a;
    unsigned char cb = (unsigned char)*b;
    if (tolower(ca) != tolower(cb))
      return 0;
    ++a;
    ++b;
  }
  return *a == '\0' && *b == '\0';
}

static void trim_whitespace_inplace(char *text) {
  if (!text)
    return;
  char *start = text;
  while (*start && isspace((unsigned char)*start))
    ++start;
  char *end = start + strlen(start);
  while (end > start && isspace((unsigned char)*(end - 1)))
    --end;
  size_t len = (size_t)(end - start);
  if (start != text && len > 0)
    memmove(text, start, len);
  else if (start != text && len == 0)
    memmove(text, start, 1);
  text[len] = '\0';
}

static int parse_bool_value(const char *text, int *out) {
  if (!text || !out)
    return -1;
  if (equals_icase(text, "true") || equals_icase(text, "1") ||
      equals_icase(text, "yes") || equals_icase(text, "on")) {
    *out = 1;
    return 0;
  }
  if (equals_icase(text, "false") || equals_icase(text, "0") ||
      equals_icase(text, "no") || equals_icase(text, "off")) {
    *out = 0;
    return 0;
  }
  return -1;
}

static int is_path_absolute_simple(const char *path) {
  if (!path || !*path)
    return 0;
#ifdef _WIN32
  if ((strlen(path) >= 2) && path[1] == ':' &&
      ((path[0] >= 'A' && path[0] <= 'Z') ||
       (path[0] >= 'a' && path[0] <= 'z')))
    return 1;
  if (path[0] == '\\' && path[1] == '\\')
    return 1;
#endif
  return path[0] == '/';
}

static void resolve_project_relative_path(char *dst, size_t dstsz,
                                          const char *base_dir,
                                          const char *rel) {
  if (!dst || dstsz == 0)
    return;
  dst[0] = '\0';
  if (!rel)
    return;
  if (is_path_absolute_simple(rel) || !base_dir || !*base_dir) {
    snprintf(dst, dstsz, "%s", rel);
    return;
  }
  size_t base_len = strlen(base_dir);
  int needs_sep = 1;
  if (base_len > 0) {
    char last = base_dir[base_len - 1];
    if (last == '/' || last == '\\')
      needs_sep = 0;
  }
  if (needs_sep)
    snprintf(dst, dstsz, "%s%c%s", base_dir, CHANCE_PATH_SEP, rel);
  else
    snprintf(dst, dstsz, "%s%s", base_dir, rel);
}

typedef struct {
  const char ***items;
  int *count;
  int *cap;
  char ***owned_items;
  int *owned_count;
  int *owned_cap;
} ProjectInputList;

static int push_input_entry(const char *value, ProjectInputList list,
                            int make_copy) {
  if (!value || !list.items || !list.count || !list.cap || !list.owned_items ||
      !list.owned_count || !list.owned_cap)
    return -1;
  const char **arr = *list.items;
  if (*list.count == *list.cap) {
    int new_cap = *list.cap ? (*list.cap * 2) : 8;
    const char **new_arr =
        (const char **)realloc((void *)arr, sizeof(char *) * (size_t)new_cap);
    if (!new_arr) {
      fprintf(stderr,
              "error: out of memory while growing project input list\n");
      return -1;
    }
    *list.items = new_arr;
    arr = new_arr;
    *list.cap = new_cap;
  }
  const char *stored = value;
  if (make_copy) {
    char *dup = xstrdup(value);
    char **owned = *list.owned_items;
    if (*list.owned_count == *list.owned_cap) {
      int new_cap = *list.owned_cap ? (*list.owned_cap * 2) : 8;
      char **new_owned =
          (char **)realloc(owned, sizeof(char *) * (size_t)new_cap);
      if (!new_owned) {
        fprintf(stderr,
                "error: out of memory while growing project-owned strings\n");
        free(dup);
        return -1;
      }
      *list.owned_items = new_owned;
      owned = new_owned;
      *list.owned_cap = new_cap;
    }
    owned[*list.owned_count] = dup;
    (*list.owned_count)++;
    stored = dup;
  }
  arr[*list.count] = stored;
  (*list.count)++;
  return 0;
}

static int add_file_list_entries(const char *value, const char *project_dir,
                                 ProjectInputList list) {
  if (!value)
    return 0;
  const char *cursor = value;
  while (*cursor) {
    while (*cursor == ';' || *cursor == ',' || isspace((unsigned char)*cursor))
      ++cursor;
    if (!*cursor)
      break;
    const char *start = cursor;
    while (*cursor && *cursor != ';' && *cursor != ',')
      ++cursor;
    const char *end = cursor;
    while (end > start && isspace((unsigned char)*(end - 1)))
      --end;
    while (start < end && isspace((unsigned char)*start))
      ++start;
    if (end > start) {
      size_t len = (size_t)(end - start);
      char token[1024];
      if (len >= sizeof(token))
        len = sizeof(token) - 1;
      memcpy(token, start, len);
      token[len] = '\0';
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    token);
      if (push_input_entry(resolved, list, 1) != 0)
        return -1;
    }
    if (*cursor == ';' || *cursor == ',')
      ++cursor;
  }
  return 0;
}

static int add_include_dir_list(const char *value, const char *project_dir,
                                char ***include_dirs, int *include_dir_count) {
  if (!value || !include_dirs || !include_dir_count)
    return 0;
  const char *cursor = value;
  while (*cursor) {
    while (*cursor == ';' || *cursor == ',' || isspace((unsigned char)*cursor))
      ++cursor;
    if (!*cursor)
      break;
    const char *start = cursor;
    while (*cursor && *cursor != ';' && *cursor != ',')
      ++cursor;
    const char *end = cursor;
    while (end > start && isspace((unsigned char)*(end - 1)))
      --end;
    while (start < end && isspace((unsigned char)*start))
      ++start;
    if (end > start) {
      size_t len = (size_t)(end - start);
      char token[1024];
      if (len >= sizeof(token))
        len = sizeof(token) - 1;
      memcpy(token, start, len);
      token[len] = '\0';
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    token);
      chance_add_include_dir(include_dirs, include_dir_count, resolved);
    }
    if (*cursor == ';' || *cursor == ',')
      ++cursor;
  }
  return 0;
}

static void strip_utf8_bom(char *text) {
  if (!text)
    return;
  unsigned char *u = (unsigned char *)text;
  if (u[0] == 0xEF && u[1] == 0xBB && u[2] == 0xBF)
    memmove(text, text + 3, strlen(text + 3) + 1);
}

static int parse_ceproj_file(
    const char *proj_path, ProjectInputList ce_list, ProjectInputList ccb_list,
    ProjectInputList cclib_list, ProjectInputList obj_list,
    char ***include_dirs, int *include_dir_count, int *output_overridden,
    const char **out, char **project_output_alloc, TargetArch *target_arch,
    const char **chancecode_backend, int *stop_after_ccb, int *stop_after_asm,
    int *emit_library, int *no_link, int *freestanding, TargetOS *target_os) {
  FILE *f = fopen(proj_path, "rb");
  if (!f) {
    fprintf(stderr, "error: failed to open project file '%s': %s\n", proj_path,
            strerror(errno));
    return -1;
  }
  char project_dir[1024] = {0};
  if (parent_directory(proj_path, project_dir, sizeof(project_dir)) != 0)
    project_dir[0] = '\0';

  char line[2048];
  int lineno = 0;
  int rc = 0;
  while (fgets(line, sizeof(line), f)) {
    lineno++;
    char work[2048];
    snprintf(work, sizeof(work), "%s", line);
    trim_whitespace_inplace(work);
    if (lineno == 1)
      strip_utf8_bom(work);
    if (!work[0])
      continue;
    if (work[0] == '#' || work[0] == ';')
      continue;
    if (work[0] == '/' && work[1] == '/')
      continue;
    if (work[0] == '[')
      continue;
    char *eq = strchr(work, '=');
    if (!eq) {
      fprintf(stderr, "error: expected key=value in '%s' (line %d)\n",
              proj_path, lineno);
      rc = -1;
      break;
    }
    *eq = '\0';
    char *key = work;
    char *value = eq + 1;
    trim_whitespace_inplace(key);
    trim_whitespace_inplace(value);
    if (!key[0])
      continue;
    for (char *p = key; *p; ++p)
      *p = (char)tolower((unsigned char)*p);
    char value_buf[2048];
    strip_wrapping_quotes(value, value_buf, sizeof(value_buf));
    trim_whitespace_inplace(value_buf);
    if (strcmp(key, "ce") == 0 || strcmp(key, "source") == 0) {
      if (add_file_list_entries(value_buf, project_dir, ce_list) != 0) {
        rc = -1;
        break;
      }
    } else if (strcmp(key, "ccb") == 0) {
      if (add_file_list_entries(value_buf, project_dir, ccb_list) != 0) {
        rc = -1;
        break;
      }
    } else if (strcmp(key, "cclib") == 0 || strcmp(key, "library") == 0) {
      if (add_file_list_entries(value_buf, project_dir, cclib_list) != 0) {
        rc = -1;
        break;
      }
    } else if (strcmp(key, "obj") == 0 || strcmp(key, "object") == 0) {
      if (add_file_list_entries(value_buf, project_dir, obj_list) != 0) {
        rc = -1;
        break;
      }
    } else if (strcmp(key, "include") == 0 || strcmp(key, "includedir") == 0) {
      if (add_include_dir_list(value_buf, project_dir, include_dirs,
                               include_dir_count) != 0) {
        rc = -1;
        break;
      }
    } else if (strcmp(key, "output") == 0) {
      if (!out || !project_output_alloc || !output_overridden)
        continue;
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    value_buf);
      if (*project_output_alloc) {
        free(*project_output_alloc);
        *project_output_alloc = NULL;
      }
      *project_output_alloc = xstrdup(resolved);
      *out = *project_output_alloc;
      *output_overridden = 1;
    } else if (strcmp(key, "backend") == 0) {
      if (equals_icase(value_buf, "x86")) {
        if (target_arch)
          *target_arch = ARCH_X86;
        if (chancecode_backend)
          *chancecode_backend = "x86-gas";
      } else if (equals_icase(value_buf, "none")) {
        if (target_arch)
          *target_arch = ARCH_NONE;
        if (chancecode_backend)
          *chancecode_backend = NULL;
      } else {
        fprintf(stderr, "error: unknown backend '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    } else if (strcmp(key, "type") == 0) {
      if (equals_icase(value_buf, "library")) {
        if (emit_library)
          *emit_library = 1;
      }
    } else if (strcmp(key, "stop_after") == 0 ||
               strcmp(key, "stopafter") == 0) {
      if ((!stop_after_asm || !*stop_after_asm) &&
          (!stop_after_ccb || !*stop_after_ccb)) {
        if (equals_icase(value_buf, "ccb")) {
          if (stop_after_ccb)
            *stop_after_ccb = 1;
        } else if (equals_icase(value_buf, "asm")) {
          if (stop_after_asm)
            *stop_after_asm = 1;
        }
      }
    } else if (strcmp(key, "no_link") == 0 || strcmp(key, "nolink") == 0) {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0) {
        fprintf(stderr,
                "error: expected boolean for no_link in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (no_link)
        *no_link = val;
    } else if (strcmp(key, "freestanding") == 0) {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0) {
        fprintf(stderr,
                "error: expected boolean for freestanding in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (freestanding)
        *freestanding = val;
    } else if (strcmp(key, "target_os") == 0 || strcmp(key, "targetos") == 0) {
      if (equals_icase(value_buf, "windows")) {
        if (target_os)
          *target_os = OS_WINDOWS;
      } else if (equals_icase(value_buf, "linux")) {
        if (target_os)
          *target_os = OS_LINUX;
      } else {
        fprintf(stderr, "error: unknown target_os '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    } else {
      fprintf(stderr,
              "warning: ignoring unrecognized key '%s' in '%s' (line %d)\n",
              key, proj_path, lineno);
    }
  }
  fclose(f);
  return rc;
}

static int path_has_separator(const char *name) {
  if (!name)
    return 0;
  for (const char *p = name; *p; ++p) {
    if (*p == '/' || *p == '\\')
      return 1;
  }
  return 0;
}

static int create_directory_checked(const char *path, int fail_if_exists) {
  if (!path || !*path)
    return -1;
  struct stat st;
  if (stat(path, &st) == 0) {
#if defined(S_ISDIR)
    int is_dir = S_ISDIR(st.st_mode) != 0;
#else
    int is_dir = (st.st_mode & _S_IFDIR) != 0;
#endif
    if (!is_dir) {
      fprintf(stderr,
              "error: path '%s' already exists and is not a directory\n", path);
      return -1;
    }
    if (fail_if_exists) {
      fprintf(stderr, "error: directory '%s' already exists\n", path);
      return -1;
    }
    return 0;
  }
#ifdef _WIN32
  if (_mkdir(path) != 0)
#else
  if (mkdir(path, 0777) != 0)
#endif
  {
    if (!fail_if_exists && errno == EEXIST)
      return 0;
    fprintf(stderr, "error: failed to create directory '%s': %s\n", path,
            strerror(errno));
    return -1;
  }
  return 0;
}

static int write_text_file(const char *path, const char *content) {
  if (!path || !content)
    return -1;
  FILE *f = fopen(path, "wb");
  if (!f) {
    fprintf(stderr, "error: failed to write '%s': %s\n", path, strerror(errno));
    return -1;
  }
  size_t len = strlen(content);
  if (fwrite(content, 1, len, f) != len) {
    fprintf(stderr, "error: failed to write '%s': %s\n", path, strerror(errno));
    fclose(f);
    return -1;
  }
  fclose(f);
  return 0;
}

static int handle_new_command(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: chancec new <Template> [ProjectName]\n");
    return 2;
  }
  if (!equals_icase(argv[0], "new")) {
    fprintf(stderr, "error: internal new-command usage mismatch\n");
    return 2;
  }
  const char *template_name = argv[1];
  if (equals_icase(template_name, "--help") ||
      equals_icase(template_name, "-h")) {
    fprintf(stderr, "Usage: chancec new Console [ProjectName]\n");
    fprintf(stderr, "Creates a new Chance project in a directory matching the "
                    "project name.\n");
    return 0;
  }
  if (!equals_icase(template_name, "console")) {
    fprintf(stderr,
            "error: unknown template '%s'. Supported templates: Console\n",
            template_name);
    return 2;
  }
  if (argc > 3) {
    fprintf(stderr, "error: too many arguments for 'chancec new'. Usage: "
                    "chancec new Console [ProjectName]\n");
    return 2;
  }
  const char *project_name =
      (argc >= 3 && argv[2] && argv[2][0]) ? argv[2] : "ConsoleApp";
  if (strcmp(project_name, ".") == 0 || strcmp(project_name, "..") == 0) {
    fprintf(stderr, "error: project name '%s' is not allowed\n", project_name);
    return 2;
  }
  if (path_has_separator(project_name)) {
    fprintf(stderr,
            "error: project name '%s' must not contain path separators\n",
            project_name);
    return 2;
  }
  char project_dir[1024];
  snprintf(project_dir, sizeof(project_dir), "%s", project_name);
  if (create_directory_checked(project_dir, 1) != 0)
    return 1;

  char src_dir[1024];
  snprintf(src_dir, sizeof(src_dir), "%s%csrc", project_dir, CHANCE_PATH_SEP);
  if (create_directory_checked(src_dir, 0) != 0)
    return 1;

  char project_base[256];
  split_path(project_name, NULL, 0, project_base, sizeof(project_base));
  if (!project_base[0])
    snprintf(project_base, sizeof(project_base), "%s", project_name);

#ifdef _WIN32
  char output_name[256];
  snprintf(output_name, sizeof(output_name), "%s.exe", project_base);
#else
  char output_name[256];
  snprintf(output_name, sizeof(output_name), "%s", project_base);
#endif

  char ceproj_path[1024];
  snprintf(ceproj_path, sizeof(ceproj_path), "%s%c%s.ceproj", project_dir,
           CHANCE_PATH_SEP, project_base);
  char main_ce_path[1024];
  snprintf(main_ce_path, sizeof(main_ce_path), "%s%cmain.ce", src_dir,
           CHANCE_PATH_SEP);

  char ceproj_content[1024];
  snprintf(ceproj_content, sizeof(ceproj_content),
           "# Chance project definition\n"
           "name=%s\n"
           "type=Console\n"
           "backend=x86\n"
           "output=%s\n"
           "ce=src/main.ce\n",
           project_base, output_name);

  char main_ce_content[1024];
  snprintf(main_ce_content, sizeof(main_ce_content),
           "module %s;\n\n"
           "extend from \"C\" i32 printf(char *, _vaargs_);\n\n"
           "[EntryPoint]\n"
           "expose fun main() -> i32\n"
           "{\n"
           "    printf(\"Hello from %s!\\n\");\n"
           "    ret 0;\n"
           "}\n",
           project_base, project_base);

  if (write_text_file(ceproj_path, ceproj_content) != 0)
    return 1;
  if (write_text_file(main_ce_path, main_ce_content) != 0)
    return 1;

  printf("Created Chance console project '%s' in %s\n", project_base,
         project_dir);
  printf("Next steps:\n  1. cd %s\n  2. chancec %s.ceproj\n", project_dir,
         project_base);
  return 0;
}

typedef struct LibraryFunction {
  char *name;
  char *backend_name;
  char *return_spec;
  char **param_specs;
  int param_count;
  int is_varargs;
  int is_noreturn;
  int is_exposed;
} LibraryFunction;

typedef struct LibraryGlobal {
  char *name;
  char *type_spec;
  int is_const;
} LibraryGlobal;

typedef struct LibraryStruct {
  char *name;
  char **field_names;
  char **field_specs;
  uint32_t *field_offsets;
  uint32_t field_count;
  uint32_t size_bytes;
  int is_exposed;
} LibraryStruct;

typedef struct LibraryEnum {
  char *name;
  char **value_names;
  int32_t *values;
  uint32_t value_count;
  int is_exposed;
} LibraryEnum;

typedef struct LibraryModuleData {
  char *module_name;
  LibraryFunction *functions;
  int function_count;
  int function_cap;
  LibraryGlobal *globals;
  int global_count;
  int global_cap;
  LibraryStruct *structs;
  int struct_count;
  int struct_cap;
  LibraryEnum *enums;
  int enum_count;
  int enum_cap;
  char *ccb_path;
  char *ccbin_path;
  uint8_t *ccbin_data;
  size_t ccbin_size;
} LibraryModuleData;

typedef struct LoadedLibraryFunction {
  char *name;
  char *backend_name;
  char *qualified_name;
  char *module_name;
  Type *return_type;
  Type **param_types;
  int param_count;
  int is_varargs;
  int is_noreturn;
  int is_exposed;
} LoadedLibraryFunction;

typedef struct LoadedLibrary {
  char *path;
  CclibFile file;
  Type **allocated_types;
  int allocated_type_count;
  int allocated_type_cap;
  char **ccbin_temp_paths;
} LoadedLibrary;

static char *dup_string_or_null(const char *s) {
  if (!s)
    return NULL;
  return xstrdup(s);
}

static char *format_string(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int needed = vsnprintf(NULL, 0, fmt, ap);
  va_end(ap);
  if (needed < 0)
    return NULL;
  size_t len = (size_t)needed;
  char *buffer = (char *)xmalloc(len + 1);
  va_start(ap, fmt);
  vsnprintf(buffer, len + 1, fmt, ap);
  va_end(ap);
  return buffer;
}

static char *module_name_to_prefix_copy(const char *module_full) {
  if (!module_full || !*module_full)
    return NULL;
  size_t len = strlen(module_full);
  char *copy = (char *)xmalloc(len + 1);
  memcpy(copy, module_full, len + 1);
  for (size_t i = 0; i < len; ++i) {
    if (copy[i] == '.')
      copy[i] = '_';
  }
  return copy;
}

static char *make_backend_name_from_module(const char *module_full,
                                           const char *fn_name) {
  if (!module_full || !fn_name)
    return NULL;
  char *prefix = module_name_to_prefix_copy(module_full);
  if (!prefix)
    return NULL;
  char *result = format_string("%s_%s", prefix, fn_name);
  free(prefix);
  return result;
}

static char *make_qualified_function_name(const char *module_full,
                                          const char *fn_name) {
  if (!module_full || !fn_name)
    return NULL;
  return format_string("%s.%s", module_full, fn_name);
}

static int read_file_bytes(const char *path, uint8_t **out_data,
                           size_t *out_size) {
  if (!path || !out_data || !out_size)
    return 1;
  *out_data = NULL;
  *out_size = 0;
  FILE *f = fopen(path, "rb");
  if (!f)
    return errno ? errno : EIO;
  if (fseek(f, 0, SEEK_END) != 0) {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  long sz = ftell(f);
  if (sz < 0) {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  if (fseek(f, 0, SEEK_SET) != 0) {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  uint8_t *buf = (uint8_t *)malloc((size_t)sz);
  if (!buf) {
    fclose(f);
    return ENOMEM;
  }
  if (sz > 0) {
    size_t read = fread(buf, 1, (size_t)sz, f);
    if (read != (size_t)sz) {
      int err = ferror(f) ? errno : EIO;
      free(buf);
      fclose(f);
      return err ? err : EIO;
    }
  }
  fclose(f);
  *out_data = buf;
  *out_size = (size_t)sz;
  return 0;
}

static int write_file_bytes(const char *path, const uint8_t *data,
                            size_t size) {
  if (!path)
    return EINVAL;
  FILE *f = fopen(path, "wb");
  if (!f)
    return errno ? errno : EIO;
  if (size > 0 && data) {
    size_t written = fwrite(data, 1, size, f);
    if (written != size) {
      int err = ferror(f) ? errno : EIO;
      fclose(f);
      remove(path);
      return err ? err : EIO;
    }
  }
  if (fclose(f) != 0) {
    int err = errno ? errno : EIO;
    remove(path);
    return err;
  }
  return 0;
}

static char *type_to_spec(Type *ty);
static Type *spec_to_type(const char *spec);

static Type builtin_ty_i8 = {.kind = TY_I8};
static Type builtin_ty_u8 = {.kind = TY_U8};
static Type builtin_ty_i16 = {.kind = TY_I16};
static Type builtin_ty_u16 = {.kind = TY_U16};
static Type builtin_ty_i32 = {.kind = TY_I32};
static Type builtin_ty_u32 = {.kind = TY_U32};
static Type builtin_ty_i64 = {.kind = TY_I64};
static Type builtin_ty_u64 = {.kind = TY_U64};
static Type builtin_ty_f32 = {.kind = TY_F32};
static Type builtin_ty_f64 = {.kind = TY_F64};
static Type builtin_ty_f128 = {.kind = TY_F128};
static Type builtin_ty_void = {.kind = TY_VOID};
static Type builtin_ty_char = {.kind = TY_CHAR};
static Type builtin_ty_bool = {.kind = TY_BOOL};

static char *type_to_spec(Type *ty) {
  if (!ty)
    return xstrdup("void");

  ty = module_registry_canonical_type(ty);
  if (!ty)
    return xstrdup("void");

  switch (ty->kind) {
  case TY_I8:
    return xstrdup("i8");
  case TY_U8:
    return xstrdup("u8");
  case TY_I16:
    return xstrdup("i16");
  case TY_U16:
    return xstrdup("u16");
  case TY_I32:
    return xstrdup("i32");
  case TY_U32:
    return xstrdup("u32");
  case TY_I64:
    return xstrdup("i64");
  case TY_U64:
    return xstrdup("u64");
  case TY_F32:
    return xstrdup("f32");
  case TY_F64:
    return xstrdup("f64");
  case TY_F128:
    return xstrdup("f128");
  case TY_VOID:
    return xstrdup("void");
  case TY_CHAR:
    return xstrdup("char");
  case TY_BOOL:
    return xstrdup("bool");
  case TY_PTR: {
    char *inner = type_to_spec(ty->pointee);
    char *res = format_string("ptr(%s)", inner ? inner : "void");
    free(inner);
    return res;
  }
  case TY_STRUCT: {
    const char *module = module_registry_find_struct_module(ty);
    const char *name = ty->struct_name ? ty->struct_name : "";
    if (!module)
      module = "";
    return format_string("struct(%s,%s)", module, name);
  }
  case TY_IMPORT: {
    const char *module = ty->import_module ? ty->import_module : "";
    const char *name = ty->import_type_name ? ty->import_type_name : "";
    return format_string("import(%s,%s)", module, name);
  }
  default:
    break;
  }
  return xstrdup("void");
}

static Type *spec_to_type(const char *spec) {
  if (!spec || !*spec)
    return &builtin_ty_void;

  if (strcmp(spec, "i8") == 0)
    return &builtin_ty_i8;
  if (strcmp(spec, "u8") == 0)
    return &builtin_ty_u8;
  if (strcmp(spec, "i16") == 0)
    return &builtin_ty_i16;
  if (strcmp(spec, "u16") == 0)
    return &builtin_ty_u16;
  if (strcmp(spec, "i32") == 0)
    return &builtin_ty_i32;
  if (strcmp(spec, "u32") == 0)
    return &builtin_ty_u32;
  if (strcmp(spec, "i64") == 0)
    return &builtin_ty_i64;
  if (strcmp(spec, "u64") == 0)
    return &builtin_ty_u64;
  if (strcmp(spec, "f32") == 0)
    return &builtin_ty_f32;
  if (strcmp(spec, "f64") == 0)
    return &builtin_ty_f64;
  if (strcmp(spec, "f128") == 0)
    return &builtin_ty_f128;
  if (strcmp(spec, "void") == 0)
    return &builtin_ty_void;
  if (strcmp(spec, "char") == 0)
    return &builtin_ty_char;
  if (strcmp(spec, "bool") == 0)
    return &builtin_ty_bool;

  if (strncmp(spec, "ptr(", 4) == 0) {
    size_t len = strlen(spec);
    if (len < 6 || spec[len - 1] != ')')
      return &builtin_ty_void;
    char *inner = (char *)xmalloc(len - 4);
    memcpy(inner, spec + 4, len - 5);
    inner[len - 5] = '\0';
    Type *inner_type = spec_to_type(inner);
    free(inner);
    return type_ptr(inner_type);
  }

  if (strncmp(spec, "struct(", 7) == 0 || strncmp(spec, "import(", 7) == 0) {
    int is_import = (spec[0] == 'i');
    const char *open = strchr(spec, '(');
    const char *comma = open ? strchr(open + 1, ',') : NULL;
    const char *close = comma ? strrchr(comma, ')') : NULL;
    if (!open || !comma || !close || close <= comma)
      return &builtin_ty_void;
    size_t module_len = (size_t)(comma - (open + 1));
    size_t name_len = (size_t)(close - (comma + 1));
    char *module = (char *)xmalloc(module_len + 1);
    memcpy(module, open + 1, module_len);
    module[module_len] = '\0';
    char *name = (char *)xmalloc(name_len + 1);
    memcpy(name, comma + 1, name_len);
    name[name_len] = '\0';
    Type *resolved = NULL;
    if (!is_import)
      resolved = module_registry_lookup_struct(module, name);
    if (resolved) {
      free(module);
      free(name);
      return resolved;
    }
    Type *imp = (Type *)xcalloc(1, sizeof(Type));
    imp->kind = TY_IMPORT;
    imp->import_module = module;
    imp->import_type_name = name;
    return imp;
  }

  return &builtin_ty_void;
}

static LibraryModuleData *find_or_add_module(LibraryModuleData **modules,
                                             int *module_count, int *module_cap,
                                             const char *module_name) {
  if (!module_name || !*module_name)
    return NULL;
  for (int i = 0; i < *module_count; ++i) {
    if (strcmp((*modules)[i].module_name, module_name) == 0)
      return &(*modules)[i];
  }
  if (*module_count == *module_cap) {
    int new_cap = *module_cap ? *module_cap * 2 : 4;
    LibraryModuleData *grown = (LibraryModuleData *)realloc(
        *modules, (size_t)new_cap * sizeof(LibraryModuleData));
    if (!grown)
      return NULL;
    memset(grown + *module_cap, 0,
           (size_t)(new_cap - *module_cap) * sizeof(LibraryModuleData));
    *modules = grown;
    *module_cap = new_cap;
  }
  LibraryModuleData *mod = &(*modules)[(*module_count)++];
  memset(mod, 0, sizeof(*mod));
  mod->module_name = xstrdup(module_name);
  return mod;
}

static int append_library_function(LibraryModuleData *mod,
                                   const LibraryFunction *fn) {
  if (!mod || !fn)
    return 1;
  if (mod->function_count == mod->function_cap) {
    int new_cap = mod->function_cap ? mod->function_cap * 2 : 4;
    LibraryFunction *grown = (LibraryFunction *)realloc(
        mod->functions, (size_t)new_cap * sizeof(LibraryFunction));
    if (!grown)
      return 1;
    mod->functions = grown;
    mod->function_cap = new_cap;
  }
  mod->functions[mod->function_count++] = *fn;
  return 0;
}

static int append_library_global(LibraryModuleData *mod,
                                 const LibraryGlobal *gl) {
  if (!mod || !gl)
    return 1;
  if (mod->global_count == mod->global_cap) {
    int new_cap = mod->global_cap ? mod->global_cap * 2 : 4;
    LibraryGlobal *grown = (LibraryGlobal *)realloc(
        mod->globals, (size_t)new_cap * sizeof(LibraryGlobal));
    if (!grown)
      return 1;
    mod->globals = grown;
    mod->global_cap = new_cap;
  }
  mod->globals[mod->global_count++] = *gl;
  return 0;
}

static int append_library_struct(LibraryModuleData *mod,
                                 const LibraryStruct *st) {
  if (!mod || !st)
    return 1;
  if (mod->struct_count == mod->struct_cap) {
    int new_cap = mod->struct_cap ? mod->struct_cap * 2 : 4;
    LibraryStruct *grown = (LibraryStruct *)realloc(
        mod->structs, (size_t)new_cap * sizeof(LibraryStruct));
    if (!grown)
      return 1;
    mod->structs = grown;
    mod->struct_cap = new_cap;
  }
  mod->structs[mod->struct_count++] = *st;
  return 0;
}

static int append_library_enum(LibraryModuleData *mod, const LibraryEnum *en) {
  if (!mod || !en)
    return 1;
  if (mod->enum_count == mod->enum_cap) {
    int new_cap = mod->enum_cap ? mod->enum_cap * 2 : 4;
    LibraryEnum *grown = (LibraryEnum *)realloc(
        mod->enums, (size_t)new_cap * sizeof(LibraryEnum));
    if (!grown)
      return 1;
    mod->enums = grown;
    mod->enum_cap = new_cap;
  }
  mod->enums[mod->enum_count++] = *en;
  return 0;
}

static void free_library_function(LibraryFunction *fn) {
  if (!fn)
    return;
  free(fn->name);
  free(fn->backend_name);
  free(fn->return_spec);
  if (fn->param_specs) {
    for (int i = 0; i < fn->param_count; ++i)
      free(fn->param_specs[i]);
    free(fn->param_specs);
  }
  memset(fn, 0, sizeof(*fn));
}

static void free_library_global(LibraryGlobal *gl) {
  if (!gl)
    return;
  free(gl->name);
  free(gl->type_spec);
  memset(gl, 0, sizeof(*gl));
}

static void free_library_struct(LibraryStruct *st) {
  if (!st)
    return;
  free(st->name);
  if (st->field_names) {
    for (uint32_t i = 0; i < st->field_count; ++i)
      free(st->field_names[i]);
    free(st->field_names);
  }
  if (st->field_specs) {
    for (uint32_t i = 0; i < st->field_count; ++i)
      free(st->field_specs[i]);
    free(st->field_specs);
  }
  free(st->field_offsets);
  memset(st, 0, sizeof(*st));
}

static void free_library_enum(LibraryEnum *en) {
  if (!en)
    return;
  free(en->name);
  if (en->value_names) {
    for (uint32_t i = 0; i < en->value_count; ++i)
      free(en->value_names[i]);
    free(en->value_names);
  }
  free(en->values);
  memset(en, 0, sizeof(*en));
}

static void free_library_module(LibraryModuleData *mod) {
  if (!mod)
    return;
  free(mod->module_name);
  for (int i = 0; i < mod->function_count; ++i)
    free_library_function(&mod->functions[i]);
  free(mod->functions);
  for (int i = 0; i < mod->global_count; ++i)
    free_library_global(&mod->globals[i]);
  free(mod->globals);
  for (int i = 0; i < mod->struct_count; ++i)
    free_library_struct(&mod->structs[i]);
  free(mod->structs);
  for (int i = 0; i < mod->enum_count; ++i)
    free_library_enum(&mod->enums[i]);
  free(mod->enums);
  free(mod->ccb_path);
  free(mod->ccbin_path);
  free(mod->ccbin_data);
  memset(mod, 0, sizeof(*mod));
}

static void free_library_module_array(LibraryModuleData *mods, int count) {
  if (!mods)
    return;
  for (int i = 0; i < count; ++i)
    free_library_module(&mods[i]);
  free(mods);
}

static int library_module_has_function(const LibraryModuleData *mod,
                                       const char *name) {
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->function_count; ++i) {
    if (mod->functions[i].name && strcmp(mod->functions[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int library_module_has_struct(const LibraryModuleData *mod,
                                     const char *name) {
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->struct_count; ++i) {
    if (mod->structs[i].name && strcmp(mod->structs[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int library_module_has_enum(const LibraryModuleData *mod,
                                   const char *name) {
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->enum_count; ++i) {
    if (mod->enums[i].name && strcmp(mod->enums[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int collect_functions_from_unit(const Node *unit,
                                       LibraryModuleData *mod) {
  if (!unit || unit->kind != ND_UNIT || !mod)
    return 0;
  for (int i = 0; i < unit->stmt_count; ++i) {
    const Node *fn = unit->stmts[i];
    if (!fn || fn->kind != ND_FUNC)
      continue;
    if (!fn->is_exposed || !fn->name)
      continue;
    if (library_module_has_function(mod, fn->name))
      continue;
    LibraryFunction out = {0};
    out.name = xstrdup(fn->name);
    const char *backend =
        fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    out.backend_name = backend ? xstrdup(backend) : NULL;
    Type *ret_ty = fn->ret_type ? fn->ret_type : type_i32();
    out.return_spec = type_to_spec(ret_ty);
    out.param_count = fn->param_count;
    if (out.param_count > 0) {
      out.param_specs =
          (char **)xcalloc((size_t)out.param_count, sizeof(char *));
      if (!out.param_specs) {
        free_library_function(&out);
        return 1;
      }
      for (int pi = 0; pi < out.param_count; ++pi) {
        Type *pt = (fn->param_types && pi < fn->param_count)
                       ? fn->param_types[pi]
                       : NULL;
        out.param_specs[pi] = type_to_spec(pt);
        if (!out.param_specs[pi]) {
          free_library_function(&out);
          return 1;
        }
      }
    }
    out.is_varargs = fn->is_varargs ? 1 : 0;
    out.is_noreturn = fn->is_noreturn;
    out.is_exposed = fn->is_exposed;
    if (append_library_function(mod, &out) != 0) {
      free_library_function(&out);
      return 1;
    }
  }
  return 0;
}

static int collect_structs_for_module(const char *module_name,
                                      LibraryModuleData *mod) {
  if (!module_name || !mod)
    return 0;
  int total = module_registry_struct_entry_count();
  for (int i = 0; i < total; ++i) {
    const char *entry_module = module_registry_struct_entry_module(i);
    if (!entry_module || strcmp(entry_module, module_name) != 0)
      continue;
    const char *struct_name = module_registry_struct_entry_name(i);
    if (library_module_has_struct(mod, struct_name))
      continue;
    Type *ty = module_registry_struct_entry_type(i);
    if (!ty)
      continue;
    LibraryStruct st = {0};
    st.name = struct_name ? xstrdup(struct_name) : NULL;
    int field_count = ty->strct.field_count;
    if (field_count > 0) {
      st.field_names = (char **)xcalloc((size_t)field_count, sizeof(char *));
      st.field_specs = (char **)xcalloc((size_t)field_count, sizeof(char *));
      st.field_offsets =
          (uint32_t *)xcalloc((size_t)field_count, sizeof(uint32_t));
      if (!st.field_names || !st.field_specs || !st.field_offsets) {
        free_library_struct(&st);
        return 1;
      }
      for (int fi = 0; fi < field_count; ++fi) {
        const char *fname = (ty->strct.field_names && fi < field_count)
                                ? ty->strct.field_names[fi]
                                : NULL;
        st.field_names[fi] = fname ? xstrdup(fname) : NULL;
        Type *ftype = (ty->strct.field_types && fi < field_count)
                          ? ty->strct.field_types[fi]
                          : NULL;
        st.field_specs[fi] = type_to_spec(ftype);
        int offset = (ty->strct.field_offsets && fi < field_count)
                         ? ty->strct.field_offsets[fi]
                         : 0;
        st.field_offsets[fi] = offset < 0 ? 0u : (uint32_t)offset;
        if (!st.field_specs[fi]) {
          free_library_struct(&st);
          return 1;
        }
      }
    }
    st.field_count = (uint32_t)(field_count < 0 ? 0 : field_count);
    st.size_bytes =
        ty->strct.size_bytes < 0 ? 0u : (uint32_t)ty->strct.size_bytes;
    st.is_exposed = ty->is_exposed != 0;
    if (append_library_struct(mod, &st) != 0) {
      free_library_struct(&st);
      return 1;
    }
  }
  return 0;
}

static int collect_enums_for_module(const char *module_name,
                                    LibraryModuleData *mod) {
  if (!module_name || !mod)
    return 0;
  int enum_total = module_registry_enum_entry_count();
  int value_total = module_registry_enum_value_entry_count();
  for (int i = 0; i < enum_total; ++i) {
    const char *entry_module = module_registry_enum_entry_module(i);
    if (!entry_module || strcmp(entry_module, module_name) != 0)
      continue;
    const char *enum_name = module_registry_enum_entry_name(i);
    if (library_module_has_enum(mod, enum_name))
      continue;
    LibraryEnum en = {0};
    en.name = enum_name ? xstrdup(enum_name) : NULL;
    int count = 0;
    for (int vi = 0; vi < value_total; ++vi) {
      const char *value_module = module_registry_enum_value_entry_module(vi);
      const char *value_enum = module_registry_enum_value_entry_enum(vi);
      if (value_module && value_enum &&
          strcmp(value_module, module_name) == 0 &&
          strcmp(value_enum, enum_name) == 0)
        ++count;
    }
    if (count > 0) {
      en.value_names = (char **)xcalloc((size_t)count, sizeof(char *));
      en.values = (int32_t *)xcalloc((size_t)count, sizeof(int32_t));
      if (!en.value_names || !en.values) {
        free_library_enum(&en);
        return 1;
      }
      int idx = 0;
      for (int vi = 0; vi < value_total; ++vi) {
        const char *value_module = module_registry_enum_value_entry_module(vi);
        const char *value_enum = module_registry_enum_value_entry_enum(vi);
        if (!value_module || !value_enum ||
            strcmp(value_module, module_name) != 0 ||
            strcmp(value_enum, enum_name) != 0)
          continue;
        const char *value_name = module_registry_enum_value_entry_name(vi);
        int value = module_registry_enum_value_entry_value(vi);
        if (idx < count) {
          en.value_names[idx] = value_name ? xstrdup(value_name) : NULL;
          en.values[idx] = (int32_t)value;
          ++idx;
        }
      }
      en.value_count = (uint32_t)count;
    }
    en.is_exposed = 1;
    if (append_library_enum(mod, &en) != 0) {
      free_library_enum(&en);
      return 1;
    }
  }
  return 0;
}

static int collect_metadata_for_unit(const Node *unit, const char *ccb_path,
                                     LibraryModuleData **modules,
                                     int *module_count, int *module_cap) {
  if (!unit || unit->kind != ND_UNIT || !modules || !module_count ||
      !module_cap)
    return 0;
  const char *module_name = unit->module_path.full_name;
  if (!module_name || !*module_name)
    return 0;
  LibraryModuleData *mod =
      find_or_add_module(modules, module_count, module_cap, module_name);
  if (!mod)
    return 1;
  if (!mod->ccb_path && ccb_path)
    mod->ccb_path = xstrdup(ccb_path);
  if (collect_functions_from_unit(unit, mod))
    return 1;
  if (collect_structs_for_module(module_name, mod))
    return 1;
  if (collect_enums_for_module(module_name, mod))
    return 1;
  return 0;
}

static int write_library_file(const char *path, LibraryModuleData *mods,
                              int module_count) {
  if (!path || !mods || module_count <= 0)
    return EINVAL;

  CclibModule *modules =
      (CclibModule *)xcalloc((size_t)module_count, sizeof(CclibModule));
  if (!modules)
    return ENOMEM;

  CclibFile file = {0};
  file.modules = modules;
  file.module_count = (uint32_t)module_count;
  file.format_version = 1;

  int err = 0;

  for (int i = 0; i < module_count && !err; ++i) {
    LibraryModuleData *src = &mods[i];
    CclibModule *dst = &modules[i];
    dst->module_name = src->module_name;

    if (!src->ccbin_data) {
      err = EINVAL;
      break;
    }
    if (src->ccbin_size > UINT32_MAX) {
      err = ERANGE;
      break;
    }

    if (src->function_count > 0) {
      dst->functions = (CclibFunction *)xcalloc((size_t)src->function_count,
                                                sizeof(CclibFunction));
      if (!dst->functions) {
        err = ENOMEM;
        break;
      }
      for (int fi = 0; fi < src->function_count; ++fi) {
        LibraryFunction *lf = &src->functions[fi];
        CclibFunction *cf = &dst->functions[fi];
        cf->name = lf->name;
        cf->backend_name = lf->backend_name;
        cf->return_type = lf->return_spec;
        cf->param_types = lf->param_specs;
        cf->param_count = (uint32_t)(lf->param_count < 0 ? 0 : lf->param_count);
        cf->is_varargs = (uint8_t)(lf->is_varargs ? 1 : 0);
        cf->is_noreturn = (uint8_t)(lf->is_noreturn ? 1 : 0);
        cf->is_exposed = (uint8_t)(lf->is_exposed ? 1 : 0);
      }
    }
    dst->function_count =
        (uint32_t)(src->function_count < 0 ? 0 : src->function_count);

    if (src->struct_count > 0) {
      dst->structs = (CclibStruct *)xcalloc((size_t)src->struct_count,
                                            sizeof(CclibStruct));
      if (!dst->structs) {
        err = ENOMEM;
        break;
      }
      for (int si = 0; si < src->struct_count; ++si) {
        LibraryStruct *ls = &src->structs[si];
        CclibStruct *cs = &dst->structs[si];
        cs->name = ls->name;
        cs->field_names = ls->field_names;
        cs->field_types = ls->field_specs;
        cs->field_offsets = ls->field_offsets;
        cs->field_count = ls->field_count;
        cs->size_bytes = ls->size_bytes;
        cs->is_exposed = (uint8_t)(ls->is_exposed ? 1 : 0);
      }
    }
    dst->struct_count =
        (uint32_t)(src->struct_count < 0 ? 0 : src->struct_count);

    if (src->enum_count > 0) {
      dst->enums =
          (CclibEnum *)xcalloc((size_t)src->enum_count, sizeof(CclibEnum));
      if (!dst->enums) {
        err = ENOMEM;
        break;
      }
      for (int ei = 0; ei < src->enum_count; ++ei) {
        LibraryEnum *le = &src->enums[ei];
        CclibEnum *ce = &dst->enums[ei];
        ce->name = le->name;
        if (le->value_count > 0) {
          ce->values = (CclibEnumValue *)xcalloc((size_t)le->value_count,
                                                 sizeof(CclibEnumValue));
          if (!ce->values) {
            err = ENOMEM;
            break;
          }
          for (uint32_t vi = 0; vi < le->value_count; ++vi) {
            ce->values[vi].name = le->value_names ? le->value_names[vi] : NULL;
            ce->values[vi].value = le->values ? le->values[vi] : 0;
          }
        }
        ce->value_count = le->value_count;
        ce->is_exposed = (uint8_t)(le->is_exposed ? 1 : 0);
      }
    }
    dst->enum_count = (uint32_t)(src->enum_count < 0 ? 0 : src->enum_count);

    if (src->global_count > 0) {
      dst->globals = (CclibGlobal *)xcalloc((size_t)src->global_count,
                                            sizeof(CclibGlobal));
      if (!dst->globals) {
        err = ENOMEM;
        break;
      }
      for (int gi = 0; gi < src->global_count; ++gi) {
        LibraryGlobal *lg = &src->globals[gi];
        CclibGlobal *cg = &dst->globals[gi];
        cg->name = lg->name;
        cg->type_spec = lg->type_spec;
        cg->is_const = (uint8_t)(lg->is_const ? 1 : 0);
      }
    }
    dst->global_count =
        (uint32_t)(src->global_count < 0 ? 0 : src->global_count);

    dst->ccbin_data = src->ccbin_data;
    dst->ccbin_size = (uint32_t)src->ccbin_size;
  }

  if (!err) {
    err = cclib_write(path, &file);
  }

  for (int i = 0; i < module_count; ++i) {
    CclibModule *dst = &modules[i];
    if (dst->functions)
      free(dst->functions);
    if (dst->structs)
      free(dst->structs);
    if (dst->enums) {
      for (uint32_t vi = 0; vi < dst->enum_count; ++vi) {
        if (dst->enums[vi].values)
          free(dst->enums[vi].values);
      }
      free(dst->enums);
    }
    if (dst->globals)
      free(dst->globals);
  }
  free(modules);

  return err;
}

static int loaded_library_add_type(LoadedLibrary *lib, Type *ty) {
  if (!lib || !ty)
    return 1;
  if (lib->allocated_type_count == lib->allocated_type_cap) {
    int new_cap = lib->allocated_type_cap ? lib->allocated_type_cap * 2 : 8;
    Type **grown = (Type **)realloc(lib->allocated_types,
                                    (size_t)new_cap * sizeof(Type *));
    if (!grown)
      return 1;
    lib->allocated_types = grown;
    lib->allocated_type_cap = new_cap;
  }
  lib->allocated_types[lib->allocated_type_count++] = ty;
  return 0;
}

static void free_loaded_library_type(Type *ty) {
  if (!ty)
    return;
  if (ty->kind == TY_STRUCT) {
    if (ty->strct.field_names) {
      char **names = (char **)ty->strct.field_names;
      for (int i = 0; i < ty->strct.field_count; ++i)
        free(names[i]);
      free(names);
    }
    if (ty->strct.field_types)
      free(ty->strct.field_types);
    if (ty->strct.field_offsets)
      free(ty->strct.field_offsets);
    free((void *)ty->struct_name);
  } else if (ty->kind == TY_IMPORT) {
    free((void *)ty->import_module);
    free((void *)ty->import_type_name);
    // import_resolved is not owned
  }
  free(ty);
}

static void free_loaded_library_function(LoadedLibraryFunction *fn) {
  if (!fn)
    return;
  free(fn->name);
  free(fn->backend_name);
  free(fn->qualified_name);
  free(fn->module_name);
  free(fn->param_types);
  memset(fn, 0, sizeof(*fn));
}

static int append_loaded_function(LoadedLibraryFunction **funcs, int *count,
                                  int *cap, const LoadedLibraryFunction *fn) {
  if (!funcs || !count || !cap || !fn)
    return 1;
  if (*count == *cap) {
    int new_cap = *cap ? *cap * 2 : 8;
    LoadedLibraryFunction *grown = (LoadedLibraryFunction *)realloc(
        *funcs, (size_t)new_cap * sizeof(LoadedLibraryFunction));
    if (!grown)
      return 1;
    *funcs = grown;
    *cap = new_cap;
  }
  (*funcs)[*count] = *fn;
  (*count)++;
  return 0;
}

static int register_structs_from_cclib_module(LoadedLibrary *lib,
                                              const char *module_name,
                                              const CclibModule *module) {
  if (!lib || !module_name || !module)
    return 0;
  for (uint32_t si = 0; si < module->struct_count; ++si) {
    const CclibStruct *st = &module->structs[si];
    if (!st->name)
      continue;
    Type *ty = (Type *)xcalloc(1, sizeof(Type));
    if (!ty)
      return 1;
    ty->kind = TY_STRUCT;
    ty->struct_name = xstrdup(st->name);
    ty->is_exposed = st->is_exposed;
    int field_count = (int)st->field_count;
    ty->strct.field_count = field_count;
    ty->strct.size_bytes = (int)st->size_bytes;
    if (loaded_library_add_type(lib, ty)) {
      free_loaded_library_type(ty);
      return 1;
    }
    module_registry_register_struct(module_name, ty);
    if (field_count > 0) {
      char **names = (char **)xcalloc((size_t)field_count, sizeof(char *));
      Type **types = (Type **)xcalloc((size_t)field_count, sizeof(Type *));
      int *offsets = (int *)xcalloc((size_t)field_count, sizeof(int));
      if (!names || !types || !offsets) {
        free(names);
        free(types);
        free(offsets);
        return 1;
      }
      for (int fi = 0; fi < field_count; ++fi) {
        names[fi] = st->field_names && fi < field_count && st->field_names[fi]
                        ? xstrdup(st->field_names[fi])
                        : NULL;
        const char *spec =
            (st->field_types && fi < field_count) ? st->field_types[fi] : NULL;
        types[fi] = spec_to_type(spec);
        offsets[fi] = (st->field_offsets && fi < field_count)
                          ? (int)st->field_offsets[fi]
                          : 0;
      }
      ty->strct.field_names = (const char **)names;
      ty->strct.field_types = types;
      ty->strct.field_offsets = offsets;
    }
  }
  return 0;
}

static int register_enums_from_cclib_module(LoadedLibrary *lib,
                                            const char *module_name,
                                            const CclibModule *module) {
  if (!lib || !module_name || !module)
    return 0;
  for (uint32_t ei = 0; ei < module->enum_count; ++ei) {
    const CclibEnum *en = &module->enums[ei];
    if (!en->name)
      continue;
    Type *enum_type = (Type *)xcalloc(1, sizeof(Type));
    if (!enum_type)
      return 1;
    enum_type->kind = TY_I32;
    enum_type->is_exposed = en->is_exposed;
    if (loaded_library_add_type(lib, enum_type)) {
      free(enum_type);
      return 1;
    }
    module_registry_register_enum(module_name, en->name, enum_type);
    for (uint32_t vi = 0; vi < en->value_count; ++vi) {
      const char *value_name = en->values[vi].name;
      int value = (int)en->values[vi].value;
      if (value_name)
        module_registry_register_enum_value(module_name, en->name, value_name,
                                            value);
    }
  }
  return 0;
}

static int harvest_functions_from_cclib_module(
    const CclibModule *module, const char *module_name,
    LoadedLibraryFunction **out_funcs, int *func_count, int *func_cap) {
  if (!module || !module_name || !out_funcs || !func_count || !func_cap)
    return 0;
  for (uint32_t fi = 0; fi < module->function_count; ++fi) {
    const CclibFunction *fn = &module->functions[fi];
    if (!fn->name || !fn->is_exposed)
      continue;
    LoadedLibraryFunction lf = {0};
    lf.name = xstrdup(fn->name);
    char *qualified = make_qualified_function_name(module_name, fn->name);
    lf.qualified_name = qualified;
    lf.module_name = xstrdup(module_name);
    const char *backend_src = fn->backend_name;
    char *generated_backend = NULL;
    if (!backend_src || !*backend_src) {
      generated_backend = make_backend_name_from_module(module_name, fn->name);
      backend_src = generated_backend;
    }
    lf.backend_name = backend_src ? xstrdup(backend_src) : NULL;
    free(generated_backend);
    lf.return_type = spec_to_type(fn->return_type);
    lf.param_count = (int)fn->param_count;
    if (lf.param_count > 0) {
      lf.param_types = (Type **)xcalloc((size_t)lf.param_count, sizeof(Type *));
      if (!lf.param_types) {
        free_loaded_library_function(&lf);
        return 1;
      }
      for (int pi = 0; pi < lf.param_count; ++pi) {
        const char *ps = (fn->param_types && (uint32_t)pi < fn->param_count)
                             ? fn->param_types[pi]
                             : NULL;
        lf.param_types[pi] = spec_to_type(ps);
      }
    }
    lf.is_varargs = fn->is_varargs ? 1 : 0;
    lf.is_noreturn = fn->is_noreturn ? 1 : 0;
    lf.is_exposed = fn->is_exposed ? 1 : 0;
    if (append_loaded_function(out_funcs, func_count, func_cap, &lf) != 0) {
      free_loaded_library_function(&lf);
      return 1;
    }
  }
  return 0;
}

static void free_loaded_library(LoadedLibrary *lib) {
  if (!lib)
    return;
  if (lib->ccbin_temp_paths) {
    for (uint32_t i = 0; i < lib->file.module_count; ++i) {
      if (lib->ccbin_temp_paths[i]) {
        remove(lib->ccbin_temp_paths[i]);
        free(lib->ccbin_temp_paths[i]);
      }
    }
    free(lib->ccbin_temp_paths);
  }
  if (lib->allocated_types) {
    for (int i = 0; i < lib->allocated_type_count; ++i)
      free_loaded_library_type(lib->allocated_types[i]);
    free(lib->allocated_types);
  }
  cclib_free(&lib->file);
  free(lib->path);
  memset(lib, 0, sizeof(*lib));
}

static int unit_has_auto_import(const Node *unit, const char *module_full) {
  if (!unit || unit->kind != ND_UNIT || !module_full || !*module_full)
    return 0;
  if (!unit->imports || unit->import_count <= 0)
    return 0;
  for (int i = 0; i < unit->import_count; ++i) {
    const ModulePath *imp = &unit->imports[i];
    if (!imp || !imp->full_name)
      continue;
    if (strcmp(imp->full_name, module_full) == 0) {
      if (imp->alias && imp->alias[0])
        return 0;
      return 1;
    }
  }
  return 0;
}

static void symtab_add_library_functions(SemaContext *sc, Node *unit,
                                         const LoadedLibraryFunction *funcs,
                                         int func_count) {
  if (!sc || !sc->syms || !funcs || func_count <= 0)
    return;
  SymTable *syms = sc->syms;
  for (int i = 0; i < func_count; ++i) {
    const LoadedLibraryFunction *lf = &funcs[i];
    if (!lf->qualified_name)
      continue;
    Symbol sym = {0};
    sym.kind = SYM_FUNC;
    sym.name = lf->qualified_name;
    sym.backend_name = lf->backend_name ? lf->backend_name : lf->qualified_name;
    sym.is_extern = 1;
    sym.abi = "C";
    sym.sig.ret = lf->return_type ? lf->return_type : type_i32();
    sym.sig.params = lf->param_types;
    sym.sig.param_count = lf->param_count;
    sym.sig.is_varargs = lf->is_varargs;
    sym.is_noreturn = lf->is_noreturn;
    symtab_add(syms, sym);

    if (sym.backend_name && strcmp(sym.backend_name, sym.name) != 0) {
      Symbol backend_alias = sym;
      backend_alias.name = sym.backend_name;
      symtab_add(syms, backend_alias);
    }

    if (!lf->name)
      continue;
    if (!lf->module_name)
      continue;
    const char *module_full = lf->module_name;

    if (unit_has_auto_import(unit, module_full)) {
      Symbol unqual = sym;
      unqual.name = lf->name;
      unqual.backend_name = sym.backend_name;
      sema_track_imported_function(sc, lf->name, module_full, &unqual);
    }
  }
}

static int load_cclib_library(const char *path, LoadedLibrary **libs,
                              int *lib_count, int *lib_cap,
                              LoadedLibraryFunction **funcs, int *func_count,
                              int *func_cap) {
  if (!path || !libs || !lib_count || !lib_cap || !funcs || !func_count ||
      !func_cap)
    return EINVAL;

  LoadedLibrary lib = {0};
  lib.path = xstrdup(path);
  int err = cclib_read(path, &lib.file);
  if (err) {
    fprintf(stderr, "error: failed to read cclib '%s' (%s)\n", path,
            strerror(err));
    free(lib.path);
    return err;
  }
  if (lib.file.module_count > 0) {
    lib.ccbin_temp_paths =
        (char **)xcalloc(lib.file.module_count, sizeof(char *));
    if (!lib.ccbin_temp_paths) {
      free_loaded_library(&lib);
      return ENOMEM;
    }
  }

  for (uint32_t mi = 0; mi < lib.file.module_count; ++mi) {
    const CclibModule *mod = &lib.file.modules[mi];
    const char *module_name = mod->module_name;
    if (!module_name)
      continue;
    if (register_structs_from_cclib_module(&lib, module_name, mod)) {
      err = ENOMEM;
      break;
    }
    if (register_enums_from_cclib_module(&lib, module_name, mod)) {
      err = ENOMEM;
      break;
    }
    if (harvest_functions_from_cclib_module(mod, module_name, funcs, func_count,
                                            func_cap)) {
      err = ENOMEM;
      break;
    }
  }

  if (!err) {
    if (*lib_count == *lib_cap) {
      int new_cap = *lib_cap ? *lib_cap * 2 : 4;
      LoadedLibrary *grown = (LoadedLibrary *)realloc(
          *libs, (size_t)new_cap * sizeof(LoadedLibrary));
      if (!grown)
        err = ENOMEM;
      else {
        *libs = grown;
        *lib_cap = new_cap;
      }
    }
    if (!err) {
      (*libs)[*lib_count] = lib;
      (*lib_count)++;
    }
  }

  if (err) {
    free_loaded_library(&lib);
  }

  return err;
}

static int run_chancecodec_process(const char *cmd, const char *backend,
                                   int opt_level, const char *asm_path,
                                   const char *ccb_path,
                                   const char *target_os_arg,
                                   int *spawn_errno_out) {
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  if (!cmd || !backend || !asm_path || !ccb_path) {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }
#ifdef _WIN32
  const char *args[12];
  int idx = 0;
  char optbuf[8];
  char target_option_buf[64];
  const char *target_option_value = NULL;
  if (target_os_arg && *target_os_arg) {
    snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s",
             target_os_arg);
    target_option_value = target_option_buf;
  }
  args[idx++] = cmd;
  args[idx++] = "--backend";
  args[idx++] = backend;
  if (opt_level > 0) {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = "--output";
  args[idx++] = asm_path;
  if (target_option_value) {
    args[idx++] = "--option";
    args[idx++] = target_option_value;
  }
  args[idx++] = ccb_path;
  args[idx] = NULL;
  intptr_t rc = _spawnvp(_P_WAIT, cmd, args);
  if (rc == -1) {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  return (int)rc;
#else
  char *args[12];
  int idx = 0;
  char optbuf[8];
  char target_option_buf[64];
  char *target_option_value = NULL;
  if (target_os_arg && *target_os_arg) {
    snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s",
             target_os_arg);
    target_option_value = target_option_buf;
  }
  args[idx++] = (char *)cmd;
  args[idx++] = "--backend";
  args[idx++] = (char *)backend;
  if (opt_level > 0) {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = "--output";
  args[idx++] = (char *)asm_path;
  if (target_option_value) {
    args[idx++] = "--option";
    args[idx++] = target_option_value;
  }
  args[idx++] = (char *)ccb_path;
  args[idx] = NULL;
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, cmd, NULL, NULL, args, environ);
  if (rc != 0) {
    if (spawn_errno_out)
      *spawn_errno_out = rc;
    errno = rc;
    return -1;
  }
  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
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

static int run_chancecodec_emit_ccbin(const char *cmd, const char *ccb_path,
                                      const char *ccbin_path, int opt_level,
                                      int *spawn_errno_out) {
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  if (!cmd || !ccb_path || !ccbin_path) {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }
#ifdef _WIN32
  const char *args[8];
  char optbuf[8];
  int idx = 0;
  args[idx++] = cmd;
  if (opt_level > 0) {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = ccb_path;
  args[idx++] = "--emit-ccbin";
  args[idx++] = ccbin_path;
  args[idx] = NULL;
  intptr_t rc = _spawnvp(_P_WAIT, cmd, args);
  if (rc == -1) {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  return (int)rc;
#else
  char *args[8];
  char optbuf[8];
  int idx = 0;
  args[idx++] = (char *)cmd;
  if (opt_level > 0) {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = (char *)ccb_path;
  args[idx++] = "--emit-ccbin";
  args[idx++] = (char *)ccbin_path;
  args[idx] = NULL;
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, cmd, NULL, NULL, args, environ);
  if (rc != 0) {
    if (spawn_errno_out)
      *spawn_errno_out = rc;
    errno = rc;
    return -1;
  }
  int status = 0;
  if (waitpid(pid, &status, 0) == -1) {
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

static int is_relocatable_obj(const char *path) {
  // Quick and permissive checks: ELF ET_REL, or COFF OBJ (not MZ/PE)
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  unsigned char hdr[64];
  size_t n = fread(hdr, 1, sizeof(hdr), f);
  fclose(f);
  if (n < 20)
    return 0;
  // MZ -> not relocatable (PE executable/library)
  if (hdr[0] == 'M' && hdr[1] == 'Z')
    return 0;
  // ELF?
  if (hdr[0] == 0x7F && hdr[1] == 'E' && hdr[2] == 'L' && hdr[3] == 'F') {
    // e_type at bytes 16-17 little-endian
    int et = hdr[16] | (hdr[17] << 8);
    return et == 1; // ET_REL
  }
  // Otherwise assume COFF OBJ if extension is .obj
  if (ends_with_icase(path, ".obj"))
    return 1;
  // Also accept .o when not ELF (some toolchains use different wrappers)
  if (ends_with_icase(path, ".o"))
    return 1;
  return 0;
}

int main(int argc, char **argv) {
  if (argc >= 2 && equals_icase(argv[1], "new"))
    return handle_new_command(argc - 1, argv + 1);
#ifdef _WIN32
  const char *out = "a.exe";
#else
  const char *out = "a";
#endif
  int output_overridden = 0;
  int stop_after_asm = 0;
  int stop_after_ccb = 0;
  int no_link = 0; // -c / --no-link
  int emit_library = 0;
  int freestanding = 0;
  int m32 = 0;
  int opt_level = 0;
  AsmSyntax asm_syntax = ASM_INTEL;
  TargetArch target_arch = ARCH_NONE;
  const char *chancecode_backend = NULL;
  const char *chancecodec_cmd_override = NULL;
#ifdef _WIN32
  TargetOS target_os = OS_WINDOWS;
#else
  TargetOS target_os = OS_LINUX;
#endif
  // Separate CHance and object inputs
  const char **ce_inputs = NULL;
  int ce_count = 0, ce_cap = 0;
  const char **ccb_inputs = NULL;
  int ccb_count = 0, ccb_cap = 0;
  const char **cclib_inputs = NULL;
  int cclib_count = 0, cclib_cap = 0;
  const char **obj_inputs = NULL;
  int obj_count = 0, obj_cap = 0;
  char **owned_ce_inputs = NULL;
  int owned_ce_count = 0, owned_ce_cap = 0;
  char **owned_ccb_inputs = NULL;
  int owned_ccb_count = 0, owned_ccb_cap = 0;
  char **owned_cclib_inputs = NULL;
  int owned_cclib_count = 0, owned_cclib_cap = 0;
  char **owned_obj_inputs = NULL;
  int owned_obj_count = 0, owned_obj_cap = 0;
  // include paths
  char **include_dirs = NULL;
  int include_dir_count = 0;
  const char *obj_override = NULL; // optional object path after -c/--no-link
  char exe_dir[1024] = {0};
  get_executable_dir(exe_dir, sizeof(exe_dir), argv[0]);
  chance_add_default_include_dirs(&include_dirs, &include_dir_count);

  LibraryModuleData *library_modules = NULL;
  int library_module_count = 0;
  int library_module_cap = 0;
  LoadedLibrary *loaded_libraries = NULL;
  int loaded_library_count = 0;
  int loaded_library_cap = 0;
  LoadedLibraryFunction *loaded_library_functions = NULL;
  int loaded_library_function_count = 0;
  int loaded_library_function_cap = 0;
  char *project_output_alloc = NULL;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
      usage(argv[0]);
      return 0;
    }
    if (strcmp(argv[i], "--version") == 0) {
      printf("chancec: CHance Compiler version 1.0.0\n");
      printf("chancec: CE language standard: H25-1\n");
      printf("chancec: License: MIT\n");
      printf("chancec: Compiled on %s %s\n", __DATE__, __TIME__);
      printf("chancec: Created by Nathan Hornby (AzureianGH)\n");
      return 0;
    }
    if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
      out = argv[++i];
      output_overridden = 1;
      continue;
    }
    if (strcmp(argv[i], "-S") == 0) {
      stop_after_asm = 1;
      continue;
    }
    if (strcmp(argv[i], "-Sccb") == 0) {
      stop_after_ccb = 1;
      continue;
    }
    if (strncmp(argv[i], "-O", 2) == 0) {
      const char *level_str = argv[i] + 2;
      int level = 1;
      if (*level_str != '\0') {
        char *endptr = NULL;
        long parsed = strtol(level_str, &endptr, 10);
        if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 3) {
          fprintf(stderr,
                  "invalid optimization level '%s' (use -O0|-O1|-O2|-O3)\n",
                  argv[i]);
          return 2;
        }
        level = (int)parsed;
      }
      opt_level = level;
      continue;
    }
    if ((strcmp(argv[i], "-c") == 0) || (strcmp(argv[i], "--no-link") == 0)) {
      no_link = 1;
      // Accept optional object filename immediately following
      if (i + 1 < argc && argv[i + 1][0] != '-') {
        const char *cand = argv[i + 1];
        size_t clen = strlen(cand);
        if (clen >= 2) {
          const char *dot = strrchr(cand, '.');
          if (dot && (_stricmp(dot, ".o") == 0 || _stricmp(dot, ".obj") == 0)) {
            obj_override = cand;
            i++;
          }
        }
      }
      continue;
    }
    if (strcmp(argv[i], "--asm-syntax") == 0 && i + 1 < argc) {
      const char *arg = argv[++i];
      if (strcmp(arg, "intel") == 0)
        asm_syntax = ASM_INTEL;
      else if (strcmp(arg, "att") == 0)
        asm_syntax = ASM_ATT;
      else if (strcmp(arg, "nasm") == 0)
        asm_syntax = ASM_NASM;
      else {
        fprintf(stderr, "Unknown asm syntax '%s' (use intel|att|nasm)\n", arg);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "--chancecodec") == 0 && i + 1 < argc) {
      chancecodec_cmd_override = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "--library") == 0) {
      emit_library = 1;
      continue;
    }
    if (strcmp(argv[i], "--freestanding") == 0) {
      freestanding = 1;
      continue;
    }
    if (strcmp(argv[i], "-x86") == 0) {
      target_arch = ARCH_X86;
      chancecode_backend = "x86-gas";
      continue;
    }
    if (strcmp(argv[i], "-m32") == 0) {
      m32 = 1;
      continue;
    }
    if (strcmp(argv[i], "-m64") == 0) {
      m32 = 0;
      continue;
    }
    if (strcmp(argv[i], "--target-os") == 0 && i + 1 < argc) {
      const char *os = argv[++i];
      if (strcmp(os, "windows") == 0)
        target_os = OS_WINDOWS;
      else if (strcmp(os, "linux") == 0)
        target_os = OS_LINUX;
      else {
        fprintf(stderr, "Unknown --target-os '%s' (use windows|linux)\n", os);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "-I") == 0 && i + 1 < argc) {
      chance_add_include_dir(&include_dirs, &include_dir_count, argv[++i]);
      continue;
    }
    if (strcmp(argv[i], "-Nno-formatting") == 0) {
      parser_set_disable_formatting_notes(1);
      continue;
    }
    if (strcmp(argv[i], "-vd") == 0 || strcmp(argv[i], "--verbose-deep") == 0) {
      compiler_verbose_set_mode(1);
      compiler_verbose_set_deep(1);
      continue;
    }
    if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
      compiler_verbose_set_mode(1);
      continue;
    }
    if (strcmp(argv[i], "--no-ansi") == 0) {
      diag_set_use_ansi(0);
      verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (argv[i][0] == '-') {
      usage(argv[0]);
      return 2;
    }
    // classify input
    const char *arg = argv[i];
    if (ends_with_icase(arg, ".ceproj")) {
      ProjectInputList ce_list = {&ce_inputs,      &ce_count,
                                  &ce_cap,         &owned_ce_inputs,
                                  &owned_ce_count, &owned_ce_cap};
      ProjectInputList ccb_list = {&ccb_inputs,      &ccb_count,
                                   &ccb_cap,         &owned_ccb_inputs,
                                   &owned_ccb_count, &owned_ccb_cap};
      ProjectInputList cclib_list = {&cclib_inputs,      &cclib_count,
                                     &cclib_cap,         &owned_cclib_inputs,
                                     &owned_cclib_count, &owned_cclib_cap};
      ProjectInputList obj_list = {&obj_inputs,      &obj_count,
                                   &obj_cap,         &owned_obj_inputs,
                                   &owned_obj_count, &owned_obj_cap};
      int perr = parse_ceproj_file(
          arg, ce_list, ccb_list, cclib_list, obj_list, &include_dirs,
          &include_dir_count, &output_overridden, &out, &project_output_alloc,
          &target_arch, &chancecode_backend, &stop_after_ccb, &stop_after_asm,
          &emit_library, &no_link, &freestanding, &target_os);
      if (perr != 0)
        return 2;
      continue;
    }
    if (ends_with_icase(arg, ".ce")) {
      if (ce_count == ce_cap) {
        ce_cap = ce_cap ? ce_cap * 2 : 8;
        ce_inputs =
            (const char **)realloc((void *)ce_inputs, sizeof(char *) * ce_cap);
      }
      ce_inputs[ce_count++] = arg;
    } else if (ends_with_icase(arg, ".ccb")) {
      if (ccb_count == ccb_cap) {
        ccb_cap = ccb_cap ? ccb_cap * 2 : 8;
        ccb_inputs = (const char **)realloc((void *)ccb_inputs,
                                            sizeof(char *) * ccb_cap);
      }
      ccb_inputs[ccb_count++] = arg;
    } else if (ends_with_icase(arg, ".cclib")) {
      if (cclib_count == cclib_cap) {
        cclib_cap = cclib_cap ? cclib_cap * 2 : 4;
        cclib_inputs = (const char **)realloc((void *)cclib_inputs,
                                              sizeof(char *) * cclib_cap);
      }
      cclib_inputs[cclib_count++] = arg;
    } else if (ends_with_icase(arg, ".o") || ends_with_icase(arg, ".obj")) {
      if (obj_count == obj_cap) {
        obj_cap = obj_cap ? obj_cap * 2 : 8;
        obj_inputs = (const char **)realloc((void *)obj_inputs,
                                            sizeof(char *) * obj_cap);
      }
      obj_inputs[obj_count++] = arg;
    } else {
      fprintf(stderr,
              "error: unknown input type '%s' (expected .ce or .o/.obj)\n",
              arg);
      return 2;
    }
  }
  if (stop_after_asm && stop_after_ccb) {
    fprintf(stderr, "error: -S and -Sccb cannot be used together\n");
    return 2;
  }
  if (stop_after_asm && target_arch == ARCH_NONE) {
    fprintf(stderr, "error: -S requires a backend selection (e.g., -x86)\n");
    return 2;
  }
  if (!emit_library && target_arch == ARCH_NONE) {
    if (!stop_after_ccb) {
      fprintf(stderr, "error: selecting a backend (e.g., -x86) is required "
                      "unless stopping at bytecode with -Sccb\n");
      return 2;
    }
    if (no_link) {
      fprintf(stderr, "error: -c/--no-link is incompatible with -Sccb (no "
                      "object emission when stopping at bytecode)\n");
      return 2;
    }
    if (obj_count > 0 || ccb_count > 0) {
      fprintf(stderr, "error: providing .ccb/.o/.obj inputs requires selecting "
                      "a backend (e.g., -x86)\n");
      return 2;
    }
  }
  if (ce_count == 0 && obj_count == 0 && ccb_count == 0) {
    usage(argv[0]);
    return 2;
  }
  if (emit_library) {
    if (stop_after_asm) {
      fprintf(stderr, "error: --library cannot be combined with -S\n");
      return 2;
    }
    if (stop_after_ccb) {
      fprintf(stderr, "error: --library already stops after bytecode output\n");
      return 2;
    }
    if (no_link) {
      fprintf(stderr, "error: --library is incompatible with -c/--no-link\n");
      return 2;
    }
    if (target_arch != ARCH_NONE) {
      fprintf(stderr, "error: --library cannot be combined with backend "
                      "selection (e.g., -x86)\n");
      return 2;
    }
    if (ccb_count > 0 || obj_count > 0 || cclib_count > 0) {
      fprintf(stderr, "error: --library currently only accepts .ce inputs\n");
      return 2;
    }
    if (ce_count == 0) {
      fprintf(stderr, "error: --library requires at least one .ce input\n");
      return 2;
    }
    if (!output_overridden) {
      out = "a.cclib";
    }
    if (!ends_with_icase(out, ".cclib")) {
      fprintf(stderr, "error: --library output must end with .cclib\n");
      return 2;
    }
  }
  if (m32) {
    fprintf(stderr, "Error: -m32 not implemented yet\n");
    return 2;
  }

  char chancecodec_exec_buf[1024] = {0};
  char chancecodec_override_buf[1024] = {0};
  const char *chancecodec_cmd_to_use = NULL;
  int chancecodec_uses_fallback = 0;
  int chancecodec_has_override = 0;
  int needs_chancecodec = (target_arch == ARCH_X86) || emit_library;
  if (needs_chancecodec) {
    if (chancecodec_cmd_override && *chancecodec_cmd_override) {
      strip_wrapping_quotes(chancecodec_cmd_override, chancecodec_override_buf,
                            sizeof(chancecodec_override_buf));
      if (!chancecodec_override_buf[0]) {
        fprintf(stderr, "error: --chancecodec path is empty after trimming "
                        "quotes/whitespace\n");
        return 2;
      }
      chancecodec_cmd_to_use = chancecodec_override_buf;
      chancecodec_has_override = 1;
    } else if (locate_chancecodec(chancecodec_exec_buf,
                                  sizeof(chancecodec_exec_buf), exe_dir) == 0 &&
               chancecodec_exec_buf[0]) {
      chancecodec_cmd_to_use = chancecodec_exec_buf;
    } else {
      chancecodec_cmd_to_use = default_chancecodec_name;
      chancecodec_uses_fallback = 1;
    }
  }

  if (compiler_verbose_enabled()) {
    verbose_print_config(out, opt_level, target_arch, target_os, stop_after_ccb,
                         stop_after_asm, no_link, emit_library, freestanding,
                         asm_syntax, include_dir_count, ce_count, ccb_count,
                         cclib_count, obj_count, chancecodec_cmd_to_use,
                         chancecodec_has_override, needs_chancecodec,
                         chancecode_backend, chancecodec_uses_fallback,
                         compiler_verbose_enabled(),
                         compiler_verbose_deep_enabled());
    if (include_dir_count > 0) {
      verbose_section("Include Directories");
      for (int idx = 0; idx < include_dir_count; ++idx) {
        char label[32];
        snprintf(label, sizeof(label), "dir[%d]", idx);
        verbose_table_row(label,
                          (include_dirs && include_dirs[idx])
                              ? include_dirs[idx]
                              : "(null)");
      }
    }
    if (ce_count > 0 && ce_inputs) {
      verbose_section("CE Inputs");
      for (int idx = 0; idx < ce_count; ++idx) {
        char label[32];
        snprintf(label, sizeof(label), "ce[%d]", idx);
        verbose_table_row(label, ce_inputs[idx]);
      }
    }
    if (ccb_count > 0 && ccb_inputs) {
      verbose_section("CCB Inputs");
      for (int idx = 0; idx < ccb_count; ++idx) {
        char label[32];
        snprintf(label, sizeof(label), "ccb[%d]", idx);
        verbose_table_row(label, ccb_inputs[idx]);
      }
    }
    if (cclib_count > 0 && cclib_inputs) {
      verbose_section("CCLib Inputs");
      for (int idx = 0; idx < cclib_count; ++idx) {
        char label[32];
        snprintf(label, sizeof(label), "cclib[%d]", idx);
        verbose_table_row(label, cclib_inputs[idx]);
      }
    }
    if (obj_count > 0 && obj_inputs) {
      verbose_section("Object Inputs");
      for (int idx = 0; idx < obj_count; ++idx) {
        char label[32];
        snprintf(label, sizeof(label), "obj[%d]", idx);
        verbose_table_row(label, obj_inputs[idx]);
      }
    }
  }
  // Allow multiple inputs: if linking to an executable (no -c) with multiple
  // .ce and/or .o, we will compile .ce to temporary objects and link them
  // together with any provided .o files.
  // Validate modes
  if (no_link) {
    if (!obj_override) {
      // Per-file compile: must not include external .o inputs
      if (obj_count > 0) {
        fprintf(
            stderr,
            "error: providing .o files with -c but without an output object is "
            "invalid. Specify an output object after -c to merge.\n");
        goto fail;
      }
      if (ce_count == 0) {
        fprintf(stderr, "error: nothing to compile.\n");
        goto fail;
      }
    } else {
      // Combined object: must have at least one .ce or .o input
      if ((ce_count + obj_count) == 0) {
        fprintf(stderr, "error: no inputs provided to combine into '%s'.\n",
                obj_override);
        goto fail;
      }
      // Validate external .o/.obj relocatable
      for (int k = 0; k < obj_count; ++k) {
        if (!is_relocatable_obj(obj_inputs[k])) {
          fprintf(stderr,
                  "error: input '%s' is not a relocatable object file.\n",
                  obj_inputs[k]);
          goto fail;
        }
      }
    }
  }

  module_registry_reset();
  int rc = 0;
  if (!emit_library && cclib_count > 0) {
    for (int i = 0; i < cclib_count; ++i) {
      if (load_cclib_library(
              cclib_inputs[i], &loaded_libraries, &loaded_library_count,
              &loaded_library_cap, &loaded_library_functions,
              &loaded_library_function_count, &loaded_library_function_cap)) {
        rc = 1;
        goto cleanup;
      }
    }
  }

  int library_codegen_units = 0;
  if (!emit_library && loaded_library_count > 0) {
    for (int i = 0; i < loaded_library_count; ++i) {
      const LoadedLibrary *lib = &loaded_libraries[i];
      if (!lib)
        continue;
      for (uint32_t mi = 0; mi < lib->file.module_count; ++mi) {
        const CclibModule *mod = &lib->file.modules[mi];
        if (mod && mod->ccbin_data && mod->ccbin_size > 0)
          ++library_codegen_units;
      }
    }
  }

  UnitCompile *units = NULL;
  if (ce_count > 0)
    units = (UnitCompile *)xcalloc((size_t)ce_count, sizeof(UnitCompile));
  int skip_backend_outputs = stop_after_ccb || stop_after_asm || emit_library;
  int total_codegen_units = ce_count + ccb_count + library_codegen_units;
  // Determine if we need a final link step combining multiple inputs
  int multi_link = (!no_link && !skip_backend_outputs &&
                    (obj_count > 0 || total_codegen_units > 1));

  // Container for temporary objects when merging or linking
  char **temp_objs = NULL;
  int to_cnt = 0, to_cap = 0;
  char single_obj_path[1024] = {0};
  int have_single_obj = 0;
  int single_obj_is_temp = 0;

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Loading CE units");
  for (int fi = 0; fi < ce_count; ++fi) {
    const char *input = ce_inputs[fi];
    if (compiler_verbose_enabled())
      verbose_progress("ce-load", fi + 1, ce_count);
    int len = 0;
    char *src = read_all(input, &len);
    if (!src) {
      rc = 1;
      break;
    }
    int pre_len = 0;
  char *preprocessed = chance_preprocess_source(
    input, src, len, &pre_len, target_arch_to_macro(target_arch));
    SourceBuffer sb = {preprocessed ? preprocessed : src,
                       preprocessed ? pre_len : len, input};
    Parser *ps = parser_create(sb);
    SemaContext *sc = sema_create();
    chance_process_includes_and_scan(input, src, len, include_dirs,
                                     include_dir_count, sc->syms);
    Node *unit = parse_unit(ps);
    parser_export_externs(ps, sc->syms);
    symtab_add_library_functions(sc, unit, loaded_library_functions,
                                 loaded_library_function_count);

    units[fi].input_path = xstrdup(input);
    units[fi].src = src;
    units[fi].stripped = preprocessed;
    units[fi].unit = unit;
    units[fi].sc = sc;
    units[fi].parser = ps;
  }
  if (rc)
    goto cleanup;

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Registering CE externs");
  for (int target = 0; target < ce_count; ++target) {
    if (compiler_verbose_enabled())
      verbose_progress("ce-extern", target + 1, ce_count);
    for (int source = 0; source < ce_count; ++source) {
      if (target == source)
        continue;
      sema_register_foreign_unit_symbols(units[target].sc, units[target].unit,
                                         units[source].unit);
    }
  }

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Codegen CE units");
  for (int fi = 0; fi < ce_count && rc == 0; ++fi) {
    UnitCompile *uc = &units[fi];
    Node *unit = uc->unit;
    SemaContext *sc = uc->sc;
    Parser *ps = uc->parser;

    if (compiler_verbose_enabled())
      verbose_progress("ce-codegen", fi + 1, ce_count);

    int serr = sema_check_unit(sc, unit);
    if (!serr) {
      char dir[512], base[512];
      split_path(uc->input_path, dir, sizeof(dir), base, sizeof(base));

      char ccb_path[1024];
      build_path_with_ext(dir, base, ".ccb", ccb_path, sizeof(ccb_path));
      if (stop_after_ccb && ends_with_icase(out, ".ccb"))
        snprintf(ccb_path, sizeof(ccb_path), "%s", out);
      int ccb_is_temp = 1;

      char asm_path[1024];
      build_path_with_ext(dir, base, ".S", asm_path, sizeof(asm_path));
      if (stop_after_asm && ends_with_icase(out, ".s"))
        snprintf(asm_path, sizeof(asm_path), "%s", out);

      char objOut[1024] = {0};
      int obj_is_temp = 0;
      int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                     (no_link || multi_link || target_arch != ARCH_NONE);
      if (need_obj) {
        if (no_link && obj_override) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
        } else if (no_link) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".obj"
#else
                              ".o"
#endif
                              ,
                              objOut, sizeof(objOut));
        } else if (multi_link) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        } else {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".tmp.obj"
#else
                              ".tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        }
      }

      CodegenOptions co = {.freestanding = freestanding != 0,
                           .m32 = m32 != 0,
                           .emit_asm = stop_after_asm != 0,
                           .no_link = (no_link || multi_link ||
                                       stop_after_ccb || stop_after_asm) != 0,
                           .asm_syntax = asm_syntax,
                           .output_path = out,
                           .obj_output_path = need_obj ? objOut : NULL,
                           .ccb_output_path = ccb_path,
                           .os = target_os,
                           .externs = NULL,
                           .extern_count = 0,
                           .opt_level = opt_level};
      int extern_count = 0;
      const Symbol *extern_syms = parser_get_externs(ps, &extern_count);
      co.externs = extern_syms;
      co.extern_count = extern_count;
      rc = codegen_ccb_write_module(unit, &co);

      if (!rc) {
        if (codegen_ccb_resolve_module_path(&co, ccb_path, sizeof(ccb_path)))
          rc = 1;
      }

      if (!rc && emit_library) {
        if (collect_metadata_for_unit(unit, ccb_path, &library_modules,
                                      &library_module_count,
                                      &library_module_cap)) {
          rc = 1;
        } else {
          const char *module_name = unit->module_path.full_name;
          LibraryModuleData *libmod =
              module_name
                  ? find_or_add_module(&library_modules, &library_module_count,
                                       &library_module_cap, module_name)
                  : NULL;
          if (!libmod) {
            rc = 1;
          } else {
            char ccbin_dir[512];
            char ccbin_base[512];
            split_path(ccb_path, ccbin_dir, sizeof(ccbin_dir), ccbin_base,
                       sizeof(ccbin_base));
            char ccbin_path[1024];
            build_path_with_ext(ccbin_dir, ccbin_base, ".ccbin", ccbin_path,
                                sizeof(ccbin_path));
            if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use) {
              fprintf(stderr, "error: chancecodec executable not resolved "
                              "(required for --library)\n");
              rc = 1;
            } else {
              int spawn_errno = 0;
              int ccbin_rc = run_chancecodec_emit_ccbin(
                  chancecodec_cmd_to_use, ccb_path, ccbin_path, opt_level,
                  &spawn_errno);
              if (ccbin_rc != 0) {
                if (ccbin_rc < 0)
                  fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                          chancecodec_cmd_to_use, strerror(spawn_errno));
                else
                  fprintf(stderr,
                          "chancecodec --emit-ccbin failed (rc=%d) for '%s'\n",
                          ccbin_rc, ccb_path);
                rc = 1;
                remove(ccbin_path);
                remove(ccb_path);
              } else {
                uint8_t *ccbin_data = NULL;
                size_t ccbin_size = 0;
                int read_err =
                    read_file_bytes(ccbin_path, &ccbin_data, &ccbin_size);
                if (read_err != 0) {
                  fprintf(stderr, "error: failed reading ccbin '%s' (%s)\n",
                          ccbin_path, strerror(read_err));
                  rc = 1;
                  remove(ccbin_path);
                  remove(ccb_path);
                } else {
                  if (libmod->ccbin_data)
                    free(libmod->ccbin_data);
                  libmod->ccbin_data = ccbin_data;
                  libmod->ccbin_size = ccbin_size;
                  free(libmod->ccbin_path);
                  libmod->ccbin_path = NULL;
                }
                remove(ccbin_path);
                remove(ccb_path);
              }
            }
          }
        }
      }

      if (!rc && target_arch == ARCH_X86 && !stop_after_ccb) {
        const char *backend =
            chancecode_backend ? chancecode_backend : "x86-gas";
        const char *target_os_option = target_os_to_option(target_os);
        if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use) {
          fprintf(stderr,
                  "internal error: ChanceCode CLI command unresolved\n");
          rc = 1;
        } else {
          char display_cmd[4096];
          if (opt_level > 0) {
            if (target_os_option)
              snprintf(display_cmd, sizeof(display_cmd),
                       "\"%s\" --backend %s -O%d --output \"%s\" --option "
                       "target-os=%s \"%s\"",
                       chancecodec_cmd_to_use, backend, opt_level, asm_path,
                       target_os_option, ccb_path);
            else
              snprintf(display_cmd, sizeof(display_cmd),
                       "\"%s\" --backend %s -O%d --output \"%s\" \"%s\"",
                       chancecodec_cmd_to_use, backend, opt_level, asm_path,
                       ccb_path);
          } else {
            if (target_os_option)
              snprintf(display_cmd, sizeof(display_cmd),
                       "\"%s\" --backend %s --output \"%s\" --option "
                       "target-os=%s \"%s\"",
                       chancecodec_cmd_to_use, backend, asm_path,
                       target_os_option, ccb_path);
            else
              snprintf(display_cmd, sizeof(display_cmd),
                       "\"%s\" --backend %s --output \"%s\" \"%s\"",
                       chancecodec_cmd_to_use, backend, asm_path, ccb_path);
          }

          int spawn_errno = 0;
          int chance_rc = run_chancecodec_process(
              chancecodec_cmd_to_use, backend, opt_level, asm_path, ccb_path,
              target_os_option, &spawn_errno);
          if (chance_rc != 0) {
            if (chance_rc < 0) {
              fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                      chancecodec_cmd_to_use, strerror(spawn_errno));
            } else {
              fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc,
                      display_cmd);
            }
            if (!chancecodec_has_override && chancecodec_uses_fallback) {
              fprintf(stderr,
                      "hint: use --chancecodec <path> or set CHANCECODEC_CMD "
                      "to point at the ChanceCode CLI executable\n");
            }
            rc = 1;
          }
        }
      }

      if (!rc && target_arch == ARCH_X86 && !stop_after_ccb &&
          !stop_after_asm) {
        if (!need_obj) {
          fprintf(stderr,
                  "internal error: object output expected but path missing\n");
          rc = 1;
        } else {
          char cc_cmd[4096];
          size_t pos =
              (size_t)snprintf(cc_cmd, sizeof(cc_cmd), "cc -c \"%s\" -o \"%s\"",
                               asm_path, objOut);
          if (pos >= sizeof(cc_cmd)) {
            fprintf(stderr, "command buffer exhausted for cc invocation\n");
            rc = 1;
          } else {
            if (freestanding) {
              strncat(cc_cmd, " -ffreestanding -nostdlib",
                      sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }
#ifdef _WIN32
            strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
            if (opt_level > 0) {
              char optbuf[8];
              snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
              strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }

            int cc_rc = system(cc_cmd);
            if (cc_rc != 0) {
              fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
              rc = 1;
            }
          }
        }
      }

      if (!rc && target_arch == ARCH_X86) {
        if (!stop_after_ccb && ccb_is_temp)
          remove(ccb_path);
        if (!stop_after_asm)
          remove(asm_path);
      }

      if (!rc && target_arch == ARCH_X86 && !stop_after_ccb &&
          !stop_after_asm && !no_link && !multi_link) {
        snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
        have_single_obj = 1;
        single_obj_is_temp = obj_is_temp;
      }

      if (!rc && ((no_link && obj_override) || multi_link) &&
          objOut[0] != '\0') {
        if (to_cnt == to_cap) {
          to_cap = to_cap ? to_cap * 2 : 8;
          temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
        }
        temp_objs[to_cnt++] = xstrdup(objOut);
      }
    } else {
      rc = 1;
    }

    sema_destroy(sc);
    uc->sc = NULL;
    ast_free(unit);
    uc->unit = NULL;
    if (ps) {
      parser_destroy(ps);
      uc->parser = NULL;
    }
    if (uc->stripped) {
      free(uc->stripped);
      uc->stripped = NULL;
    }
    if (uc->src) {
      free(uc->src);
      uc->src = NULL;
    }
    if (uc->input_path) {
      free(uc->input_path);
      uc->input_path = NULL;
    }

    if (rc)
      break;
  }
  if (!emit_library) {
    if (compiler_verbose_enabled() && ccb_count > 0)
      verbose_section("Processing CCB inputs");
    for (int ci = 0; !rc && ci < ccb_count; ++ci) {
      const char *ccb_input = ccb_inputs[ci];
      if (compiler_verbose_enabled())
        verbose_progress("ccb-proc", ci + 1, ccb_count);
      char dir[512], base[512];
      split_path(ccb_input, dir, sizeof(dir), base, sizeof(base));

      char ccb_path[1024];
      snprintf(ccb_path, sizeof(ccb_path), "%s", ccb_input);
      int ccb_is_temp = 0;

      char asm_path[1024];
      build_path_with_ext(dir, base, ".S", asm_path, sizeof(asm_path));
      if (stop_after_asm && ends_with_icase(out, ".s"))
        snprintf(asm_path, sizeof(asm_path), "%s", out);

      char objOut[1024] = {0};
      int obj_is_temp = 0;
      int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                     (no_link || multi_link || target_arch != ARCH_NONE);
      if (need_obj) {
        if (no_link && obj_override) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
        } else if (no_link) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".obj"
#else
                              ".o"
#endif
                              ,
                              objOut, sizeof(objOut));
        } else if (multi_link) {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        } else {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".tmp.obj"
#else
                              ".tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        }
      }

      if (target_arch == ARCH_X86 && !stop_after_ccb) {
        const char *backend =
            chancecode_backend ? chancecode_backend : "x86-gas";
        const char *target_os_option = target_os_to_option(target_os);
        if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use) {
          fprintf(stderr,
                  "internal error: ChanceCode CLI command unresolved\n");
          rc = 1;
          break;
        }
        char display_cmd[4096];
        if (opt_level > 0) {
          if (target_os_option)
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s -O%d --output \"%s\" --option "
                     "target-os=%s \"%s\"",
                     chancecodec_cmd_to_use, backend, opt_level, asm_path,
                     target_os_option, ccb_path);
          else
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s -O%d --output \"%s\" \"%s\"",
                     chancecodec_cmd_to_use, backend, opt_level, asm_path,
                     ccb_path);
        } else {
          if (target_os_option)
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s --output \"%s\" --option "
                     "target-os=%s \"%s\"",
                     chancecodec_cmd_to_use, backend, asm_path,
                     target_os_option, ccb_path);
          else
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s --output \"%s\" \"%s\"",
                     chancecodec_cmd_to_use, backend, asm_path, ccb_path);
        }

        int spawn_errno = 0;
        int chance_rc = run_chancecodec_process(chancecodec_cmd_to_use, backend,
                                                opt_level, asm_path, ccb_path,
                                                target_os_option, &spawn_errno);
        if (chance_rc != 0) {
          if (chance_rc < 0) {
            fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                    chancecodec_cmd_to_use, strerror(spawn_errno));
          } else {
            fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc,
                    display_cmd);
          }
          if (!chancecodec_has_override && chancecodec_uses_fallback) {
            fprintf(stderr,
                    "hint: use --chancecodec <path> or set CHANCECODEC_CMD to "
                    "point at the ChanceCode CLI executable\n");
          }
          rc = 1;
          break;
        }
      }

      if (target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm) {
        if (!need_obj) {
          fprintf(stderr,
                  "internal error: object output expected but path missing\n");
          rc = 1;
          break;
        }

        char cc_cmd[4096];
        size_t pos = (size_t)snprintf(
            cc_cmd, sizeof(cc_cmd), "cc -c \"%s\" -o \"%s\"", asm_path, objOut);
        if (pos >= sizeof(cc_cmd)) {
          fprintf(stderr, "command buffer exhausted for cc invocation\n");
          rc = 1;
          break;
        }
        if (freestanding) {
          strncat(cc_cmd, " -ffreestanding -nostdlib",
                  sizeof(cc_cmd) - strlen(cc_cmd) - 1);
        }
#ifdef _WIN32
        strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
        if (opt_level > 0) {
          char optbuf[8];
          snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
          strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
        }

        int cc_rc = system(cc_cmd);
        if (cc_rc != 0) {
          fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
          rc = 1;
          break;
        }
      }

      if (target_arch == ARCH_X86) {
        if (!stop_after_asm)
          remove(asm_path);
      }

      if (target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm &&
          !no_link && !multi_link) {
        snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
        have_single_obj = 1;
        single_obj_is_temp = obj_is_temp;
      }

      if (((no_link && obj_override) || multi_link) && objOut[0] != '\0') {
        if (to_cnt == to_cap) {
          to_cap = to_cap ? to_cap * 2 : 8;
          temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
        }
        temp_objs[to_cnt++] = xstrdup(objOut);
      }
    }
  }
  if (!rc && !emit_library && target_arch == ARCH_X86 && !stop_after_ccb) {
    if (compiler_verbose_enabled() && library_codegen_units > 0)
      verbose_section("Processing library modules");
    int verbose_lib_progress = 0;
    for (int li = 0; li < loaded_library_count && !rc; ++li) {
      LoadedLibrary *lib = &loaded_libraries[li];
      for (uint32_t mi = 0; mi < lib->file.module_count && !rc; ++mi) {
        const CclibModule *mod = &lib->file.modules[mi];
        if (!mod->ccbin_data || mod->ccbin_size == 0)
          continue;

        if (compiler_verbose_enabled())
          verbose_progress("lib-module", ++verbose_lib_progress,
                           library_codegen_units);

        char lib_dir[512], lib_base[512];
        split_path(lib->path, lib_dir, sizeof(lib_dir), lib_base,
                   sizeof(lib_base));
        char *prefix = module_name_to_prefix_copy(
            mod->module_name ? mod->module_name : "module");
        char base_component[512];
        if (prefix && *prefix)
          snprintf(base_component, sizeof(base_component), "%s_%s_%u", lib_base,
                   prefix, (unsigned)mi);
        else
          snprintf(base_component, sizeof(base_component), "%s_module_%u",
                   lib_base, (unsigned)mi);
        free(prefix);

        char ccbin_path[1024];
        build_path_with_ext(lib_dir, base_component, ".tmp.ccbin", ccbin_path,
                            sizeof(ccbin_path));
        if (lib->ccbin_temp_paths) {
          free(lib->ccbin_temp_paths[mi]);
          lib->ccbin_temp_paths[mi] = xstrdup(ccbin_path);
        }
        int write_err =
            write_file_bytes(ccbin_path, mod->ccbin_data, mod->ccbin_size);
        if (write_err != 0) {
          fprintf(stderr, "error: failed to materialize ccbin '%s' (%s)\n",
                  ccbin_path, strerror(write_err));
          rc = 1;
          break;
        }

        char asm_path[1024];
        build_path_with_ext(lib_dir, base_component, ".S", asm_path,
                            sizeof(asm_path));
        if (stop_after_asm && ends_with_icase(out, ".s"))
          snprintf(asm_path, sizeof(asm_path), "%s", out);

        char objOut[1024] = {0};
        int obj_is_temp = 0;
        int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                       (no_link || multi_link || target_arch != ARCH_NONE);
        if (need_obj) {
          if (no_link && obj_override) {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".co.tmp.obj"
#else
                                ".co.tmp.o"
#endif
                                ,
                                objOut, sizeof(objOut));
          } else if (no_link) {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".obj"
#else
                                ".o"
#endif
                                ,
                                objOut, sizeof(objOut));
          } else if (multi_link) {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".co.tmp.obj"
#else
                                ".co.tmp.o"
#endif
                                ,
                                objOut, sizeof(objOut));
            obj_is_temp = 1;
          } else {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".tmp.obj"
#else
                                ".tmp.o"
#endif
                                ,
                                objOut, sizeof(objOut));
            obj_is_temp = 1;
          }
        }

        const char *backend =
            chancecode_backend ? chancecode_backend : "x86-gas";
        const char *target_os_option = target_os_to_option(target_os);
        char display_cmd[4096];
        if (opt_level > 0) {
          if (target_os_option)
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s -O%d --output \"%s\" --option "
                     "target-os=%s \"%s\"",
                     chancecodec_cmd_to_use, backend, opt_level, asm_path,
                     target_os_option, ccbin_path);
          else
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s -O%d --output \"%s\" \"%s\"",
                     chancecodec_cmd_to_use, backend, opt_level, asm_path,
                     ccbin_path);
        } else {
          if (target_os_option)
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s --output \"%s\" --option "
                     "target-os=%s \"%s\"",
                     chancecodec_cmd_to_use, backend, asm_path,
                     target_os_option, ccbin_path);
          else
            snprintf(display_cmd, sizeof(display_cmd),
                     "\"%s\" --backend %s --output \"%s\" \"%s\"",
                     chancecodec_cmd_to_use, backend, asm_path, ccbin_path);
        }

        int spawn_errno = 0;
        int chance_rc = run_chancecodec_process(chancecodec_cmd_to_use, backend,
                                                opt_level, asm_path, ccbin_path,
                                                target_os_option, &spawn_errno);
        if (chance_rc != 0) {
          if (chance_rc < 0)
            fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                    chancecodec_cmd_to_use, strerror(spawn_errno));
          else
            fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc,
                    display_cmd);
          if (!chancecodec_has_override && chancecodec_uses_fallback)
            fprintf(stderr,
                    "hint: use --chancecodec <path> or set CHANCECODEC_CMD to "
                    "point at the ChanceCode CLI executable\n");
          rc = 1;
          break;
        }

        if (!stop_after_ccb && !stop_after_asm) {
          if (!need_obj) {
            fprintf(
                stderr,
                "internal error: object output expected but path missing\n");
            rc = 1;
            break;
          }

          char cc_cmd[4096];
          size_t pos =
              (size_t)snprintf(cc_cmd, sizeof(cc_cmd), "cc -c \"%s\" -o \"%s\"",
                               asm_path, objOut);
          if (pos >= sizeof(cc_cmd)) {
            fprintf(stderr, "command buffer exhausted for cc invocation\n");
            rc = 1;
            break;
          }
          if (freestanding)
            strncat(cc_cmd, " -ffreestanding -nostdlib",
                    sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#ifdef _WIN32
          strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
          if (opt_level > 0) {
            char optbuf[8];
            snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
            strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
          }
          int cc_rc = system(cc_cmd);
          if (cc_rc != 0) {
            fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
            rc = 1;
            break;
          }
        }

        if (!stop_after_asm)
          remove(asm_path);

        if (!stop_after_ccb && !stop_after_asm && !no_link && !multi_link) {
          snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
          have_single_obj = 1;
          single_obj_is_temp = obj_is_temp;
        }

        if (((no_link && obj_override) || multi_link) && objOut[0] != '\0') {
          if (to_cnt == to_cap) {
            to_cap = to_cap ? to_cap * 2 : 8;
            temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
          }
          temp_objs[to_cnt++] = xstrdup(objOut);
        }

        remove(ccbin_path);
        if (lib->ccbin_temp_paths) {
          free(lib->ccbin_temp_paths[mi]);
          lib->ccbin_temp_paths[mi] = NULL;
        }
      }
    }
  }

  if (!rc && emit_library) {
    if (library_module_count == 0) {
      fprintf(stderr, "error: --library did not collect any modules\n");
      rc = 1;
    } else {
      if (compiler_verbose_enabled()) {
        verbose_section("Writing library");
        verbose_table_row("Output", out);
        char buf[32];
        snprintf(buf, sizeof(buf), "%d", library_module_count);
        verbose_table_row("Modules", buf);
      }
      int werr = write_library_file(out, library_modules, library_module_count);
      if (werr != 0) {
        fprintf(stderr, "error: failed to write library '%s' (%s)\n", out,
                strerror(werr));
        rc = 1;
      }
    }
    goto cleanup;
  }

  if (!rc && multi_link) {
    // Final link of temps + provided objects into an executable
    size_t cmdsz = 4096;
    char *cmd = (char *)xmalloc(cmdsz);
    snprintf(cmd, cmdsz, "cc -o \"%s\"", out);
    for (int i = 0; i < to_cnt; ++i) {
      size_t need = strlen(cmd) + strlen(temp_objs[i]) + 8;
      if (need > cmdsz) {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, temp_objs[i]);
      strcat(cmd, "\"");
    }
    for (int i = 0; i < obj_count; ++i) {
      size_t need = strlen(cmd) + strlen(obj_inputs[i]) + 8;
      if (need > cmdsz) {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, obj_inputs[i]);
      strcat(cmd, "\"");
    }
    if (compiler_verbose_enabled()) {
      verbose_section("Linking executable");
      verbose_table_row("Command", cmd);
    }
    int lrc = system(cmd);
    if (lrc != 0) {
      fprintf(stderr, "link failed (rc=%d): %s\n", lrc, cmd);
      rc = 1;
    }
    free(cmd);
    // cleanup temp objects
    for (int i = 0; i < to_cnt; ++i) {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
    temp_objs = NULL;
    to_cnt = 0;
    to_cap = 0;
  }
  if (!rc && have_single_obj) {
    char link_cmd[4096];
    snprintf(link_cmd, sizeof(link_cmd), "cc -o \"%s\" \"%s\"", out,
             single_obj_path);
    if (compiler_verbose_enabled()) {
      verbose_section("Linking single object");
      verbose_table_row("Command", link_cmd);
    }
    int lrc = system(link_cmd);
    if (lrc != 0) {
      fprintf(stderr, "link failed (rc=%d): %s\n", lrc, link_cmd);
      rc = 1;
    }
    if (single_obj_is_temp)
      remove(single_obj_path);
  }
  if (!rc && no_link && obj_override) {
    // Merge temps + external objects into obj_override (relocatable link)
    // Build command
    // Validate external objects again (already checked)
    size_t cmdsz = 4096;
    char *cmd = (char *)xmalloc(cmdsz);
    snprintf(cmd, cmdsz, "cc -r -o \"%s\"", obj_override);
    for (int i = 0; i < to_cnt; ++i) {
      size_t need = strlen(cmd) + strlen(temp_objs[i]) + 8;
      if (need > cmdsz) {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, temp_objs[i]);
      strcat(cmd, "\"");
    }
    for (int i = 0; i < obj_count; ++i) {
      size_t need = strlen(cmd) + strlen(obj_inputs[i]) + 8;
      if (need > cmdsz) {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, obj_inputs[i]);
      strcat(cmd, "\"");
    }
    if (compiler_verbose_enabled()) {
      verbose_section("Merging objects");
      verbose_table_row("Command", cmd);
    }
    int lrc = system(cmd);
    if (lrc != 0) {
      fprintf(stderr, "relocatable link failed (rc=%d): %s\n", lrc, cmd);
      rc = 1;
    }
    free(cmd);
    // cleanup temp objects
    for (int i = 0; i < to_cnt; ++i) {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
    temp_objs = NULL;
    to_cnt = 0;
    to_cap = 0;
  }
cleanup:
  if (temp_objs) {
    for (int i = 0; i < to_cnt; ++i) {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
  }
  if (units) {
    for (int i = 0; i < ce_count; ++i) {
      UnitCompile *uc = &units[i];
      if (uc->sc)
        sema_destroy(uc->sc);
      if (uc->unit)
        ast_free(uc->unit);
      if (uc->parser)
        parser_destroy(uc->parser);
      if (uc->stripped)
        free(uc->stripped);
      if (uc->src)
        free(uc->src);
      if (uc->input_path)
        free(uc->input_path);
    }
    free(units);
  }
  for (int i = 0; i < include_dir_count; i++)
    free(include_dirs[i]);
  free(include_dirs);
  free_library_module_array(library_modules, library_module_count);
  if (loaded_library_functions) {
    for (int i = 0; i < loaded_library_function_count; ++i)
      free_loaded_library_function(&loaded_library_functions[i]);
    free(loaded_library_functions);
  }
  if (loaded_libraries) {
    for (int i = 0; i < loaded_library_count; ++i)
      free_loaded_library(&loaded_libraries[i]);
    free(loaded_libraries);
  }
  if (owned_ce_inputs) {
    for (int i = 0; i < owned_ce_count; ++i)
      free(owned_ce_inputs[i]);
    free(owned_ce_inputs);
  }
  if (owned_ccb_inputs) {
    for (int i = 0; i < owned_ccb_count; ++i)
      free(owned_ccb_inputs[i]);
    free(owned_ccb_inputs);
  }
  if (owned_cclib_inputs) {
    for (int i = 0; i < owned_cclib_count; ++i)
      free(owned_cclib_inputs[i]);
    free(owned_cclib_inputs);
  }
  if (owned_obj_inputs) {
    for (int i = 0; i < owned_obj_count; ++i)
      free(owned_obj_inputs[i]);
    free(owned_obj_inputs);
  }
  if (project_output_alloc)
    free(project_output_alloc);
  free((void *)ce_inputs);
  free((void *)ccb_inputs);
  free((void *)cclib_inputs);
  free((void *)obj_inputs);
  return rc;
fail:
  rc = 2;
  goto cleanup;
}
