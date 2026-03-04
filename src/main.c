#include "ast.h"
#include "cclib.h"
#include "includes.h"
#include "mangle.h"
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
#if defined(__APPLE__)
#include <mach-o/dyld.h>
#endif
#endif

#define CHANCECODEC_BASE "chancecodec"
#ifdef _WIN32
#define CHANCE_PATH_SEP '\\'
#define CHANCECODEC_EXT ".exe"
#else
#define CHANCE_PATH_SEP '/'
#define CHANCECODEC_EXT ""
#endif

#ifndef STRIP_MAP_PATH_MAX
#define STRIP_MAP_PATH_MAX 4096
#endif

typedef enum
{
  ARCH_NONE = 0,
  ARCH_X86,
  ARCH_ARM64,
  ARCH_BSLASH
} TargetArch;

typedef struct LoadedLibrary LoadedLibrary;
typedef struct LoadedLibraryFunction LoadedLibraryFunction;

static const char *default_chancecodec_name =
#ifdef _WIN32
    "chancecodec.exe"
#else
    "chancecodec"
#endif
    ;

static int verbose_use_ansi = 1;
static const char *target_os_to_option(TargetOS os);
static char *make_backend_name_from_module(const char *module_full,
                                           const char *fn_name);

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
  switch (syntax)
  {
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
  switch (arch)
  {
  case ARCH_X86:
    return "x86";
  case ARCH_ARM64:
    return "arm64";
  case ARCH_BSLASH:
    return "bslash";
  case ARCH_NONE:
  default:
    return NULL;
  }
}

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

static void verbose_print_config(
    const char *output_path, int opt_level, TargetArch target_arch,
    TargetOS target_os, int stop_after_ccb, int stop_after_asm, int no_link,
    int emit_library, int freestanding, AsmSyntax asm_syntax,
    int include_dir_count, int ce_count, int ccb_count, int cclib_count,
    int obj_count, int symbol_ref_ce_count, int symbol_ref_cclib_count,
    const char *host_cc_cmd, int host_cc_has_override,
    const char *chancecodec_cmd, int chancecodec_has_override,
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
  const char *arch = "none";
  if (target_arch == ARCH_X86)
    arch = "x86-64";
  else if (target_arch == ARCH_ARM64)
    arch = "arm64";
  else if (target_arch == ARCH_BSLASH)
    arch = "bslash";
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
  snprintf(count_buf, sizeof(count_buf), "%d", symbol_ref_ce_count);
  verbose_table_row("Symbol-ref CE inputs", count_buf);
  snprintf(count_buf, sizeof(count_buf), "%d", symbol_ref_cclib_count);
  verbose_table_row("Symbol-ref CCLib inputs", count_buf);
  verbose_table_row("ChanceCode backend",
                    chancecode_backend ? chancecode_backend : "(auto)");
  verbose_table_row("Host CC",
                    (host_cc_cmd && *host_cc_cmd) ? host_cc_cmd : "cc");
  verbose_table_row("Host CC override", bool_str(host_cc_has_override));
  if (needs_chancecodec)
  {
    verbose_table_row("Chancecodec cmd",
                      chancecodec_cmd && *chancecodec_cmd ? chancecodec_cmd
                                                          : "(unresolved)");
    verbose_table_row("Chancecodec override",
                      bool_str(chancecodec_has_override));
    verbose_table_row("Chancecodec fallback",
                      bool_str(chancecodec_uses_fallback));
  }
  else
  {
    verbose_table_row("Chancecodec cmd", "(not required)");
  }
}

typedef struct
{
  char *input_path;
  char *src;
  char *stripped;
  Node *unit;
  SemaContext *sc;
  Parser *parser;
} UnitCompile;

typedef struct
{
  char *input_path;
  char *src;
  char *stripped;
  Node *unit;
} SymbolRefUnit;

typedef struct
{
  char *original;
  char *replacement;
} OverrideFile;

// sema
int sema_eval_const_i32(Node *expr);
SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
int sema_check_unit(SemaContext *sc, Node *unit);

#if defined(__linux__) || defined(__APPLE__)
#define _stricmp strcasecmp
#endif

static void usage(const char *prog)
{
  fprintf(stderr, "Usage: %s [options] input.ce [more.ce ...]\n", prog);
  fprintf(stderr, "       %s [options] project.ceproj\n", prog);
  fprintf(stderr, "       %s new <template> [name]\n", prog);
  fprintf(stderr, "Options:\n");
  fprintf(stderr,
          "  -o <file>         Output executable path (default a.exe) or object when using -c\n");
  fprintf(stderr, "  -S                Emit pseudo-asm alongside exe (.S)\n");
  fprintf(stderr,
          "  -Sccb             Stop after emitting Chance bytecode (.ccb)\n");
  fprintf(stderr,
          "  -O0|-O1|-O2|-O3   Select optimization level (default -O0)\n");
  fprintf(stderr,
          "  -g                Emit debug symbols in assembler/link stages\n");
  fprintf(stderr,
          "  --strip           Remove debug metadata before backend emission\n");
  fprintf(stderr,
          "  --strip-hard      Strip metadata and obfuscate all exported symbols\n");
  fprintf(stderr, "  -c [obj] | --no-link [obj]\n");
  fprintf(stderr, "                    Compile only; do not link (emit "
                  "object). Optional obj output path or use -o.\n");
  fprintf(stderr, "                    Repeat '-c <src> -o <obj>' to emit multiple object files in one run.\n");
  fprintf(stderr, "  --freestanding    Freestanding mode (no default libs)\n");
  fprintf(stderr, "  -m32|-m64         Target bitness (currently -m64 only)\n");
  fprintf(stderr, "  -x86              Select x86-64 backend for "
                  "assembly/object/executable output\n");
  fprintf(stderr, "  -arm64            Select ARM64 backend (use --target-os macos|linux|windows) for "
                  "assembly/object/executable output\n");
  fprintf(stderr, "  -bslash           Select BSlash backend for assembly output\n");
  fprintf(stderr, "  --target-os <os>  Backend target OS: windows|linux|macos\n");
  fprintf(
      stderr,
      "  --asm-syntax <s>  Assembly syntax: intel|att|nasm (default intel)\n");
  fprintf(stderr, "  --chancecodec <path>\n");
  fprintf(stderr, "                    Override ChanceCode CLI executable path "
                  "(default: auto-detect or PATH)\n");
  fprintf(stderr, "  --cc <path>       Override host C compiler used for asm/link (default cc)\n");
  fprintf(stderr,
          "  -sr:<path>       Load .ce/.cclib for symbols only (no codegen)\n");
  fprintf(stderr,
          "  -I <dir>          Add include search directory for #include <>\n");
  fprintf(stderr, "  -Nno-formatting  Disable formatting guidance notes\n");
  fprintf(stderr,
          "  -v, --verbose    Enable verbose diagnostics and progress\n");
  fprintf(stderr,
          "  -vd             Verbose + optimizer deep-dive visuals\n");
  fprintf(stderr,
          "  --no-ansi        Disable colored diagnostics (verbose too)\n");
    fprintf(stderr,
      "  --data-log      Emit machine-readable diagnostics (JSON lines)\n");
      fprintf(stderr,
        "  --request-ast   Emit AST JSON to stdout and exit\n");
        fprintf(stderr,
          "  --diagnostics-only Run parser/sema checks and emit diagnostics only\n");
        fprintf(stderr,
          "  --override-file <orig>=<path> Use override source for a specific input\n");
    fprintf(stderr,
      "  --implicit-voidp Allow implicit pointer to void* conversions\n");
      fprintf(stderr,
        "  --implicit-sizeof Allow implicit sizeof/alignof/offsetof to integer conversions\n");
  fprintf(stderr, "  --library         Emit a .cclib library instead of "
                  "compiling/linking\n");
}

static char *read_all(const char *path, int *out_len)
{
  FILE *f = fopen(path, "rb");
  if (!f)
  {
    perror("fopen");
    return NULL;
  }
  fseek(f, 0, SEEK_END);
  long sz = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *buf = (char *)xmalloc((size_t)sz + 1);
  if (fread(buf, 1, sz, f) != (size_t)sz)
  {
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

static int ends_with_icase(const char *s, const char *suf)
{
  if (!s || !suf)
    return 0;
  size_t ls = strlen(s), lu = strlen(suf);
  if (lu > ls)
    return 0;
  const char *p = s + (ls - lu);
#if defined(_WIN32)
  return _stricmp(p, suf) == 0;
#else
  for (size_t i = 0; i < lu; ++i)
  {
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

static int is_ce_source_arg(const char *path)
{
  return path && (ends_with_icase(path, ".ce") || ends_with_icase(path, ".ceproj"));
}

static int is_object_file_arg(const char *path)
{
  return path && (ends_with_icase(path, ".o") || ends_with_icase(path, ".obj"));
}

static void split_path(const char *path, char *dir, size_t dsz,
                       char *base_noext, size_t bsz)
{
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
  if (slash1 && slash2)
  {
    const char *mx = (slash1 > slash2 ? slash1 : slash2);
    base = mx + 1;
    dirend = mx;
  }
  else if (slash1)
  {
    base = slash1 + 1;
    dirend = slash1;
  }
  else if (slash2)
  {
    base = slash2 + 1;
    dirend = slash2;
  }
  if (dir && dsz)
  {
    size_t n = (size_t)(dirend - path);
    if (n >= dsz)
      n = dsz - 1;
    memcpy(dir, path, n);
    dir[n] = '\0';
  }
  const char *dot = strrchr(base, '.');
  if (base_noext && bsz)
  {
    size_t n = dot ? (size_t)(dot - base) : strlen(base);
    if (n >= bsz)
      n = bsz - 1;
    memcpy(base_noext, base, n);
    base_noext[n] = '\0';
  }
}

static void build_path_with_ext(const char *dir, const char *base,
                                const char *ext, char *buffer, size_t bufsz)
{
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

static int is_regular_file(const char *path)
{
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

static int parent_directory(const char *path, char *out, size_t outsz)
{
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
  if (len == 0)
  {
#ifdef _WIN32
    if (path[0] && path[1] == ':')
    {
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

static void strip_wrapping_quotes(const char *in, char *out, size_t outsz)
{
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
  if (len >= 2)
  {
    if ((in[0] == '"' && in[len - 1] == '"') ||
        (in[0] == '\'' && in[len - 1] == '\''))
    {
      ++in;
      len -= 2;
    }
  }
  if (len >= outsz)
    len = outsz - 1;
  memcpy(out, in, len);
  out[len] = '\0';
}

static int get_executable_dir(char *dir, size_t dirsz, const char *argv0)
{
  if (!dir || dirsz == 0)
    return 1;
  dir[0] = '\0';
#ifdef _WIN32
  char resolved[1024];
  if (_fullpath(resolved, argv0 ? argv0 : "", sizeof(resolved)) == NULL)
    return 1;
#else
  char resolved[PATH_MAX];
  resolved[0] = '\0';
  if (!argv0)
    argv0 = "";

  if (!realpath(argv0, resolved))
  {
#if defined(__linux__)
    ssize_t len = readlink("/proc/self/exe", resolved, sizeof(resolved) - 1);
    if (len > 0)
      resolved[len] = '\0';
    else
      resolved[0] = '\0';
#elif defined(__APPLE__)
    uint32_t size = (uint32_t)sizeof(resolved);
    if (_NSGetExecutablePath(resolved, &size) != 0)
    {
      char *dyn = (char *)malloc(size + 1);
      if (dyn)
      {
        if (_NSGetExecutablePath(dyn, &size) == 0 && realpath(dyn, resolved))
        {
          // resolved ok
        }
        else
        {
          if (_NSGetExecutablePath(dyn, &size) == 0)
          {
            strncpy(resolved, dyn, sizeof(resolved) - 1);
            resolved[sizeof(resolved) - 1] = '\0';
          }
          else
          {
            resolved[0] = '\0';
          }
        }
        free(dyn);
      }
      else
      {
        resolved[0] = '\0';
      }
    }
    else
    {
      if (!realpath(resolved, resolved))
      {
        // Keep the unresolved path; it is still useful for locating siblings.
      }
    }
#else
    resolved[0] = '\0';
#endif
  }

  if (!resolved[0] && argv0 && *argv0)
  {
    if (strchr(argv0, '/') || strchr(argv0, '\\'))
    {
      strncpy(resolved, argv0, sizeof(resolved) - 1);
      resolved[sizeof(resolved) - 1] = '\0';
    }
  }

  if (!resolved[0] && argv0 && *argv0)
  {
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
          char candidate[PATH_MAX];
          if (snprintf(candidate, sizeof(candidate), "%.*s/%s", (int)seg_len, s,
                       argv0) > 0)
          {
            if (access(candidate, X_OK) == 0)
            {
              if (realpath(candidate, resolved))
                break;
              strncpy(resolved, candidate, sizeof(resolved) - 1);
              resolved[sizeof(resolved) - 1] = '\0';
              break;
            }
          }
        }
        if (*p)
          p++;
      }
    }
  }

  if (!resolved[0])
    return 1;
#endif
  split_path(resolved, dir, dirsz, NULL, 0);
  return dir[0] ? 0 : 1;
}

static int locate_chancecodec(char *out, size_t outsz, const char *exe_dir)
{
  if (!out || outsz == 0)
    return 1;
  out[0] = '\0';
  const char *env_cmd = getenv("CHANCECODEC_CMD");
  if (!env_cmd || !*env_cmd)
    env_cmd = getenv("CHANCECODEC");
  if (env_cmd && *env_cmd)
  {
    strip_wrapping_quotes(env_cmd, out, outsz);
    if (out[0])
      return 0;
  }
  const char *env_home = getenv("CHANCECODE_HOME");
  if (env_home && *env_home)

  {
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
  if (exe_dir && *exe_dir)
  {
    build_path_with_ext(exe_dir, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
    if (is_regular_file(out))
      return 0;
    char parent[1024];
    if (parent_directory(exe_dir, parent, sizeof(parent)) == 0 && parent[0])
    {
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
          grandparent[0])
      {
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

static const char *target_os_to_option(TargetOS os)
{
  switch (os)
  {
  case OS_WINDOWS:
    return "windows";
  case OS_LINUX:
    return "linux";
  case OS_MACOS:
    return "macos";
  default:
    return NULL;
  }
}

static const char *arm64_backend_name_for_os(TargetOS os)
{
  switch (os)
  {
  case OS_MACOS:
    return "arm64-macos";
  case OS_LINUX:
    return "arm64-elf";
  case OS_WINDOWS:
    return "arm64-windows";
  default:
    return NULL;
  }
}

static int host_cc_is_aarch64_elf_gcc(const char *cc_cmd)
{
  if (!cc_cmd)
    return 0;
  return strstr(cc_cmd, "aarch64-elf-gcc") != NULL;
}

static void append_arm64_arch_flag(char *cmd, size_t cmd_cap, TargetOS target_os,
                                   const char *host_cc_cmd)
{
  if (!cmd || cmd_cap == 0)
    return;
  const char *flag = NULL;
  if (host_cc_is_aarch64_elf_gcc(host_cc_cmd) || target_os == OS_LINUX)
    flag = " -march=armv8-a";
  else
    flag = " -arch arm64";
  strncat(cmd, flag, cmd_cap - strlen(cmd) - 1);
}

static int equals_icase(const char *a, const char *b)
{
  if (!a || !b)
    return 0;
  while (*a && *b)
  {
    unsigned char ca = (unsigned char)*a;
    unsigned char cb = (unsigned char)*b;
    if (tolower(ca) != tolower(cb))
      return 0;
    ++a;
    ++b;
  }
  return *a == '\0' && *b == '\0';
}

static void trim_whitespace_inplace(char *text)
{
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

static int parse_bool_value(const char *text, int *out)
{
  if (!text || !out)
    return -1;
  if (equals_icase(text, "true") || equals_icase(text, "1") ||
      equals_icase(text, "yes") || equals_icase(text, "on"))
  {
    *out = 1;
    return 0;
  }
  if (equals_icase(text, "false") || equals_icase(text, "0") ||
      equals_icase(text, "no") || equals_icase(text, "off"))
  {
    *out = 0;
    return 0;
  }
  return -1;
}

static int is_path_absolute_simple(const char *path)
{
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
                                          const char *rel)
{
  if (!dst || dstsz == 0)
    return;
  dst[0] = '\0';
  if (!rel)
    return;
  if (is_path_absolute_simple(rel) || !base_dir || !*base_dir)
  {
    snprintf(dst, dstsz, "%s", rel);
    return;
  }
  size_t base_len = strlen(base_dir);
  int needs_sep = 1;
  if (base_len > 0)
  {
    char last = base_dir[base_len - 1];
    if (last == '/' || last == '\\')
      needs_sep = 0;
  }
  if (needs_sep)
    snprintf(dst, dstsz, "%s%c%s", base_dir, CHANCE_PATH_SEP, rel);
  else
    snprintf(dst, dstsz, "%s%s", base_dir, rel);
}

static void normalize_path_simple(char *dst, size_t dstsz, const char *src)
{
  if (!dst || dstsz == 0)
    return;
  dst[0] = '\0';
  if (!src || !*src)
    return;
#ifdef _WIN32
  if (_fullpath(dst, src, dstsz))
    return;
#else
  if (realpath(src, dst))
    return;
#endif
  snprintf(dst, dstsz, "%s", src);
}

static const char *get_cwd_path(char *buf, size_t bufsz)
{
  if (!buf || bufsz == 0)
    return NULL;
#ifdef _WIN32
  if (_getcwd(buf, (int)bufsz))
    return buf;
#else
  if (getcwd(buf, bufsz))
    return buf;
#endif
  buf[0] = '\0';
  return NULL;
}

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

static int add_override_file(const char *spec, const char *base_dir,
                             OverrideFile **list, int *count, int *cap)
{
  if (!spec || !list || !count || !cap)
    return -1;
  char orig[1024];
  char repl[1024];
  if (parse_override_spec(spec, base_dir, orig, sizeof(orig), repl, sizeof(repl)) != 0)
    return -1;
  if (*count == *cap)
  {
    int new_cap = *cap ? (*cap * 2) : 4;
    OverrideFile *grown = (OverrideFile *)realloc(
        *list, sizeof(OverrideFile) * (size_t)new_cap);
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

static const char *find_override_path(const char *input,
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

typedef struct
{
  const char ***items;
  int *count;
  int *cap;
  char ***owned_items;
  int *owned_count;
  int *owned_cap;
  char ***parallel_strings;
} ProjectInputList;

static int push_input_entry(const char *value, ProjectInputList list,
                            int make_copy)
{
  if (!value || !list.items || !list.count || !list.cap || !list.owned_items ||
      !list.owned_count || !list.owned_cap)
    return -1;
  const char **arr = *list.items;
  if (*list.count == *list.cap)
  {
    int new_cap = *list.cap ? (*list.cap * 2) : 8;
    const char **new_arr =
        (const char **)realloc((void *)arr, sizeof(char *) * (size_t)new_cap);
    if (!new_arr)
    {
      fprintf(stderr,
              "error: out of memory while growing project input list\n");
      return -1;
    }
    if (list.parallel_strings)
    {
      char **parallel = *list.parallel_strings;
      char **new_parallel =
          (char **)realloc(parallel, sizeof(char *) * (size_t)new_cap);
      if (!new_parallel)
      {
        fprintf(stderr,
                "error: out of memory while growing parallel input metadata\n");
        return -1;
      }
      for (int idx = *list.cap; idx < new_cap; ++idx)
        new_parallel[idx] = NULL;
      *list.parallel_strings = new_parallel;
    }
    *list.items = new_arr;
    arr = new_arr;
    *list.cap = new_cap;
  }
  const char *stored = value;
  if (make_copy)
  {
    char *dup = xstrdup(value);
    char **owned = *list.owned_items;
    if (*list.owned_count == *list.owned_cap)
    {
      int new_cap = *list.owned_cap ? (*list.owned_cap * 2) : 8;
      char **new_owned =
          (char **)realloc(owned, sizeof(char *) * (size_t)new_cap);
      if (!new_owned)
      {
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
  if (list.parallel_strings && *list.parallel_strings)
    (*list.parallel_strings)[*list.count] = NULL;
  (*list.count)++;
  return 0;
}

static int add_file_list_entries(const char *value, const char *project_dir,
                                 ProjectInputList list)
{
  if (!value)
    return 0;
  const char *cursor = value;
  while (*cursor)
  {
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
    if (end > start)
    {
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
                                char ***include_dirs, int *include_dir_count)
{
  if (!value || !include_dirs || !include_dir_count)
    return 0;
  const char *cursor = value;
  while (*cursor)
  {
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
    if (end > start)
    {
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

static void strip_utf8_bom(char *text)
{
  if (!text)
    return;
  unsigned char *u = (unsigned char *)text;
  if (u[0] == 0xEF && u[1] == 0xBB && u[2] == 0xBF)
    memmove(text, text + 3, strlen(text + 3) + 1);
}

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
        if (snprintf(candidate, sizeof(candidate), "%.*s/%s/%s",
                     (int)seg_len, s, subdir, filename) > 0)
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

static int parse_project_block_value(FILE *f, int *lineno,
                                     const char *initial_value,
                                     char *out, size_t outsz)
{
  if (!out || outsz == 0)
    return -1;
  out[0] = '\0';
  if (!initial_value)
    return 0;

  const char *start = initial_value;
  while (*start && isspace((unsigned char)*start))
    ++start;
  if (*start != '{')
  {
    snprintf(out, outsz, "%s", initial_value);
    return 0;
  }

  start++;
  while (*start && isspace((unsigned char)*start))
    ++start;
  size_t len = 0;
  const char *end = strchr(start, '}');
  if (end)
  {
    const char *after_end = end + 1;
    while (*after_end && isspace((unsigned char)*after_end))
      ++after_end;
    if (*after_end == '\0')
    {
      size_t chunk = (size_t)(end - start);
      if (chunk >= outsz)
        chunk = outsz - 1;
      memcpy(out, start, chunk);
      out[chunk] = '\0';
      return 0;
    }
  }

  if (*start)
  {
    len = strnlen(start, outsz - 1);
    memcpy(out, start, len);
    out[len] = '\0';
  }

  char line[2048];
  while (fgets(line, sizeof(line), f))
  {
    if (lineno)
      (*lineno)++;
    char work[2048];
    snprintf(work, sizeof(work), "%s", line);
    char trimmed[2048];
    snprintf(trimmed, sizeof(trimmed), "%s", work);
    trim_whitespace_inplace(trimmed);
    if (strcmp(trimmed, "}") == 0)
      return 0;

    size_t seg_len = strlen(work);
    while (seg_len > 0 && (work[seg_len - 1] == '\n' || work[seg_len - 1] == '\r'))
      --seg_len;
    if (seg_len > 0)
    {
      if (len + 1 < outsz)
      {
        if (len > 0)
          out[len++] = '\n';
        size_t copy = seg_len;
        if (copy > outsz - 1 - len)
          copy = outsz - 1 - len;
        memcpy(out + len, work, copy);
        len += copy;
        out[len] = '\0';
      }
    }
  }
  return -1;
}

typedef struct
{
  const char **out;
  char **project_output_alloc;
  int *output_overridden;
  int *stop_after_ccb;
  int *stop_after_asm;
  int *no_link;
  int *emit_library;
  int *freestanding;
  int *freestanding_requested;
  int *m32;
  int *opt_level;
  int *debug_symbols;
  int *strip_metadata;
  int *strip_hard;
  int *obfuscate;
  AsmSyntax *asm_syntax;
  TargetArch *target_arch;
  const char **chancecode_backend;
  const char **chancecodec_cmd_override;
  const char **host_cc_cmd_override;
  TargetOS *target_os;
  char ***include_dirs;
  int *include_dir_count;
  const char **obj_override;
  int *implicit_voidp;
  int *implicit_void_function;
  int *implicit_sizeof;
  int *request_ast;
  int *diagnostics_only;
  OverrideFile **override_files;
  int *override_file_count;
  int *override_file_cap;
  ProjectInputList symbol_ref_ce_list;
  ProjectInputList symbol_ref_cclib_list;
} ProjectArgState;

static int project_args_push(char ***items, int *count, int *cap, const char *value)
{
  if (!items || !count || !cap || !value)
    return -1;
  if (*count == *cap)
  {
    int new_cap = *cap ? (*cap * 2) : 8;
    char **grown = (char **)realloc(*items, sizeof(char *) * (size_t)new_cap);
    if (!grown)
      return -1;
    *items = grown;
    *cap = new_cap;
  }
  (*items)[*count] = xstrdup(value);
  if (!(*items)[*count])
    return -1;
  (*count)++;
  return 0;
}

static int project_args_tokenize(const char *text, char ***out_args, int *out_count)
{
  if (!out_args || !out_count)
    return -1;
  *out_args = NULL;
  *out_count = 0;
  if (!text)
    return 0;
  int cap = 0;
  const char *p = text;
  while (*p)
  {
    while (*p && (isspace((unsigned char)*p) || *p == ','))
      ++p;
    if (!*p)
      break;
    char token[1024];
    size_t len = 0;
    char quote = 0;
    if (*p == '"' || *p == '\'')
    {
      quote = *p++;
    }
    while (*p)
    {
      if (quote)
      {
        if (*p == quote)
        {
          ++p;
          break;
        }
      }
      else if (isspace((unsigned char)*p) || *p == ',')
      {
        break;
      }
      if (*p == '\\' && p[1])
      {
        if (len + 1 < sizeof(token))
          token[len++] = p[1];
        p += 2;
        continue;
      }
      if (len + 1 < sizeof(token))
        token[len++] = *p;
      ++p;
    }
    token[len] = '\0';
    if (len > 0)
    {
      if (project_args_push(out_args, out_count, &cap, token) != 0)
        return -1;
    }
  }
  return 0;
}

static void project_args_free(char **args, int count)
{
  if (!args)
    return;
  for (int i = 0; i < count; ++i)
    free(args[i]);
  free(args);
}

static int project_args_apply(const char *proj_path, int lineno,
                              const char *project_dir, char **args, int count,
                              ProjectArgState *state)
{
  if (!state)
    return -1;

  const char *pending_output = NULL;
  for (int i = 0; i < count; ++i)
  {
    const char *arg = args[i];
    if (!arg || !*arg)
      continue;
    if (strcmp(arg, "-o") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: -o expects a path in '%s' (line %d)\n", proj_path, lineno);
        return -1;
      }
      const char *dest = args[++i];
      if (!dest || !*dest)
      {
        fprintf(stderr, "error: -o expects a path in '%s' (line %d)\n", proj_path, lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir, dest);
      if (pending_output)
        free((char *)pending_output);
      pending_output = xstrdup(resolved);
      continue;
    }
    if (strcmp(arg, "-S") == 0)
    {
      if (state->stop_after_asm)
        *state->stop_after_asm = 1;
      continue;
    }
    if (strcmp(arg, "-Sccb") == 0)
    {
      if (state->stop_after_ccb)
        *state->stop_after_ccb = 1;
      continue;
    }
    if (strcmp(arg, "-g") == 0)
    {
      if (state->debug_symbols)
        *state->debug_symbols = 1;
      continue;
    }
    if (strcmp(arg, "--strip") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      continue;
    }
    if (strcmp(arg, "--strip-hard") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      if (state->strip_hard)
        *state->strip_hard = 1;
      continue;
    }
    if (strcmp(arg, "--obfuscate") == 0)
    {
      if (state->strip_metadata)
        *state->strip_metadata = 1;
      if (state->strip_hard)
        *state->strip_hard = 1;
      if (state->obfuscate)
        *state->obfuscate = 1;
      continue;
    }
    if (strncmp(arg, "-O", 2) == 0)
    {
      const char *level_str = arg + 2;
      int level = 1;
      if (*level_str != '\0')
      {
        char *endptr = NULL;
        long parsed = strtol(level_str, &endptr, 10);
        if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 3)
        {
          fprintf(stderr,
                  "invalid optimization level '%s' in '%s' (line %d)\n",
                  arg, proj_path, lineno);
          return -1;
        }
        level = (int)parsed;
      }
      if (state->opt_level)
        *state->opt_level = level;
      continue;
    }
    if (strcmp(arg, "--no-link") == 0)
    {
      if (state->no_link)
        *state->no_link = 1;
      if (i + 1 < count && args[i + 1][0] != '-')
      {
        const char *cand = args[i + 1];
        if (is_object_file_arg(cand))
        {
          if (state->obj_override)
            *state->obj_override = xstrdup(cand);
          i++;
        }
      }
      continue;
    }
    if (strcmp(arg, "-c") == 0)
    {
      if (state->no_link)
        *state->no_link = 1;
      if (i + 1 < count && args[i + 1][0] != '-')
      {
        const char *cand = args[i + 1];
        if (is_object_file_arg(cand))
        {
          if (state->obj_override)
            *state->obj_override = xstrdup(cand);
          i++;
        }
        else if (is_ce_source_arg(cand))
        {
          fprintf(stderr,
                  "error: -c in project args cannot list .ce inputs; use ce= instead (line %d)\n",
                  lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "--asm-syntax") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: --asm-syntax expects a value in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *val = args[++i];
      if (state->asm_syntax)
      {
        if (strcmp(val, "intel") == 0)
          *state->asm_syntax = ASM_INTEL;
        else if (strcmp(val, "att") == 0)
          *state->asm_syntax = ASM_ATT;
        else if (strcmp(val, "nasm") == 0)
          *state->asm_syntax = ASM_NASM;
        else
        {
          fprintf(stderr, "Unknown asm syntax '%s' in '%s' (line %d)\n",
                  val, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "--chancecodec") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: --chancecodec expects a path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (state->chancecodec_cmd_override)
        *state->chancecodec_cmd_override = xstrdup(args[++i]);
      else
        i++;
      continue;
    }
    if (strcmp(arg, "--cc") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: --cc expects a path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      if (state->host_cc_cmd_override)
        *state->host_cc_cmd_override = xstrdup(args[++i]);
      else
        i++;
      continue;
    }
    if (strcmp(arg, "--library") == 0)
    {
      if (state->emit_library)
        *state->emit_library = 1;
      continue;
    }
    if (strcmp(arg, "--freestanding") == 0)
    {
      if (state->freestanding)
        *state->freestanding = 1;
      if (state->freestanding_requested)
        *state->freestanding_requested = 1;
      continue;
    }
    if (strcmp(arg, "-x86") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_X86;
      if (state->chancecode_backend)
        *state->chancecode_backend = "x86-gas";
      continue;
    }
    if (strcmp(arg, "-arm64") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_ARM64;
      if (state->chancecode_backend)
        *state->chancecode_backend = NULL;
      continue;
    }
    if (strcmp(arg, "-bslash") == 0)
    {
      if (state->target_arch)
        *state->target_arch = ARCH_BSLASH;
      if (state->chancecode_backend)
        *state->chancecode_backend = "bslash";
      if (state->stop_after_asm)
        *state->stop_after_asm = 1;
      continue;
    }
    if (strcmp(arg, "-m32") == 0)
    {
      if (state->m32)
        *state->m32 = 1;
      continue;
    }
    if (strcmp(arg, "-m64") == 0)
    {
      if (state->m32)
        *state->m32 = 0;
      continue;
    }
    if (strcmp(arg, "--target-os") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: --target-os expects a value in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *os = args[++i];
      if (state->target_os)
      {
        if (strcmp(os, "windows") == 0)
          *state->target_os = OS_WINDOWS;
        else if (strcmp(os, "linux") == 0)
          *state->target_os = OS_LINUX;
        else if (strcmp(os, "macos") == 0)
          *state->target_os = OS_MACOS;
        else
        {
          fprintf(stderr,
                  "Unknown --target-os '%s' in '%s' (line %d)\n",
                  os, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strcmp(arg, "-I") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr, "error: -I expects a directory in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    args[++i]);
      chance_add_include_dir(state->include_dirs, state->include_dir_count,
                             resolved);
      continue;
    }
    if (strcmp(arg, "-Nno-formatting") == 0)
    {
      parser_set_disable_formatting_notes(1);
      continue;
    }
    if (strcmp(arg, "-vd") == 0 || strcmp(arg, "--verbose-deep") == 0)
    {
      compiler_verbose_set_mode(1);
      compiler_verbose_set_deep(1);
      continue;
    }
    if (strcmp(arg, "-v") == 0 || strcmp(arg, "--verbose") == 0)
    {
      compiler_verbose_set_mode(1);
      continue;
    }
    if (strcmp(arg, "--no-ansi") == 0)
    {
      diag_set_use_ansi(0);
      verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(arg, "--data-log") == 0)
    {
      diag_set_data_log(1);
      diag_set_use_ansi(0);
      verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(arg, "--request-ast") == 0)
    {
      if (state->request_ast)
        *state->request_ast = 1;
      continue;
    }
    if (strcmp(arg, "--diagnostics-only") == 0 || strcmp(arg, "--diag-only") == 0)
    {
      if (state->diagnostics_only)
        *state->diagnostics_only = 1;
      continue;
    }
    if (strcmp(arg, "--override-file") == 0)
    {
      if (i + 1 >= count)
      {
        fprintf(stderr,
                "error: --override-file expects <original>=<override> in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      const char *spec = args[++i];
      if (state->override_files && state->override_file_count && state->override_file_cap)
      {
        if (add_override_file(spec, project_dir,
                              state->override_files, state->override_file_count,
                              state->override_file_cap) != 0)
        {
          fprintf(stderr,
                  "error: invalid --override-file '%s' in '%s' (line %d)\n",
                  spec, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strncmp(arg, "--override-file=", 16) == 0)
    {
      const char *spec = arg + 16;
      if (state->override_files && state->override_file_count && state->override_file_cap)
      {
        if (add_override_file(spec, project_dir,
                              state->override_files, state->override_file_count,
                              state->override_file_cap) != 0)
        {
          fprintf(stderr,
                  "error: invalid --override-file '%s' in '%s' (line %d)\n",
                  spec, proj_path, lineno);
          return -1;
        }
      }
      continue;
    }
    if (strncmp(arg, "-sr:", 4) == 0)
    {
      const char *sr_path = arg + 4;
      if (!sr_path || !*sr_path)
      {
        fprintf(stderr,
                "error: -sr: requires a .ce or .cclib path in '%s' (line %d)\n",
                proj_path, lineno);
        return -1;
      }
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    sr_path);
      if (ends_with_icase(sr_path, ".ce"))
      {
        if (push_input_entry(resolved, state->symbol_ref_ce_list, 1) != 0)
          return -1;
      }
      else if (ends_with_icase(sr_path, ".cclib"))
      {
        if (push_input_entry(resolved, state->symbol_ref_cclib_list, 1) != 0)
          return -1;
      }
      else
      {
        fprintf(stderr,
                "error: -sr: path '%s' must be a .ce or .cclib file (line %d)\n",
                sr_path, lineno);
        return -1;
      }
      continue;
    }
    if (strcmp(arg, "--implicit-voidp") == 0)
    {
      if (state->implicit_voidp)
        *state->implicit_voidp = 1;
      continue;
    }
    if (strcmp(arg, "--implicit-void-function") == 0)
    {
      if (state->implicit_void_function)
        *state->implicit_void_function = 1;
      continue;
    }
    if (strcmp(arg, "--implicit-sizeof") == 0)
    {
      if (state->implicit_sizeof)
        *state->implicit_sizeof = 1;
      continue;
    }
    if (arg[0] == '-')
    {
      fprintf(stderr,
              "error: unknown project arg '%s' in '%s' (line %d)\n",
              arg, proj_path, lineno);
      return -1;
    }
    fprintf(stderr,
            "error: unexpected token '%s' in project args for '%s' (line %d)\n",
            arg, proj_path, lineno);
    return -1;
  }

  if (pending_output)
  {
    if (state->output_overridden)
      *state->output_overridden = 1;
    if (state->project_output_alloc && state->out)
    {
      if (*state->project_output_alloc)
        free(*state->project_output_alloc);
      *state->project_output_alloc = (char *)pending_output;
      if (state->no_link && *state->no_link && state->obj_override)
        *state->obj_override = *state->project_output_alloc;
      else
        *state->out = *state->project_output_alloc;
    }
    else
    {
      free((char *)pending_output);
    }
  }
  return 0;
}

static int parse_ceproj_file(
    const char *proj_path, ProjectInputList ce_list, ProjectInputList ccb_list,
    ProjectInputList cclib_list, ProjectInputList obj_list,
    char ***include_dirs, int *include_dir_count, int *output_overridden,
    const char **out, char **project_output_alloc, TargetArch *target_arch,
    const char **chancecode_backend, int *stop_after_ccb, int *stop_after_asm,
  int *emit_library, int *no_link, int *freestanding, TargetOS *target_os,
  int *freestanding_requested, int *m32, int *opt_level, int *debug_symbols,
  int *strip_metadata, int *strip_hard, int *obfuscate,
  AsmSyntax *asm_syntax, const char **chancecodec_cmd_override,
  const char **host_cc_cmd_override, const char **obj_override,
  int *implicit_voidp, int *implicit_void_function, int *implicit_sizeof, int *request_ast,
  int *diagnostics_only,
  OverrideFile **override_files, int *override_file_count, int *override_file_cap,
  ProjectInputList symbol_ref_ce_list,
  ProjectInputList symbol_ref_cclib_list, char **after_cmd)
{
  FILE *f = fopen(proj_path, "rb");
  if (!f)
  {
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
  while (fgets(line, sizeof(line), f))
  {
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
    if (!eq)
    {
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
    if (strcmp(key, "ce") == 0 || strcmp(key, "source") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, ce_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "ccb") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, ccb_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "cclib") == 0 || strcmp(key, "library") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, cclib_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "obj") == 0 || strcmp(key, "object") == 0)
    {
      if (add_file_list_entries(value_buf, project_dir, obj_list) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "include") == 0 || strcmp(key, "includedir") == 0)
    {
      if (add_include_dir_list(value_buf, project_dir, include_dirs,
                               include_dir_count) != 0)
      {
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "output") == 0)
    {
      if (!out || !project_output_alloc || !output_overridden)
        continue;
      char resolved[1024];
      resolve_project_relative_path(resolved, sizeof(resolved), project_dir,
                                    value_buf);
      if (*project_output_alloc)
      {
        free(*project_output_alloc);
        *project_output_alloc = NULL;
      }
      *project_output_alloc = xstrdup(resolved);
      *out = *project_output_alloc;
      *output_overridden = 1;
    }
    else if (strcmp(key, "backend") == 0)
    {
      if (equals_icase(value_buf, "x86"))
      {
        if (target_arch)
          *target_arch = ARCH_X86;
        if (chancecode_backend)
          *chancecode_backend = "x86-gas";
      }
      else if (equals_icase(value_buf, "arm64"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = NULL;
      }
      else if (equals_icase(value_buf, "arm64-macos"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = "arm64-macos";
        if (target_os)
          *target_os = OS_MACOS;
      }
      else if (equals_icase(value_buf, "arm64-elf") ||
               equals_icase(value_buf, "arm64-linux"))
      {
        if (target_arch)
          *target_arch = ARCH_ARM64;
        if (chancecode_backend)
          *chancecode_backend = "arm64-elf";
        if (target_os)
          *target_os = OS_LINUX;
      }
      else if (equals_icase(value_buf, "bslash") || equals_icase(value_buf, "bslash"))
      {
        if (target_arch)
          *target_arch = ARCH_BSLASH;
        if (chancecode_backend)
          *chancecode_backend = "bslash";
        if (stop_after_asm)
          *stop_after_asm = 1;
      }
      else if (equals_icase(value_buf, "none"))
      {
        if (target_arch)
          *target_arch = ARCH_NONE;
        if (chancecode_backend)
          *chancecode_backend = NULL;
      }
      else
      {
        fprintf(stderr, "error: unknown backend '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    }
    else if (strcmp(key, "type") == 0)
    {
      if (equals_icase(value_buf, "library"))
      {
        if (emit_library)
          *emit_library = 1;
      }
      else if (equals_icase(value_buf, "object") || equals_icase(value_buf, "obj"))
      {
        if (no_link)
          *no_link = 1;
      }
    }
    else if (strcmp(key, "after") == 0)
    {
      if (after_cmd)
      {
        char block[4096];
        if (parse_project_block_value(f, &lineno, value_buf, block, sizeof(block)) != 0)
        {
          fprintf(stderr, "error: unterminated after block in '%s' (line %d)\n",
                  proj_path, lineno);
          rc = -1;
          break;
        }
        if (*after_cmd)
        {
          free(*after_cmd);
          *after_cmd = NULL;
        }
        trim_whitespace_inplace(block);
        *after_cmd = block[0] ? xstrdup(block) : NULL;
      }
    }
    else if (strcmp(key, "args") == 0 || strcmp(key, "control_args") == 0 ||
             strcmp(key, "control-args") == 0)
    {
      ProjectArgState state = {
          .out = out,
          .project_output_alloc = project_output_alloc,
          .output_overridden = output_overridden,
          .stop_after_ccb = stop_after_ccb,
          .stop_after_asm = stop_after_asm,
          .no_link = no_link,
          .emit_library = emit_library,
          .freestanding = freestanding,
          .freestanding_requested = freestanding_requested,
          .m32 = m32,
          .opt_level = opt_level,
          .debug_symbols = debug_symbols,
          .strip_metadata = strip_metadata,
          .strip_hard = strip_hard,
          .obfuscate = obfuscate,
          .asm_syntax = asm_syntax,
          .target_arch = target_arch,
          .chancecode_backend = chancecode_backend,
          .chancecodec_cmd_override = chancecodec_cmd_override,
          .host_cc_cmd_override = host_cc_cmd_override,
          .target_os = target_os,
          .include_dirs = include_dirs,
          .include_dir_count = include_dir_count,
          .obj_override = obj_override,
          .implicit_voidp = implicit_voidp,
          .implicit_void_function = implicit_void_function,
          .implicit_sizeof = implicit_sizeof,
          .request_ast = request_ast,
          .diagnostics_only = diagnostics_only,
          .override_files = override_files,
          .override_file_count = override_file_count,
          .override_file_cap = override_file_cap,
          .symbol_ref_ce_list = symbol_ref_ce_list,
          .symbol_ref_cclib_list = symbol_ref_cclib_list};

      char **args = NULL;
      int arg_count = 0;
      if (project_args_tokenize(value_buf, &args, &arg_count) != 0)
      {
        fprintf(stderr, "error: failed to parse args in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (project_args_apply(proj_path, lineno, project_dir, args, arg_count,
                             &state) != 0)
      {
        project_args_free(args, arg_count);
        rc = -1;
        break;
      }
      project_args_free(args, arg_count);
    }
    else if (strcmp(key, "stop_after") == 0 ||
             strcmp(key, "stopafter") == 0)
    {
      if ((!stop_after_asm || !*stop_after_asm) &&
          (!stop_after_ccb || !*stop_after_ccb))
      {
        if (equals_icase(value_buf, "ccb"))
        {
          if (stop_after_ccb)
            *stop_after_ccb = 1;
        }
        else if (equals_icase(value_buf, "asm"))
        {
          if (stop_after_asm)
            *stop_after_asm = 1;
        }
      }
    }
    else if (strcmp(key, "no_link") == 0 || strcmp(key, "nolink") == 0)
    {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0)
      {
        fprintf(stderr,
                "error: expected boolean for no_link in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (no_link)
        *no_link = val;
    }
    else if (strcmp(key, "freestanding") == 0)
    {
      int val = 0;
      if (parse_bool_value(value_buf, &val) != 0)
      {
        fprintf(stderr,
                "error: expected boolean for freestanding in '%s' (line %d)\n",
                proj_path, lineno);
        rc = -1;
        break;
      }
      if (freestanding)
        *freestanding = val;
    }
    else if (strcmp(key, "target_os") == 0 || strcmp(key, "targetos") == 0)
    {
      if (equals_icase(value_buf, "windows"))
      {
        if (target_os)
          *target_os = OS_WINDOWS;
      }
      else if (equals_icase(value_buf, "linux"))
      {
        if (target_os)
          *target_os = OS_LINUX;
      }
      else if (equals_icase(value_buf, "macos"))
      {
        if (target_os)
          *target_os = OS_MACOS;
      }
      else
      {
        fprintf(stderr, "error: unknown target_os '%s' in '%s' (line %d)\n",
                value_buf, proj_path, lineno);
        rc = -1;
        break;
      }
    }
    else
    {
      fprintf(stderr,
              "warning: ignoring unrecognized key '%s' in '%s' (line %d)\n",
              key, proj_path, lineno);
    }
  }
  fclose(f);
  if (no_link && *no_link && output_overridden && *output_overridden &&
      obj_override && out && *out)
  {
    if (!*obj_override)
      *obj_override = *out;
  }
  return rc;
}

static int path_has_separator(const char *name)
{
  if (!name)
    return 0;
  for (const char *p = name; *p; ++p)
  {
    if (*p == '/' || *p == '\\')
      return 1;
  }
  return 0;
}

static int create_directory_checked(const char *path, int fail_if_exists)
{
  if (!path || !*path)
    return -1;
  struct stat st;
  if (stat(path, &st) == 0)
  {
#if defined(S_ISDIR)
    int is_dir = S_ISDIR(st.st_mode) != 0;
#else
    int is_dir = (st.st_mode & _S_IFDIR) != 0;
#endif
    if (!is_dir)
    {
      fprintf(stderr,
              "error: path '%s' already exists and is not a directory\n", path);
      return -1;
    }
    if (fail_if_exists)
    {
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

static int write_text_file(const char *path, const char *content)
{
  if (!path || !content)
    return -1;
  FILE *f = fopen(path, "wb");
  if (!f)
  {
    fprintf(stderr, "error: failed to write '%s': %s\n", path, strerror(errno));
    return -1;
  }
  size_t len = strlen(content);
  if (fwrite(content, 1, len, f) != len)
  {
    fprintf(stderr, "error: failed to write '%s': %s\n", path, strerror(errno));
    fclose(f);
    return -1;
  }
  fclose(f);
  return 0;
}

static const char *derive_module_name_from_path(const char *path, char *buffer, size_t bufsz)
{
  if (!path || !buffer || bufsz == 0)
    return NULL;
  split_path(path, NULL, 0, buffer, bufsz);
  if (!buffer[0])
    return NULL;
  return buffer;
}

typedef struct
{
  char **items;
  size_t count;
  size_t capacity;
} StripSymbolList;

static void strip_symbol_list_destroy(StripSymbolList *list)
{
  if (!list)
    return;
  for (size_t i = 0; i < list->count; ++i)
  {
    free(list->items[i]);
    list->items[i] = NULL;
  }
  free(list->items);
  list->items = NULL;
  list->count = 0;
  list->capacity = 0;
}

static int strip_symbol_list_reserve(StripSymbolList *list, size_t desired)
{
  if (!list)
    return 0;
  if (list->capacity >= desired)
    return 0;
  size_t new_cap = list->capacity ? list->capacity * 2 : 16;
  while (new_cap < desired)
    new_cap *= 2;
  char **grown = (char **)realloc(list->items, new_cap * sizeof(char *));
  if (!grown)
    return ENOMEM;
  list->items = grown;
  list->capacity = new_cap;
  return 0;
}

static int strip_symbol_list_add(StripSymbolList *list, const char *name)
{
  if (!list || !name || !*name)
    return 0;
  for (size_t i = 0; i < list->count; ++i)
  {
    if (strcmp(list->items[i], name) == 0)
      return 0;
  }
  if (strip_symbol_list_reserve(list, list->count + 1) != 0)
    return ENOMEM;
  char *copy = xstrdup(name);
  if (!copy)
    return ENOMEM;
  list->items[list->count++] = copy;
  return 0;
}

static int strip_symbol_name_cmp(const void *a, const void *b)
{
  const char *const *sa = (const char *const *)a;
  const char *const *sb = (const char *const *)b;
  if (!sa || !sb || !*sa || !*sb)
    return 0;
  return strcmp(*sa, *sb);
}

static void strip_symbol_list_sort(StripSymbolList *list)
{
  if (!list || list->count <= 1 || !list->items)
    return;
  qsort(list->items, list->count, sizeof(char *), strip_symbol_name_cmp);
}

static const char *node_backend_name(const Node *n)
{
  if (!n)
    return NULL;
  if (n->metadata.backend_name && *n->metadata.backend_name)
    return n->metadata.backend_name;
  if (n->kind == ND_FUNC)
    return n->name;
  if (n->kind == ND_VAR_DECL)
    return n->var_name;
  return NULL;
}

static uint32_t hash_strip_symbol_name(const char *name)
{
  if (!name)
    return 0;
  uint32_t hash = 2166136261u;
  while (*name)
  {
    hash ^= (uint8_t)(*name++);
    hash *= 16777619u;
  }
  return hash;
}

static int collect_strip_symbols_from_unit(const Node *unit,
                                           StripSymbolList *symbols)
{
  if (!unit || unit->kind != ND_UNIT || !symbols)
    return 0;
  const char *module_full_name =
      (unit->module_path.full_name && unit->module_path.full_name[0])
          ? unit->module_path.full_name
          : NULL;
  for (int i = 0; i < unit->stmt_count; ++i)
  {
    const Node *stmt = unit->stmts ? unit->stmts[i] : NULL;
    if (!stmt)
      continue;
    if (stmt->kind == ND_FUNC)
    {
      const Node *fn = stmt;
      const char *name = node_backend_name(fn);
      char *generated = NULL;
      if ((!fn->metadata.backend_name || !fn->metadata.backend_name[0]) &&
          fn->name && *fn->name)
      {
        if (module_full_name && *module_full_name)
          generated = module_backend_name(module_full_name, fn->name, fn);
        else
          generated = append_param_signature(fn->name, fn);
        if (generated)
          name = generated;
      }
      if (name && *name)
      {
        if (strip_symbol_list_add(symbols, name) != 0)
        {
          free(generated);
          return 1;
        }
      }
      free(generated);
      continue;
    }
    if (stmt->kind == ND_VAR_DECL && stmt->var_is_global)
    {
      const char *name = node_backend_name(stmt);
      char *generated = NULL;
      if (module_full_name && *module_full_name && !stmt->export_name &&
          (!stmt->metadata.backend_name || !stmt->metadata.backend_name[0]) &&
          stmt->var_name && *stmt->var_name)
      {
        generated = module_backend_name(module_full_name, stmt->var_name, NULL);
        if (generated)
          name = generated;
      }
      if (name && *name)
      {
        if (strip_symbol_list_add(symbols, name) != 0)
        {
          free(generated);
          return 1;
        }
      }
      free(generated);
    }
  }
  return 0;
}

static int collect_strip_symbols_from_ccb(const char *path,
                                          StripSymbolList *symbols)
{
  if (!path || !symbols)
    return 0;
  FILE *f = fopen(path, "rb");
  if (!f)
  {
    fprintf(stderr, "error: failed to open '%s' while preparing --strip-hard map (%s)\n",
            path, strerror(errno));
    return 1;
  }
  char line[1024];
  while (fgets(line, sizeof(line), f))
  {
    char *p = line;
    while (*p && isspace((unsigned char)*p))
      ++p;
    if (*p == '\0' || *p == ';' || *p == '#')
      continue;
    int is_func = 0;
    int is_global = 0;
    if (strncmp(p, ".func", 5) == 0 && isspace((unsigned char)p[5]))
    {
      is_func = 1;
      p += 5;
    }
    else if (strncmp(p, ".global", 7) == 0 && isspace((unsigned char)p[7]))
    {
      is_global = 1;
      p += 7;
    }
    else
    {
      continue;
    }
    (void)is_func;
    (void)is_global;
    while (*p && isspace((unsigned char)*p))
      ++p;
    if (*p == '\0')
      continue;
    char name_buf[512];
    size_t ni = 0;
    while (*p && !isspace((unsigned char)*p) && ni + 1 < sizeof(name_buf))
      name_buf[ni++] = *p++;
    name_buf[ni] = '\0';
    if (!name_buf[0])
      continue;
    if (strip_symbol_list_add(symbols, name_buf) != 0)
    {
      fclose(f);
      return 1;
    }
  }
  fclose(f);
  return 0;
}

static int write_strip_symbol_map(const StripSymbolList *symbols,
                                  const char *path)
{
  if (!symbols || !path || !*path)
    return 0;
  FILE *out = fopen(path, "wb");
  if (!out)
  {
    fprintf(stderr, "error: failed to write strip map '%s': %s\n", path,
            strerror(errno));
    return 1;
  }
  for (size_t i = 0; i < symbols->count; ++i)
  {
    const char *sym = symbols->items[i];
    uint32_t hash = hash_strip_symbol_name(sym);
    char alias[64];
    snprintf(alias, sizeof(alias), "_data_cc_%08x_%zu", hash, i);
    fprintf(out, "%s %s\n", sym ? sym : "", alias);
  }
  fclose(out);
  return 0;
}

static void choose_strip_map_path(char *buffer, size_t bufsz)
{
  if (!buffer || bufsz == 0)
    return;
  buffer[0] = '\0';
#ifdef _WIN32
  const char *tmp = getenv("TEMP");
  if (!tmp || !*tmp)
    tmp = getenv("TMP");
  if (!tmp || !*tmp)
    tmp = ".";
  long pid = _getpid();
  snprintf(buffer, bufsz, "%s%cchancec_strip_%ld.map", tmp, CHANCE_PATH_SEP,
           pid);
#else
  const char *tmp = getenv("TMPDIR");
  if (!tmp || !*tmp)
    tmp = "/tmp";
  long pid = (long)getpid();
  snprintf(buffer, bufsz, "%s/chancec_strip_%ld.map", tmp, pid);
#endif
}

static void build_chancecodec_display_cmd(char *buffer, size_t bufsz,
                                          const char *cmd, const char *backend,
                                          int opt_level, int strip_metadata,
                                          int strip_hard, int obfuscate,
                                          const char *strip_map_path,
                                          int debug_symbols,
                                          const char *target_os_option,
                                          const char *output_path,
                                          const char *input_path)
{
  if (!buffer || bufsz == 0)
    return;
  buffer[0] = '\0';
  if (!cmd)
    cmd = "chancecodec";
  if (!backend)
    backend = "<unset>";
  snprintf(buffer, bufsz, "\"%s\" --backend %s", cmd, backend);
  if (strip_metadata)
    strncat(buffer, " --strip", bufsz - strlen(buffer) - 1);
  if (strip_hard)
    strncat(buffer, " --strip-hard", bufsz - strlen(buffer) - 1);
  if (obfuscate)
    strncat(buffer, " --obfuscate", bufsz - strlen(buffer) - 1);
  if (strip_map_path && *strip_map_path)
  {
    char stripmapbuf[STRIP_MAP_PATH_MAX + 32];
    snprintf(stripmapbuf, sizeof(stripmapbuf), " --option strip-map=%s",
             strip_map_path);
    strncat(buffer, stripmapbuf, bufsz - strlen(buffer) - 1);
  }
  if (opt_level > 0)
  {
    char optbuf[16];
    snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
    strncat(buffer, optbuf, bufsz - strlen(buffer) - 1);
  }
  if (output_path && *output_path)
  {
    char outbuf[2048];
    snprintf(outbuf, sizeof(outbuf), " --output \"%s\"", output_path);
    strncat(buffer, outbuf, bufsz - strlen(buffer) - 1);
  }
  if (target_os_option && *target_os_option)
  {
    char targetbuf[128];
    snprintf(targetbuf, sizeof(targetbuf), " --option target-os=%s",
             target_os_option);
    strncat(buffer, targetbuf, bufsz - strlen(buffer) - 1);
  }
  if (debug_symbols)
    strncat(buffer, " --option debug=1", bufsz - strlen(buffer) - 1);
  if (input_path && *input_path)
  {
    char inbuf[2048];
    snprintf(inbuf, sizeof(inbuf), " \"%s\"", input_path);
    strncat(buffer, inbuf, bufsz - strlen(buffer) - 1);
  }
}

static int handle_new_command(int argc, char **argv)
{
  if (argc < 2)
  {
    fprintf(stderr, "Usage: chancec new <Template> [ProjectName]\n");
    return 2;
  }
  if (!equals_icase(argv[0], "new"))
  {
    fprintf(stderr, "error: internal new-command usage mismatch\n");
    return 2;
  }
  const char *template_name = argv[1];
  if (equals_icase(template_name, "--help") ||
      equals_icase(template_name, "-h"))
  {
    fprintf(stderr, "Usage: chancec new Console [ProjectName]\n");
    fprintf(stderr, "Creates a new Chance project in a directory matching the "
                    "project name.\n");
    return 0;
  }
  if (!equals_icase(template_name, "console"))
  {
    fprintf(stderr,
            "error: unknown template '%s'. Supported templates: Console\n",
            template_name);
    return 2;
  }
  if (argc > 3)
  {
    fprintf(stderr, "error: too many arguments for 'chancec new'. Usage: "
                    "chancec new Console [ProjectName]\n");
    return 2;
  }
  const char *project_name =
      (argc >= 3 && argv[2] && argv[2][0]) ? argv[2] : "ConsoleApp";
  if (strcmp(project_name, ".") == 0 || strcmp(project_name, "..") == 0)
  {
    fprintf(stderr, "error: project name '%s' is not allowed\n", project_name);
    return 2;
  }
  if (path_has_separator(project_name))
  {
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

typedef struct LibraryFunction
{
  char *name;
  char *backend_name;
  char *return_spec;
  char **param_specs;
  int param_count;
  int is_varargs;
  int is_noreturn;
  int is_exposed;
} LibraryFunction;

typedef struct LibraryGlobal
{
  char *name;
  char *type_spec;
  int is_const;
} LibraryGlobal;

typedef struct LibraryStruct
{
  char *name;
  char **field_names;
  char **field_specs;
  uint32_t *field_offsets;
  uint32_t field_count;
  uint32_t size_bytes;
  int is_exposed;
} LibraryStruct;

typedef struct LibraryEnum
{
  char *name;
  char **value_names;
  int32_t *values;
  uint32_t value_count;
  int is_exposed;
} LibraryEnum;

typedef struct LibraryModuleData
{
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

struct LoadedLibraryFunction
{
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
};

struct LoadedLibrary
{
  char *path;
  CclibFile file;
  Type **allocated_types;
  int allocated_type_count;
  int allocated_type_cap;
  char **ccbin_temp_paths;
};

static int collect_strip_symbols_from_cclib_module(const CclibModule *module,
                                                   StripSymbolList *symbols)
{
  if (!module || !symbols)
    return 0;
  const char *module_name =
      (module->module_name && module->module_name[0]) ? module->module_name
                                                      : NULL;
  for (uint32_t fi = 0; fi < module->function_count; ++fi)
  {
    const CclibFunction *fn = &module->functions[fi];
    if (!fn->name && !fn->backend_name)
      continue;
    const char *name = NULL;
    char *generated = NULL;
    if (fn->backend_name && fn->backend_name[0])
    {
      name = fn->backend_name;
    }
    else if (module_name && module_name[0] && fn->name && fn->name[0])
    {
      generated = make_backend_name_from_module(module_name, fn->name);
      name = generated;
    }
    else if (fn->name && fn->name[0])
    {
      name = fn->name;
    }
    if (name && *name)
    {
      if (strip_symbol_list_add(symbols, name) != 0)
      {
        free(generated);
        return 1;
      }
    }
    free(generated);
  }
  for (uint32_t gi = 0; gi < module->global_count; ++gi)
  {
    const CclibGlobal *glob = &module->globals[gi];
    if (!glob->name)
      continue;
    if (strip_symbol_list_add(symbols, glob->name) != 0)
      return 1;
  }
  return 0;
}

static int collect_strip_symbols_from_loaded_libraries(const LoadedLibrary *libs,
                                                       int lib_count,
                                                       StripSymbolList *symbols)
{
  if (!libs || lib_count <= 0 || !symbols)
    return 0;
  for (int li = 0; li < lib_count; ++li)
  {
    const LoadedLibrary *lib = &libs[li];
    if (!lib)
      continue;
    for (uint32_t mi = 0; mi < lib->file.module_count; ++mi)
    {
      if (collect_strip_symbols_from_cclib_module(&lib->file.modules[mi],
                                                  symbols))
        return 1;
    }
  }
  return 0;
}

static int collect_strip_symbols_from_library_functions(
    const LoadedLibraryFunction *funcs, int func_count,
    StripSymbolList *symbols)
{
  if (!funcs || func_count <= 0 || !symbols)
    return 0;
  for (int i = 0; i < func_count; ++i)
  {
    const LoadedLibraryFunction *fn = &funcs[i];
    if (!fn->backend_name)
      continue;
    if (strip_symbol_list_add(symbols, fn->backend_name) != 0)
      return 1;
  }
  return 0;
}

static int build_strip_symbol_map_file(const UnitCompile *units,
                                       int unit_count,
                                       const char **ccb_inputs,
                                       int ccb_count,
                                       const LoadedLibrary *libraries,
                                       int library_count,
                                       const LoadedLibraryFunction *lib_funcs,
                                       int lib_func_count,
                                       const char *map_path)
{
  if (!map_path || !*map_path)
    return 0;
  StripSymbolList symbols = {0};
  for (int i = 0; i < unit_count; ++i)
  {
    if (collect_strip_symbols_from_unit(units[i].unit, &symbols))
    {
      strip_symbol_list_destroy(&symbols);
      return 1;
    }
  }
  for (int ci = 0; ci < ccb_count; ++ci)
  {
    if (collect_strip_symbols_from_ccb(ccb_inputs[ci], &symbols))
    {
      strip_symbol_list_destroy(&symbols);
      return 1;
    }
  }
  if (collect_strip_symbols_from_loaded_libraries(libraries, library_count,
                                                  &symbols))
  {
    strip_symbol_list_destroy(&symbols);
    return 1;
  }
  if (collect_strip_symbols_from_library_functions(lib_funcs, lib_func_count,
                                                   &symbols))
  {
    strip_symbol_list_destroy(&symbols);
    return 1;
  }
  strip_symbol_list_sort(&symbols);
  int rc = write_strip_symbol_map(&symbols, map_path);
  strip_symbol_list_destroy(&symbols);
  return rc;
}

static char *dup_string_or_null(const char *s)
{
  if (!s)
    return NULL;
  return xstrdup(s);
}

static char *format_string(const char *fmt, ...)
{
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

static char *module_name_to_prefix_copy(const char *module_full)
{
  if (!module_full || !*module_full)
    return NULL;
  size_t len = strlen(module_full);
  char *copy = (char *)xmalloc(len + 1);
  memcpy(copy, module_full, len + 1);
  for (size_t i = 0; i < len; ++i)
  {
    if (copy[i] == '.')
      copy[i] = '_';
  }
  return copy;
}

static char *make_backend_name_from_module(const char *module_full,
                                           const char *fn_name)
{
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
                                          const char *fn_name)
{
  if (!module_full || !fn_name)
    return NULL;
  return format_string("%s.%s", module_full, fn_name);
}

static int read_file_bytes(const char *path, uint8_t **out_data,
                           size_t *out_size)
{
  if (!path || !out_data || !out_size)
    return 1;
  *out_data = NULL;
  *out_size = 0;
  FILE *f = fopen(path, "rb");
  if (!f)
    return errno ? errno : EIO;
  if (fseek(f, 0, SEEK_END) != 0)
  {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  long sz = ftell(f);
  if (sz < 0)
  {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  if (fseek(f, 0, SEEK_SET) != 0)
  {
    int err = errno ? errno : EIO;
    fclose(f);
    return err;
  }
  uint8_t *buf = (uint8_t *)malloc((size_t)sz);
  if (!buf)
  {
    fclose(f);
    return ENOMEM;
  }
  if (sz > 0)
  {
    size_t read = fread(buf, 1, (size_t)sz, f);
    if (read != (size_t)sz)
    {
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
                            size_t size)
{
  if (!path)
    return EINVAL;
  FILE *f = fopen(path, "wb");
  if (!f)
    return errno ? errno : EIO;
  if (size > 0 && data)
  {
    size_t written = fwrite(data, 1, size, f);
    if (written != size)
    {
      int err = ferror(f) ? errno : EIO;
      fclose(f);
      remove(path);
      return err ? err : EIO;
    }
  }
  if (fclose(f) != 0)
  {
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

static char *type_to_spec(Type *ty)
{
  if (!ty)
    return xstrdup("void");

  ty = module_registry_canonical_type(ty);
  if (!ty)
    return xstrdup("void");

  switch (ty->kind)
  {
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
  case TY_PTR:
  {
    char *inner = type_to_spec(ty->pointee);
    char *res = format_string("ptr(%s)", inner ? inner : "void");
    free(inner);
    return res;
  }
  case TY_ARRAY:
  {
    char *inner = type_to_spec(ty->array.elem);
    int length = ty->array.is_unsized ? -1 : ty->array.length;
    char *res = format_string("arr(%d,%s)", length, inner ? inner : "void");
    free(inner);
    return res;
  }
  case TY_STRUCT:
  {
    const char *module = module_registry_find_struct_module(ty);
    const char *name = ty->struct_name ? ty->struct_name : "";
    if (!module)
      module = "";
    return format_string("struct(%s,%s)", module, name);
  }
  case TY_IMPORT:
  {
    const char *module = ty->import_module ? ty->import_module : "";
    const char *name = ty->import_type_name ? ty->import_type_name : "";
    return format_string("import(%s,%s)", module, name);
  }
  default:
    break;
  }
  return xstrdup("void");
}

static Type *spec_to_type(const char *spec)
{
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

  if (strncmp(spec, "ptr(", 4) == 0)
  {
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

  if (strncmp(spec, "arr(", 4) == 0)
  {
    size_t len = strlen(spec);
    if (len < 7 || spec[len - 1] != ')')
      return &builtin_ty_void;
    const char *inner = spec + 4;
    const char *end = spec + len - 1;
    int depth = 0;
    const char *comma = NULL;
    for (const char *p = inner; p < end; ++p)
    {
      if (*p == '(')
        depth++;
      else if (*p == ')')
        depth--;
      else if (*p == ',' && depth == 0)
      {
        comma = p;
        break;
      }
    }
    if (!comma)
      return &builtin_ty_void;
    size_t len_len = (size_t)(comma - inner);
    char *len_text = (char *)xmalloc(len_len + 1);
    memcpy(len_text, inner, len_len);
    len_text[len_len] = '\0';
    char *endptr = NULL;
    long length = strtol(len_text, &endptr, 10);
    free(len_text);
    if (!endptr || *endptr != '\0')
      return &builtin_ty_void;
    const char *inner_spec = comma + 1;
    size_t inner_len = (size_t)(end - inner_spec);
    char *inner_text = (char *)xmalloc(inner_len + 1);
    memcpy(inner_text, inner_spec, inner_len);
    inner_text[inner_len] = '\0';
    Type *inner_type = spec_to_type(inner_text);
    free(inner_text);
    return type_array(inner_type, (int)length);
  }

  if (strncmp(spec, "struct(", 7) == 0 || strncmp(spec, "import(", 7) == 0)
  {
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
    if (resolved)
    {
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
                                             const char *module_name)
{
  if (!module_name || !*module_name)
    return NULL;
  for (int i = 0; i < *module_count; ++i)
  {
    if (strcmp((*modules)[i].module_name, module_name) == 0)
      return &(*modules)[i];
  }
  if (*module_count == *module_cap)
  {
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
                                   const LibraryFunction *fn)
{
  if (!mod || !fn)
    return 1;
  if (mod->function_count == mod->function_cap)
  {
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
                                 const LibraryGlobal *gl)
{
  if (!mod || !gl)
    return 1;
  if (mod->global_count == mod->global_cap)
  {
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
                                 const LibraryStruct *st)
{
  if (!mod || !st)
    return 1;
  if (mod->struct_count == mod->struct_cap)
  {
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

static int append_library_enum(LibraryModuleData *mod, const LibraryEnum *en)
{
  if (!mod || !en)
    return 1;
  if (mod->enum_count == mod->enum_cap)
  {
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

static void free_library_function(LibraryFunction *fn)
{
  if (!fn)
    return;
  free(fn->name);
  free(fn->backend_name);
  free(fn->return_spec);
  if (fn->param_specs)
  {
    for (int i = 0; i < fn->param_count; ++i)
      free(fn->param_specs[i]);
    free(fn->param_specs);
  }
  memset(fn, 0, sizeof(*fn));
}

static void free_library_global(LibraryGlobal *gl)
{
  if (!gl)
    return;
  free(gl->name);
  free(gl->type_spec);
  memset(gl, 0, sizeof(*gl));
}

static void free_library_struct(LibraryStruct *st)
{
  if (!st)
    return;
  free(st->name);
  if (st->field_names)
  {
    for (uint32_t i = 0; i < st->field_count; ++i)
      free(st->field_names[i]);
    free(st->field_names);
  }
  if (st->field_specs)
  {
    for (uint32_t i = 0; i < st->field_count; ++i)
      free(st->field_specs[i]);
    free(st->field_specs);
  }
  free(st->field_offsets);
  memset(st, 0, sizeof(*st));
}

static void free_library_enum(LibraryEnum *en)
{
  if (!en)
    return;
  free(en->name);
  if (en->value_names)
  {
    for (uint32_t i = 0; i < en->value_count; ++i)
      free(en->value_names[i]);
    free(en->value_names);
  }
  free(en->values);
  memset(en, 0, sizeof(*en));
}

static void free_library_module(LibraryModuleData *mod)
{
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

static void free_library_module_array(LibraryModuleData *mods, int count)
{
  if (!mods)
    return;
  for (int i = 0; i < count; ++i)
    free_library_module(&mods[i]);
  free(mods);
}

static int library_module_has_function(const LibraryModuleData *mod,
                                       const char *name)
{
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->function_count; ++i)
  {
    if (mod->functions[i].name && strcmp(mod->functions[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int library_module_has_struct(const LibraryModuleData *mod,
                                     const char *name)
{
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->struct_count; ++i)
  {
    if (mod->structs[i].name && strcmp(mod->structs[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int library_module_has_enum(const LibraryModuleData *mod,
                                   const char *name)
{
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->enum_count; ++i)
  {
    if (mod->enums[i].name && strcmp(mod->enums[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int collect_functions_from_unit(const Node *unit,
                                       LibraryModuleData *mod)
{
  if (!unit || unit->kind != ND_UNIT || !mod)
    return 0;
  for (int i = 0; i < unit->stmt_count; ++i)
  {
    const Node *fn = unit->stmts[i];
    if (!fn || fn->kind != ND_FUNC)
      continue;
    if (!fn->is_exposed || !fn->name)
      continue;
    if (library_module_has_function(mod, fn->name))
      continue;
    LibraryFunction out = {0};
    out.name = xstrdup(fn->name);
    const char *backend = NULL;
    if (fn->export_name)
      backend = fn->name;
    else
      backend = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    out.backend_name = backend ? xstrdup(backend) : NULL;
    Type *ret_ty = fn->ret_type ? fn->ret_type : type_i32();
    out.return_spec = type_to_spec(ret_ty);
    out.param_count = fn->param_count;
    if (out.param_count > 0)
    {
      out.param_specs =
          (char **)xcalloc((size_t)out.param_count, sizeof(char *));
      if (!out.param_specs)
      {
        free_library_function(&out);
        return 1;
      }
      for (int pi = 0; pi < out.param_count; ++pi)
      {
        Type *pt = (fn->param_types && pi < fn->param_count)
                       ? fn->param_types[pi]
                       : NULL;
        out.param_specs[pi] = type_to_spec(pt);
        if (!out.param_specs[pi])
        {
          free_library_function(&out);
          return 1;
        }
      }
    }
    out.is_varargs = fn->is_varargs ? 1 : 0;
    out.is_noreturn = fn->is_noreturn;
    out.is_exposed = fn->is_exposed;
    if (append_library_function(mod, &out) != 0)
    {
      free_library_function(&out);
      return 1;
    }
  }
  return 0;
}

static int collect_structs_for_module(const char *module_name,
                                      LibraryModuleData *mod)
{
  if (!module_name || !mod)
    return 0;
  int total = module_registry_struct_entry_count();
  for (int i = 0; i < total; ++i)
  {
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
    if (field_count > 0)
    {
      st.field_names = (char **)xcalloc((size_t)field_count, sizeof(char *));
      st.field_specs = (char **)xcalloc((size_t)field_count, sizeof(char *));
      st.field_offsets =
          (uint32_t *)xcalloc((size_t)field_count, sizeof(uint32_t));
      if (!st.field_names || !st.field_specs || !st.field_offsets)
      {
        free_library_struct(&st);
        return 1;
      }
      for (int fi = 0; fi < field_count; ++fi)
      {
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
        if (!st.field_specs[fi])
        {
          free_library_struct(&st);
          return 1;
        }
      }
    }
    st.field_count = (uint32_t)(field_count < 0 ? 0 : field_count);
    st.size_bytes =
        ty->strct.size_bytes < 0 ? 0u : (uint32_t)ty->strct.size_bytes;
    st.is_exposed = ty->is_exposed != 0;
    if (append_library_struct(mod, &st) != 0)
    {
      free_library_struct(&st);
      return 1;
    }
  }
  return 0;
}

static int collect_enums_for_module(const char *module_name,
                                    LibraryModuleData *mod)
{
  if (!module_name || !mod)
    return 0;
  int enum_total = module_registry_enum_entry_count();
  int value_total = module_registry_enum_value_entry_count();
  for (int i = 0; i < enum_total; ++i)
  {
    const char *entry_module = module_registry_enum_entry_module(i);
    if (!entry_module || strcmp(entry_module, module_name) != 0)
      continue;
    const char *enum_name = module_registry_enum_entry_name(i);
    if (library_module_has_enum(mod, enum_name))
      continue;
    LibraryEnum en = {0};
    en.name = enum_name ? xstrdup(enum_name) : NULL;
    int count = 0;
    for (int vi = 0; vi < value_total; ++vi)
    {
      const char *value_module = module_registry_enum_value_entry_module(vi);
      const char *value_enum = module_registry_enum_value_entry_enum(vi);
      if (value_module && value_enum &&
          strcmp(value_module, module_name) == 0 &&
          strcmp(value_enum, enum_name) == 0)
        ++count;
    }
    if (count > 0)
    {
      en.value_names = (char **)xcalloc((size_t)count, sizeof(char *));
      en.values = (int32_t *)xcalloc((size_t)count, sizeof(int32_t));
      if (!en.value_names || !en.values)
      {
        free_library_enum(&en);
        return 1;
      }
      int idx = 0;
      for (int vi = 0; vi < value_total; ++vi)
      {
        const char *value_module = module_registry_enum_value_entry_module(vi);
        const char *value_enum = module_registry_enum_value_entry_enum(vi);
        if (!value_module || !value_enum ||
            strcmp(value_module, module_name) != 0 ||
            strcmp(value_enum, enum_name) != 0)
          continue;
        const char *value_name = module_registry_enum_value_entry_name(vi);
        int value = module_registry_enum_value_entry_value(vi);
        if (idx < count)
        {
          en.value_names[idx] = value_name ? xstrdup(value_name) : NULL;
          en.values[idx] = (int32_t)value;
          ++idx;
        }
      }
      en.value_count = (uint32_t)count;
    }
    en.is_exposed = 1;
    if (append_library_enum(mod, &en) != 0)
    {
      free_library_enum(&en);
      return 1;
    }
  }
  return 0;
}

static int collect_metadata_for_unit(const Node *unit, const char *ccb_path,
                                     const char *fallback_module_name,
                                     LibraryModuleData **modules,
                                     int *module_count, int *module_cap)
{
  if (!unit || unit->kind != ND_UNIT || !modules || !module_count ||
      !module_cap)
    return 0;
  const char *module_name = unit->module_path.full_name;
  if (!module_name || !*module_name)
    module_name = fallback_module_name;
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
                              int module_count)
{
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

  for (int i = 0; i < module_count && !err; ++i)
  {
    LibraryModuleData *src = &mods[i];
    CclibModule *dst = &modules[i];
    dst->module_name = src->module_name;

    int needs_ccbin = (src->function_count > 0) || (src->global_count > 0);
    if (needs_ccbin)
    {
      if (!src->ccbin_data || src->ccbin_size == 0)
      {
        err = EINVAL;
        break;
      }
      if (src->ccbin_size > UINT32_MAX)
      {
        err = ERANGE;
        break;
      }
    }

    if (src->function_count > 0)
    {
      dst->functions = (CclibFunction *)xcalloc((size_t)src->function_count,
                                                sizeof(CclibFunction));
      if (!dst->functions)
      {
        err = ENOMEM;
        break;
      }
      for (int fi = 0; fi < src->function_count; ++fi)
      {
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

    if (src->struct_count > 0)
    {
      dst->structs = (CclibStruct *)xcalloc((size_t)src->struct_count,
                                            sizeof(CclibStruct));
      if (!dst->structs)
      {
        err = ENOMEM;
        break;
      }
      for (int si = 0; si < src->struct_count; ++si)
      {
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

    if (src->enum_count > 0)
    {
      dst->enums =
          (CclibEnum *)xcalloc((size_t)src->enum_count, sizeof(CclibEnum));
      if (!dst->enums)
      {
        err = ENOMEM;
        break;
      }
      for (int ei = 0; ei < src->enum_count; ++ei)
      {
        LibraryEnum *le = &src->enums[ei];
        CclibEnum *ce = &dst->enums[ei];
        ce->name = le->name;
        if (le->value_count > 0)
        {
          ce->values = (CclibEnumValue *)xcalloc((size_t)le->value_count,
                                                 sizeof(CclibEnumValue));
          if (!ce->values)
          {
            err = ENOMEM;
            break;
          }
          for (uint32_t vi = 0; vi < le->value_count; ++vi)
          {
            ce->values[vi].name = le->value_names ? le->value_names[vi] : NULL;
            ce->values[vi].value = le->values ? le->values[vi] : 0;
          }
        }
        ce->value_count = le->value_count;
        ce->is_exposed = (uint8_t)(le->is_exposed ? 1 : 0);
      }
    }
    dst->enum_count = (uint32_t)(src->enum_count < 0 ? 0 : src->enum_count);

    if (src->global_count > 0)
    {
      dst->globals = (CclibGlobal *)xcalloc((size_t)src->global_count,
                                            sizeof(CclibGlobal));
      if (!dst->globals)
      {
        err = ENOMEM;
        break;
      }
      for (int gi = 0; gi < src->global_count; ++gi)
      {
        LibraryGlobal *lg = &src->globals[gi];
        CclibGlobal *cg = &dst->globals[gi];
        cg->name = lg->name;
        cg->type_spec = lg->type_spec;
        cg->is_const = (uint8_t)(lg->is_const ? 1 : 0);
      }
    }
    dst->global_count =
        (uint32_t)(src->global_count < 0 ? 0 : src->global_count);

    dst->ccbin_data = needs_ccbin ? src->ccbin_data : NULL;
    dst->ccbin_size = needs_ccbin ? (uint32_t)src->ccbin_size : 0u;
  }

  if (!err)
  {
    err = cclib_write(path, &file);
  }

  for (int i = 0; i < module_count; ++i)
  {
    CclibModule *dst = &modules[i];
    if (dst->functions)
      free(dst->functions);
    if (dst->structs)
      free(dst->structs);
    if (dst->enums)
    {
      for (uint32_t vi = 0; vi < dst->enum_count; ++vi)
      {
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

static int loaded_library_add_type(LoadedLibrary *lib, Type *ty)
{
  if (!lib || !ty)
    return 1;
  if (lib->allocated_type_count == lib->allocated_type_cap)
  {
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

static void free_loaded_library_type(Type *ty)
{
  if (!ty)
    return;
  if (ty->kind == TY_STRUCT)
  {
    if (ty->strct.field_names)
    {
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
  }
  else if (ty->kind == TY_IMPORT)
  {
    free((void *)ty->import_module);
    free((void *)ty->import_type_name);
    // import_resolved is not owned
  }
  free(ty);
}

static void free_loaded_library_function(LoadedLibraryFunction *fn)
{
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
                                  int *cap, const LoadedLibraryFunction *fn)
{
  if (!funcs || !count || !cap || !fn)
    return 1;
  if (*count == *cap)
  {
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
                                              const CclibModule *module)
{
  if (!lib || !module_name || !module)
    return 0;
  for (uint32_t si = 0; si < module->struct_count; ++si)
  {
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
    if (loaded_library_add_type(lib, ty))
    {
      free_loaded_library_type(ty);
      return 1;
    }
    module_registry_register_struct(module_name, ty);
    if (field_count > 0)
    {
      char **names = (char **)xcalloc((size_t)field_count, sizeof(char *));
      Type **types = (Type **)xcalloc((size_t)field_count, sizeof(Type *));
      int *offsets = (int *)xcalloc((size_t)field_count, sizeof(int));
      if (!names || !types || !offsets)
      {
        free(names);
        free(types);
        free(offsets);
        return 1;
      }
      for (int fi = 0; fi < field_count; ++fi)
      {
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
                                            const CclibModule *module)
{
  if (!lib || !module_name || !module)
    return 0;
  for (uint32_t ei = 0; ei < module->enum_count; ++ei)
  {
    const CclibEnum *en = &module->enums[ei];
    if (!en->name)
      continue;
    Type *enum_type = (Type *)xcalloc(1, sizeof(Type));
    if (!enum_type)
      return 1;
    enum_type->kind = TY_I32;
    enum_type->is_exposed = en->is_exposed;
    if (loaded_library_add_type(lib, enum_type))
    {
      free(enum_type);
      return 1;
    }
    module_registry_register_enum(module_name, en->name, enum_type);
    for (uint32_t vi = 0; vi < en->value_count; ++vi)
    {
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
    LoadedLibraryFunction **out_funcs, int *func_count, int *func_cap)
{
  if (!module || !module_name || !out_funcs || !func_count || !func_cap)
    return 0;
  for (uint32_t fi = 0; fi < module->function_count; ++fi)
  {
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
    if (!backend_src || !*backend_src)
    {
      generated_backend = make_backend_name_from_module(module_name, fn->name);
      backend_src = generated_backend;
    }
    lf.backend_name = backend_src ? xstrdup(backend_src) : NULL;
    free(generated_backend);
    lf.return_type = spec_to_type(fn->return_type);
    lf.param_count = (int)fn->param_count;
    if (lf.param_count > 0)
    {
      lf.param_types = (Type **)xcalloc((size_t)lf.param_count, sizeof(Type *));
      if (!lf.param_types)
      {
        free_loaded_library_function(&lf);
        return 1;
      }
      for (int pi = 0; pi < lf.param_count; ++pi)
      {
        const char *ps = (fn->param_types && (uint32_t)pi < fn->param_count)
                             ? fn->param_types[pi]
                             : NULL;
        lf.param_types[pi] = spec_to_type(ps);
      }
    }
    lf.is_varargs = fn->is_varargs ? 1 : 0;
    lf.is_noreturn = fn->is_noreturn ? 1 : 0;
    lf.is_exposed = fn->is_exposed ? 1 : 0;
    if (append_loaded_function(out_funcs, func_count, func_cap, &lf) != 0)
    {
      free_loaded_library_function(&lf);
      return 1;
    }
  }
  return 0;
}

static void free_loaded_library(LoadedLibrary *lib)
{
  if (!lib)
    return;
  if (lib->ccbin_temp_paths)
  {
    for (uint32_t i = 0; i < lib->file.module_count; ++i)
    {
      if (lib->ccbin_temp_paths[i])
      {
        remove(lib->ccbin_temp_paths[i]);
        free(lib->ccbin_temp_paths[i]);
      }
    }
    free(lib->ccbin_temp_paths);
  }
  if (lib->allocated_types)
  {
    for (int i = 0; i < lib->allocated_type_count; ++i)
      free_loaded_library_type(lib->allocated_types[i]);
    free(lib->allocated_types);
  }
  cclib_free(&lib->file);
  free(lib->path);
  memset(lib, 0, sizeof(*lib));
}

static int unit_has_auto_import(const Node *unit, const char *module_full)
{
  if (!unit || unit->kind != ND_UNIT || !module_full || !*module_full)
    return 0;
  if (!unit->imports || unit->import_count <= 0)
    return 0;
  for (int i = 0; i < unit->import_count; ++i)
  {
    const ModulePath *imp = &unit->imports[i];
    if (!imp || !imp->full_name)
      continue;
    if (strcmp(imp->full_name, module_full) == 0)
    {
      if (imp->alias && imp->alias[0])
        return 0;
      return 1;
    }
  }
  return 0;
}

static void symtab_add_library_functions(SemaContext *sc, Node *unit,
                                         const LoadedLibraryFunction *funcs,
                                         int func_count)
{
  if (!sc || !sc->syms || !funcs || func_count <= 0)
    return;
  SymTable *syms = sc->syms;
  for (int i = 0; i < func_count; ++i)
  {
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

    if (sym.backend_name && strcmp(sym.backend_name, sym.name) != 0)
    {
      Symbol backend_alias = sym;
      backend_alias.name = sym.backend_name;
      symtab_add(syms, backend_alias);
    }

    if (!lf->name)
      continue;
    if (!lf->module_name)
      continue;
    const char *module_full = lf->module_name;

    if (unit_has_auto_import(unit, module_full))
    {
      Symbol unqual = sym;
      unqual.name = lf->name;
      unqual.backend_name = sym.backend_name;
      sema_track_imported_function(sc, lf->name, module_full, &unqual);
    }
  }
}

static int is_stdlib_module_name(const char *module_name)
{
  if (!module_name || !*module_name)
    return 0;
  if (strcmp(module_name, "Std") == 0)
    return 1;
  return strncmp(module_name, "Std.", 4) == 0;
}

static int imported_symbol_has_name(const Symbol *list, int count,
                                    const char *name)
{
  if (!list || count <= 0 || !name || !*name)
    return 0;
  for (int i = 0; i < count; ++i)
  {
    const Symbol *sym = &list[i];
    const char *sym_name = (sym->backend_name && *sym->backend_name)
                               ? sym->backend_name
                               : sym->name;
    if (sym_name && strcmp(sym_name, name) == 0)
      return 1;
  }
  return 0;
}

static int append_imported_symbol(Symbol **list, int *count, int *cap,
                                  const Symbol *sym)
{
  if (!list || !count || !cap || !sym)
    return 1;
  if (*count == *cap)
  {
    int new_cap = *cap ? *cap * 2 : 8;
    Symbol *grown = (Symbol *)realloc(*list, (size_t)new_cap * sizeof(Symbol));
    if (!grown)
      return 1;
    *list = grown;
    *cap = new_cap;
  }
  (*list)[*count] = *sym;
  (*count)++;
  return 0;
}

static int merge_stdlib_externs_into_imported(
    Symbol **imported_syms, int *imported_count, int *imported_cap,
    const LoadedLibraryFunction *funcs, int func_count)
{
  if (!imported_syms || !imported_count || !imported_cap || !funcs ||
      func_count <= 0)
    return 0;

  for (int i = 0; i < func_count; ++i)
  {
    const LoadedLibraryFunction *lf = &funcs[i];
    if (!lf->qualified_name || !lf->module_name)
      continue;
    if (!lf->is_exposed)
      continue;
    if (!is_stdlib_module_name(lf->module_name))
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

    const char *effective = (sym.backend_name && *sym.backend_name)
                                ? sym.backend_name
                                : sym.name;
    if (effective && imported_symbol_has_name(*imported_syms, *imported_count,
                                              effective))
      continue;
    if (append_imported_symbol(imported_syms, imported_count, imported_cap,
                               &sym) != 0)
      return 1;
  }

  return 0;
}

static int merge_cert_externs_into_imported(
    Symbol **imported_syms, int *imported_count, int *imported_cap,
    const LoadedLibraryFunction *funcs, int func_count)
{
  if (!imported_syms || !imported_count || !imported_cap || !funcs ||
      func_count <= 0)
    return 0;

  for (int i = 0; i < func_count; ++i)
  {
    const LoadedLibraryFunction *lf = &funcs[i];
    if (!lf->qualified_name)
      continue;
    if (!lf->is_exposed)
      continue;

    const char *backend = lf->backend_name ? lf->backend_name : lf->qualified_name;
    const char *logical = lf->name ? lf->name : lf->qualified_name;
    const char *name = backend && *backend ? backend : logical;
    if (!name || strncmp(name, "__cert__", 8) != 0)
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

    const char *effective = (sym.backend_name && *sym.backend_name)
                                ? sym.backend_name
                                : sym.name;
    if (effective && imported_symbol_has_name(*imported_syms, *imported_count,
                                              effective))
      continue;
    if (append_imported_symbol(imported_syms, imported_count, imported_cap,
                               &sym) != 0)
      return 1;
  }

  return 0;
}

static int load_cclib_library(const char *path, LoadedLibrary **libs,
                              int *lib_count, int *lib_cap,
                              LoadedLibraryFunction **funcs, int *func_count,
                              int *func_cap)
{
  if (!path || !libs || !lib_count || !lib_cap || !funcs || !func_count ||
      !func_cap)
    return EINVAL;

  LoadedLibrary lib = {0};
  lib.path = xstrdup(path);
  int err = cclib_read(path, &lib.file);
  if (err)
  {
    fprintf(stderr, "error: failed to read cclib '%s' (%s)\n", path,
            strerror(err));
    free(lib.path);
    return err;
  }
  if (lib.file.module_count > 0)
  {
    lib.ccbin_temp_paths =
        (char **)xcalloc(lib.file.module_count, sizeof(char *));
    if (!lib.ccbin_temp_paths)
    {
      free_loaded_library(&lib);
      return ENOMEM;
    }
  }

  for (uint32_t mi = 0; mi < lib.file.module_count; ++mi)
  {
    const CclibModule *mod = &lib.file.modules[mi];
    const char *module_name = mod->module_name;
    if (!module_name)
      continue;
    if (register_structs_from_cclib_module(&lib, module_name, mod))
    {
      err = ENOMEM;
      break;
    }
    if (register_enums_from_cclib_module(&lib, module_name, mod))
    {
      err = ENOMEM;
      break;
    }
    if (harvest_functions_from_cclib_module(mod, module_name, funcs, func_count,
                                            func_cap))
    {
      err = ENOMEM;
      break;
    }
  }

  if (!err)
  {
    if (*lib_count == *lib_cap)
    {
      int new_cap = *lib_cap ? *lib_cap * 2 : 4;
      LoadedLibrary *grown = (LoadedLibrary *)realloc(
          *libs, (size_t)new_cap * sizeof(LoadedLibrary));
      if (!grown)
        err = ENOMEM;
      else
      {
        *libs = grown;
        *lib_cap = new_cap;
      }
    }
    if (!err)
    {
      (*libs)[*lib_count] = lib;
      (*lib_count)++;
    }
  }

  if (err)
  {
    free_loaded_library(&lib);
  }

  return err;
}

static int load_cclib_library_symbols_only(const char *path,
                                           LoadedLibraryFunction **funcs,
                                           int *func_count, int *func_cap)
{
  if (!path || !funcs || !func_count || !func_cap)
    return EINVAL;
  LoadedLibrary *tmp_libs = NULL;
  int tmp_count = 0;
  int tmp_cap = 0;
  int err = load_cclib_library(path, &tmp_libs, &tmp_count, &tmp_cap, funcs,
                               func_count, func_cap);
  if (tmp_libs)
  {
    for (int i = 0; i < tmp_count; ++i)
      free_loaded_library(&tmp_libs[i]);
    free(tmp_libs);
  }
  return err;
}

static int run_chancecodec_process(const char *cmd, const char *backend,
                                   int opt_level, int strip_metadata,
                                   int strip_hard, int obfuscate,
                                   const char *strip_map_path,
                                   int debug_symbols, const char *asm_path,
                                   const char *ccb_path,
                                   const char *target_os_arg,
                                   int *spawn_errno_out)
{
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  if (!cmd || !backend || !asm_path || !ccb_path)
  {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }
#ifdef _WIN32
  const char *args[20];
  int idx = 0;
  char optbuf[8];
  char target_option_buf[64];
  const char *target_option_value = NULL;
  char strip_map_option_buf[STRIP_MAP_PATH_MAX + 16];
  const char *strip_map_option_value = NULL;
  if (target_os_arg && *target_os_arg)
  {
    snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s",
             target_os_arg);
    target_option_value = target_option_buf;
  }
  if (strip_map_path && *strip_map_path)
  {
    snprintf(strip_map_option_buf, sizeof(strip_map_option_buf),
             "strip-map=%s", strip_map_path);
    strip_map_option_value = strip_map_option_buf;
  }
  args[idx++] = cmd;
  args[idx++] = "--backend";
  args[idx++] = backend;
  if (opt_level > 0)
  {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  if (strip_metadata)
    args[idx++] = "--strip";
  if (strip_hard)
    args[idx++] = "--strip-hard";
  if (obfuscate)
    args[idx++] = "--obfuscate";
  args[idx++] = "--output";
  args[idx++] = asm_path;
  if (target_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = target_option_value;
  }
  if (strip_map_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = strip_map_option_value;
  }
  if (debug_symbols)
  {
    args[idx++] = "--option";
    args[idx++] = "debug=1";
  }
  args[idx++] = ccb_path;
  args[idx] = NULL;
  intptr_t rc = _spawnvp(_P_WAIT, cmd, args);
  if (rc == -1)
  {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  return (int)rc;
#else
  char *args[20];
  int idx = 0;
  char optbuf[8];
  char target_option_buf[64];
  char *target_option_value = NULL;
  char strip_map_option_buf[STRIP_MAP_PATH_MAX + 16];
  char *strip_map_option_value = NULL;
  if (target_os_arg && *target_os_arg)
  {
    snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s",
             target_os_arg);
    target_option_value = target_option_buf;
  }
  if (strip_map_path && *strip_map_path)
  {
    snprintf(strip_map_option_buf, sizeof(strip_map_option_buf),
             "strip-map=%s", strip_map_path);
    strip_map_option_value = strip_map_option_buf;
  }
  args[idx++] = (char *)cmd;
  args[idx++] = "--backend";
  args[idx++] = (char *)backend;
  if (opt_level > 0)
  {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  if (strip_metadata)
    args[idx++] = "--strip";
  if (strip_hard)
    args[idx++] = "--strip-hard";
  if (obfuscate)
    args[idx++] = "--obfuscate";
  args[idx++] = "--output";
  args[idx++] = (char *)asm_path;
  if (target_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = target_option_value;
  }
  if (strip_map_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = strip_map_option_value;
  }
  if (debug_symbols)
  {
    args[idx++] = "--option";
    args[idx++] = "debug=1";
  }
  args[idx++] = (char *)ccb_path;
  args[idx] = NULL;
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, cmd, NULL, NULL, args, environ);
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

static int run_chancecodec_emit_ccbin(const char *cmd, const char *ccb_path,
                                      const char *ccbin_path, int opt_level,
                                      int strip_metadata, int strip_hard,
                                      int obfuscate,
                                      const char *strip_map_path,
                                      int *spawn_errno_out)
{
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  if (!cmd || !ccb_path || !ccbin_path)
  {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }
#ifdef _WIN32
  const char *args[14];
  char optbuf[8];
  int idx = 0;
  char strip_map_option_buf[STRIP_MAP_PATH_MAX + 16];
  const char *strip_map_option_value = NULL;
  if (strip_map_path && *strip_map_path)
  {
    snprintf(strip_map_option_buf, sizeof(strip_map_option_buf),
             "strip-map=%s", strip_map_path);
    strip_map_option_value = strip_map_option_buf;
  }
  args[idx++] = cmd;
  if (opt_level > 0)
  {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = ccb_path;
  if (strip_metadata)
    args[idx++] = "--strip";
  if (strip_hard)
    args[idx++] = "--strip-hard";
  if (obfuscate)
    args[idx++] = "--obfuscate";
  if (strip_map_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = strip_map_option_value;
  }
  args[idx++] = "--emit-ccbin";
  args[idx++] = ccbin_path;
  args[idx] = NULL;
  intptr_t rc = _spawnvp(_P_WAIT, cmd, args);
  if (rc == -1)
  {
    if (spawn_errno_out)
      *spawn_errno_out = errno;
    return -1;
  }
  return (int)rc;
#else
  char *args[14];
  char optbuf[8];
  int idx = 0;
  char strip_map_option_buf[STRIP_MAP_PATH_MAX + 16];
  char *strip_map_option_value = NULL;
  if (strip_map_path && *strip_map_path)
  {
    snprintf(strip_map_option_buf, sizeof(strip_map_option_buf),
             "strip-map=%s", strip_map_path);
    strip_map_option_value = strip_map_option_buf;
  }
  args[idx++] = (char *)cmd;
  if (opt_level > 0)
  {
    snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
    args[idx++] = optbuf;
  }
  args[idx++] = (char *)ccb_path;
  if (strip_metadata)
    args[idx++] = "--strip";
  if (strip_hard)
    args[idx++] = "--strip-hard";
  if (obfuscate)
    args[idx++] = "--obfuscate";
  if (strip_map_option_value)
  {
    args[idx++] = "--option";
    args[idx++] = strip_map_option_value;
  }
  args[idx++] = "--emit-ccbin";
  args[idx++] = (char *)ccbin_path;
  args[idx] = NULL;
  pid_t pid = 0;
  int rc = posix_spawnp(&pid, cmd, NULL, NULL, args, environ);
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

static int is_relocatable_obj(const char *path)
{
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
  if (hdr[0] == 0x7F && hdr[1] == 'E' && hdr[2] == 'L' && hdr[3] == 'F')
  {
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

int main(int argc, char **argv)
{
  if (argc == 1)
  {
    usage(argv[0]);
    return 0;
  }
  if (argc >= 2 && equals_icase(argv[1], "new"))
    return handle_new_command(argc - 1, argv + 1);
#ifdef _WIN32
  const char *out = "a.exe";
#else
  const char *out = "a";
#endif
  const char *pending_output = NULL;
  int output_overridden = 0;
  int stop_after_asm = 0;
  int stop_after_ccb = 0;
  int no_link = 0; // -c / --no-link
  int emit_library = 0;
  int freestanding = 0;
  int freestanding_requested = 0;
  int m32 = 0;
  int opt_level = 0;
  int debug_symbols = 0;
  int strip_metadata = 0;
  int strip_hard = 0;
  int obfuscate = 0;
  int implicit_voidp = 0;
  int implicit_void_function = 0;
  int implicit_sizeof = 0;
  int request_ast = 0;
  int diagnostics_only = 0;
  OverrideFile *override_files = NULL;
  int override_file_count = 0;
  int override_file_cap = 0;
  char strip_map_path[STRIP_MAP_PATH_MAX] = {0};
  int strip_map_ready = 0;
  int pending_ce_output_index = -1;
  AsmSyntax asm_syntax = ASM_INTEL;
  TargetArch target_arch = ARCH_NONE;
  const char *chancecode_backend = NULL;
  const char *chancecodec_cmd_override = NULL;
  const char *host_cc_cmd_override = NULL;
#ifdef _WIN32
  TargetOS target_os = OS_WINDOWS;
#elif defined(__APPLE__)
  TargetOS target_os = OS_MACOS;
#else
  TargetOS target_os = OS_LINUX;
#endif
  // Separate CHance and object inputs
  const char **ce_inputs = NULL;
  int ce_count = 0, ce_cap = 0;
  char **ce_obj_outputs = NULL;
  const char **ccb_inputs = NULL;
  int ccb_count = 0, ccb_cap = 0;
  const char **cclib_inputs = NULL;
  int cclib_count = 0, cclib_cap = 0;
  const char **obj_inputs = NULL;
  int obj_count = 0, obj_cap = 0;
  const char **symbol_ref_ce_inputs = NULL;
  int symbol_ref_ce_count = 0, symbol_ref_ce_cap = 0;
  const char **symbol_ref_cclib_inputs = NULL;
  int symbol_ref_cclib_count = 0, symbol_ref_cclib_cap = 0;
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
  char cwd_buf[1024] = {0};
  const char *cwd = get_cwd_path(cwd_buf, sizeof(cwd_buf));

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
  char *project_after_cmd = NULL;

  ProjectInputList ce_cli_list = {&ce_inputs, &ce_count, &ce_cap,
                                  &owned_ce_inputs, &owned_ce_count,
                                  &owned_ce_cap, &ce_obj_outputs};

  for (int i = 1; i < argc; i++)
  {
    if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0)
    {
      usage(argv[0]);
      return 0;
    }
    if (strcmp(argv[i], "--version") == 0)
    {
      printf("chancec: CHance Compiler version 1.1.0\n");
      printf("chancec: CE language standard: H26\n");
      printf("chancec: License: OpenAzure License\n");
      printf("chancec: Compiled on %s %s\n", __DATE__, __TIME__);
      printf("chancec: Created by Nathan Hornby (AzureianGH)\n");
      return 0;
    }
    if (strcmp(argv[i], "-o") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: -o expects an output path\n");
        return 2;
      }
      const char *dest = argv[++i];
      if (pending_ce_output_index >= 0 && no_link)
      {
        if (!ce_obj_outputs)
        {
          fprintf(stderr, "error: internal state mismatch for -o handling\n");
          goto fail;
        }
        char *dup = xstrdup(dest);
        if (!dup)
        {
          fprintf(stderr, "error: out of memory while assigning -o path\n");
          goto fail;
        }
        if (ce_obj_outputs[pending_ce_output_index])
          free(ce_obj_outputs[pending_ce_output_index]);
        ce_obj_outputs[pending_ce_output_index] = dup;
        pending_ce_output_index = -1;
      }
      else
      {
        pending_output = dest;
        output_overridden = 1;
      }
      continue;
    }
    if (strcmp(argv[i], "-S") == 0)
    {
      stop_after_asm = 1;
      continue;
    }
    if (strcmp(argv[i], "-Sccb") == 0)
    {
      stop_after_ccb = 1;
      continue;
    }
    if (strcmp(argv[i], "-g") == 0)
    {
      debug_symbols = 1;
      continue;
    }
    if (strcmp(argv[i], "--strip") == 0)
    {
      strip_metadata = 1;
      continue;
    }
    if (strcmp(argv[i], "--strip-hard") == 0)
    {
      strip_metadata = 1;
      strip_hard = 1;
      continue;
    }
    if (strcmp(argv[i], "--obfuscate") == 0)
    {
      strip_metadata = 1;
      strip_hard = 1;
      obfuscate = 1;
      continue;
    }
    if (strncmp(argv[i], "-O", 2) == 0)
    {
      const char *level_str = argv[i] + 2;
      int level = 1;
      if (*level_str != '\0')
      {
        char *endptr = NULL;
        long parsed = strtol(level_str, &endptr, 10);
        if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 3)
        {
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
    if (strcmp(argv[i], "--no-link") == 0)
    {
      no_link = 1;
      if (i + 1 < argc && argv[i + 1][0] != '-')
      {
        const char *cand = argv[i + 1];
        if (is_object_file_arg(cand))
        {
          obj_override = cand;
          i++;
        }
      }
      continue;
    }
    if (strcmp(argv[i], "-c") == 0)
    {
      no_link = 1;
      if (i + 1 < argc && argv[i + 1][0] != '-')
      {
        const char *cand = argv[i + 1];
        if (is_ce_source_arg(cand))
        {
          if (push_input_entry(cand, ce_cli_list, 0) != 0)
            goto fail;
          pending_ce_output_index = ce_count - 1;
          i++;
          continue;
        }
        if (is_object_file_arg(cand))
        {
          obj_override = cand;
          i++;
          continue;
        }
      }
      continue;
    }
    if (strcmp(argv[i], "--asm-syntax") == 0 && i + 1 < argc)
    {
      const char *arg = argv[++i];
      if (strcmp(arg, "intel") == 0)
        asm_syntax = ASM_INTEL;
      else if (strcmp(arg, "att") == 0)
        asm_syntax = ASM_ATT;
      else if (strcmp(arg, "nasm") == 0)
        asm_syntax = ASM_NASM;
      else
      {
        fprintf(stderr, "Unknown asm syntax '%s' (use intel|att|nasm)\n", arg);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "--chancecodec") == 0 && i + 1 < argc)
    {
      chancecodec_cmd_override = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "--cc") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: --cc expects a compiler path\n");
        return 2;
      }
      host_cc_cmd_override = argv[++i];
      continue;
    }
    if (strcmp(argv[i], "--library") == 0)
    {
      emit_library = 1;
      continue;
    }
    if (strcmp(argv[i], "--freestanding") == 0)
    {
      freestanding = 1;
      freestanding_requested = 1;
      continue;
    }
    if (strcmp(argv[i], "-x86") == 0)
    {
      target_arch = ARCH_X86;
      chancecode_backend = "x86-gas";
      continue;
    }
    if (strcmp(argv[i], "-arm64") == 0)
    {
      target_arch = ARCH_ARM64;
      chancecode_backend = NULL;
      continue;
    }
    if (strcmp(argv[i], "-bslash") == 0)
    {
      target_arch = ARCH_BSLASH;
      chancecode_backend = "bslash";
      stop_after_asm = 1;
      continue;
    }
    if (strcmp(argv[i], "-m32") == 0)
    {
      m32 = 1;
      continue;
    }
    if (strcmp(argv[i], "-m64") == 0)
    {
      m32 = 0;
      continue;
    }
    if (strcmp(argv[i], "--target-os") == 0 && i + 1 < argc)
    {
      const char *os = argv[++i];
      if (strcmp(os, "windows") == 0)
        target_os = OS_WINDOWS;
      else if (strcmp(os, "linux") == 0)
        target_os = OS_LINUX;
      else if (strcmp(os, "macos") == 0)
        target_os = OS_MACOS;
      else
      {
        fprintf(stderr,
                "Unknown --target-os '%s' (use windows|linux|macos)\n", os);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "-I") == 0 && i + 1 < argc)
    {
      chance_add_include_dir(&include_dirs, &include_dir_count, argv[++i]);
      continue;
    }
    if (strcmp(argv[i], "-Nno-formatting") == 0)
    {
      parser_set_disable_formatting_notes(1);
      continue;
    }
    if (strcmp(argv[i], "-vd") == 0 || strcmp(argv[i], "--verbose-deep") == 0)
    {
      compiler_verbose_set_mode(1);
      compiler_verbose_set_deep(1);
      continue;
    }
    if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0)
    {
      compiler_verbose_set_mode(1);
      continue;
    }
    if (strcmp(argv[i], "--no-ansi") == 0)
    {
      diag_set_use_ansi(0);
      verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(argv[i], "--data-log") == 0)
    {
      diag_set_data_log(1);
      diag_set_use_ansi(0);
      verbose_use_ansi = 0;
      compiler_verbose_set_use_ansi(0);
      continue;
    }
    if (strcmp(argv[i], "--request-ast") == 0)
    {
      request_ast = 1;
      continue;
    }
    if (strcmp(argv[i], "--diagnostics-only") == 0 || strcmp(argv[i], "--diag-only") == 0)
    {
      diagnostics_only = 1;
      continue;
    }
    if (strcmp(argv[i], "--override-file") == 0)
    {
      if (i + 1 >= argc)
      {
        fprintf(stderr, "error: --override-file expects <original>=<override>\n");
        return 2;
      }
      const char *spec = argv[++i];
      if (add_override_file(spec, cwd, &override_files, &override_file_count,
                            &override_file_cap) != 0)
      {
        fprintf(stderr, "error: invalid --override-file '%s'\n", spec);
        return 2;
      }
      continue;
    }
    if (strncmp(argv[i], "--override-file=", 16) == 0)
    {
      const char *spec = argv[i] + 16;
      if (add_override_file(spec, cwd, &override_files, &override_file_count,
                            &override_file_cap) != 0)
      {
        fprintf(stderr, "error: invalid --override-file '%s'\n", spec);
        return 2;
      }
      continue;
    }
    if (strcmp(argv[i], "--implicit-voidp") == 0)
    {
      implicit_voidp = 1;
      continue;
    }
    if (strcmp(argv[i], "--implicit-void-function") == 0)
    {
      implicit_void_function = 1;
      continue;
    }
    if (strcmp(argv[i], "--implicit-sizeof") == 0)
    {
      implicit_sizeof = 1;
      continue;
    }
    if (strncmp(argv[i], "-sr:", 4) == 0)
    {
      const char *sr_path = argv[i] + 4;
      if (!sr_path || !*sr_path)
      {
        fprintf(stderr,
                "error: -sr: requires a .ce or .cclib path immediately following\n");
        return 2;
      }
      if (ends_with_icase(sr_path, ".ce"))
      {
        if (symbol_ref_ce_count == symbol_ref_ce_cap)
        {
          symbol_ref_ce_cap = symbol_ref_ce_cap ? symbol_ref_ce_cap * 2 : 4;
          symbol_ref_ce_inputs = (const char **)realloc(
              (void *)symbol_ref_ce_inputs,
              sizeof(char *) * (size_t)symbol_ref_ce_cap);
        }
        symbol_ref_ce_inputs[symbol_ref_ce_count++] = sr_path;
      }
      else if (ends_with_icase(sr_path, ".cclib"))
      {
        if (symbol_ref_cclib_count == symbol_ref_cclib_cap)
        {
          symbol_ref_cclib_cap = symbol_ref_cclib_cap ? symbol_ref_cclib_cap * 2 : 4;
          symbol_ref_cclib_inputs = (const char **)realloc(
              (void *)symbol_ref_cclib_inputs,
              sizeof(char *) * (size_t)symbol_ref_cclib_cap);
        }
        symbol_ref_cclib_inputs[symbol_ref_cclib_count++] = sr_path;
      }
      else
      {
        fprintf(stderr,
                "error: -sr: path '%s' must be a .ce or .cclib file\n",
                sr_path);
        return 2;
      }
      continue;
    }
    if (argv[i][0] == '-')
    {
      usage(argv[0]);
      return 2;
    }
    // classify input
    const char *arg = argv[i];
    if (ends_with_icase(arg, ".ceproj"))
    {
      ProjectInputList ce_list = {&ce_inputs, &ce_count,
                                  &ce_cap, &owned_ce_inputs,
                                  &owned_ce_count, &owned_ce_cap,
                                  &ce_obj_outputs};
      ProjectInputList ccb_list = {&ccb_inputs, &ccb_count,
                                   &ccb_cap, &owned_ccb_inputs,
                                   &owned_ccb_count, &owned_ccb_cap,
                                   NULL};
      ProjectInputList cclib_list = {&cclib_inputs, &cclib_count,
                                     &cclib_cap, &owned_cclib_inputs,
                                     &owned_cclib_count, &owned_cclib_cap,
                                     NULL};
      ProjectInputList obj_list = {&obj_inputs, &obj_count,
                                   &obj_cap, &owned_obj_inputs,
                                   &owned_obj_count, &owned_obj_cap,
                                   NULL};
      int freestanding_before = freestanding;
      int perr = parse_ceproj_file(
          arg, ce_list, ccb_list, cclib_list, obj_list, &include_dirs,
          &include_dir_count, &output_overridden, &out, &project_output_alloc,
          &target_arch, &chancecode_backend, &stop_after_ccb, &stop_after_asm,
          &emit_library, &no_link, &freestanding, &target_os,
          &freestanding_requested, &m32, &opt_level, &debug_symbols,
          &strip_metadata, &strip_hard, &obfuscate, &asm_syntax,
          &chancecodec_cmd_override, &host_cc_cmd_override, &obj_override,
              &implicit_voidp, &implicit_void_function, &implicit_sizeof, &request_ast,
          &diagnostics_only,
          &override_files, &override_file_count, &override_file_cap,
          (ProjectInputList){&symbol_ref_ce_inputs, &symbol_ref_ce_count,
                   &symbol_ref_ce_cap, &owned_ce_inputs,
                   &owned_ce_count, &owned_ce_cap, NULL},
          (ProjectInputList){&symbol_ref_cclib_inputs,
                   &symbol_ref_cclib_count,
                             &symbol_ref_cclib_cap, &owned_cclib_inputs,
                             &owned_cclib_count, &owned_cclib_cap, NULL},
          &project_after_cmd);
      if (perr != 0)
        return 2;
      if (!freestanding_requested && !freestanding_before && freestanding)
        freestanding_requested = 1;
      continue;
    }
    if (ends_with_icase(arg, ".ce"))
    {
      if (push_input_entry(arg, ce_cli_list, 0) != 0)
        goto fail;
      pending_ce_output_index = -1;
    }
    else if (ends_with_icase(arg, ".ccb"))
    {
      if (ccb_count == ccb_cap)
      {
        ccb_cap = ccb_cap ? ccb_cap * 2 : 8;
        ccb_inputs = (const char **)realloc((void *)ccb_inputs,
                                            sizeof(char *) * ccb_cap);
      }
      ccb_inputs[ccb_count++] = arg;
    }
    else if (ends_with_icase(arg, ".cclib"))
    {
      if (cclib_count == cclib_cap)
      {
        cclib_cap = cclib_cap ? cclib_cap * 2 : 4;
        cclib_inputs = (const char **)realloc((void *)cclib_inputs,
                                              sizeof(char *) * cclib_cap);
      }
      cclib_inputs[cclib_count++] = arg;
    }
    else if (ends_with_icase(arg, ".o") || ends_with_icase(arg, ".obj"))
    {
      if (obj_count == obj_cap)
      {
        obj_cap = obj_cap ? obj_cap * 2 : 8;
        obj_inputs = (const char **)realloc((void *)obj_inputs,
                                            sizeof(char *) * obj_cap);
      }
      obj_inputs[obj_count++] = arg;
    }
    else
    {
      fprintf(stderr,
              "error: unknown input type '%s' (expected .ce or .o/.obj)\n",
              arg);
      return 2;
    }
  }
  if (pending_output)
  {
    if (no_link)
      obj_override = pending_output;
    else
      out = pending_output;
  }
  if (strip_metadata)
    debug_symbols = 0;
  if (stop_after_asm && stop_after_ccb)
  {
    fprintf(stderr, "error: -S and -Sccb cannot be used together\n");
    return 2;
  }
  if (stop_after_asm && target_arch == ARCH_NONE && !request_ast && !diagnostics_only)
  {
    fprintf(stderr, "error: -S requires a backend selection (e.g., -x86 or -arm64)\n");
    return 2;
  }
  if (!emit_library && target_arch == ARCH_NONE && !request_ast && !diagnostics_only)
  {
    if (!stop_after_ccb)
    {
      fprintf(stderr, "error: selecting a backend (e.g., -x86 or -arm64) is required "
                      "unless stopping at bytecode with -Sccb\n");
      return 2;
    }
    if (no_link)
    {
      fprintf(stderr, "error: -c/--no-link is incompatible with -Sccb (no "
                      "object emission when stopping at bytecode)\n");
      return 2;
    }
    if (obj_count > 0 || ccb_count > 0)
    {
      fprintf(stderr, "error: providing .ccb/.o/.obj inputs requires selecting "
                      "a backend (e.g., -x86 or -arm64)\n");
      return 2;
    }
  }
  if (ce_count == 0 && obj_count == 0 && ccb_count == 0)
  {
    usage(argv[0]);
    return 2;
  }
  if (emit_library)
  {
    if (stop_after_asm)
    {
      fprintf(stderr, "error: --library cannot be combined with -S\n");
      return 2;
    }
    if (stop_after_ccb)
    {
      fprintf(stderr, "error: --library already stops after bytecode output\n");
      return 2;
    }
    if (no_link)
    {
      fprintf(stderr, "error: --library is incompatible with -c/--no-link\n");
      return 2;
    }
    if (target_arch != ARCH_NONE)
    {
      fprintf(stderr, "error: --library cannot be combined with backend "
                      "selection (e.g., -x86 or -arm64)\n");
      return 2;
    }
    if (ccb_count > 0 || obj_count > 0 || cclib_count > 0)
    {
      fprintf(stderr, "error: --library currently only accepts .ce inputs\n");
      return 2;
    }
    if (ce_count == 0)
    {
      fprintf(stderr, "error: --library requires at least one .ce input\n");
      return 2;
    }
    if (!output_overridden)
    {
      out = "a.cclib";
    }
    if (!ends_with_icase(out, ".cclib"))
    {
      fprintf(stderr, "error: --library output must end with .cclib\n");
      return 2;
    }
  }
  if (m32)
  {
    fprintf(stderr, "Error: -m32 not implemented yet\n");
    return 2;
  }

  char host_cc_override_buf[1024] = {0};
  const char *host_cc_cmd_to_use = "cc";
  int host_cc_has_override = 0;
  if (host_cc_cmd_override && *host_cc_cmd_override)
  {
    strip_wrapping_quotes(host_cc_cmd_override, host_cc_override_buf,
                          sizeof(host_cc_override_buf));
    if (!host_cc_override_buf[0])
    {
      fprintf(stderr, "error: --cc path is empty after trimming quotes/whitespace\n");
      return 2;
    }
    host_cc_cmd_to_use = host_cc_override_buf;
    host_cc_has_override = 1;
  }

  char chancecodec_exec_buf[1024] = {0};
  char chancecodec_override_buf[1024] = {0};
  const char *chancecodec_cmd_to_use = NULL;
  int chancecodec_uses_fallback = 0;
  int chancecodec_has_override = 0;
  int needs_chancecodec = (target_arch != ARCH_NONE) || emit_library;
  if (needs_chancecodec)
  {
    if (chancecodec_cmd_override && *chancecodec_cmd_override)
    {
      strip_wrapping_quotes(chancecodec_cmd_override, chancecodec_override_buf,
                            sizeof(chancecodec_override_buf));
      if (!chancecodec_override_buf[0])
      {
        fprintf(stderr, "error: --chancecodec path is empty after trimming "
                        "quotes/whitespace\n");
        return 2;
      }
      chancecodec_cmd_to_use = chancecodec_override_buf;
      chancecodec_has_override = 1;
    }
    else if (locate_chancecodec(chancecodec_exec_buf,
                                sizeof(chancecodec_exec_buf), exe_dir) == 0 &&
             chancecodec_exec_buf[0])
    {
      chancecodec_cmd_to_use = chancecodec_exec_buf;
    }
    else
    {
      chancecodec_cmd_to_use = default_chancecodec_name;
      chancecodec_uses_fallback = 1;
    }
  }

#ifndef DEFAULT_STDLIB
#define DEFAULT_STDLIB ""
#endif
#ifndef DEFAULT_RUNTIME
#define DEFAULT_RUNTIME ""
#endif

  if (!freestanding_requested)
  {
    char stdlib_path[1024];
    if (!locate_cclib_path(exe_dir, "stdlib", "stdlib.cclib", DEFAULT_STDLIB,
                           stdlib_path, sizeof(stdlib_path)))
    {
      (void)locate_cclib_path(exe_dir, "src/stdlib", "stdlib.cclib",
                              DEFAULT_STDLIB, stdlib_path,
                              sizeof(stdlib_path));
    }

    if (stdlib_path[0] && is_regular_file(stdlib_path))
    {
      ProjectInputList cclib_list = {&cclib_inputs, &cclib_count, &cclib_cap,
                                     &owned_cclib_inputs, &owned_cclib_count,
                                     &owned_cclib_cap, NULL};
      if (push_input_entry(stdlib_path, cclib_list, 1) != 0)
        goto fail;
    }
    else
    {
      freestanding = 1;
      fprintf(stderr,
              "warning: stdlib not found at '%s'; enabling freestanding (--nostdlib)\n",
              stdlib_path[0] ? stdlib_path : "<unknown>");
    }
  }

  {
    char runtime_path[1024];
    if (!locate_cclib_path(exe_dir, "runtime", "runtime.cclib",
                           DEFAULT_RUNTIME, runtime_path,
                           sizeof(runtime_path)))
    {
      (void)locate_cclib_path(exe_dir, "stdlib", "runtime.cclib",
                              DEFAULT_RUNTIME, runtime_path,
                              sizeof(runtime_path));
    }

    if (runtime_path[0] && is_regular_file(runtime_path))
    {
      ProjectInputList cclib_list = {&cclib_inputs, &cclib_count, &cclib_cap,
                                     &owned_cclib_inputs, &owned_cclib_count,
                                     &owned_cclib_cap, NULL};
      if (push_input_entry(runtime_path, cclib_list, 1) != 0)
        goto fail;
    }
    else
    {
      fprintf(stderr,
              "error: runtime.cclib not found at '%s'\n",
              runtime_path[0] ? runtime_path : "<unknown>");
      goto fail;
    }
  }

  if (compiler_verbose_enabled())
  {
    verbose_print_config(out, opt_level, target_arch, target_os, stop_after_ccb,
                         stop_after_asm, no_link, emit_library, freestanding,
                         asm_syntax, include_dir_count, ce_count, ccb_count,
                         cclib_count, obj_count, symbol_ref_ce_count,
                         symbol_ref_cclib_count, host_cc_cmd_to_use,
                         host_cc_has_override, chancecodec_cmd_to_use,
                         chancecodec_has_override, needs_chancecodec,
                         chancecode_backend, chancecodec_uses_fallback,
                         compiler_verbose_enabled(),
                         compiler_verbose_deep_enabled());
    if (include_dir_count > 0)
    {
      verbose_section("Include Directories");
      for (int idx = 0; idx < include_dir_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "dir[%d]", idx);
        verbose_table_row(label,
                          (include_dirs && include_dirs[idx])
                              ? include_dirs[idx]
                              : "(null)");
      }
    }
    if (ce_count > 0 && ce_inputs)
    {
      verbose_section("CE Inputs");
      for (int idx = 0; idx < ce_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "ce[%d]", idx);
        verbose_table_row(label, ce_inputs[idx]);
      }
    }
    if (symbol_ref_ce_count > 0 && symbol_ref_ce_inputs)
    {
      verbose_section("Symbol-ref CE Inputs");
      for (int idx = 0; idx < symbol_ref_ce_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "sr-ce[%d]", idx);
        verbose_table_row(label, symbol_ref_ce_inputs[idx]);
      }
    }
    if (ccb_count > 0 && ccb_inputs)
    {
      verbose_section("CCB Inputs");
      for (int idx = 0; idx < ccb_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "ccb[%d]", idx);
        verbose_table_row(label, ccb_inputs[idx]);
      }
    }
    if (cclib_count > 0 && cclib_inputs)
    {
      verbose_section("CCLib Inputs");
      for (int idx = 0; idx < cclib_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "cclib[%d]", idx);
        verbose_table_row(label, cclib_inputs[idx]);
      }
    }
    if (symbol_ref_cclib_count > 0 && symbol_ref_cclib_inputs)
    {
      verbose_section("Symbol-ref CCLib Inputs");
      for (int idx = 0; idx < symbol_ref_cclib_count; ++idx)
      {
        char label[32];
        snprintf(label, sizeof(label), "sr-cclib[%d]", idx);
        verbose_table_row(label, symbol_ref_cclib_inputs[idx]);
      }
    }
    if (obj_count > 0 && obj_inputs)
    {
      verbose_section("Object Inputs");
      for (int idx = 0; idx < obj_count; ++idx)
      {
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
  if (no_link)
  {
    if (!obj_override)
    {
      // Per-file compile: must not include external .o inputs
      if (obj_count > 0)
      {
        fprintf(
            stderr,
            "error: providing .o files with -c but without an output object is "
            "invalid. Specify an output object after -c to merge.\n");
        goto fail;
      }
      if (ce_count == 0)
      {
        fprintf(stderr, "error: nothing to compile.\n");
        goto fail;
      }
    }
    else
    {
      // Combined object: must have at least one .ce or .o input
      if ((ce_count + obj_count) == 0)
      {
        fprintf(stderr, "error: no inputs provided to combine into '%s'.\n",
                obj_override);
        goto fail;
      }
      // Validate external .o/.obj relocatable
      for (int k = 0; k < obj_count; ++k)
      {
        if (!is_relocatable_obj(obj_inputs[k]))
        {
          fprintf(stderr,
                  "error: input '%s' is not a relocatable object file.\n",
                  obj_inputs[k]);
          goto fail;
        }
      }
    }
  }
  sema_set_allow_implicit_voidp(implicit_voidp);
  sema_set_allow_implicit_void_function(implicit_void_function);
  sema_set_allow_implicit_sizeof(implicit_sizeof);

  module_registry_reset();
  if (strip_hard)
  {
    choose_strip_map_path(strip_map_path, sizeof(strip_map_path));
    if (!strip_map_path[0])
    {
      fprintf(stderr, "error: failed to determine path for strip-hard map\n");
      goto fail;
    }
    remove(strip_map_path);
  }
  int rc = 0;
  if (symbol_ref_cclib_count > 0)
  {
    for (int i = 0; i < symbol_ref_cclib_count; ++i)
    {
      if (load_cclib_library_symbols_only(
              symbol_ref_cclib_inputs[i], &loaded_library_functions,
              &loaded_library_function_count,
              &loaded_library_function_cap))
      {
        rc = 1;
        goto cleanup;
      }
    }
  }
  if (!emit_library && cclib_count > 0)
  {
    for (int i = 0; i < cclib_count; ++i)
    {
      if (load_cclib_library(
              cclib_inputs[i], &loaded_libraries, &loaded_library_count,
              &loaded_library_cap, &loaded_library_functions,
              &loaded_library_function_count, &loaded_library_function_cap))
      {
        rc = 1;
        goto cleanup;
      }
    }
  }

  int library_codegen_units = 0;
  if (!emit_library && loaded_library_count > 0)
  {
    for (int i = 0; i < loaded_library_count; ++i)
    {
      const LoadedLibrary *lib = &loaded_libraries[i];
      if (!lib)
        continue;
      for (uint32_t mi = 0; mi < lib->file.module_count; ++mi)
      {
        const CclibModule *mod = &lib->file.modules[mi];
        if (mod && mod->ccbin_data && mod->ccbin_size > 0)
          ++library_codegen_units;
      }
    }
  }

  UnitCompile *units = NULL;
  if (ce_count > 0)
    units = (UnitCompile *)xcalloc((size_t)ce_count, sizeof(UnitCompile));
  SymbolRefUnit *symbol_ref_units = NULL;
  if (symbol_ref_ce_count > 0)
    symbol_ref_units =
        (SymbolRefUnit *)xcalloc((size_t)symbol_ref_ce_count,
                                 sizeof(SymbolRefUnit));
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

  if (symbol_ref_ce_count > 0 && symbol_ref_units)
  {
    if (compiler_verbose_enabled())
      verbose_section("Loading symbol reference CE units");
    for (int si = 0; si < symbol_ref_ce_count; ++si)
    {
      const char *input = symbol_ref_ce_inputs[si];
      const char *override_path =
          find_override_path(input, override_files, override_file_count);
      const char *read_path = override_path ? override_path : input;
      if (compiler_verbose_enabled())
        verbose_progress("sr-ce-load", si + 1, symbol_ref_ce_count);
      int len = 0;
      char *src = read_all(read_path, &len);
      if (!src)
      {
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

      symbol_ref_units[si].input_path = input ? xstrdup(input) : NULL;
      symbol_ref_units[si].src = src;
      symbol_ref_units[si].stripped = preprocessed;
      symbol_ref_units[si].unit = unit;

      sema_destroy(sc);
      parser_destroy(ps);
    }
    if (rc)
      goto cleanup;
  }

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Loading CE units");
  for (int fi = 0; fi < ce_count; ++fi)
  {
    const char *input = ce_inputs[fi];
    const char *override_path =
        find_override_path(input, override_files, override_file_count);
    const char *read_path = override_path ? override_path : input;
    if (compiler_verbose_enabled())
      verbose_progress("ce-load", fi + 1, ce_count);
    int len = 0;
    char *src = read_all(read_path, &len);
    if (!src)
    {
      rc = 1;
      break;
    }
    int pre_len = 0;
    char *preprocessed = chance_preprocess_source(
        input, src, len, &pre_len, target_arch_to_macro(target_arch));
    if (getenv("DUMP_PREPROC") && input && strstr(input, "aemu/cpu8086.ce"))
    {
      printf("%s", preprocessed ? preprocessed : src);
      exit(0);
    }
    (void)pre_len;
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

  if (request_ast)
  {
    FILE *ast_out = stdout;
    fputc('[', ast_out);
    int emitted = 0;
    for (int fi = 0; fi < ce_count; ++fi)
    {
      if (!units || !units[fi].unit)
        continue;
      if (emitted)
        fputc(',', ast_out);
      ast_emit_json(ast_out, units[fi].unit, units[fi].input_path);
      emitted = 1;
    }
    fputs("]\n", ast_out);
    fflush(ast_out);
    rc = 0;
    goto cleanup;
  }

  if (diagnostics_only)
  {
    if (compiler_verbose_enabled() && ce_count > 0)
      verbose_section("Diagnostics-only checks");
    for (int fi = 0; fi < ce_count; ++fi)
    {
      if (compiler_verbose_enabled())
        verbose_progress("ce-sema", fi + 1, ce_count);
      UnitCompile *uc = &units[fi];
      if (sema_check_unit(uc->sc, uc->unit) != 0)
        rc = 1;
    }
    goto cleanup;
  }

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Registering CE externs");
  for (int target = 0; target < ce_count; ++target)
  {
    if (compiler_verbose_enabled())
      verbose_progress("ce-extern", target + 1, ce_count);
    for (int source = 0; source < ce_count; ++source)
    {
      if (target == source)
        continue;
      sema_register_foreign_unit_symbols(units[target].sc, units[target].unit,
                                         units[source].unit);
    }
  }

  if (ce_count > 0 && symbol_ref_ce_count > 0 && symbol_ref_units)
  {
    if (compiler_verbose_enabled())
      verbose_section("Registering symbol reference CE externs");
    for (int target = 0; target < ce_count; ++target)
    {
      if (compiler_verbose_enabled())
        verbose_progress("sr-ce-extern", target + 1, ce_count);
      for (int sr = 0; sr < symbol_ref_ce_count; ++sr)
      {
        if (!symbol_ref_units[sr].unit)
          continue;
        sema_register_foreign_unit_symbols(units[target].sc,
                                           units[target].unit,
                                           symbol_ref_units[sr].unit);
      }
    }
  }
  if (!rc && strip_hard)
  {
    if (!strip_map_path[0])
    {
      fprintf(stderr, "internal error: strip-hard map path unavailable\n");
      rc = 1;
      goto cleanup;
    }
    int map_rc = build_strip_symbol_map_file(
        units, ce_count, ccb_inputs, ccb_count, loaded_libraries,
        loaded_library_count, loaded_library_functions,
        loaded_library_function_count, strip_map_path);
    if (map_rc != 0)
    {
      fprintf(stderr,
              "error: failed to build strip-hard symbol map at '%s'\n",
              strip_map_path);
      rc = 1;
      goto cleanup;
    }
    strip_map_ready = 1;
  }

  if (compiler_verbose_enabled() && ce_count > 0)
    verbose_section("Codegen CE units");
  for (int fi = 0; fi < ce_count && rc == 0; ++fi)
  {
    UnitCompile *uc = &units[fi];
    Node *unit = uc->unit;
    SemaContext *sc = uc->sc;
    Parser *ps = uc->parser;

    if (compiler_verbose_enabled())
      verbose_progress("ce-codegen", fi + 1, ce_count);

    int serr = sema_check_unit(sc, unit);
    if (!serr)
    {
      char dir[512], base[512];
      split_path(uc->input_path, dir, sizeof(dir), base, sizeof(base));

      char ccb_path[1024];
      build_path_with_ext(dir, base, ".ccb", ccb_path, sizeof(ccb_path));
      if (stop_after_ccb && ends_with_icase(out, ".ccb"))
        snprintf(ccb_path, sizeof(ccb_path), "%s", out);
      int ccb_is_temp = 1;

      char asm_path[1024];
      const char *asm_ext = (target_arch == ARCH_BSLASH) ? ".bas" : ".S";
      build_path_with_ext(dir, base, asm_ext, asm_path, sizeof(asm_path));
      if (stop_after_asm)
      {
        int wants_override = ends_with_icase(out, ".s");
        if (target_arch == ARCH_BSLASH)
          wants_override |= ends_with_icase(out, ".bas");
        if (wants_override)
          snprintf(asm_path, sizeof(asm_path), "%s", out);
      }

      char objOut[1024] = {0};
      int obj_is_temp = 0;
      int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                     (no_link || multi_link || target_arch != ARCH_NONE);
      const char *explicit_obj =
          (ce_obj_outputs && fi < ce_count) ? ce_obj_outputs[fi] : NULL;
      if (explicit_obj && *explicit_obj)
        need_obj = 1;
      if (need_obj)
      {
        if (explicit_obj && *explicit_obj)
        {
          snprintf(objOut, sizeof(objOut), "%s", explicit_obj);
          obj_is_temp = 0;
        }
        else if (no_link && obj_override)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
        }
        else if (no_link)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".obj"
#else
                              ".o"
#endif
                              ,
                              objOut, sizeof(objOut));
        }
        else if (multi_link)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        }
        else
        {
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

      int imported_count = 0;
      Symbol *imported_syms = sema_copy_imported_function_symbols(sc, &imported_count);
      int imported_cap = imported_count;
      if (merge_stdlib_externs_into_imported(
              &imported_syms, &imported_count, &imported_cap,
              loaded_library_functions, loaded_library_function_count) != 0)
      {
        rc = 1;
        free(imported_syms);
        break;
      }
      if (merge_cert_externs_into_imported(
              &imported_syms, &imported_count, &imported_cap,
              loaded_library_functions, loaded_library_function_count) != 0)
      {
        rc = 1;
        free(imported_syms);
        break;
      }
      int imported_global_count = 0;
      Symbol *imported_global_syms = sema_copy_imported_global_symbols(sc, &imported_global_count);

      CodegenOptions co = {.freestanding = freestanding != 0,
                           .m32 = m32 != 0,
                           .debug_symbols = debug_symbols != 0,
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
                           .imported_externs = imported_syms,
                           .imported_extern_count = imported_count,
                           .imported_globals = imported_global_syms,
                           .imported_global_count = imported_global_count,
                           .opt_level = opt_level};
      int extern_count = 0;
      const Symbol *extern_syms = parser_get_externs(ps, &extern_count);
      co.externs = extern_syms;
      co.extern_count = extern_count;
      rc = codegen_ccb_write_module(unit, &co);
      free(imported_syms);
      free(imported_global_syms);

      if (!rc)
      {
        if (codegen_ccb_resolve_module_path(&co, ccb_path, sizeof(ccb_path)))
          rc = 1;
      }

      if (!rc && emit_library)
      {
        char module_name_buf[256];
        const char *fallback_name = derive_module_name_from_path(
            ccb_path, module_name_buf, sizeof(module_name_buf));
        const char *module_name = unit->module_path.full_name;
        const char *effective_module =
            (module_name && *module_name) ? module_name : fallback_name;
        if (collect_metadata_for_unit(unit, ccb_path, effective_module,
                                      &library_modules,
                                      &library_module_count,
                                      &library_module_cap))
        {
          rc = 1;
        }
        else
        {
          LibraryModuleData *libmod =
              effective_module
                  ? find_or_add_module(&library_modules, &library_module_count,
                           &library_module_cap, effective_module)
                  : NULL;
          if (!libmod)
          {
            rc = 1;
          }
          else if (libmod->function_count == 0 && libmod->global_count == 0)
          {
            if (libmod->ccbin_data)
              free(libmod->ccbin_data);
            libmod->ccbin_data = NULL;
            libmod->ccbin_size = 0;
            if (libmod->ccbin_path)
            {
              free(libmod->ccbin_path);
              libmod->ccbin_path = NULL;
            }
            remove(ccb_path);
          }
          else
          {
            char ccbin_dir[512];
            char ccbin_base[512];
            split_path(ccb_path, ccbin_dir, sizeof(ccbin_dir), ccbin_base,
                       sizeof(ccbin_base));
            char ccbin_path[1024];
            build_path_with_ext(ccbin_dir, ccbin_base, ".ccbin", ccbin_path,
                                sizeof(ccbin_path));
            if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
            {
              fprintf(stderr, "error: chancecodec executable not resolved "
                              "(required for --library)\n");
              rc = 1;
            }
            else
            {
              int spawn_errno = 0;
              int ccbin_rc = run_chancecodec_emit_ccbin(
                  chancecodec_cmd_to_use, ccb_path, ccbin_path, opt_level,
                  strip_metadata, strip_hard, obfuscate,
                  strip_map_ready ? strip_map_path : NULL, &spawn_errno);
              if (ccbin_rc != 0)
              {
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
              }
              else
              {
                uint8_t *ccbin_data = NULL;
                size_t ccbin_size = 0;
                int read_err =
                    read_file_bytes(ccbin_path, &ccbin_data, &ccbin_size);
                if (read_err != 0)
                {
                  fprintf(stderr, "error: failed reading ccbin '%s' (%s)\n",
                          ccbin_path, strerror(read_err));
                  rc = 1;
                  remove(ccbin_path);
                  remove(ccb_path);
                }
                else
                {
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

      if (!rc && (target_arch == ARCH_X86 || target_arch == ARCH_ARM64 || target_arch == ARCH_BSLASH) &&
          !stop_after_ccb)
      {
        const char *backend = chancecode_backend;
        if (!backend)
        {
          if (target_arch == ARCH_X86)
            backend = "x86-gas";
          else if (target_arch == ARCH_ARM64)
            backend = arm64_backend_name_for_os(target_os);
          else
            backend = "bslash";
        }
        const char *target_os_option = NULL;
        if (target_arch == ARCH_X86 || target_arch == ARCH_ARM64)
          target_os_option = target_os_to_option(target_os);
        if (target_arch == ARCH_ARM64)
        {
          int os_invalid = (!target_os_option ||
                            (strcmp(target_os_option, "macos") != 0 &&
                             strcmp(target_os_option, "linux") != 0 &&
                             strcmp(target_os_option, "windows") != 0));
          if (os_invalid)
          {
            fprintf(stderr,
                    "arm64 backend requires --target-os macos, linux, or windows (current target-os is '%s')\n",
                    target_os_option ? target_os_option : "<unset>");
            rc = 1;
          }
          else if (!backend)
          {
            fprintf(stderr,
                    "arm64 backend unavailable for target-os '%s'\n",
                    target_os_option);
            rc = 1;
          }
        }
        if (rc)
          break;
        if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
        {
          fprintf(stderr,
                  "internal error: ChanceCode CLI command unresolved\n");
          rc = 1;
        }
        else
        {
          char display_cmd[4096];
          build_chancecodec_display_cmd(display_cmd, sizeof(display_cmd),
                                        chancecodec_cmd_to_use, backend,
                                        opt_level, strip_metadata,
                                        strip_hard, obfuscate,
                                        strip_map_ready ? strip_map_path
                                                        : NULL,
                                        debug_symbols,
                                        target_os_option, asm_path, ccb_path);

          int spawn_errno = 0;
          int chance_rc = run_chancecodec_process(
              chancecodec_cmd_to_use, backend, opt_level, strip_metadata,
              strip_hard, obfuscate,
              strip_map_ready ? strip_map_path : NULL,
              debug_symbols, asm_path, ccb_path,
              target_os_option, &spawn_errno);
          if (chance_rc != 0)
          {
            if (chance_rc < 0)
            {
              fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                      chancecodec_cmd_to_use, strerror(spawn_errno));
            }
            else
            {
              fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc,
                      display_cmd);
            }
            if (!chancecodec_has_override && chancecodec_uses_fallback)
            {
              fprintf(stderr,
                      "hint: use --chancecodec <path> or set CHANCECODEC_CMD "
                      "to point at the ChanceCode CLI executable\n");
            }
            rc = 1;
          }
        }
      }

      if (!rc && (target_arch == ARCH_X86 || target_arch == ARCH_ARM64) &&
          !stop_after_ccb &&
          !stop_after_asm)
      {
        if (!need_obj)
        {
          fprintf(stderr,
                  "internal error: object output expected but path missing\n");
          rc = 1;
        }
        else
        {
          char cc_cmd[4096];
          size_t pos = (size_t)snprintf(
              cc_cmd, sizeof(cc_cmd), "\"%s\" -c \"%s\" -o \"%s\"",
              host_cc_cmd_to_use, asm_path, objOut);
          if (pos >= sizeof(cc_cmd))
          {
            fprintf(stderr, "command buffer exhausted for cc invocation\n");
            rc = 1;
          }
          else
          {
            if (debug_symbols)
            {
              strncat(cc_cmd, " -g -gdwarf-4",
                      sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }
            if (freestanding)
            {
              strncat(cc_cmd, " -ffreestanding -nostdlib",
                      sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }
#ifdef _WIN32
            if (target_arch == ARCH_X86)
              strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
            if (target_arch == ARCH_ARM64)
              append_arm64_arch_flag(cc_cmd, sizeof(cc_cmd), target_os,
                                     host_cc_cmd_to_use);
            if (opt_level > 0)
            {
              char optbuf[8];
              snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
              strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }

            int cc_rc = system(cc_cmd);
            if (cc_rc != 0)
            {
              fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
              rc = 1;
            }
          }
        }
      }

      if (!rc && (target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
                  target_arch == ARCH_BSLASH))
      {
        if (!stop_after_ccb && ccb_is_temp)
          remove(ccb_path);
        if (!stop_after_asm)
          remove(asm_path);
      }

      if (!rc && (target_arch == ARCH_X86 || target_arch == ARCH_ARM64) &&
          !stop_after_ccb &&
          !stop_after_asm && !no_link && !multi_link)
      {
        snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
        have_single_obj = 1;
        single_obj_is_temp = obj_is_temp;
      }

      if (!rc && ((no_link && obj_override) || multi_link) &&
          objOut[0] != '\0')
      {
        if (to_cnt == to_cap)
        {
          to_cap = to_cap ? to_cap * 2 : 8;
          temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
        }
        temp_objs[to_cnt++] = xstrdup(objOut);
      }
    }
    else
    {
      rc = 1;
    }

    sema_destroy(sc);
    uc->sc = NULL;
    ast_free(unit);
    uc->unit = NULL;
    if (ps)
    {
      parser_destroy(ps);
      uc->parser = NULL;
    }
    if (uc->stripped)
    {
      free(uc->stripped);
      uc->stripped = NULL;
    }
    if (uc->src)
    {
      free(uc->src);
      uc->src = NULL;
    }
    if (uc->input_path)
    {
      free(uc->input_path);
      uc->input_path = NULL;
    }

    if (rc)
      break;
  }
  if (!emit_library)
  {
    if (compiler_verbose_enabled() && ccb_count > 0)
      verbose_section("Processing CCB inputs");
    for (int ci = 0; !rc && ci < ccb_count; ++ci)
    {
      const char *ccb_input = ccb_inputs[ci];
      if (compiler_verbose_enabled())
        verbose_progress("ccb-proc", ci + 1, ccb_count);
      char dir[512], base[512];
      split_path(ccb_input, dir, sizeof(dir), base, sizeof(base));

      char ccb_path[1024];
      snprintf(ccb_path, sizeof(ccb_path), "%s", ccb_input);
      int ccb_is_temp = 0;

      char asm_path[1024];
      const char *asm_ext = (target_arch == ARCH_BSLASH) ? ".bas" : ".S";
      build_path_with_ext(dir, base, asm_ext, asm_path, sizeof(asm_path));
      if (stop_after_asm)
      {
        int wants_override = ends_with_icase(out, ".s");
        if (target_arch == ARCH_BSLASH)
          wants_override |= ends_with_icase(out, ".bas");
        if (wants_override)
          snprintf(asm_path, sizeof(asm_path), "%s", out);
      }

      char objOut[1024] = {0};
      int obj_is_temp = 0;
      int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                     (no_link || multi_link || target_arch != ARCH_NONE);
      if (need_obj)
      {
        if (no_link && obj_override)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
        }
        else if (no_link)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".obj"
#else
                              ".o"
#endif
                              ,
                              objOut, sizeof(objOut));
        }
        else if (multi_link)
        {
          build_path_with_ext(dir, base,
#ifdef _WIN32
                              ".co.tmp.obj"
#else
                              ".co.tmp.o"
#endif
                              ,
                              objOut, sizeof(objOut));
          obj_is_temp = 1;
        }
        else
        {
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

      if ((target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
           target_arch == ARCH_BSLASH) &&
          !stop_after_ccb)
      {
        const char *backend = chancecode_backend;
        if (!backend)
        {
          if (target_arch == ARCH_X86)
            backend = "x86-gas";
          else if (target_arch == ARCH_ARM64)
            backend = arm64_backend_name_for_os(target_os);
          else
            backend = "bslash";
        }
        const char *target_os_option = NULL;
        if (target_arch == ARCH_X86 || target_arch == ARCH_ARM64)
          target_os_option = target_os_to_option(target_os);
        if (target_arch == ARCH_ARM64)
        {
          int os_invalid = (!target_os_option ||
                            (strcmp(target_os_option, "macos") != 0 &&
                             strcmp(target_os_option, "linux") != 0 &&
                             strcmp(target_os_option, "windows") != 0));
          if (os_invalid)
          {
            fprintf(stderr,
                    "arm64 backend requires --target-os macos, linux, or windows (current target-os is '%s')\n",
                    target_os_option ? target_os_option : "<unset>");
            rc = 1;
            break;
          }
          if (!backend)
          {
            fprintf(stderr,
                    "arm64 backend unavailable for target-os '%s'\n",
                    target_os_option);
            rc = 1;
            break;
          }
        }
        if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
        {
          fprintf(stderr,
                  "internal error: ChanceCode CLI command unresolved\n");
          rc = 1;
          break;
        }
        char display_cmd[4096];
        build_chancecodec_display_cmd(display_cmd, sizeof(display_cmd),
                                      chancecodec_cmd_to_use, backend,
                                      opt_level, strip_metadata,
                                      strip_hard, obfuscate,
                                      strip_map_ready ? strip_map_path : NULL,
                                      debug_symbols,
                                      target_os_option, asm_path, ccb_path);

        int spawn_errno = 0;
        int chance_rc = run_chancecodec_process(
            chancecodec_cmd_to_use, backend, opt_level, strip_metadata,
            strip_hard, obfuscate,
            strip_map_ready ? strip_map_path : NULL, debug_symbols,
            asm_path, ccb_path, target_os_option, &spawn_errno);
        if (chance_rc != 0)
        {
          if (chance_rc < 0)
          {
            fprintf(stderr, "failed to launch chancecodec '%s': %s\n",
                    chancecodec_cmd_to_use, strerror(spawn_errno));
          }
          else
          {
            fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc,
                    display_cmd);
          }
          if (!chancecodec_has_override && chancecodec_uses_fallback)
          {
            fprintf(stderr,
                    "hint: use --chancecodec <path> or set CHANCECODEC_CMD to "
                    "point at the ChanceCode CLI executable\n");
          }
          rc = 1;
          break;
        }
      }

      if ((target_arch == ARCH_X86 || target_arch == ARCH_ARM64) &&
          !stop_after_ccb && !stop_after_asm)
      {
        if (!need_obj)
        {
          fprintf(stderr,
                  "internal error: object output expected but path missing\n");
          rc = 1;
          break;
        }

        char cc_cmd[4096];
        size_t pos = (size_t)snprintf(
            cc_cmd, sizeof(cc_cmd), "\"%s\" -c \"%s\" -o \"%s\"",
            host_cc_cmd_to_use, asm_path, objOut);
        if (pos >= sizeof(cc_cmd))
        {
          fprintf(stderr, "command buffer exhausted for cc invocation\n");
          rc = 1;
          break;
        }
        if (debug_symbols)
          strncat(cc_cmd, " -g -gdwarf-4",
                  sizeof(cc_cmd) - strlen(cc_cmd) - 1);
        if (freestanding)
        {
          strncat(cc_cmd, " -ffreestanding -nostdlib",
                  sizeof(cc_cmd) - strlen(cc_cmd) - 1);
        }
#ifdef _WIN32
  if (target_arch == ARCH_X86)
    strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
        if (target_arch == ARCH_ARM64)
          append_arm64_arch_flag(cc_cmd, sizeof(cc_cmd), target_os,
                                 host_cc_cmd_to_use);
        if (opt_level > 0)
        {
          char optbuf[8];
          snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
          strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
        }

        int cc_rc = system(cc_cmd);
        if (cc_rc != 0)
        {
          fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
          rc = 1;
          break;
        }
      }

      if (target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
          target_arch == ARCH_BSLASH)
      {
        if (!stop_after_asm)
          remove(asm_path);
      }

      if ((target_arch == ARCH_X86 || target_arch == ARCH_ARM64) &&
          !stop_after_ccb && !stop_after_asm &&
          !no_link && !multi_link)
      {
        snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
        have_single_obj = 1;
        single_obj_is_temp = obj_is_temp;
      }

      if (((no_link && obj_override) || multi_link) && objOut[0] != '\0')
      {
        if (to_cnt == to_cap)
        {
          to_cap = to_cap ? to_cap * 2 : 8;
          temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
        }
        temp_objs[to_cnt++] = xstrdup(objOut);
      }
    }
  }
  if (!rc && !emit_library &&
      (target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
       target_arch == ARCH_BSLASH) &&
      !stop_after_ccb)
  {
    if (compiler_verbose_enabled() && library_codegen_units > 0)
      verbose_section("Processing library modules");
    int verbose_lib_progress = 0;
    for (int li = 0; li < loaded_library_count && !rc; ++li)
    {
      LoadedLibrary *lib = &loaded_libraries[li];
      for (uint32_t mi = 0; mi < lib->file.module_count && !rc; ++mi)
      {
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
        if (lib->ccbin_temp_paths)
        {
          free(lib->ccbin_temp_paths[mi]);
          lib->ccbin_temp_paths[mi] = xstrdup(ccbin_path);
        }
        int write_err =
            write_file_bytes(ccbin_path, mod->ccbin_data, mod->ccbin_size);
        if (write_err != 0)
        {
          fprintf(stderr, "error: failed to materialize ccbin '%s' (%s)\n",
                  ccbin_path, strerror(write_err));
          rc = 1;
          break;
        }

        char asm_path[1024];
        const char *asm_ext = (target_arch == ARCH_BSLASH) ? ".bas" : ".S";
        build_path_with_ext(lib_dir, base_component, asm_ext, asm_path,
                            sizeof(asm_path));
        if (stop_after_asm)
        {
          int wants_override = ends_with_icase(out, ".s");
          if (target_arch == ARCH_BSLASH)
            wants_override |= ends_with_icase(out, ".bas");
          if (wants_override)
            snprintf(asm_path, sizeof(asm_path), "%s", out);
        }

        char objOut[1024] = {0};
        int obj_is_temp = 0;
        int need_obj = (!stop_after_ccb && !stop_after_asm) &&
                       (no_link || multi_link || target_arch != ARCH_NONE);
        if (need_obj)
        {
          if (no_link && obj_override)
          {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".co.tmp.obj"
#else
                                ".co.tmp.o"
#endif
                                ,
                                objOut, sizeof(objOut));
          }
          else if (no_link)
          {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".obj"
#else
                                ".o"
#endif
                                ,
                                objOut, sizeof(objOut));
          }
          else if (multi_link)
          {
            build_path_with_ext(lib_dir, base_component,
#ifdef _WIN32
                                ".co.tmp.obj"
#else
                                ".co.tmp.o"
#endif
                                ,
                                objOut, sizeof(objOut));
            obj_is_temp = 1;
          }
          else
          {
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

        const char *backend = chancecode_backend;
        if (!backend)
        {
          if (target_arch == ARCH_X86)
            backend = "x86-gas";
          else if (target_arch == ARCH_ARM64)
            backend = arm64_backend_name_for_os(target_os);
          else
            backend = "bslash";
        }
        const char *target_os_option = NULL;
        if (target_arch == ARCH_X86 || target_arch == ARCH_ARM64)
          target_os_option = target_os_to_option(target_os);
        if (target_arch == ARCH_ARM64)
        {
          int os_invalid = (!target_os_option ||
                            (strcmp(target_os_option, "macos") != 0 &&
                             strcmp(target_os_option, "linux") != 0 &&
                             strcmp(target_os_option, "windows") != 0));
          if (os_invalid)
          {
            fprintf(stderr,
                    "arm64 backend requires --target-os macos, linux, or windows (current target-os is '%s')\n",
                    target_os_option ? target_os_option : "<unset>");
            rc = 1;
            break;
          }
          if (!backend)
          {
            fprintf(stderr,
                    "arm64 backend unavailable for target-os '%s'\n",
                    target_os_option);
            rc = 1;
            break;
          }
        }
        char display_cmd[4096];
        build_chancecodec_display_cmd(display_cmd, sizeof(display_cmd),
                                      chancecodec_cmd_to_use, backend,
                                      opt_level, strip_metadata,
                                      strip_hard, obfuscate,
                                      strip_map_ready ? strip_map_path : NULL,
                                      debug_symbols,
                                      target_os_option, asm_path, ccbin_path);

        int spawn_errno = 0;
        int chance_rc = run_chancecodec_process(
            chancecodec_cmd_to_use, backend, opt_level, strip_metadata,
            strip_hard, obfuscate,
            strip_map_ready ? strip_map_path : NULL, debug_symbols,
            asm_path, ccbin_path, target_os_option, &spawn_errno);
        if (chance_rc != 0)
        {
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

        if (!stop_after_ccb && !stop_after_asm)
        {
          if (!need_obj)
          {
            fprintf(
                stderr,
                "internal error: object output expected but path missing\n");
            rc = 1;
            break;
          }

          char cc_cmd[4096];
          size_t pos = (size_t)snprintf(
              cc_cmd, sizeof(cc_cmd), "\"%s\" -c \"%s\" -o \"%s\"",
              host_cc_cmd_to_use, asm_path, objOut);
          if (pos >= sizeof(cc_cmd))
          {
            fprintf(stderr, "command buffer exhausted for cc invocation\n");
            rc = 1;
            break;
          }
          strncat(cc_cmd, " -g -gdwarf-4",
                  sizeof(cc_cmd) - strlen(cc_cmd) - 1);
          if (freestanding)
            strncat(cc_cmd, " -ffreestanding -nostdlib",
                    sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#ifdef _WIN32
          if (target_arch == ARCH_X86)
            strncat(cc_cmd, " -m64", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
#endif
          if (opt_level > 0)
          {
            char optbuf[8];
            snprintf(optbuf, sizeof(optbuf), " -O%d", opt_level);
            strncat(cc_cmd, optbuf, sizeof(cc_cmd) - strlen(cc_cmd) - 1);
          }
          int cc_rc = system(cc_cmd);
          if (cc_rc != 0)
          {
            fprintf(stderr, "assembler failed (rc=%d): %s\n", cc_rc, cc_cmd);
            rc = 1;
            break;
          }
        }

        if (!stop_after_asm)
          remove(asm_path);

        if (!stop_after_ccb && !stop_after_asm && !no_link && !multi_link)
        {
          snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
          have_single_obj = 1;
          single_obj_is_temp = obj_is_temp;
        }

        if (((no_link && obj_override) || multi_link) && objOut[0] != '\0')
        {
          if (to_cnt == to_cap)
          {
            to_cap = to_cap ? to_cap * 2 : 8;
            temp_objs = (char **)realloc(temp_objs, sizeof(char *) * to_cap);
          }
          temp_objs[to_cnt++] = xstrdup(objOut);
        }

        remove(ccbin_path);
        if (lib->ccbin_temp_paths)
        {
          free(lib->ccbin_temp_paths[mi]);
          lib->ccbin_temp_paths[mi] = NULL;
        }
      }
    }
  }

  if (!rc && emit_library)
  {
    if (library_module_count == 0)
    {
      fprintf(stderr, "error: --library did not collect any modules\n");
      rc = 1;
    }
    else
    {
      if (compiler_verbose_enabled())
      {
        verbose_section("Writing library");
        verbose_table_row("Output", out);
        char buf[32];
        snprintf(buf, sizeof(buf), "%d", library_module_count);
        verbose_table_row("Modules", buf);
      }
      int werr = write_library_file(out, library_modules, library_module_count);
      if (werr != 0)
      {
        fprintf(stderr, "error: failed to write library '%s' (%s)\n", out,
                strerror(werr));
        rc = 1;
      }
    }
    goto cleanup;
  }

  if (!rc && multi_link)
  {
    // Final link of temps + provided objects into an executable
    size_t cmdsz = 4096;
    char *cmd = (char *)xmalloc(cmdsz);
    if (debug_symbols)
      snprintf(cmd, cmdsz, "\"%s\" -g -gdwarf-4 -o \"%s\"",
               host_cc_cmd_to_use, out);
    else
      snprintf(cmd, cmdsz, "\"%s\" -o \"%s\"", host_cc_cmd_to_use, out);
    if (target_arch == ARCH_ARM64)
      append_arm64_arch_flag(cmd, cmdsz, target_os, host_cc_cmd_to_use);
    if (freestanding)
      strncat(cmd, " -ffreestanding -nostdlib", cmdsz - strlen(cmd) - 1);
    for (int i = 0; i < to_cnt; ++i)
    {
      size_t need = strlen(cmd) + strlen(temp_objs[i]) + 8;
      if (need > cmdsz)
      {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, temp_objs[i]);
      strcat(cmd, "\"");
    }
    for (int i = 0; i < obj_count; ++i)
    {
      size_t need = strlen(cmd) + strlen(obj_inputs[i]) + 8;
      if (need > cmdsz)
      {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, obj_inputs[i]);
      strcat(cmd, "\"");
    }
    if (compiler_verbose_enabled())
    {
      verbose_section("Linking executable");
      verbose_table_row("Command", cmd);
    }
    int lrc = system(cmd);
    if (lrc != 0)
    {
      fprintf(stderr, "link failed (rc=%d): %s\n", lrc, cmd);
      rc = 1;
    }
    else
    {
      maybe_generate_dsym(out, debug_symbols, target_os);
    }
    free(cmd);
    // cleanup temp objects
    for (int i = 0; i < to_cnt; ++i)
    {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
    temp_objs = NULL;
    to_cnt = 0;
    to_cap = 0;
  }
  if (!rc && have_single_obj)
  {
    char link_cmd[4096];
    if (debug_symbols)
      snprintf(link_cmd, sizeof(link_cmd),
               "\"%s\" -g -gdwarf-4 -o \"%s\" \"%s\"",
               host_cc_cmd_to_use, out, single_obj_path);
    else
      snprintf(link_cmd, sizeof(link_cmd),
               "\"%s\" -o \"%s\" \"%s\"", host_cc_cmd_to_use, out,
               single_obj_path);
    if (target_arch == ARCH_ARM64)
      append_arm64_arch_flag(link_cmd, sizeof(link_cmd), target_os,
                             host_cc_cmd_to_use);
    if (freestanding)
      strncat(link_cmd, " -ffreestanding -nostdlib",
              sizeof(link_cmd) - strlen(link_cmd) - 1);
    if (compiler_verbose_enabled())
    {
      verbose_section("Linking single object");
      verbose_table_row("Command", link_cmd);
    }
    int lrc = system(link_cmd);
    if (lrc != 0)
    {
      fprintf(stderr, "link failed (rc=%d): %s\n", lrc, link_cmd);
      rc = 1;
    }
    else
    {
      maybe_generate_dsym(out, debug_symbols, target_os);
    }
    if (single_obj_is_temp)
      remove(single_obj_path);
  }
  if (!rc && no_link && obj_override)
  {
    // Merge temps + external objects into obj_override (relocatable link)
    // Build command
    // Validate external objects again (already checked)
    size_t cmdsz = 4096;
    char *cmd = (char *)xmalloc(cmdsz);
    if (debug_symbols)
      snprintf(cmd, cmdsz, "\"%s\" -g -gdwarf-4 -r -o \"%s\"",
               host_cc_cmd_to_use, obj_override);
    else
      snprintf(cmd, cmdsz, "\"%s\" -r -o \"%s\"", host_cc_cmd_to_use,
               obj_override);
    if (target_arch == ARCH_ARM64)
      append_arm64_arch_flag(cmd, cmdsz, target_os, host_cc_cmd_to_use);
    if (freestanding)
      strncat(cmd, " -ffreestanding -nostdlib", cmdsz - strlen(cmd) - 1);
    for (int i = 0; i < to_cnt; ++i)
    {
      size_t need = strlen(cmd) + strlen(temp_objs[i]) + 8;
      if (need > cmdsz)
      {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, temp_objs[i]);
      strcat(cmd, "\"");
    }
    for (int i = 0; i < obj_count; ++i)
    {
      size_t need = strlen(cmd) + strlen(obj_inputs[i]) + 8;
      if (need > cmdsz)
      {
        cmdsz = need + 1024;
        cmd = (char *)realloc(cmd, cmdsz);
      }
      strcat(cmd, " ");
      strcat(cmd, "\"");
      strcat(cmd, obj_inputs[i]);
      strcat(cmd, "\"");
    }
    if (compiler_verbose_enabled())
    {
      verbose_section("Merging objects");
      verbose_table_row("Command", cmd);
    }
    int lrc = system(cmd);
    if (lrc != 0)
    {
      fprintf(stderr, "relocatable link failed (rc=%d): %s\n", lrc, cmd);
      rc = 1;
    }
    free(cmd);
    // cleanup temp objects
    for (int i = 0; i < to_cnt; ++i)
    {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
    temp_objs = NULL;
    to_cnt = 0;
    to_cap = 0;
  }
cleanup:
  if (!rc && project_after_cmd && project_after_cmd[0])
  {
    int after_rc = system(project_after_cmd);
    if (after_rc != 0)
      fprintf(stderr, "warning: after command failed (rc=%d): %s\n", after_rc, project_after_cmd);
  }
  if (temp_objs)
  {
    for (int i = 0; i < to_cnt; ++i)
    {
      remove(temp_objs[i]);
      free(temp_objs[i]);
    }
    free(temp_objs);
  }
  if (units)
  {
    for (int i = 0; i < ce_count; ++i)
    {
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
  if (symbol_ref_units)
  {
    for (int i = 0; i < symbol_ref_ce_count; ++i)
    {
      SymbolRefUnit *sr = &symbol_ref_units[i];
      if (sr->unit)
        ast_free(sr->unit);
      if (sr->stripped)
        free(sr->stripped);
      if (sr->src)
        free(sr->src);
      if (sr->input_path)
        free(sr->input_path);
    }
    free(symbol_ref_units);
  }
  for (int i = 0; i < include_dir_count; i++)
    free(include_dirs[i]);
  free(include_dirs);
  free_library_module_array(library_modules, library_module_count);
  if (loaded_library_functions)
  {
    for (int i = 0; i < loaded_library_function_count; ++i)
      free_loaded_library_function(&loaded_library_functions[i]);
    free(loaded_library_functions);
  }
  if (loaded_libraries)
  {
    for (int i = 0; i < loaded_library_count; ++i)
      free_loaded_library(&loaded_libraries[i]);
    free(loaded_libraries);
  }
  if (owned_ce_inputs)
  {
    for (int i = 0; i < owned_ce_count; ++i)
      free(owned_ce_inputs[i]);
    free(owned_ce_inputs);
  }
  if (ce_obj_outputs)
  {
    for (int i = 0; i < ce_cap; ++i)
      free(ce_obj_outputs[i]);
    free(ce_obj_outputs);
  }
  if (owned_ccb_inputs)
  {
    for (int i = 0; i < owned_ccb_count; ++i)
      free(owned_ccb_inputs[i]);
    free(owned_ccb_inputs);
  }
  if (owned_cclib_inputs)
  {
    for (int i = 0; i < owned_cclib_count; ++i)
      free(owned_cclib_inputs[i]);
    free(owned_cclib_inputs);
  }
  if (owned_obj_inputs)
  {
    for (int i = 0; i < owned_obj_count; ++i)
      free(owned_obj_inputs[i]);
    free(owned_obj_inputs);
  }
  if (project_output_alloc)
    free(project_output_alloc);
  if (project_after_cmd)
    free(project_after_cmd);
  if (override_files)
  {
    for (int i = 0; i < override_file_count; ++i)
    {
      free(override_files[i].original);
      free(override_files[i].replacement);
    }
    free(override_files);
  }
  free((void *)ce_inputs);
  free((void *)ccb_inputs);
  free((void *)cclib_inputs);
  free((void *)obj_inputs);
  free((void *)symbol_ref_ce_inputs);
  free((void *)symbol_ref_cclib_inputs);
  if (strip_map_ready && strip_map_path[0])
    remove(strip_map_path);
  return rc;
fail:
  rc = 2;
  goto cleanup;
}
