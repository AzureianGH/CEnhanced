#include "ast.h"
#include "cclib.h"
#include "chance_version.h"
#include "driver_cli.h"
#include "driver_link.h"
#include "driver_options.h"
#include "driver_overrides.h"
#include "driver_paths.h"
#include "driver_project.h"
#include "driver_runtime.h"
#include "driver_toolchain.h"
#include "driver_types.h"
#include "driver_validate.h"
#include "driver_verbose.h"
#include "frontend.h"
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
#define CLD_BASE "cld"
#define CHS_BASE "chs"
#ifdef _WIN32
#define CHANCE_PATH_SEP '\\'
#define CHANCECODEC_EXT ".exe"
#define CLD_EXT ".exe"
#define CHS_EXT ".exe"
#else
#define CHANCE_PATH_SEP '/'
#define CHANCECODEC_EXT ""
#define CLD_EXT ""
#define CHS_EXT ""
#endif

#ifndef STRIP_MAP_PATH_MAX
#define STRIP_MAP_PATH_MAX 4096
#endif

typedef struct LoadedLibrary LoadedLibrary;
typedef struct LoadedLibraryFunction LoadedLibraryFunction;

static int verbose_use_ansi = 1;
static const char *target_os_to_option(TargetOS os);
static char *make_backend_name_from_module(const char *module_full,
                                           const char *fn_name);
static int run_chs_assemble_process(const char *cmd, TargetArch arch,
                                    TargetOS target_os,
                                    const char *asm_path,
                                    const char *obj_path,
                                    int toolchain_debug_mode,
                                    int toolchain_debug_deep,
                                    int *spawn_errno_out);

static void emit_asm_cstr_bytes(FILE *f, const char *text)
{
  size_t n = text ? strlen(text) : 0;
  fprintf(f, "  .byte ");
  for (size_t i = 0; i < n; ++i)
  {
    fprintf(f, "0x%02x", (unsigned char)text[i]);
    fprintf(f, ", ");
  }
  fprintf(f, "0x00\n");
}

static void verbose_section(const char *title)
{
  driver_verbose_section(title);
}

static void verbose_table_row(const char *label, const char *value)
{
  driver_verbose_table_row(label, value);
}

static void verbose_progress(const char *tag, int current, int total)
{
  driver_verbose_progress(tag, current, total);
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

static void verbose_print_config(
    const char *output_path, int opt_level, TargetArch target_arch,
    TargetOS target_os, int stop_after_ccb, int stop_after_asm, int no_link,
    int emit_library, int freestanding, AsmSyntax asm_syntax,
  const char *entry_symbol,
    int include_dir_count, int ce_count, int ccb_count, int cclib_count,
    int obj_count, int symbol_ref_ce_count, int symbol_ref_cclib_count,
    const char *chancecodec_cmd, int chancecodec_has_override,
    int needs_chancecodec, const char *chancecode_backend,
    int chancecodec_uses_fallback, int verbose_active, int verbose_deep)
{
  DriverVerboseConfigState state = {
      .output_path = output_path,
      .opt_level = opt_level,
      .target_arch = target_arch,
      .target_os_name = target_os_to_option(target_os),
      .stop_after_ccb = stop_after_ccb,
      .stop_after_asm = stop_after_asm,
      .no_link = no_link,
      .emit_library = emit_library,
      .freestanding = freestanding,
      .entry_symbol = entry_symbol,
      .asm_syntax = asm_syntax,
      .include_dir_count = include_dir_count,
      .ce_count = ce_count,
      .ccb_count = ccb_count,
      .cclib_count = cclib_count,
      .obj_count = obj_count,
      .symbol_ref_ce_count = symbol_ref_ce_count,
      .symbol_ref_cclib_count = symbol_ref_cclib_count,
      .chancecodec_cmd = chancecodec_cmd,
      .chancecodec_has_override = chancecodec_has_override,
      .needs_chancecodec = needs_chancecodec,
      .chancecode_backend = chancecode_backend,
      .chancecodec_uses_fallback = chancecodec_uses_fallback,
      .verbose_active = verbose_active,
      .verbose_deep = verbose_deep,
  };
  driver_verbose_print_config(&state);
}

typedef struct
{
  char *input_path;
  char *src;
  char *stripped;
  Node *unit;
  FrontendUnit *frontend_unit;
  const ChanceFrontend *frontend;
} UnitCompile;

typedef struct
{
  char *input_path;
  char *src;
  char *stripped;
  Node *unit;
  FrontendUnit *frontend_unit;
  const ChanceFrontend *frontend;
} SymbolRefUnit;


int sema_eval_const_i32(Node *expr);

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

static int emit_export_launcher_executable(const char *output_exe,
                                           const char *bundle_cclib,
                                           const char *exe_dir,
                                           const char *chs_cmd_to_use,
                                           const char *cld_cmd_to_use,
                                           const char *cld_target_to_use,
                                           int cld_supported_link_target,
                                           TargetArch target_arch,
                                           TargetOS target_os,
                                           int toolchain_debug_mode,
                                           int toolchain_debug_deep,
                                           int debug_symbols)
{
  TargetArch launcher_arch = target_arch;
  if (!output_exe || !*output_exe || !bundle_cclib || !*bundle_cclib)
  {
    fprintf(stderr, "error: export launcher missing required inputs\n");
    return -1;
  }

  char chs_cmd_buf[1024] = {0};
  char cld_cmd_buf[1024] = {0};
  const char *launcher_chs_cmd = chs_cmd_to_use;
  const char *launcher_cld_cmd = cld_cmd_to_use;
  if (!launcher_chs_cmd || !*launcher_chs_cmd)
  {
    const char *env_chs = getenv("CHS_CMD");
    if (env_chs && *env_chs)
    {
      launcher_chs_cmd = env_chs;
    }
    else if (locate_chs(chs_cmd_buf, sizeof(chs_cmd_buf), exe_dir) == 0 &&
             chs_cmd_buf[0])
    {
      launcher_chs_cmd = chs_cmd_buf;
    }
    else
    {
      launcher_chs_cmd = "chs";
    }

    if (!launcher_chs_cmd || !*launcher_chs_cmd)
    {
      fprintf(stderr, "error: failed to resolve CHS for --export-exe\n");
      return -1;
    }
  }
  if (!launcher_cld_cmd || !*launcher_cld_cmd)
  {
    const char *env_cld = getenv("CLD_CMD");
    if (env_cld && *env_cld)
    {
      launcher_cld_cmd = env_cld;
    }
    else if (locate_cld(cld_cmd_buf, sizeof(cld_cmd_buf), exe_dir) == 0 &&
             cld_cmd_buf[0])
    {
      launcher_cld_cmd = cld_cmd_buf;
    }
    else
    {
      launcher_cld_cmd = "cld";
    }

    if (!launcher_cld_cmd || !*launcher_cld_cmd)
    {
      fprintf(stderr, "error: failed to resolve CLD for --export-exe\n");
      return -1;
    }
  }

  if (launcher_arch == ARCH_NONE)
  {
#if defined(__aarch64__) || defined(__arm64__)
    launcher_arch = ARCH_ARM64;
#elif defined(__x86_64__) || defined(_M_X64)
    launcher_arch = ARCH_X86;
#endif
  }

  if (target_os != OS_MACOS || launcher_arch != ARCH_ARM64)
  {
    fprintf(stderr,
            "error: --export-exe native launcher currently supports only macOS arm64 in CHS/CLD mode (arch=%d, os=%d)\n",
            (int)launcher_arch, (int)target_os);
    return -1;
  }
  const char *launcher_cld_target = cld_target_to_use;
  if (!launcher_cld_target || !*launcher_cld_target)
    launcher_cld_target = "macos-arm64";
  if ((!cld_supported_link_target && cld_target_to_use && *cld_target_to_use) ||
      !launcher_cld_target || !*launcher_cld_target)
  {
    fprintf(stderr,
            "error: --export-exe requires CLD target support in CHS/CLD mode\n");
    return -1;
  }

  int blob_len = 0;
  char *blob = read_all(bundle_cclib, &blob_len);
  if (!blob || blob_len <= 0)
  {
    fprintf(stderr, "error: failed to read bundled cclib '%s'\n", bundle_cclib);
    free(blob);
    return -1;
  }

  char asm_template[] = "/tmp/chance_export_launcher_XXXXXX";
  int asm_fd = mkstemp(asm_template);
  if (asm_fd < 0)
  {
    fprintf(stderr, "error: failed to create temporary launcher asm path\n");
    free(blob);
    return -1;
  }
  FILE *asmf = fdopen(asm_fd, "w");
  if (!asmf)
  {
    fprintf(stderr, "error: failed to open temporary launcher asm file\n");
    close(asm_fd);
    remove(asm_template);
    free(blob);
    return -1;
  }

  fprintf(asmf, ".build_version macos, 15, 0\n");
  fprintf(asmf, ".extern _getenv\n");
  fprintf(asmf, ".extern _access\n");
  fprintf(asmf, ".extern _mkstemp\n");
  fprintf(asmf, ".extern _write\n");
  fprintf(asmf, ".extern _close\n");
  fprintf(asmf, ".extern _snprintf\n");
  fprintf(asmf, ".extern _system\n");
  fprintf(asmf, ".extern _remove\n");
  fprintf(asmf, ".extern _strlen\n");
  fprintf(asmf, "\n.section __DATA,__data\n");
  fprintf(asmf, ".p2align 3\n");
  fprintf(asmf, "_launcher_tmp_template:\n");
  emit_asm_cstr_bytes(asmf, "/tmp/chance_bundle_XXXXXX");
  fprintf(asmf, "_launcher_cvm_home:\n");
  emit_asm_cstr_bytes(asmf, "CVM_HOME");
  fprintf(asmf, "_launcher_cvm_suffix_fmt:\n");
  emit_asm_cstr_bytes(asmf, "%s/cvm");
  fprintf(asmf, "_launcher_cvm_fallback:\n");
  emit_asm_cstr_bytes(asmf, "cvm");
  fprintf(asmf, "_launcher_cmd_fmt:\n");
  emit_asm_cstr_bytes(asmf, "\"%s\" \"%s\"");
  fprintf(asmf, "_launcher_err_invalid:\n");
  emit_asm_cstr_bytes(asmf, "error: CVM_HOME is invalid");
  fprintf(asmf, "_launcher_err_nf:\n");
  emit_asm_cstr_bytes(asmf, "framework not found");
  fprintf(asmf, "_launcher_cvm_buf:\n");
  fprintf(asmf, "  .space 1024\n");
  fprintf(asmf, "_launcher_cmd_buf:\n");
  fprintf(asmf, "  .space 2048\n");
  fprintf(asmf, "\n.section __DATA,__const\n");
  fprintf(asmf, ".p2align 3\n");
  fprintf(asmf, "_launcher_payload_start:\n");
  for (int i = 0; i < blob_len; ++i)
  {
    if (i % 16 == 0)
      fprintf(asmf, "  .byte ");
    fprintf(asmf, "0x%02x", (unsigned char)blob[i]);
    if (i + 1 < blob_len && (i % 16) != 15)
      fprintf(asmf, ", ");
    if ((i % 16) == 15 || i + 1 == blob_len)
      fprintf(asmf, "\n");
  }
  fprintf(asmf, "_launcher_payload_end:\n");
  fprintf(asmf, "\n.section __TEXT,__text,regular,pure_instructions\n");
  fprintf(asmf, ".p2align 2\n");
  fprintf(asmf, "_launcher_write_err:\n");
  fprintf(asmf, "  stp x29, x30, [sp, #-16]!\n");
  fprintf(asmf, "  mov x29, sp\n");
  fprintf(asmf, "  stp x19, x20, [sp, #-16]!\n");
  fprintf(asmf, "  mov x19, x0\n");
  fprintf(asmf, "  bl _strlen\n");
  fprintf(asmf, "  mov x2, x0\n");
  fprintf(asmf, "  movz w0, #2\n");
  fprintf(asmf, "  mov x1, x19\n");
  fprintf(asmf, "  bl _write\n");
  fprintf(asmf, "  movz w0, #1\n");
  fprintf(asmf, "  ldp x19, x20, [sp], #16\n");
  fprintf(asmf, "  ldp x29, x30, [sp], #16\n");
  fprintf(asmf, "  ret\n\n");
  fprintf(asmf, ".globl _main\n");
  fprintf(asmf, "_main:\n");
  fprintf(asmf, "  stp x29, x30, [sp, #-16]!\n");
  fprintf(asmf, "  mov x29, sp\n");
  fprintf(asmf, "  stp x19, x20, [sp, #-16]!\n");
  fprintf(asmf, "  stp x21, x22, [sp, #-16]!\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  adrp x0, _launcher_cvm_home@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_cvm_home@PAGEOFF\n");
  fprintf(asmf, "  bl _getenv\n");
  fprintf(asmf, "  cbz x0, Lno_home\n");
  fprintf(asmf, "  mov x19, x0\n");
  fprintf(asmf, "  mov x0, x19\n");
  fprintf(asmf, "  movz w1, #1\n");
  fprintf(asmf, "  bl _access\n");
  fprintf(asmf, "  cmp w0, wzr\n");
  fprintf(asmf, "  b.eq Lhave_cvm\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  adrp x0, _launcher_cvm_buf@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_cvm_buf@PAGEOFF\n");
  fprintf(asmf, "  movz x1, #1024\n");
  fprintf(asmf, "  adrp x2, _launcher_cvm_suffix_fmt@PAGE\n");
  fprintf(asmf, "  add x2, x2, _launcher_cvm_suffix_fmt@PAGEOFF\n");
  fprintf(asmf, "  mov x3, x19\n");
  fprintf(asmf, "  bl _snprintf\n");
  fprintf(asmf, "  adrp x0, _launcher_cvm_buf@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_cvm_buf@PAGEOFF\n");
  fprintf(asmf, "  movz w1, #1\n");
  fprintf(asmf, "  bl _access\n");
  fprintf(asmf, "  cmp w0, wzr\n");
  fprintf(asmf, "  b.ne Linvalid_home\n");
  fprintf(asmf, "  adrp x19, _launcher_cvm_buf@PAGE\n");
  fprintf(asmf, "  add x19, x19, _launcher_cvm_buf@PAGEOFF\n");
  fprintf(asmf, "  b Lhave_cvm\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "Lno_home:\n");
  fprintf(asmf, "  adrp x19, _launcher_cvm_fallback@PAGE\n");
  fprintf(asmf, "  add x19, x19, _launcher_cvm_fallback@PAGEOFF\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "Lhave_cvm:\n");
  fprintf(asmf, "  adrp x0, _launcher_tmp_template@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_tmp_template@PAGEOFF\n");
  fprintf(asmf, "  bl _mkstemp\n");
  fprintf(asmf, "  cmp w0, wzr\n");
  fprintf(asmf, "  b.lt Lnot_found\n");
  fprintf(asmf, "  mov w20, w0\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  sxtw x0, w20\n");
  fprintf(asmf, "  adrp x1, _launcher_payload_start@PAGE\n");
  fprintf(asmf, "  add x1, x1, _launcher_payload_start@PAGEOFF\n");
  fprintf(asmf, "  adrp x2, _launcher_payload_end@PAGE\n");
  fprintf(asmf, "  add x2, x2, _launcher_payload_end@PAGEOFF\n");
  fprintf(asmf, "  sub x2, x2, x1\n");
  fprintf(asmf, "  bl _write\n");
  fprintf(asmf, "  sxtw x0, w20\n");
  fprintf(asmf, "  bl _close\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  adrp x0, _launcher_cmd_buf@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_cmd_buf@PAGEOFF\n");
  fprintf(asmf, "  movz x1, #2048\n");
  fprintf(asmf, "  adrp x2, _launcher_cmd_fmt@PAGE\n");
  fprintf(asmf, "  add x2, x2, _launcher_cmd_fmt@PAGEOFF\n");
  fprintf(asmf, "  mov x3, x19\n");
  fprintf(asmf, "  adrp x4, _launcher_tmp_template@PAGE\n");
  fprintf(asmf, "  add x4, x4, _launcher_tmp_template@PAGEOFF\n");
  fprintf(asmf, "  bl _snprintf\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  adrp x0, _launcher_cmd_buf@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_cmd_buf@PAGEOFF\n");
  fprintf(asmf, "  bl _system\n");
  fprintf(asmf, "  mov w21, w0\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  adrp x0, _launcher_tmp_template@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_tmp_template@PAGEOFF\n");
  fprintf(asmf, "  bl _remove\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "  mov w0, w21\n");
  fprintf(asmf, "  b Ldone\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "Linvalid_home:\n");
  fprintf(asmf, "  adrp x0, _launcher_err_invalid@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_err_invalid@PAGEOFF\n");
  fprintf(asmf, "  bl _launcher_write_err\n");
  fprintf(asmf, "  b Ldone\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "Lnot_found:\n");
  fprintf(asmf, "  adrp x0, _launcher_err_nf@PAGE\n");
  fprintf(asmf, "  add x0, x0, _launcher_err_nf@PAGEOFF\n");
  fprintf(asmf, "  bl _launcher_write_err\n");
  fprintf(asmf, "\n");
  fprintf(asmf, "Ldone:\n");
  fprintf(asmf, "  ldp x21, x22, [sp], #16\n");
  fprintf(asmf, "  ldp x19, x20, [sp], #16\n");
  fprintf(asmf, "  ldp x29, x30, [sp], #16\n");
  fprintf(asmf, "  ret\n");

  fclose(asmf);
  free(blob);

  char obj_template[] = "/tmp/chance_export_launcher_obj_XXXXXX";
  int obj_fd = mkstemp(obj_template);
  if (obj_fd < 0)
  {
    fprintf(stderr, "error: failed to create temporary launcher object path\n");
    remove(asm_template);
    return -1;
  }
  close(obj_fd);

  int spawn_errno = 0;
  int chs_rc = run_chs_assemble_process(launcher_chs_cmd, launcher_arch, target_os,
                                        asm_template, obj_template,
                                        toolchain_debug_mode,
                                        toolchain_debug_deep, &spawn_errno);
  remove(asm_template);
  if (chs_rc != 0)
  {
    fprintf(stderr, "error: export launcher CHS stage failed (rc=%d)\n", chs_rc);
    if (chs_rc < 0)
      fprintf(stderr, "error: failed to run CHS for export launcher (%s)\n",
              strerror(spawn_errno ? spawn_errno : errno));
    remove(obj_template);
    return -1;
  }

  DriverLinkPhaseState launcher_link_state = {
      .multi_link = 0,
      .have_single_obj = 1,
      .single_obj_is_temp = 1,
      .no_link = 0,
      .obj_override = NULL,
      .cld_supported_link_target = 1,
      .cld_cmd_to_use = launcher_cld_cmd,
      .cld_target_to_use = launcher_cld_target,
      .cld_uses_fallback = 0,
      .freestanding = 0,
      .toolchain_debug_mode = toolchain_debug_mode,
      .toolchain_debug_deep = toolchain_debug_deep,
      .debug_symbols = debug_symbols,
      .target_os = target_os,
      .entry_symbol = "main",
      .out = output_exe,
      .obj_inputs = NULL,
      .obj_count = 0,
      .single_obj_path = obj_template,
      .temp_objs = NULL,
      .to_cnt = NULL,
      .to_cap = NULL,
  };
  int link_rc = run_driver_link_phase(&launcher_link_state);
  if (link_rc != 0)
    fprintf(stderr, "error: export launcher CLD stage failed\n");
  remove(obj_template);
  return link_rc;
}

static int unit_is_embedded_force_inline_literal_only(const Node *unit)
{
  if (!unit || unit->kind != ND_UNIT)
    return 0;

  int saw_function = 0;
  for (int i = 0; i < unit->stmt_count; ++i)
  {
    const Node *decl = unit->stmts[i];
    if (!decl)
      continue;
    if (decl->kind == ND_FUNC)
    {
      saw_function = 1;
      if (!decl->is_exposed || !decl->is_literal || !decl->force_inline_literal ||
          decl->is_preserve)
        return 0;
      continue;
    }
    if (decl->kind == ND_VAR_DECL && decl->var_is_global)
      return 0;
  }

  return saw_function;
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

static int strip_symbol_list_contains(const StripSymbolList *list,
                                      const char *name)
{
  if (!list || !name || !*name)
    return 0;
  for (size_t i = 0; i < list->count; ++i)
  {
    if (list->items[i] && strcmp(list->items[i], name) == 0)
      return 1;
  }
  return 0;
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
      if (fn->is_entrypoint)
        continue;
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
  StripSymbolList ccb_symbols = {0};
  StripSymbolList preserved_symbols = {0};
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

    if (strncmp(p, ".preserve", 9) == 0 && isspace((unsigned char)p[9]))
    {
      p += 9;
      while (*p && isspace((unsigned char)*p))
        ++p;
      if (*p == '\0')
        continue;
      char name_buf[512];
      size_t ni = 0;
      while (*p && !isspace((unsigned char)*p) && ni + 1 < sizeof(name_buf))
        name_buf[ni++] = *p++;
      name_buf[ni] = '\0';
      if (name_buf[0] && strip_symbol_list_add(&preserved_symbols, name_buf) != 0)
      {
        fclose(f);
        strip_symbol_list_destroy(&ccb_symbols);
        strip_symbol_list_destroy(&preserved_symbols);
        return 1;
      }
      continue;
    }

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
    if (strip_symbol_list_add(&ccb_symbols, name_buf) != 0)
    {
      fclose(f);
      strip_symbol_list_destroy(&ccb_symbols);
      strip_symbol_list_destroy(&preserved_symbols);
      return 1;
    }
  }
  fclose(f);

  if (strip_symbol_list_add(&preserved_symbols, "main") != 0)
  {
    strip_symbol_list_destroy(&ccb_symbols);
    strip_symbol_list_destroy(&preserved_symbols);
    return 1;
  }

  for (size_t i = 0; i < ccb_symbols.count; ++i)
  {
    const char *name = ccb_symbols.items[i];
    if (!name || !*name)
      continue;
    if (strip_symbol_list_contains(&preserved_symbols, name))
      continue;
    if (strip_symbol_list_add(symbols, name) != 0)
    {
      strip_symbol_list_destroy(&ccb_symbols);
      strip_symbol_list_destroy(&preserved_symbols);
      return 1;
    }
  }

  strip_symbol_list_destroy(&ccb_symbols);
  strip_symbol_list_destroy(&preserved_symbols);
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
                                          int toolchain_debug_mode,
                                          int toolchain_debug_deep,
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
  if (toolchain_debug_deep)
    strncat(buffer, " -vd", bufsz - strlen(buffer) - 1);
  else if (toolchain_debug_mode)
    strncat(buffer, " -d", bufsz - strlen(buffer) - 1);
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
  char **field_defaults;
  uint32_t *field_offsets;
  uint32_t field_count;
  uint32_t size_bytes;
  int is_exposed;
  int is_bundle;
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
static Type builtin_ty_void_ptr = {.kind = TY_PTR, .pointee = &builtin_ty_void};
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
  if (st->field_defaults)
  {
    for (uint32_t i = 0; i < st->field_count; ++i)
      free(st->field_defaults[i]);
    free(st->field_defaults);
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

static int library_specs_equal(const char *a, const char *b)
{
  if (!a)
    a = "";
  if (!b)
    b = "";
  return strcmp(a, b) == 0;
}

static int library_function_params_equal(const LibraryFunction *fn,
                                         const char **param_specs,
                                         int param_count)
{
  if (!fn)
    return 0;
  if (fn->param_count != param_count)
    return 0;
  for (int i = 0; i < param_count; ++i)
  {
    const char *lhs = (fn->param_specs && i < fn->param_count)
                          ? fn->param_specs[i]
                          : NULL;
    const char *rhs = (param_specs && i < param_count)
                          ? param_specs[i]
                          : NULL;
    if (!library_specs_equal(lhs, rhs))
      return 0;
  }
  return 1;
}

static int library_module_has_function_signature(const LibraryModuleData *mod,
                                                 const char *name,
                                                 const char *backend_name,
                                                 const char **param_specs,
                                                 int param_count,
                                                 int is_varargs)
{
  if (!mod)
    return 0;
  for (int i = 0; i < mod->function_count; ++i)
  {
    const LibraryFunction *fn = &mod->functions[i];
    if (!fn)
      continue;

    if (backend_name && fn->backend_name && strcmp(fn->backend_name, backend_name) == 0)
      return 1;

    if (!name || !fn->name || strcmp(fn->name, name) != 0)
      continue;
    if ((fn->is_varargs ? 1 : 0) != (is_varargs ? 1 : 0))
      continue;
    if (!library_function_params_equal(fn, param_specs, param_count))
      continue;

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

static int library_module_has_global(const LibraryModuleData *mod,
                                     const char *name)
{
  if (!mod || !name)
    return 0;
  for (int i = 0; i < mod->global_count; ++i)
  {
    if (mod->globals[i].name && strcmp(mod->globals[i].name, name) == 0)
      return 1;
  }
  return 0;
}

static int merge_loaded_libraries_into_library_modules(
    const LoadedLibrary *loaded_libraries, int loaded_library_count,
    LibraryModuleData **library_modules, int *library_module_count,
    int *library_module_cap)
{
  if (!loaded_libraries || loaded_library_count <= 0 || !library_modules ||
      !library_module_count || !library_module_cap)
    return 0;

  for (int li = 0; li < loaded_library_count; ++li)
  {
    const LoadedLibrary *lib = &loaded_libraries[li];
    if (!lib)
      continue;

    for (uint32_t mi = 0; mi < lib->file.module_count; ++mi)
    {
      const CclibModule *src = &lib->file.modules[mi];
      if (!src || !src->module_name || !src->module_name[0])
        continue;

      LibraryModuleData *dst =
          find_or_add_module(library_modules, library_module_count,
                             library_module_cap, src->module_name);
      if (!dst)
        return 1;

      for (uint32_t fi = 0; fi < src->function_count; ++fi)
      {
        const CclibFunction *cf = &src->functions[fi];
        if (!cf->name ||
            library_module_has_function_signature(dst, cf->name, cf->backend_name,
                                                  (const char **)cf->param_types,
                                                  (int)cf->param_count,
                                                  cf->is_varargs ? 1 : 0))
          continue;

        LibraryFunction lf = {0};
        lf.name = xstrdup(cf->name);
        lf.backend_name = cf->backend_name ? xstrdup(cf->backend_name) : NULL;
        lf.return_spec = cf->return_type ? xstrdup(cf->return_type) : NULL;
        lf.param_count = (int)cf->param_count;
        lf.is_varargs = cf->is_varargs ? 1 : 0;
        lf.is_noreturn = cf->is_noreturn ? 1 : 0;
        lf.is_exposed = cf->is_exposed ? 1 : 0;

        if (lf.param_count > 0)
        {
          lf.param_specs =
              (char **)xcalloc((size_t)lf.param_count, sizeof(char *));
          if (!lf.param_specs)
          {
            free_library_function(&lf);
            return 1;
          }
          for (int pi = 0; pi < lf.param_count; ++pi)
          {
            const char *spec =
                (cf->param_types && (uint32_t)pi < cf->param_count)
                    ? cf->param_types[pi]
                    : NULL;
            lf.param_specs[pi] = spec ? xstrdup(spec) : NULL;
          }
        }

        if (append_library_function(dst, &lf) != 0)
        {
          free_library_function(&lf);
          return 1;
        }
      }

      for (uint32_t gi = 0; gi < src->global_count; ++gi)
      {
        const CclibGlobal *cg = &src->globals[gi];
        if (!cg->name || library_module_has_global(dst, cg->name))
          continue;

        LibraryGlobal lg = {0};
        lg.name = xstrdup(cg->name);
        lg.type_spec = cg->type_spec ? xstrdup(cg->type_spec) : NULL;
        lg.is_const = cg->is_const ? 1 : 0;
        if (append_library_global(dst, &lg) != 0)
        {
          free_library_global(&lg);
          return 1;
        }
      }

      for (uint32_t si = 0; si < src->struct_count; ++si)
      {
        const CclibStruct *cs = &src->structs[si];
        if (!cs->name || library_module_has_struct(dst, cs->name))
          continue;

        LibraryStruct ls = {0};
        ls.name = xstrdup(cs->name);
        ls.field_count = cs->field_count;
        ls.size_bytes = cs->size_bytes;
        ls.is_exposed = cs->is_exposed ? 1 : 0;
        ls.is_bundle = cs->is_bundle ? 1 : 0;

        if (ls.field_count > 0)
        {
          ls.field_names =
              (char **)xcalloc((size_t)ls.field_count, sizeof(char *));
          ls.field_specs =
              (char **)xcalloc((size_t)ls.field_count, sizeof(char *));
          ls.field_defaults =
              (char **)xcalloc((size_t)ls.field_count, sizeof(char *));
          ls.field_offsets =
              (uint32_t *)xcalloc((size_t)ls.field_count, sizeof(uint32_t));
          if (!ls.field_names || !ls.field_specs || !ls.field_defaults ||
              !ls.field_offsets)
          {
            free_library_struct(&ls);
            return 1;
          }

          for (uint32_t fi = 0; fi < ls.field_count; ++fi)
          {
            ls.field_names[fi] =
                (cs->field_names && cs->field_names[fi])
                    ? xstrdup(cs->field_names[fi])
                    : NULL;
            ls.field_specs[fi] =
                (cs->field_types && cs->field_types[fi])
                    ? xstrdup(cs->field_types[fi])
                    : NULL;
            ls.field_defaults[fi] =
                (cs->field_defaults && cs->field_defaults[fi])
                    ? xstrdup(cs->field_defaults[fi])
                    : NULL;
            ls.field_offsets[fi] =
                (cs->field_offsets ? cs->field_offsets[fi] : 0u);
          }
        }

        if (append_library_struct(dst, &ls) != 0)
        {
          free_library_struct(&ls);
          return 1;
        }
      }

      for (uint32_t ei = 0; ei < src->enum_count; ++ei)
      {
        const CclibEnum *ce = &src->enums[ei];
        if (!ce->name || library_module_has_enum(dst, ce->name))
          continue;

        LibraryEnum le = {0};
        le.name = xstrdup(ce->name);
        le.value_count = ce->value_count;
        le.is_exposed = ce->is_exposed ? 1 : 0;

        if (le.value_count > 0)
        {
          le.value_names =
              (char **)xcalloc((size_t)le.value_count, sizeof(char *));
          le.values =
              (int32_t *)xcalloc((size_t)le.value_count, sizeof(int32_t));
          if (!le.value_names || !le.values)
          {
            free_library_enum(&le);
            return 1;
          }

          for (uint32_t vi = 0; vi < le.value_count; ++vi)
          {
            le.value_names[vi] =
                (ce->values && ce->values[vi].name)
                    ? xstrdup(ce->values[vi].name)
                    : NULL;
            le.values[vi] =
                (ce->values ? ce->values[vi].value : 0);
          }
        }

        if (append_library_enum(dst, &le) != 0)
        {
          free_library_enum(&le);
          return 1;
        }
      }

      if (!dst->ccbin_data && src->ccbin_data && src->ccbin_size > 0)
      {
        dst->ccbin_data = (uint8_t *)xmalloc(src->ccbin_size);
        if (!dst->ccbin_data)
          return 1;
        memcpy(dst->ccbin_data, src->ccbin_data, src->ccbin_size);
        dst->ccbin_size = src->ccbin_size;
      }
    }
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
    if ((!fn->is_exposed && !fn->is_entrypoint) || !fn->name)
      continue;
    LibraryFunction out = {0};
    out.name = xstrdup(fn->name);
    const char *backend = NULL;
    if (fn->export_name)
      backend = (fn->raw_export_name && fn->metadata.backend_name && fn->metadata.backend_name[0])
                    ? fn->metadata.backend_name
                    : fn->name;
    else
      backend = fn->metadata.backend_name ? fn->metadata.backend_name : fn->name;
    out.backend_name = backend ? xstrdup(backend) : NULL;
    Type *ret_ty = fn->ret_type ? fn->ret_type : type_i32();
    out.return_spec = type_to_spec(ret_ty);
    int hidden_param_count = 0;
    for (int pi = 0; pi < fn->param_count; ++pi)
    {
      Type *pt = (fn->param_types && pi < fn->param_count)
                     ? module_registry_canonical_type(fn->param_types[pi])
                     : NULL;
      if (fn->is_managed && pt && pt->kind == TY_ARRAY && pt->array.is_unsized)
        hidden_param_count++;
    }
    Type *canon_ret = module_registry_canonical_type(ret_ty);
    if (fn->is_managed && canon_ret && canon_ret->kind == TY_ARRAY && canon_ret->array.is_unsized)
      hidden_param_count++;

    out.param_count = fn->param_count + hidden_param_count;
    if (out.param_count > 0)
    {
      out.param_specs =
          (char **)xcalloc((size_t)out.param_count, sizeof(char *));
      if (!out.param_specs)
      {
        free_library_function(&out);
        return 1;
      }
      int out_pi = 0;
      for (int pi = 0; pi < fn->param_count; ++pi)
      {
        Type *pt = (fn->param_types && pi < fn->param_count)
                       ? fn->param_types[pi]
                       : NULL;
        out.param_specs[out_pi] = type_to_spec(pt);
        if (!out.param_specs[out_pi])
        {
          free_library_function(&out);
          return 1;
        }
        out_pi++;

        Type *canon_pt = module_registry_canonical_type(pt);
        if (fn->is_managed && canon_pt && canon_pt->kind == TY_ARRAY && canon_pt->array.is_unsized)
        {
          out.param_specs[out_pi] = type_to_spec(&builtin_ty_u64);
          if (!out.param_specs[out_pi])
          {
            free_library_function(&out);
            return 1;
          }
          out_pi++;
        }
      }

      if (fn->is_managed && canon_ret && canon_ret->kind == TY_ARRAY && canon_ret->array.is_unsized)
      {
        out.param_specs[out_pi] = type_to_spec(&builtin_ty_void_ptr);
        if (!out.param_specs[out_pi])
        {
          free_library_function(&out);
          return 1;
        }
      }
    }
    out.is_varargs = fn->is_varargs ? 1 : 0;
    out.is_noreturn = fn->is_noreturn;
    out.is_exposed = (fn->is_exposed || fn->is_entrypoint) ? 1 : 0;

    if (library_module_has_function_signature(mod, out.name, out.backend_name,
                                              (const char **)out.param_specs,
                                              out.param_count,
                                              out.is_varargs))
    {
      free_library_function(&out);
      continue;
    }

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
      st.field_defaults = (char **)xcalloc((size_t)field_count, sizeof(char *));
      st.field_offsets =
          (uint32_t *)xcalloc((size_t)field_count, sizeof(uint32_t));
      if (!st.field_names || !st.field_specs || !st.field_defaults || !st.field_offsets)
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
        const char *field_default = (ty->strct.field_default_values && fi < field_count)
                ? ty->strct.field_default_values[fi]
                : NULL;
        st.field_defaults[fi] = field_default ? xstrdup(field_default) : NULL;
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
    st.is_bundle = ty->is_bundle ? 1 : 0;
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

static void trim_ascii_inplace(char *s)
{
  if (!s)
    return;
  size_t len = strlen(s);
  size_t start = 0;
  while (start < len && isspace((unsigned char)s[start]))
    ++start;
  size_t end = len;
  while (end > start && isspace((unsigned char)s[end - 1]))
    --end;
  if (start > 0)
    memmove(s, s + start, end - start);
  s[end - start] = '\0';
}

static int parse_func_signature_from_ccb_line(const char *line,
                                              LibraryFunction *out)
{
  if (!line || !out)
    return 0;
  memset(out, 0, sizeof(*out));

  const char *prefix = ".func ";
  size_t prefix_len = strlen(prefix);
  if (strncmp(line, prefix, prefix_len) != 0)
    return 0;

  const char *cursor = line + prefix_len;
  while (*cursor && isspace((unsigned char)*cursor))
    ++cursor;
  if (!*cursor)
    return 0;

  const char *name_end = cursor;
  while (*name_end && !isspace((unsigned char)*name_end))
    ++name_end;
  if (name_end == cursor)
    return 0;

  size_t name_len = (size_t)(name_end - cursor);
  char *name = (char *)xmalloc(name_len + 1);
  memcpy(name, cursor, name_len);
  name[name_len] = '\0';
  out->name = name;
  out->backend_name = xstrdup(name);

  const char *ret_pos = strstr(name_end, " ret=");
  if (!ret_pos)
    ret_pos = strstr(name_end, "ret=");
  if (!ret_pos)
  {
    free_library_function(out);
    return 0;
  }
  ret_pos = strstr(ret_pos, "ret=");
  ret_pos += 4;
  const char *ret_end = ret_pos;
  while (*ret_end && !isspace((unsigned char)*ret_end))
    ++ret_end;
  size_t ret_len = (size_t)(ret_end - ret_pos);
  out->return_spec = (char *)xmalloc(ret_len + 1);
  memcpy(out->return_spec, ret_pos, ret_len);
  out->return_spec[ret_len] = '\0';

  out->param_count = 0;
  const char *params_pos = strstr(name_end, " params=");
  if (!params_pos)
    params_pos = strstr(name_end, "params=");
  if (params_pos)
  {
    params_pos = strstr(params_pos, "params=");
    params_pos += 7;
    out->param_count = (int)strtol(params_pos, NULL, 10);
    if (out->param_count < 0)
      out->param_count = 0;
  }

  if (out->param_count > 0)
  {
    out->param_specs =
        (char **)xcalloc((size_t)out->param_count, sizeof(char *));
    if (!out->param_specs)
    {
      free_library_function(out);
      return 0;
    }
    for (int i = 0; i < out->param_count; ++i)
      out->param_specs[i] = xstrdup("i32");
  }

  out->is_varargs = strstr(name_end, " varargs") ? 1 : 0;
  out->is_noreturn = 0;
  out->is_exposed = 1;
  return 1;
}

static void apply_params_line_to_function(LibraryFunction *fn,
                                          const char *line)
{
  if (!fn || !line || fn->param_count <= 0)
    return;
  const char *prefix = ".params";
  if (strncmp(line, prefix, strlen(prefix)) != 0)
    return;

  char *copy = xstrdup(line + strlen(prefix));
  if (!copy)
    return;
  trim_ascii_inplace(copy);
  if (!copy[0])
  {
    free(copy);
    return;
  }

  int idx = 0;
  char *tok = strtok(copy, " \t\r\n");
  while (tok && idx < fn->param_count)
  {
    free(fn->param_specs[idx]);
    fn->param_specs[idx] = xstrdup(tok);
    ++idx;
    tok = strtok(NULL, " \t\r\n");
  }
  free(copy);
}

static int collect_metadata_for_ccb_text(const char *ccb_path,
                                         const char *module_name,
                                         LibraryModuleData **modules,
                                         int *module_count,
                                         int *module_cap)
{
  if (!ccb_path || !*ccb_path || !module_name || !*module_name || !modules ||
      !module_count || !module_cap)
    return 1;

  FILE *f = fopen(ccb_path, "rb");
  if (!f)
    return 1;

  LibraryModuleData *mod =
      find_or_add_module(modules, module_count, module_cap, module_name);
  if (!mod)
  {
    fclose(f);
    return 1;
  }
  if (!mod->ccb_path)
    mod->ccb_path = xstrdup(ccb_path);

  char line[4096];
  int current_fn = -1;
  while (fgets(line, sizeof(line), f))
  {
    trim_ascii_inplace(line);
    if (!line[0])
      continue;

    if (strncmp(line, ".func ", 6) == 0)
    {
      LibraryFunction fn = {0};
      if (!parse_func_signature_from_ccb_line(line, &fn))
      {
        fclose(f);
        return 1;
      }
      if (append_library_function(mod, &fn) != 0)
      {
        free_library_function(&fn);
        fclose(f);
        return 1;
      }
      current_fn = mod->function_count - 1;
      continue;
    }

    if (strncmp(line, ".params", 7) == 0 && current_fn >= 0 &&
        current_fn < mod->function_count)
    {
      apply_params_line_to_function(&mod->functions[current_fn], line);
      continue;
    }

    if (strncmp(line, ".endfunc", 8) == 0)
    {
      current_fn = -1;
      continue;
    }
  }

  fclose(f);
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
        cs->field_defaults = ls->field_defaults;
        cs->field_offsets = ls->field_offsets;
        cs->field_count = ls->field_count;
        cs->size_bytes = ls->size_bytes;
        cs->is_exposed = (uint8_t)(ls->is_exposed ? 1 : 0);
        cs->is_bundle = (uint8_t)(ls->is_bundle ? 1 : 0);
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
    if (ty->strct.field_default_values)
    {
      char **defaults = (char **)ty->strct.field_default_values;
      for (int i = 0; i < ty->strct.field_count; ++i)
        free(defaults[i]);
      free(defaults);
    }
    if (ty->strct.field_offsets)
      free(ty->strct.field_offsets);
    free((void *)ty->struct_name);
  }
  else if (ty->kind == TY_IMPORT)
  {
    free((void *)ty->import_module);
    free((void *)ty->import_type_name);
    
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
    ty->is_bundle = st->is_bundle ? 1 : 0;
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
      char **defaults = (char **)xcalloc((size_t)field_count, sizeof(char *));
      int *offsets = (int *)xcalloc((size_t)field_count, sizeof(int));
      if (!names || !types || !defaults || !offsets)
      {
        free(names);
        free(types);
        free(defaults);
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
        defaults[fi] = (st->field_defaults && fi < field_count && st->field_defaults[fi])
                           ? xstrdup(st->field_defaults[fi])
                           : NULL;
        offsets[fi] = (st->field_offsets && fi < field_count)
                          ? (int)st->field_offsets[fi]
                          : 0;
      }
      ty->strct.field_names = (const char **)names;
      ty->strct.field_types = types;
      ty->strct.field_default_values = (const char **)defaults;
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

static void symtab_add_library_functions(FrontendUnit *frontend_unit, Node *unit,
                                         const LoadedLibraryFunction *funcs,
                                         int func_count)
{
  if (!frontend_unit || !unit || !funcs || func_count <= 0)
    return;
  SymTable *syms = chance_frontend_unit_symtab(frontend_unit);
  if (!syms)
    return;
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
      chance_frontend_unit_track_imported_function(frontend_unit, lf->name,
                                                   module_full, &unqual);
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
                                   int toolchain_debug_mode,
                                   int toolchain_debug_deep,
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
  if (toolchain_debug_deep)
    args[idx++] = "-vd";
  else if (toolchain_debug_mode)
    args[idx++] = "-d";
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
  if (toolchain_debug_deep)
    args[idx++] = "-vd";
  else if (toolchain_debug_mode)
    args[idx++] = "-d";
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
                                      int toolchain_debug_mode,
                                      int toolchain_debug_deep,
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
  if (toolchain_debug_deep)
    args[idx++] = "-vd";
  else if (toolchain_debug_mode)
    args[idx++] = "-d";
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
  if (toolchain_debug_deep)
    args[idx++] = "-vd";
  else if (toolchain_debug_mode)
    args[idx++] = "-d";
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

static const char *chs_arch_name_for_target(TargetArch arch)
{
  switch (arch)
  {
  case ARCH_X86:
    return "x86_64";
  case ARCH_ARM64:
    return "arm64";
  case ARCH_BSLASH:
    return "bslash";
  default:
    return NULL;
  }
}

static const char *chs_format_name_for_target(TargetOS os)
{
  switch (os)
  {
  case OS_MACOS:
    return "macho";
  case OS_LINUX:
    return "elf64";
  case OS_WINDOWS:
    return "elf64";
  default:
    return NULL;
  }
}

static int run_chs_assemble_process(const char *cmd, TargetArch arch,
                                    TargetOS target_os,
                                    const char *asm_path,
                                    const char *obj_path,
                                    int toolchain_debug_mode,
                                    int toolchain_debug_deep,
                                    int *spawn_errno_out)
{
  if (spawn_errno_out)
    *spawn_errno_out = 0;
  const char *arch_name = chs_arch_name_for_target(arch);
  const char *format_name = chs_format_name_for_target(target_os);
  if (!cmd || !*cmd || !arch_name || !format_name || !asm_path || !*asm_path ||
      !obj_path || !*obj_path)
  {
    if (spawn_errno_out)
      *spawn_errno_out = EINVAL;
    return -1;
  }
#ifdef _WIN32
  const char *args[10];
  int idx = 0;
  args[idx++] = cmd;
  args[idx++] = "--arch";
  args[idx++] = arch_name;
  args[idx++] = "--format";
  args[idx++] = format_name;
  args[idx++] = "--output";
  args[idx++] = obj_path;
  if (toolchain_debug_deep)
    args[idx++] = "-vd";
  else if (toolchain_debug_mode)
    args[idx++] = "-d";
  args[idx++] = asm_path;
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
  char *args[10];
  int idx = 0;
  args[idx++] = (char *)cmd;
  args[idx++] = (char *)"--arch";
  args[idx++] = (char *)arch_name;
  args[idx++] = (char *)"--format";
  args[idx++] = (char *)format_name;
  args[idx++] = (char *)"--output";
  args[idx++] = (char *)obj_path;
  if (toolchain_debug_deep)
    args[idx++] = (char *)"-vd";
  else if (toolchain_debug_mode)
    args[idx++] = (char *)"-d";
  args[idx++] = (char *)asm_path;
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

static int assemble_to_object(const char *chs_cmd_to_use,
                              int chs_has_override,
                              int chs_uses_fallback,
                              TargetArch target_arch, TargetOS target_os,
                              const char *asm_path, const char *objOut,
                              int toolchain_debug_mode,
                              int toolchain_debug_deep,
                              int debug_symbols, int freestanding,
                              int opt_level)
{
  (void)debug_symbols;
  (void)freestanding;
  (void)opt_level;

  if (!chs_cmd_to_use || !*chs_cmd_to_use)
  {
    fprintf(stderr, "internal error: CHS command unresolved\n");
    return 1;
  }
  const char *arch_name = chs_arch_name_for_target(target_arch);
  const char *format_name = chs_format_name_for_target(target_os);
  if (!arch_name || !format_name)
  {
    fprintf(stderr,
            "CHS does not support target-os '%s' for %s assembly\n",
            target_os_to_option(target_os)
                ? target_os_to_option(target_os)
                : "<unset>",
            target_arch == ARCH_BSLASH ? "bslash" :
            (target_arch == ARCH_ARM64 ? "arm64" : "x86"));
    return 1;
  }
  int spawn_errno = 0;
  int chs_rc = run_chs_assemble_process(chs_cmd_to_use, target_arch,
                                        target_os, asm_path, objOut,
                                        toolchain_debug_mode,
                                        toolchain_debug_deep,
                                        &spawn_errno);
  if (chs_rc != 0)
  {
    if (chs_rc < 0)
    {
      fprintf(stderr, "failed to launch chs '%s': %s\n", chs_cmd_to_use,
              strerror(spawn_errno));
    }
    else
    {
      fprintf(stderr,
              "CHS assembler failed (rc=%d): \"%s\" --arch %s --format %s --output \"%s\" \"%s\"\n",
              chs_rc, chs_cmd_to_use, arch_name, format_name, objOut,
              asm_path);
    }
    if (!chs_has_override && chs_uses_fallback)
    {
      fprintf(stderr,
              "hint: use --chs <path> or set CHS_CMD to point at the CHS executable\n");
    }
    return 1;
  }
  return 0;
}

static int is_codegen_target(TargetArch target_arch)
{
  return target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
         target_arch == ARCH_BSLASH;
}

static const char *resolve_codegen_backend(TargetArch target_arch,
                                           TargetOS target_os,
                                           const char *chancecode_backend)
{
  if (chancecode_backend)
    return chancecode_backend;
  if (target_arch == ARCH_X86)
    return "x86-gas";
  if (target_arch == ARCH_ARM64)
    return arm64_backend_name_for_os(target_os);
  return "bslash";
}

static int run_codegen_backend_step(
    const char *chancecodec_cmd_to_use, int chancecodec_has_override,
    int chancecodec_uses_fallback, const char *chancecode_backend,
    TargetArch target_arch, TargetOS target_os, int opt_level,
    int strip_metadata, int strip_hard, int obfuscate,
    const char *strip_map_path_or_null, int debug_symbols,
    int toolchain_debug_mode, int toolchain_debug_deep,
    const char *asm_path, const char *input_path)
{
  const char *backend =
      resolve_codegen_backend(target_arch, target_os, chancecode_backend);
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
      return 1;
    }
    if (!backend)
    {
      fprintf(stderr,
              "arm64 backend unavailable for target-os '%s'\n",
              target_os_option);
      return 1;
    }
  }

  if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
  {
    fprintf(stderr,
            "internal error: ChanceCode CLI command unresolved\n");
    return 1;
  }

  char display_cmd[4096];
  build_chancecodec_display_cmd(display_cmd, sizeof(display_cmd),
                                chancecodec_cmd_to_use, backend,
                                opt_level, strip_metadata,
                                strip_hard, obfuscate,
                                strip_map_path_or_null,
                                debug_symbols,
                                toolchain_debug_mode,
                                toolchain_debug_deep,
                                target_os_option, asm_path, input_path);

  int spawn_errno = 0;
  int chance_rc = run_chancecodec_process(
      chancecodec_cmd_to_use, backend, opt_level, strip_metadata,
      strip_hard, obfuscate,
      strip_map_path_or_null, debug_symbols,
      asm_path, input_path, toolchain_debug_mode, toolchain_debug_deep,
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
              "hint: use --chancecodec <path> or set CHANCECODEC_CMD to "
              "point at the ChanceCode CLI executable\n");
    }
    return 1;
  }

  return 0;
}

static int run_codegen_assembly_step(
  const char *chs_cmd_to_use,
    int chs_has_override, int chs_uses_fallback,
    TargetArch target_arch, TargetOS target_os,
    const char *asm_path, const char *objOut,
    int need_obj, int toolchain_debug_mode,
    int toolchain_debug_deep, int debug_symbols,
    int freestanding, int opt_level)
{
  if (!need_obj)
  {
    fprintf(stderr,
            "internal error: object output expected but path missing\n");
    return 1;
  }

  return assemble_to_object(chs_cmd_to_use,
                            chs_has_override, chs_uses_fallback,
                            target_arch, target_os, asm_path, objOut,
                            toolchain_debug_mode,
                            toolchain_debug_deep,
                            debug_symbols, freestanding, opt_level);
}

static int append_temp_object(char ***temp_objs, int *to_cnt, int *to_cap,
                              const char *obj_path)
{
  if (!temp_objs || !to_cnt || !to_cap || !obj_path || !*obj_path)
    return 1;
  if (*to_cnt == *to_cap)
  {
    int new_cap = *to_cap ? *to_cap * 2 : 8;
    char **grown = (char **)realloc(*temp_objs, sizeof(char *) * (size_t)new_cap);
    if (!grown)
      return 1;
    *temp_objs = grown;
    *to_cap = new_cap;
  }
  (*temp_objs)[(*to_cnt)++] = xstrdup(obj_path);
  return 0;
}

static void maybe_capture_single_obj(int no_link, int multi_link,
                                     const char *objOut, int obj_is_temp,
                                     char *single_obj_path,
                                     size_t single_obj_path_size,
                                     int *have_single_obj,
                                     int *single_obj_is_temp)
{
  if (no_link || multi_link)
    return;
  snprintf(single_obj_path, single_obj_path_size, "%s", objOut);
  *have_single_obj = 1;
  *single_obj_is_temp = obj_is_temp;
}

static int maybe_track_output_obj(int no_link, const char *obj_override,
                                  int multi_link, const char *objOut,
                                  char ***temp_objs, int *to_cnt,
                                  int *to_cap)
{
  if (((no_link && obj_override) || multi_link) && objOut && objOut[0] != '\0')
    return append_temp_object(temp_objs, to_cnt, to_cap, objOut);
  return 0;
}

static int is_relocatable_obj(const char *path)
{
  
  FILE *f = fopen(path, "rb");
  if (!f)
    return 0;
  unsigned char hdr[64];
  size_t n = fread(hdr, 1, sizeof(hdr), f);
  fclose(f);
  if (n < 20)
    return 0;
  
  if (hdr[0] == 'M' && hdr[1] == 'Z')
    return 0;
  
  if (hdr[0] == 0x7F && hdr[1] == 'E' && hdr[2] == 'L' && hdr[3] == 'F')
  {
    
    int et = hdr[16] | (hdr[17] << 8);
    return et == 1; 
  }
  
  if (ends_with_icase(path, ".obj"))
    return 1;
  
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
  int output_overridden = 0;
  int stop_after_asm = 0;
  int stop_after_ccb = 0;
  int no_link = 0; 
  int emit_library = 0;
  int export_executable = 0;
  int static_link = 0;
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
  int language_standard = CHANCEC_DEFAULT_STANDARD;
  int c_dialect = CHANCE_C_DIALECT_GNU23;
  int request_ast = 0;
  int diagnostics_only = 0;
  int toolchain_debug_mode = 0;
  int toolchain_debug_deep = 0;
  OverrideFile *override_files = NULL;
  int override_file_count = 0;
  int override_file_cap = 0;
  char strip_map_path[STRIP_MAP_PATH_MAX] = {0};
  int strip_map_ready = 0;
  AsmSyntax asm_syntax = ASM_INTEL;
  TargetArch target_arch = ARCH_NONE;
  const char *chancecode_backend = NULL;
  const char *chancecodec_cmd_override = NULL;
  const char *chs_cmd_override = NULL;
  const char *entry_symbol = NULL;
#ifdef _WIN32
  TargetOS target_os = OS_WINDOWS;
#elif defined(__APPLE__)
  TargetOS target_os = OS_MACOS;
#else
  TargetOS target_os = OS_LINUX;
#endif
  
  const char **ce_inputs = NULL;
  int ce_count = 0, ce_cap = 0;
  char **ce_obj_outputs = NULL;
  const char **ccb_inputs = NULL;
  int ccb_count = 0, ccb_cap = 0;
  const char **cclib_inputs = NULL;
  int cclib_count = 0, cclib_cap = 0;
  const char **obj_inputs = NULL;
  int obj_count = 0, obj_cap = 0;
  const char **asm_inputs = NULL;
  int asm_count = 0, asm_cap = 0;
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
  char **owned_asm_inputs = NULL;
  int owned_asm_count = 0, owned_asm_cap = 0;
  
  char **include_dirs = NULL;
  int include_dir_count = 0;
  const char *obj_override = NULL; 
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
  int rc = 0;
  UnitCompile *units = NULL;
  SymbolRefUnit *symbol_ref_units = NULL;
  char **temp_objs = NULL;
  int to_cnt = 0;
  int to_cap = 0;
  char single_obj_path[1024] = {0};
  int have_single_obj = 0;
  int single_obj_is_temp = 0;

  DriverOptionsState options_state = {
      .prog_name = argv[0],
      .cwd = cwd,
      .out = &out,
      .obj_override = &obj_override,
      .output_overridden = &output_overridden,
      .stop_after_asm = &stop_after_asm,
      .stop_after_ccb = &stop_after_ccb,
      .no_link = &no_link,
      .emit_library = &emit_library,
      .export_executable = &export_executable,
      .static_link = &static_link,
      .freestanding = &freestanding,
      .freestanding_requested = &freestanding_requested,
      .m32 = &m32,
      .opt_level = &opt_level,
      .debug_symbols = &debug_symbols,
      .strip_metadata = &strip_metadata,
      .strip_hard = &strip_hard,
      .obfuscate = &obfuscate,
      .implicit_voidp = &implicit_voidp,
      .implicit_void_function = &implicit_void_function,
      .implicit_sizeof = &implicit_sizeof,
      .request_ast = &request_ast,
      .diagnostics_only = &diagnostics_only,
      .toolchain_debug_mode = &toolchain_debug_mode,
      .toolchain_debug_deep = &toolchain_debug_deep,
      .language_standard = &language_standard,
      .c_dialect = &c_dialect,
      .verbose_use_ansi = &verbose_use_ansi,
      .asm_syntax = &asm_syntax,
      .target_arch = &target_arch,
      .target_os = &target_os,
      .chancecode_backend = &chancecode_backend,
      .chancecodec_cmd_override = &chancecodec_cmd_override,
      .chs_cmd_override = &chs_cmd_override,
      .entry_symbol = &entry_symbol,
      .ce_inputs = &ce_inputs,
      .ce_count = &ce_count,
      .ce_cap = &ce_cap,
      .ce_obj_outputs = &ce_obj_outputs,
      .ccb_inputs = &ccb_inputs,
      .ccb_count = &ccb_count,
      .ccb_cap = &ccb_cap,
      .cclib_inputs = &cclib_inputs,
      .cclib_count = &cclib_count,
      .cclib_cap = &cclib_cap,
      .obj_inputs = &obj_inputs,
      .obj_count = &obj_count,
      .obj_cap = &obj_cap,
      .asm_inputs = &asm_inputs,
      .asm_count = &asm_count,
      .asm_cap = &asm_cap,
      .symbol_ref_ce_inputs = &symbol_ref_ce_inputs,
      .symbol_ref_ce_count = &symbol_ref_ce_count,
      .symbol_ref_ce_cap = &symbol_ref_ce_cap,
      .symbol_ref_cclib_inputs = &symbol_ref_cclib_inputs,
      .symbol_ref_cclib_count = &symbol_ref_cclib_count,
      .symbol_ref_cclib_cap = &symbol_ref_cclib_cap,
      .owned_ce_inputs = &owned_ce_inputs,
      .owned_ce_count = &owned_ce_count,
      .owned_ce_cap = &owned_ce_cap,
      .owned_ccb_inputs = &owned_ccb_inputs,
      .owned_ccb_count = &owned_ccb_count,
      .owned_ccb_cap = &owned_ccb_cap,
      .owned_cclib_inputs = &owned_cclib_inputs,
      .owned_cclib_count = &owned_cclib_count,
      .owned_cclib_cap = &owned_cclib_cap,
      .owned_obj_inputs = &owned_obj_inputs,
      .owned_obj_count = &owned_obj_count,
      .owned_obj_cap = &owned_obj_cap,
      .owned_asm_inputs = &owned_asm_inputs,
      .owned_asm_count = &owned_asm_count,
      .owned_asm_cap = &owned_asm_cap,
      .include_dirs = &include_dirs,
      .include_dir_count = &include_dir_count,
      .override_files = &override_files,
      .override_file_count = &override_file_count,
      .override_file_cap = &override_file_cap,
      .project_output_alloc = &project_output_alloc,
      .project_after_cmd = &project_after_cmd,
  };

  int options_rc = parse_driver_options_argv(argc, argv, &options_state);
  if (options_rc == 1)
    return 0;
  if (options_rc == 2)
    return 2;
  if (options_rc != 0)
    goto fail;
  driver_verbose_set_use_ansi(verbose_use_ansi);

  DriverValidationState validation_state = {
      .prog_name = argv[0],
      .out = &out,
      .output_overridden = &output_overridden,
      .stop_after_asm = &stop_after_asm,
      .stop_after_ccb = &stop_after_ccb,
      .no_link = &no_link,
      .emit_library = &emit_library,
      .export_executable = &export_executable,
      .static_link = &static_link,
      .freestanding = &freestanding,
      .m32 = &m32,
      .debug_symbols = &debug_symbols,
      .strip_metadata = &strip_metadata,
      .implicit_void_function = &implicit_void_function,
      .language_standard = &language_standard,
      .request_ast = &request_ast,
      .diagnostics_only = &diagnostics_only,
      .target_arch = &target_arch,
      .ce_count = &ce_count,
      .ccb_count = &ccb_count,
      .cclib_count = &cclib_count,
      .obj_count = &obj_count,
      .asm_count = &asm_count,
  };
  if (validate_driver_options(&validation_state) != 0)
    return 2;

  DriverToolchainInputs toolchain_inputs = {
      .exe_dir = exe_dir,
      .chancecodec_cmd_override = chancecodec_cmd_override,
      .chs_cmd_override = chs_cmd_override,
      .target_arch = target_arch,
      .target_os = target_os,
      .emit_library = emit_library,
      .stop_after_ccb = stop_after_ccb,
      .stop_after_asm = stop_after_asm,
  };
  DriverToolchainSelection toolchain = {0};
  int toolchain_rc = resolve_driver_toolchain(&toolchain_inputs, &toolchain);
  if (toolchain_rc != 0)
    return toolchain_rc;

  const char *chancecodec_cmd_to_use = toolchain.chancecodec_cmd_to_use;
  int chancecodec_uses_fallback = toolchain.chancecodec_uses_fallback;
  int chancecodec_has_override = toolchain.chancecodec_has_override;
  int needs_chancecodec = toolchain.needs_chancecodec;

  const char *chs_cmd_to_use = toolchain.chs_cmd_to_use;
  int chs_uses_fallback = toolchain.chs_uses_fallback;
  int chs_has_override = toolchain.chs_has_override;
  int needs_chs = toolchain.needs_chs;

  const char *cld_cmd_to_use = toolchain.cld_cmd_to_use;
  int cld_uses_fallback = toolchain.cld_uses_fallback;
  const char *cld_target_to_use = toolchain.cld_target_to_use;
  int cld_supported_link_target = toolchain.cld_supported_link_target;

#ifndef DEFAULT_STDLIB
#define DEFAULT_STDLIB ""
#endif
#ifndef DEFAULT_RUNTIME
#define DEFAULT_RUNTIME ""
#endif

  DriverRuntimeLibState runtime_libs = {
      .exe_dir = exe_dir,
      .out_path = out,
      .no_link = no_link,
      .emit_library = emit_library,
      .freestanding_requested = &freestanding_requested,
      .freestanding = &freestanding,
      .default_stdlib_path = DEFAULT_STDLIB,
      .default_runtime_path = DEFAULT_RUNTIME,
      .cclib_inputs = &cclib_inputs,
      .cclib_count = &cclib_count,
      .cclib_cap = &cclib_cap,
      .owned_cclib_inputs = &owned_cclib_inputs,
      .owned_cclib_count = &owned_cclib_count,
      .owned_cclib_cap = &owned_cclib_cap,
  };
  if (maybe_inject_default_runtime_libs(&runtime_libs) != 0)
    goto fail;

  if (compiler_verbose_enabled())
  {
    verbose_print_config(out, opt_level, target_arch, target_os, stop_after_ccb,
                         stop_after_asm, no_link, emit_library, freestanding,
                         asm_syntax, entry_symbol, include_dir_count, ce_count, ccb_count,
                         cclib_count, obj_count, symbol_ref_ce_count,
                         symbol_ref_cclib_count, chancecodec_cmd_to_use,
                         chancecodec_has_override, needs_chancecodec,
                         chancecode_backend, chancecodec_uses_fallback,
                         compiler_verbose_enabled(),
                         compiler_verbose_deep_enabled());
    DriverVerboseInputsState verbose_inputs = {
        .include_dir_count = include_dir_count,
        .include_dirs = include_dirs,
        .ce_count = ce_count,
        .ce_inputs = ce_inputs,
        .symbol_ref_ce_count = symbol_ref_ce_count,
        .symbol_ref_ce_inputs = symbol_ref_ce_inputs,
        .ccb_count = ccb_count,
        .ccb_inputs = ccb_inputs,
        .cclib_count = cclib_count,
        .cclib_inputs = cclib_inputs,
        .symbol_ref_cclib_count = symbol_ref_cclib_count,
        .symbol_ref_cclib_inputs = symbol_ref_cclib_inputs,
        .obj_count = obj_count,
        .obj_inputs = obj_inputs,
        .asm_count = asm_count,
        .asm_inputs = asm_inputs,
    };
    driver_verbose_print_inputs(&verbose_inputs);
  }
  
  
  
  
  if (no_link)
  {
    if (!obj_override)
    {
      
      
      if (ce_count == 0 && ccb_count == 0 && asm_count == 0)
      {
        fprintf(stderr, "error: nothing to compile.\n");
        goto fail;
      }
    }
    else
    {
      
      if ((ce_count + obj_count) == 0)
      {
        fprintf(stderr, "error: no inputs provided to combine into '%s'.\n",
                obj_override);
        goto fail;
      }
      
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
  parser_set_language_standard((ChanceLanguageStandard)language_standard);

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
  if (cclib_count > 0)
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

  if (ce_count > 0)
    units = (UnitCompile *)xcalloc((size_t)ce_count, sizeof(UnitCompile));
  if (symbol_ref_ce_count > 0)
    symbol_ref_units =
        (SymbolRefUnit *)xcalloc((size_t)symbol_ref_ce_count,
                                 sizeof(SymbolRefUnit));
  int skip_backend_outputs = stop_after_ccb || stop_after_asm || emit_library;
  int total_codegen_units = ce_count + ccb_count + library_codegen_units;
  int link_input_units = obj_count + asm_count + total_codegen_units;
  
  int multi_link = (!no_link && !skip_backend_outputs &&
                    (link_input_units > 1));

  
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
          input, src, len, &pre_len, target_arch_to_macro(target_arch),
          freestanding);
      const ChanceFrontend *frontend = chance_frontend_for_input_path(input);
      if (!frontend)
      {
        fprintf(stderr, "error: no frontend found for input '%s'\n", input);
        rc = 1;
        free(preprocessed);
        free(src);
        break;
      }
      SourceBuffer sb = {preprocessed ? preprocessed : src,
                         preprocessed ? pre_len : len, input};
      ChanceFrontendLoadRequest req = {
          .input_path = input,
          .raw_source = src,
          .raw_source_len = len,
          .source = sb,
          .include_dirs = include_dirs,
          .include_dir_count = include_dir_count,
            .c_dialect = (ChanceCDialect)c_dialect,
      };
      FrontendUnit *frontend_unit =
          chance_frontend_load_unit(frontend, &req, 1);
      if (!frontend_unit)
      {
        fprintf(stderr, "error: failed to parse input '%s'\n", input);
        rc = 1;
        free(preprocessed);
        free(src);
        break;
      }
      Node *unit = chance_frontend_unit_ast(frontend_unit);
      if (!unit)
      {
        if (chance_frontend_unit_supports_direct_ccb_emit(frontend_unit))
          fprintf(stderr,
                  "error: -sr currently requires AST-capable frontends; '%s' is direct-emission only\n",
                  chance_frontend_name(frontend));
        else
          fprintf(stderr, "error: failed to parse input '%s'\n", input);
        chance_frontend_unit_destroy(frontend_unit);
        rc = 1;
        free(preprocessed);
        free(src);
        break;
      }
      symtab_add_library_functions(frontend_unit, unit,
                                   loaded_library_functions,
                                   loaded_library_function_count);
      chance_frontend_unit_discard_semantic_state(frontend_unit);

      symbol_ref_units[si].input_path = input ? xstrdup(input) : NULL;
      symbol_ref_units[si].src = src;
      symbol_ref_units[si].stripped = preprocessed;
      symbol_ref_units[si].unit = unit;
      symbol_ref_units[si].frontend = frontend;
      symbol_ref_units[si].frontend_unit = frontend_unit;
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
      input, src, len, &pre_len, target_arch_to_macro(target_arch),
      freestanding);
    if (getenv("DUMP_PREPROC") && input && strstr(input, "aemu/cpu8086.ce"))
    {
      printf("%s", preprocessed ? preprocessed : src);
      exit(0);
    }
    (void)pre_len;
    const ChanceFrontend *frontend = chance_frontend_for_input_path(input);
    if (!frontend)
    {
      fprintf(stderr, "error: no frontend found for input '%s'\n", input);
      free(preprocessed);
      free(src);
      rc = 1;
      break;
    }
    SourceBuffer sb = {preprocessed ? preprocessed : src,
                       preprocessed ? pre_len : len, input};
    ChanceFrontendLoadRequest req = {
        .input_path = input,
        .raw_source = src,
        .raw_source_len = len,
        .source = sb,
        .include_dirs = include_dirs,
        .include_dir_count = include_dir_count,
        .c_dialect = (ChanceCDialect)c_dialect,
    };
    FrontendUnit *frontend_unit = chance_frontend_load_unit(frontend, &req, 1);
    if (!frontend_unit)
    {
      fprintf(stderr, "error: failed to parse input '%s'\n", input);
      free(preprocessed);
      free(src);
      rc = 1;
      break;
    }
    Node *unit = chance_frontend_unit_ast(frontend_unit);
    if (!unit && !chance_frontend_unit_supports_direct_ccb_emit(frontend_unit))
    {
      fprintf(stderr, "error: failed to parse input '%s'\n", input);
      chance_frontend_unit_destroy(frontend_unit);
      free(preprocessed);
      free(src);
      rc = 1;
      break;
    }
    if (unit)
      symtab_add_library_functions(frontend_unit, unit,
                                   loaded_library_functions,
                                   loaded_library_function_count);

    units[fi].input_path = xstrdup(input);
    units[fi].src = src;
    units[fi].stripped = preprocessed;
    units[fi].unit = unit;
    units[fi].frontend = frontend;
    units[fi].frontend_unit = frontend_unit;
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
      if (chance_frontend_unit_check(uc->frontend_unit) != 0)
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
      chance_frontend_unit_register_foreign_symbols(
          units[target].frontend_unit,
          units[source].frontend_unit);
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
        chance_frontend_unit_register_foreign_symbols(
            units[target].frontend_unit,
            symbol_ref_units[sr].frontend_unit);
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
    FrontendUnit *frontend_unit = uc->frontend_unit;
    Node *unit = uc->unit;

    if (compiler_verbose_enabled())
      verbose_progress("ce-codegen", fi + 1, ce_count);

    int serr = chance_frontend_unit_check(frontend_unit);
    if (!serr)
    {
      char dir[512], base[512];
      split_path(uc->input_path, dir, sizeof(dir), base, sizeof(base));

        if (unit && !emit_library && ce_count > 1 &&
          unit_is_embedded_force_inline_literal_only(unit))
      {
        if (!(stop_after_ccb && ends_with_icase(out, ".ccb")))
        {
          char stale_ccb_path[1024];
          build_path_with_ext(dir, base, ".ccb", stale_ccb_path, sizeof(stale_ccb_path));
          remove(stale_ccb_path);
        }
        if (compiler_verbose_enabled())
          compiler_verbose_logf("codegen", "skip standalone CCB for inline-only literal unit '%s'", uc->input_path);
        continue;
      }

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
      Symbol *imported_syms = chance_frontend_unit_copy_imported_function_symbols(frontend_unit, &imported_count);
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
      Symbol *imported_global_syms = chance_frontend_unit_copy_imported_global_symbols(frontend_unit, &imported_global_count);

      CodegenOptions co = {.freestanding = freestanding != 0,
                           .m32 = (m32 != 0) || (target_arch == ARCH_BSLASH),
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
      if (chance_frontend_unit_supports_direct_ccb_emit(frontend_unit))
      {
        ChanceFrontendEmitRequest fe = {
            .input_path = uc->input_path,
            .ccb_output_path = ccb_path,
            .opt_level = opt_level,
        };
        rc = chance_frontend_unit_emit_ccb(frontend_unit, &fe);
      }
      else
      {
        int extern_count = 0;
        const Symbol *extern_syms = chance_frontend_unit_externs(frontend_unit,
                                                                  &extern_count);
        co.externs = extern_syms;
        co.extern_count = extern_count;
        rc = codegen_ccb_write_module(unit, &co);

        if (!rc)
        {
          if (codegen_ccb_resolve_module_path(&co, ccb_path, sizeof(ccb_path)))
            rc = 1;
        }
      }
      free(imported_syms);
      free(imported_global_syms);

      if (!rc && emit_library)
      {
        char module_name_buf[256];
        const char *fallback_name = derive_module_name_from_path(
            ccb_path, module_name_buf, sizeof(module_name_buf));
        const char *module_name = (unit ? unit->module_path.full_name : NULL);
        const char *effective_module =
            (module_name && *module_name) ? module_name : fallback_name;
        int metadata_rc = 0;
        if (unit)
        {
          metadata_rc = collect_metadata_for_unit(unit, ccb_path,
                                                  effective_module,
                                                  &library_modules,
                                                  &library_module_count,
                                                  &library_module_cap);
        }
        else
        {
          metadata_rc = collect_metadata_for_ccb_text(
              ccb_path, effective_module,
              &library_modules, &library_module_count,
              &library_module_cap);
        }

        if (metadata_rc)
        {
          fprintf(stderr,
                  "error: failed collecting library metadata for frontend '%s'\n",
                  chance_frontend_name(uc->frontend));
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
                  strip_map_ready ? strip_map_path : NULL,
                  toolchain_debug_mode, toolchain_debug_deep,
                  &spawn_errno);
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

      if (!rc && is_codegen_target(target_arch) && !stop_after_ccb)
      {
        rc = run_codegen_backend_step(
            chancecodec_cmd_to_use, chancecodec_has_override,
            chancecodec_uses_fallback, chancecode_backend,
            target_arch, target_os, opt_level, strip_metadata,
            strip_hard, obfuscate,
            strip_map_ready ? strip_map_path : NULL,
            debug_symbols, toolchain_debug_mode,
            toolchain_debug_deep, asm_path, ccb_path);
      }

      if (!rc && is_codegen_target(target_arch) &&
          !stop_after_ccb && !stop_after_asm)
      {
        rc = run_codegen_assembly_step(
          chs_cmd_to_use,
            chs_has_override, chs_uses_fallback,
            target_arch, target_os, asm_path, objOut, need_obj,
            toolchain_debug_mode, toolchain_debug_deep,
            debug_symbols, freestanding, opt_level);
      }

      if (!rc && is_codegen_target(target_arch))
      {
        if (!stop_after_ccb && ccb_is_temp)
          remove(ccb_path);
        if (!stop_after_asm)
          remove(asm_path);
      }

      if (!rc && is_codegen_target(target_arch) &&
          !stop_after_ccb && !stop_after_asm)
      {
        maybe_capture_single_obj(no_link, multi_link, objOut, obj_is_temp,
                                 single_obj_path,
                                 sizeof(single_obj_path),
                                 &have_single_obj,
                                 &single_obj_is_temp);
      }

      if (!rc)
      {
        if (maybe_track_output_obj(no_link, obj_override, multi_link, objOut,
                                   &temp_objs, &to_cnt, &to_cap) != 0)
          rc = 1;
      }
    }
    else
    {
      rc = 1;
    }

  unit_done:
    chance_frontend_unit_destroy(frontend_unit);
    uc->frontend_unit = NULL;
    uc->unit = NULL;
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

      if (is_codegen_target(target_arch) && !stop_after_ccb)
      {
        rc = run_codegen_backend_step(
            chancecodec_cmd_to_use, chancecodec_has_override,
            chancecodec_uses_fallback, chancecode_backend,
            target_arch, target_os, opt_level, strip_metadata,
            strip_hard, obfuscate,
            strip_map_ready ? strip_map_path : NULL,
            debug_symbols, toolchain_debug_mode,
            toolchain_debug_deep, asm_path, ccb_path);
        if (rc)
          break;
      }

      if (is_codegen_target(target_arch) &&
          !stop_after_ccb && !stop_after_asm)
      {
        rc = run_codegen_assembly_step(
          chs_cmd_to_use,
            chs_has_override, chs_uses_fallback,
            target_arch, target_os, asm_path, objOut, need_obj,
            toolchain_debug_mode, toolchain_debug_deep,
            debug_symbols, freestanding, opt_level);
        if (rc != 0)
          break;
      }

      if (is_codegen_target(target_arch))
      {
        if (!stop_after_asm)
          remove(asm_path);
      }

      if (is_codegen_target(target_arch) &&
          !stop_after_ccb && !stop_after_asm)
      {
        maybe_capture_single_obj(no_link, multi_link, objOut, obj_is_temp,
                                 single_obj_path,
                                 sizeof(single_obj_path),
                                 &have_single_obj,
                                 &single_obj_is_temp);
      }

      if (maybe_track_output_obj(no_link, obj_override, multi_link, objOut,
                                 &temp_objs, &to_cnt, &to_cap) != 0)
      {
        rc = 1;
        break;
      }
    }
  }
  if (!rc && !emit_library && asm_count > 0 &&
      (target_arch == ARCH_X86 || target_arch == ARCH_ARM64 ||
       target_arch == ARCH_BSLASH) &&
      !stop_after_ccb && !stop_after_asm)
  {
    if (compiler_verbose_enabled())
      verbose_section("Processing assembly inputs");
    for (int ai = 0; !rc && ai < asm_count; ++ai)
    {
      const char *asm_input = asm_inputs[ai];
      if (compiler_verbose_enabled())
        verbose_progress("asm-proc", ai + 1, asm_count);

      char dir[512], base[512];
      split_path(asm_input, dir, sizeof(dir), base, sizeof(base));

      char objOut[1024] = {0};
      int obj_is_temp = 0;
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

      rc = run_codegen_assembly_step(
          chs_cmd_to_use,
          chs_has_override, chs_uses_fallback,
          target_arch, target_os, asm_input, objOut, 1,
          toolchain_debug_mode, toolchain_debug_deep,
          debug_symbols, freestanding, opt_level);
      if (rc != 0)
        break;

      maybe_capture_single_obj(no_link, multi_link, objOut, obj_is_temp,
                               single_obj_path, sizeof(single_obj_path),
                               &have_single_obj, &single_obj_is_temp);

      if (maybe_track_output_obj(no_link, obj_override, multi_link, objOut,
                                 &temp_objs, &to_cnt, &to_cap) != 0)
      {
        rc = 1;
        break;
      }
    }
  }
  if (!rc && !emit_library && !no_link &&
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

        rc = run_codegen_backend_step(
            chancecodec_cmd_to_use, chancecodec_has_override,
            chancecodec_uses_fallback, chancecode_backend,
            target_arch, target_os, opt_level, strip_metadata,
            strip_hard, obfuscate,
            strip_map_ready ? strip_map_path : NULL,
            debug_symbols, toolchain_debug_mode,
            toolchain_debug_deep, asm_path, ccbin_path);
        if (rc)
          break;

        if (!stop_after_ccb && !stop_after_asm)
        {
          rc = run_codegen_assembly_step(
              chs_cmd_to_use,
              chs_has_override, chs_uses_fallback,
              target_arch, target_os, asm_path, objOut, need_obj,
              toolchain_debug_mode, toolchain_debug_deep,
              debug_symbols, freestanding, opt_level);
          if (rc != 0)
            break;
        }

        if (!stop_after_asm)
          remove(asm_path);

        if (!stop_after_ccb && !stop_after_asm)
        {
          maybe_capture_single_obj(no_link, multi_link, objOut, obj_is_temp,
                                   single_obj_path,
                                   sizeof(single_obj_path),
                                   &have_single_obj,
                                   &single_obj_is_temp);
        }

        if (maybe_track_output_obj(no_link, obj_override, multi_link, objOut,
                                   &temp_objs, &to_cnt, &to_cap) != 0)
        {
          rc = 1;
          break;
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
    char export_cclib_path[1024] = {0};
    const char *library_out_path = out;
    if (export_executable)
    {
      char t[] = "/tmp/chance_export_bundle_XXXXXX";
      int fd = mkstemp(t);
      if (fd < 0)
      {
        fprintf(stderr, "error: failed to create temporary bundle path for --export-exe\n");
        rc = 1;
        goto cleanup;
      }
      close(fd);
      snprintf(export_cclib_path, sizeof(export_cclib_path), "%s", t);
      library_out_path = export_cclib_path;
    }

    if (static_link && loaded_library_count > 0)
    {
      if (merge_loaded_libraries_into_library_modules(
              loaded_libraries, loaded_library_count,
              &library_modules, &library_module_count,
              &library_module_cap) != 0)
      {
        fprintf(stderr,
                "error: failed to merge cclib modules into library output\n");
        rc = 1;
        goto cleanup;
      }
    }

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
      int werr = write_library_file(library_out_path, library_modules, library_module_count);
      if (werr != 0)
      {
        fprintf(stderr, "error: failed to write library '%s' (%s)\n", library_out_path,
                strerror(werr));
        rc = 1;
      }
      else if (export_executable)
      {
        int export_rc = emit_export_launcher_executable(out,
                                                        library_out_path,
                                                        exe_dir,
                                                        chs_cmd_to_use,
                                                        cld_cmd_to_use,
                                                        cld_target_to_use,
                                                        cld_supported_link_target,
                                                        target_arch,
                                                        target_os,
                                                        toolchain_debug_mode,
                                                        toolchain_debug_deep,
                                                        debug_symbols);
        if (export_rc != 0)
        {
          fprintf(stderr,
                  "error: failed to emit exported executable '%s'\n",
                  out);
          rc = 1;
        }
      }
    }
    if (export_executable && export_cclib_path[0])
      remove(export_cclib_path);
    goto cleanup;
  }

  if (!rc)
  {
    DriverLinkPhaseState link_state = {
        .multi_link = multi_link,
        .have_single_obj = have_single_obj,
        .single_obj_is_temp = single_obj_is_temp,
        .no_link = no_link,
        .obj_override = obj_override,
        .cld_supported_link_target = cld_supported_link_target,
        .cld_cmd_to_use = cld_cmd_to_use,
        .cld_target_to_use = cld_target_to_use,
        .cld_uses_fallback = cld_uses_fallback,
        .freestanding = freestanding,
        .toolchain_debug_mode = toolchain_debug_mode,
        .toolchain_debug_deep = toolchain_debug_deep,
        .debug_symbols = debug_symbols,
        .target_os = target_os,
        .entry_symbol = entry_symbol,
        .out = out,
        .obj_inputs = obj_inputs,
        .obj_count = obj_count,
        .single_obj_path = single_obj_path,
        .temp_objs = &temp_objs,
        .to_cnt = &to_cnt,
        .to_cap = &to_cap,
    };
    rc = run_driver_link_phase(&link_state);
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
      if (uc->frontend_unit)
        chance_frontend_unit_destroy(uc->frontend_unit);
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
      if (sr->frontend_unit)
        chance_frontend_unit_destroy(sr->frontend_unit);
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
  if (owned_asm_inputs)
  {
    for (int i = 0; i < owned_asm_count; ++i)
      free(owned_asm_inputs[i]);
    free(owned_asm_inputs);
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
  free((void *)asm_inputs);
  free((void *)symbol_ref_ce_inputs);
  free((void *)symbol_ref_cclib_inputs);
  if (strip_map_ready && strip_map_path[0])
    remove(strip_map_path);
  return rc;
fail:
  rc = 2;
  goto cleanup;
}
