#include "ast.h"
#include "includes.h"
#include "module_registry.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <errno.h>
#ifdef _WIN32
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

static const char *default_chancecodec_name =
#ifdef _WIN32
    "chancecodec.exe"
#else
    "chancecodec"
#endif
    ;

typedef struct
{
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

static void usage(const char *prog)
{
    fprintf(stderr, "Usage: %s [options] input.ce [more.ce ...]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -o <file>         Output executable path (default a.exe)\n");
    fprintf(stderr, "  -S                Emit pseudo-asm alongside exe (.S)\n");
    fprintf(stderr, "  -Sccb             Stop after emitting Chance bytecode (.ccb)\n");
    fprintf(stderr, "  -O0|-O1|-O2       Select optimization level (default -O0)\n");
    fprintf(stderr, "  -c [obj] | --no-link [obj]\n");
    fprintf(stderr, "                    Compile only; do not link (emit object). Optional obj output path.\n");
    fprintf(stderr, "  --freestanding    Freestanding mode (no default libs)\n");
    fprintf(stderr, "  -m32|-m64         Target bitness (currently -m64 only)\n");
    fprintf(stderr, "  -x86              Select x86-64 backend for assembly/object/executable output\n");
    fprintf(stderr, "  --target-os <os>  Backend target OS: windows|linux\n");
    fprintf(stderr, "  --asm-syntax <s>  Assembly syntax: intel|att|nasm (default intel)\n");
    fprintf(stderr, "  --chancecodec <path>\n");
    fprintf(stderr, "                    Override ChanceCode CLI executable path (default: auto-detect or PATH)\n");
    fprintf(stderr, "  -I <dir>          Add include search directory for #include <>\n");
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

static void split_path(const char *path, char *dir, size_t dsz, char *base_noext,
                       size_t bsz)
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

static void build_path_with_ext(const char *dir, const char *base, const char *ext,
                                char *buffer, size_t bufsz)
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
    while (len > 0 && (in[len - 1] == ' ' || in[len - 1] == '\t' || in[len - 1] == '\n' || in[len - 1] == '\r'))
        --len;
    if (len >= 2)
    {
        if ((in[0] == '"' && in[len - 1] == '"') || (in[0] == '\'' && in[len - 1] == '\''))
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
    if (!argv0)
        argv0 = "";
    if (!realpath(argv0, resolved))
    {
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
        snprintf(home_build, sizeof(home_build), "%s%cbuild", env_home, CHANCE_PATH_SEP);
        build_path_with_ext(home_build, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
        if (is_regular_file(out))
            return 0;
        build_path_with_ext(env_home, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
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
            snprintf(sibling, sizeof(sibling), "%s%cChanceCode", parent, CHANCE_PATH_SEP);
            build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
            if (is_regular_file(out))
                return 0;
            snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", parent, CHANCE_PATH_SEP, CHANCE_PATH_SEP);
            build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
            if (is_regular_file(out))
                return 0;
            char grandparent[1024];
            if (parent_directory(parent, grandparent, sizeof(grandparent)) == 0 && grandparent[0])
            {
                snprintf(sibling, sizeof(sibling), "%s%cChanceCode", grandparent, CHANCE_PATH_SEP);
                build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
                if (is_regular_file(out))
                    return 0;
                snprintf(sibling, sizeof(sibling), "%s%cChanceCode%cbuild", grandparent, CHANCE_PATH_SEP, CHANCE_PATH_SEP);
                build_path_with_ext(sibling, CHANCECODEC_BASE, CHANCECODEC_EXT, out, outsz);
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
    default:
        return NULL;
    }
}

static int run_chancecodec_process(const char *cmd,
                                   const char *backend,
                                   int opt_level,
                                   const char *asm_path,
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
    const char *args[12];
    int idx = 0;
    char optbuf[8];
    char target_option_buf[64];
    const char *target_option_value = NULL;
    if (target_os_arg && *target_os_arg)
    {
        snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s", target_os_arg);
        target_option_value = target_option_buf;
    }
    args[idx++] = cmd;
    args[idx++] = "--backend";
    args[idx++] = backend;
    if (opt_level > 0)
    {
        snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
        args[idx++] = optbuf;
    }
    args[idx++] = "--output";
    args[idx++] = asm_path;
    if (target_option_value)
    {
        args[idx++] = "--option";
        args[idx++] = target_option_value;
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
    char *args[12];
    int idx = 0;
    char optbuf[8];
    char target_option_buf[64];
    char *target_option_value = NULL;
    if (target_os_arg && *target_os_arg)
    {
        snprintf(target_option_buf, sizeof(target_option_buf), "target-os=%s", target_os_arg);
        target_option_value = target_option_buf;
    }
    args[idx++] = (char *)cmd;
    args[idx++] = "--backend";
    args[idx++] = (char *)backend;
    if (opt_level > 0)
    {
        snprintf(optbuf, sizeof(optbuf), "-O%d", opt_level);
        args[idx++] = optbuf;
    }
    args[idx++] = "--output";
    args[idx++] = (char *)asm_path;
    if (target_option_value)
    {
        args[idx++] = "--option";
        args[idx++] = target_option_value;
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
#ifdef _WIN32
    const char *out = "a.exe";
#else
    const char *out = "a";
#endif
    int stop_after_asm = 0;
    int stop_after_ccb = 0;
    int no_link = 0; // -c / --no-link
    int freestanding = 0;
    int m32 = 0;
    int opt_level = 0;
    AsmSyntax asm_syntax = ASM_INTEL;
    typedef enum
    {
        ARCH_NONE = 0,
        ARCH_X86
    } TargetArch;
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
    const char **obj_inputs = NULL;
    int obj_count = 0, obj_cap = 0;
    // include paths
    char **include_dirs = NULL;
    int include_dir_count = 0;
    const char *obj_override = NULL; // optional object path after -c/--no-link
    char exe_dir[1024] = {0};
    get_executable_dir(exe_dir, sizeof(exe_dir), argv[0]);
    chance_add_default_include_dirs(&include_dirs, &include_dir_count);

    for (int i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0)
        {
            usage(argv[0]);
            return 0;
        }
        if (strcmp(argv[i], "--version") == 0)
        {
            printf("chancec: CHance Compiler version 1.0.0\n");
            printf("chancec: CE language standard: H25-1\n");
            printf("chancec: License: MIT\n");
            printf("chancec: Compiled on %s %s\n", __DATE__, __TIME__);
            printf("chancec: Created by Nathan Hornby (AzureianGH)\n");
            return 0;
        }
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc)
        {
            out = argv[++i];
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
        if (strncmp(argv[i], "-O", 2) == 0)
        {
            const char *level_str = argv[i] + 2;
            int level = 1;
            if (*level_str != '\0')
            {
                char *endptr = NULL;
                long parsed = strtol(level_str, &endptr, 10);
                if (!endptr || *endptr != '\0' || parsed < 0 || parsed > 2)
                {
                    fprintf(stderr, "invalid optimization level '%s' (use -O0|-O1|-O2)\n", argv[i]);
                    return 2;
                }
                level = (int)parsed;
            }
            opt_level = level;
            continue;
        }
        if ((strcmp(argv[i], "-c") == 0) || (strcmp(argv[i], "--no-link") == 0))
        {
            no_link = 1;
            // Accept optional object filename immediately following
            if (i + 1 < argc && argv[i + 1][0] != '-')
            {
                const char *cand = argv[i + 1];
                size_t clen = strlen(cand);
                if (clen >= 2)
                {
                    const char *dot = strrchr(cand, '.');
                    if (dot && (_stricmp(dot, ".o") == 0 || _stricmp(dot, ".obj") == 0))
                    {
                        obj_override = cand;
                        i++;
                    }
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
        if (strcmp(argv[i], "--freestanding") == 0)
        {
            freestanding = 1;
            continue;
        }
        if (strcmp(argv[i], "-x86") == 0)
        {
            target_arch = ARCH_X86;
            chancecode_backend = "x86-gas";
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
            else
            {
                fprintf(stderr, "Unknown --target-os '%s' (use windows|linux)\n", os);
                return 2;
            }
            continue;
        }
        if (strcmp(argv[i], "-I") == 0 && i + 1 < argc)
        {
            chance_add_include_dir(&include_dirs, &include_dir_count, argv[++i]);
            continue;
        }
        if (argv[i][0] == '-')
        {
            usage(argv[0]);
            return 2;
        }
        // classify input
        const char *arg = argv[i];
        if (ends_with_icase(arg, ".ce"))
        {
            if (ce_count == ce_cap)
            {
                ce_cap = ce_cap ? ce_cap * 2 : 8;
                ce_inputs = (const char **)realloc((void *)ce_inputs, sizeof(char *) * ce_cap);
            }
            ce_inputs[ce_count++] = arg;
        }
        else if (ends_with_icase(arg, ".ccb"))
        {
            if (ccb_count == ccb_cap)
            {
                ccb_cap = ccb_cap ? ccb_cap * 2 : 8;
                ccb_inputs = (const char **)realloc((void *)ccb_inputs, sizeof(char *) * ccb_cap);
            }
            ccb_inputs[ccb_count++] = arg;
        }
        else if (ends_with_icase(arg, ".o") || ends_with_icase(arg, ".obj"))
        {
            if (obj_count == obj_cap)
            {
                obj_cap = obj_cap ? obj_cap * 2 : 8;
                obj_inputs = (const char **)realloc((void *)obj_inputs, sizeof(char *) * obj_cap);
            }
            obj_inputs[obj_count++] = arg;
        }
        else
        {
            fprintf(stderr, "error: unknown input type '%s' (expected .ce or .o/.obj)\n", arg);
            return 2;
        }
    }
    if (stop_after_asm && stop_after_ccb)
    {
        fprintf(stderr, "error: -S and -Sccb cannot be used together\n");
        return 2;
    }
    if (stop_after_asm && target_arch == ARCH_NONE)
    {
        fprintf(stderr, "error: -S requires a backend selection (e.g., -x86)\n");
        return 2;
    }
    if (target_arch == ARCH_NONE)
    {
        if (!stop_after_ccb)
        {
            fprintf(stderr, "error: selecting a backend (e.g., -x86) is required unless stopping at bytecode with -Sccb\n");
            return 2;
        }
        if (no_link)
        {
            fprintf(stderr, "error: -c/--no-link is incompatible with -Sccb (no object emission when stopping at bytecode)\n");
            return 2;
        }
        if (obj_count > 0 || ccb_count > 0)
        {
            fprintf(stderr, "error: providing .ccb/.o/.obj inputs requires selecting a backend (e.g., -x86)\n");
            return 2;
        }
    }
    if (ce_count == 0 && obj_count == 0 && ccb_count == 0)
    {
        usage(argv[0]);
        return 2;
    }
    if (m32)
    {
        fprintf(stderr, "Error: -m32 not implemented yet\n");
        return 2;
    }

    char chancecodec_exec_buf[1024] = {0};
    char chancecodec_override_buf[1024] = {0};
    const char *chancecodec_cmd_to_use = NULL;
    int chancecodec_uses_fallback = 0;
    int chancecodec_has_override = 0;
    if (target_arch == ARCH_X86)
    {
        if (chancecodec_cmd_override && *chancecodec_cmd_override)
        {
            strip_wrapping_quotes(chancecodec_cmd_override, chancecodec_override_buf, sizeof(chancecodec_override_buf));
            if (!chancecodec_override_buf[0])
            {
                fprintf(stderr, "error: --chancecodec path is empty after trimming quotes/whitespace\n");
                return 2;
            }
            chancecodec_cmd_to_use = chancecodec_override_buf;
            chancecodec_has_override = 1;
        }
        else if (locate_chancecodec(chancecodec_exec_buf, sizeof(chancecodec_exec_buf), exe_dir) == 0 && chancecodec_exec_buf[0])
        {
            chancecodec_cmd_to_use = chancecodec_exec_buf;
        }
        else
        {
            chancecodec_cmd_to_use = default_chancecodec_name;
            chancecodec_uses_fallback = 1;
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
                fprintf(stderr, "error: providing .o files with -c but without an output object is invalid. Specify an output object after -c to merge.\n");
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
                fprintf(stderr, "error: no inputs provided to combine into '%s'.\n", obj_override);
                goto fail;
            }
            // Validate external .o/.obj relocatable
            for (int k = 0; k < obj_count; ++k)
            {
                if (!is_relocatable_obj(obj_inputs[k]))
                {
                    fprintf(stderr, "error: input '%s' is not a relocatable object file.\n", obj_inputs[k]);
                    goto fail;
                }
            }
        }
    }

    module_registry_reset();
    UnitCompile *units = NULL;
    if (ce_count > 0)
        units = (UnitCompile *)xcalloc((size_t)ce_count, sizeof(UnitCompile));

    int rc = 0;
    int skip_backend_outputs = stop_after_ccb || stop_after_asm;
    int total_codegen_units = ce_count + ccb_count;
    // Determine if we need a final link step combining multiple inputs
    int multi_link = (!no_link && !skip_backend_outputs && (obj_count > 0 || total_codegen_units > 1));

    // Container for temporary objects when merging or linking
    char **temp_objs = NULL;
    int to_cnt = 0, to_cap = 0;
    char single_obj_path[1024] = {0};
    int have_single_obj = 0;
    int single_obj_is_temp = 0;

    for (int fi = 0; fi < ce_count; ++fi)
    {
        const char *input = ce_inputs[fi];
        int len = 0;
        char *src = read_all(input, &len);
        if (!src)
        {
            rc = 1;
            break;
        }
        int stripped_len = 0;
        char *stripped = chance_strip_preprocessor_lines(src, len, &stripped_len);
        SourceBuffer sb = {stripped ? stripped : src,
                           stripped ? stripped_len : len, input};
        Parser *ps = parser_create(sb);
        SemaContext *sc = sema_create();
        chance_process_includes_and_scan(input, src, len, include_dirs,
                                         include_dir_count, sc->syms);
        Node *unit = parse_unit(ps);
        parser_export_externs(ps, sc->syms);

        units[fi].input_path = xstrdup(input);
        units[fi].src = src;
        units[fi].stripped = stripped;
        units[fi].unit = unit;
        units[fi].sc = sc;
        units[fi].parser = ps;
    }
    if (rc)
        goto cleanup;

    for (int fi = 0; fi < ce_count && rc == 0; ++fi)
    {
        UnitCompile *uc = &units[fi];
        Node *unit = uc->unit;
        SemaContext *sc = uc->sc;
        Parser *ps = uc->parser;

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
            build_path_with_ext(dir, base, ".S", asm_path, sizeof(asm_path));
            if (stop_after_asm && ends_with_icase(out, ".s"))
                snprintf(asm_path, sizeof(asm_path), "%s", out);

            char objOut[1024] = {0};
            int obj_is_temp = 0;
            int need_obj = (!stop_after_ccb && !stop_after_asm) && (no_link || multi_link || target_arch != ARCH_NONE);
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

            CodegenOptions co = {.freestanding = freestanding != 0,
                                 .m32 = m32 != 0,
                                 .emit_asm = stop_after_asm != 0,
                                 .no_link = (no_link || multi_link || stop_after_ccb || stop_after_asm) != 0,
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

            if (!rc)
            {
                if (codegen_ccb_resolve_module_path(&co, ccb_path, sizeof(ccb_path)))
                    rc = 1;
            }

            if (!rc && target_arch == ARCH_X86 && !stop_after_ccb)
            {
                const char *backend = chancecode_backend ? chancecode_backend : "x86-gas";
                const char *target_os_option = target_os_to_option(target_os);
                if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
                {
                    fprintf(stderr, "internal error: ChanceCode CLI command unresolved\n");
                    rc = 1;
                }
                else
                {
                    char display_cmd[4096];
                    if (opt_level > 0)
                    {
                        if (target_os_option)
                            snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s -O%d --output \"%s\" --option target-os=%s \"%s\"",
                                     chancecodec_cmd_to_use, backend, opt_level, asm_path, target_os_option, ccb_path);
                        else
                            snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s -O%d --output \"%s\" \"%s\"",
                                     chancecodec_cmd_to_use, backend, opt_level, asm_path, ccb_path);
                    }
                    else
                    {
                        if (target_os_option)
                            snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s --output \"%s\" --option target-os=%s \"%s\"",
                                     chancecodec_cmd_to_use, backend, asm_path, target_os_option, ccb_path);
                        else
                            snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s --output \"%s\" \"%s\"",
                                     chancecodec_cmd_to_use, backend, asm_path, ccb_path);
                    }

                    int spawn_errno = 0;
                    int chance_rc = run_chancecodec_process(chancecodec_cmd_to_use, backend, opt_level, asm_path, ccb_path, target_os_option, &spawn_errno);
                    if (chance_rc != 0)
                    {
                        if (chance_rc < 0)
                        {
                            fprintf(stderr, "failed to launch chancecodec '%s': %s\n", chancecodec_cmd_to_use, strerror(spawn_errno));
                        }
                        else
                        {
                            fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc, display_cmd);
                        }
                        if (!chancecodec_has_override && chancecodec_uses_fallback)
                        {
                            fprintf(stderr, "hint: use --chancecodec <path> or set CHANCECODEC_CMD to point at the ChanceCode CLI executable\n");
                        }
                        rc = 1;
                    }
                }
            }

            if (!rc && target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm)
            {
                if (!need_obj)
                {
                    fprintf(stderr, "internal error: object output expected but path missing\n");
                    rc = 1;
                }
                else
                {
                    char cc_cmd[4096];
                    size_t pos = (size_t)snprintf(cc_cmd, sizeof(cc_cmd), "cc -c \"%s\" -o \"%s\"", asm_path, objOut);
                    if (pos >= sizeof(cc_cmd))
                    {
                        fprintf(stderr, "command buffer exhausted for cc invocation\n");
                        rc = 1;
                    }
                    else
                    {
                        if (freestanding)
                        {
                            strncat(cc_cmd, " -ffreestanding -nostdlib", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
                        }
#ifdef _WIN32
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
                        }
                    }
                }
            }

            if (!rc && target_arch == ARCH_X86)
            {
                if (!stop_after_ccb && ccb_is_temp)
                    remove(ccb_path);
                if (!stop_after_asm)
                    remove(asm_path);
            }

            if (!rc && target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm && !no_link && !multi_link)
            {
                snprintf(single_obj_path, sizeof(single_obj_path), "%s", objOut);
                have_single_obj = 1;
                single_obj_is_temp = obj_is_temp;
            }

            if (!rc && ((no_link && obj_override) || multi_link) && objOut[0] != '\0')
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
    for (int ci = 0; !rc && ci < ccb_count; ++ci)
    {
        const char *ccb_input = ccb_inputs[ci];
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
        int need_obj = (!stop_after_ccb && !stop_after_asm) && (no_link || multi_link || target_arch != ARCH_NONE);
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

        if (target_arch == ARCH_X86 && !stop_after_ccb)
        {
            const char *backend = chancecode_backend ? chancecode_backend : "x86-gas";
            const char *target_os_option = target_os_to_option(target_os);
            if (!chancecodec_cmd_to_use || !*chancecodec_cmd_to_use)
            {
                fprintf(stderr, "internal error: ChanceCode CLI command unresolved\n");
                rc = 1;
                break;
            }
            char display_cmd[4096];
            if (opt_level > 0)
            {
                if (target_os_option)
                    snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s -O%d --output \"%s\" --option target-os=%s \"%s\"",
                             chancecodec_cmd_to_use, backend, opt_level, asm_path, target_os_option, ccb_path);
                else
                    snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s -O%d --output \"%s\" \"%s\"",
                             chancecodec_cmd_to_use, backend, opt_level, asm_path, ccb_path);
            }
            else
            {
                if (target_os_option)
                    snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s --output \"%s\" --option target-os=%s \"%s\"",
                             chancecodec_cmd_to_use, backend, asm_path, target_os_option, ccb_path);
                else
                    snprintf(display_cmd, sizeof(display_cmd), "\"%s\" --backend %s --output \"%s\" \"%s\"",
                             chancecodec_cmd_to_use, backend, asm_path, ccb_path);
            }

            int spawn_errno = 0;
            int chance_rc = run_chancecodec_process(chancecodec_cmd_to_use, backend, opt_level, asm_path, ccb_path, target_os_option, &spawn_errno);
            if (chance_rc != 0)
            {
                if (chance_rc < 0)
                {
                    fprintf(stderr, "failed to launch chancecodec '%s': %s\n", chancecodec_cmd_to_use, strerror(spawn_errno));
                }
                else
                {
                    fprintf(stderr, "chancecodec failed (rc=%d): %s\n", chance_rc, display_cmd);
                }
                if (!chancecodec_has_override && chancecodec_uses_fallback)
                {
                    fprintf(stderr, "hint: use --chancecodec <path> or set CHANCECODEC_CMD to point at the ChanceCode CLI executable\n");
                }
                rc = 1;
                break;
            }
        }

        if (target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm)
        {
            if (!need_obj)
            {
                fprintf(stderr, "internal error: object output expected but path missing\n");
                rc = 1;
                break;
            }

            char cc_cmd[4096];
            size_t pos = (size_t)snprintf(cc_cmd, sizeof(cc_cmd), "cc -c \"%s\" -o \"%s\"", asm_path, objOut);
            if (pos >= sizeof(cc_cmd))
            {
                fprintf(stderr, "command buffer exhausted for cc invocation\n");
                rc = 1;
                break;
            }
            if (freestanding)
            {
                strncat(cc_cmd, " -ffreestanding -nostdlib", sizeof(cc_cmd) - strlen(cc_cmd) - 1);
            }
#ifdef _WIN32
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

        if (target_arch == ARCH_X86)
        {
            if (!stop_after_asm)
                remove(asm_path);
        }

        if (target_arch == ARCH_X86 && !stop_after_ccb && !stop_after_asm && !no_link && !multi_link)
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
    if (!rc && multi_link)
    {
        // Final link of temps + provided objects into an executable
        size_t cmdsz = 4096;
        char *cmd = (char *)xmalloc(cmdsz);
        snprintf(cmd, cmdsz, "cc -o \"%s\"", out);
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
        int lrc = system(cmd);
        if (lrc != 0)
        {
            fprintf(stderr, "link failed (rc=%d): %s\n", lrc, cmd);
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
    if (!rc && have_single_obj)
    {
        char link_cmd[4096];
        snprintf(link_cmd, sizeof(link_cmd), "cc -o \"%s\" \"%s\"", out, single_obj_path);
        int lrc = system(link_cmd);
        if (lrc != 0)
        {
            fprintf(stderr, "link failed (rc=%d): %s\n", lrc, link_cmd);
            rc = 1;
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
        snprintf(cmd, cmdsz, "cc -r -o \"%s\"", obj_override);
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
    for (int i = 0; i < include_dir_count; i++)
        free(include_dirs[i]);
    free(include_dirs);
    free((void *)ce_inputs);
    free((void *)ccb_inputs);
    free((void *)obj_inputs);
    return rc;
fail:
    rc = 2;
    goto cleanup;
}
