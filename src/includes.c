#include "includes.h"
#include "ast.h"
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#ifndef _WIN32
#include <dirent.h>
#endif

#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

#ifdef _WIN32
#define CHANCE_PATH_LIST_SEP ';'
#else
#define CHANCE_PATH_LIST_SEP ':'
#endif

static int chance_dir_exists(const char *path)
{
    if (!path || !*path)
        return 0;
    struct stat st;
    if (stat(path, &st) != 0)
        return 0;
    return (st.st_mode & S_IFDIR) != 0;
}

static int chance_has_include_dir(char **dirs, int count, const char *dir)
{
    if (!dirs || !dir)
        return 0;
    for (int i = 0; i < count; ++i)
    {
        if (dirs[i] && strcmp(dirs[i], dir) == 0)
            return 1;
    }
    return 0;
}

static void chance_add_include_dir_if_exists(char ***dirs, int *count,
                                             const char *dir)
{
    if (!dirs || !count || !dir || !*dir)
        return;
    if (!chance_dir_exists(dir))
        return;
    if (chance_has_include_dir(*dirs, *count, dir))
        return;

    int c = *count;
    char **grown = (char **)realloc(*dirs, sizeof(char *) * (size_t)(c + 1));
    if (!grown)
        return;
    *dirs = grown;
    (*dirs)[c] = xstrdup(dir);
    if (!(*dirs)[c])
        return;
    *count = c + 1;
}

static void chance_add_env_path_list(char ***dirs, int *count,
                                     const char *env_name)
{
    if (!env_name)
        return;
    const char *env = getenv(env_name);
    if (!env || !*env)
        return;

    const char *p = env;
    while (*p)
    {
        const char *s = p;
        while (*p && *p != CHANCE_PATH_LIST_SEP)
            ++p;
        size_t L = (size_t)(p - s);
        while (L > 0 && isspace((unsigned char)s[0]))
        {
            ++s;
            --L;
        }
        while (L > 0 && isspace((unsigned char)s[L - 1]))
            --L;
        if (L > 0)
        {
            char *seg = (char *)xmalloc(L + 1);
            memcpy(seg, s, L);
            seg[L] = '\0';
            chance_add_include_dir_if_exists(dirs, count, seg);
            free(seg);
        }
        if (*p == CHANCE_PATH_LIST_SEP)
            ++p;
    }
}

static void chance_add_joined_dir(char ***dirs, int *count,
                                  const char *base, const char *suffix)
{
    if (!base || !*base || !suffix || !*suffix)
        return;
    size_t bl = strlen(base);
    size_t sl = strlen(suffix);
    int need_sep = (bl > 0 && base[bl - 1] != '/' && base[bl - 1] != '\\');
    char *buf = (char *)xmalloc(bl + (size_t)need_sep + sl + 1);
    memcpy(buf, base, bl);
    size_t off = bl;
    if (need_sep)
        buf[off++] = '/';
    memcpy(buf + off, suffix, sl);
    buf[off + sl] = '\0';
    chance_add_include_dir_if_exists(dirs, count, buf);
    free(buf);
}

#ifndef _WIN32
static void chance_add_linux_multiarch_dirs(char ***dirs, int *count)
{
    DIR *d = opendir("/usr/include");
    if (!d)
        return;
    struct dirent *ent = NULL;
    while ((ent = readdir(d)) != NULL)
    {
        const char *name = ent->d_name;
        if (!name || name[0] == '.')
            continue;
        if (!strstr(name, "-linux-gnu"))
            continue;
        chance_add_joined_dir(dirs, count, "/usr/include", name);
    }
    closedir(d);
}

#if defined(__APPLE__)
static void chance_add_latest_macos_sdk_include(char ***dirs, int *count,
                                                const char *sdk_root)
{
    if (!sdk_root)
        return;
    DIR *d = opendir(sdk_root);
    if (!d)
        return;

    char best[PATH_MAX];
    best[0] = '\0';
    struct dirent *ent = NULL;
    while ((ent = readdir(d)) != NULL)
    {
        const char *name = ent->d_name;
        if (!name || name[0] == '.')
            continue;
        if (!strstr(name, ".sdk"))
            continue;
        if (strncmp(name, "MacOSX", 6) != 0)
            continue;
        if (best[0] == '\0' || strcmp(name, best) > 0)
        {
            snprintf(best, sizeof(best), "%s", name);
        }
    }
    closedir(d);

    if (best[0])
    {
        char base[PATH_MAX];
        snprintf(base, sizeof(base), "%s/%s", sdk_root, best);
        chance_add_joined_dir(dirs, count, base, "usr/include");
    }
}
#endif
#endif

static char *read_all_file(const char *path, int *out_len)
{
    FILE *f = fopen(path, "rb");
    if (!f)
        return NULL;
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *buf = (char *)xmalloc((size_t)sz + 1);
    size_t n = fread(buf, 1, sz, f);
    fclose(f);
    if (n != (size_t)sz)
    {
        free(buf);
        return NULL;
    }
    buf[sz] = '\0';
    if (out_len)
        *out_len = (int)sz;
    return buf;
}

void chance_add_include_dir(char ***dirs, int *count, const char *dir)
{
    chance_add_include_dir_if_exists(dirs, count, dir);
}

void chance_add_default_include_dirs(char ***dirs, int *count)
{
    chance_add_env_path_list(dirs, count, "CHANCE_INCLUDE_PATH");
    chance_add_env_path_list(dirs, count, "CPATH");
    chance_add_env_path_list(dirs, count, "C_INCLUDE_PATH");

#ifdef _WIN32
    chance_add_env_path_list(dirs, count, "INCLUDE");

    {
        const char *vc_tools = getenv("VCToolsInstallDir");
        if (vc_tools && *vc_tools)
            chance_add_joined_dir(dirs, count, vc_tools, "include");
    }

    {
        const char *ucrt_root = getenv("UniversalCRTSdkDir");
        const char *ucrt_ver = getenv("UCRTVersion");
        if (ucrt_root && *ucrt_root && ucrt_ver && *ucrt_ver)
        {
            char suffix[PATH_MAX];
            snprintf(suffix, sizeof(suffix), "Include/%s/ucrt", ucrt_ver);
            chance_add_joined_dir(dirs, count, ucrt_root, suffix);
        }
    }

    {
        const char *sdk_root = getenv("WindowsSdkDir");
        const char *sdk_ver = getenv("WindowsSDKVersion");
        if (sdk_root && *sdk_root && sdk_ver && *sdk_ver)
        {
            char suffix[PATH_MAX];
            snprintf(suffix, sizeof(suffix), "Include/%s/ucrt", sdk_ver);
            chance_add_joined_dir(dirs, count, sdk_root, suffix);
            snprintf(suffix, sizeof(suffix), "Include/%s/shared", sdk_ver);
            chance_add_joined_dir(dirs, count, sdk_root, suffix);
            snprintf(suffix, sizeof(suffix), "Include/%s/um", sdk_ver);
            chance_add_joined_dir(dirs, count, sdk_root, suffix);
            snprintf(suffix, sizeof(suffix), "Include/%s/winrt", sdk_ver);
            chance_add_joined_dir(dirs, count, sdk_root, suffix);
        }
    }
#else
    chance_add_include_dir_if_exists(dirs, count, "/usr/local/include");
    chance_add_include_dir_if_exists(dirs, count, "/usr/include");

#if defined(__APPLE__)
    chance_add_include_dir_if_exists(dirs, count, "/opt/homebrew/include");
    chance_add_include_dir_if_exists(dirs, count, "/opt/local/include");

    {
        const char *sdkroot = getenv("SDKROOT");
        if (sdkroot && *sdkroot)
            chance_add_joined_dir(dirs, count, sdkroot, "usr/include");
    }

    chance_add_latest_macos_sdk_include(
        dirs, count, "/Library/Developer/CommandLineTools/SDKs");
    chance_add_latest_macos_sdk_include(
        dirs, count,
        "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs");
    chance_add_include_dir_if_exists(dirs, count,
                                     "/Library/Developer/CommandLineTools/usr/include");
#elif defined(__linux__)
    chance_add_linux_multiarch_dirs(dirs, count);
#endif
#endif
}

static int try_form_path(char *dst, size_t dstsz, const char *base_dir,
                         const char *name)
{
    if (!base_dir)
    {
        snprintf(dst, dstsz, "%s", name);
        return 0;
    }
    snprintf(dst, dstsz, "%s/%s", base_dir, name);
    return 0;
}



static void scan_header_for_prototypes(const char *buf, int len,
                                       SymTable *syms)
{
    const char *p = buf, *end = buf + len;
    while (p < end)
    {
        
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n'))
            p++;
        if (p >= end)
            break;
        if (*p == '#')
        {
            while (p < end && *p != '\n')
                p++;
            continue;
        }
        
        
        const char *line = p;
        while (p < end && *p != '\n' && *p != ';')
            p++; 
        size_t L = (size_t)(p - line);
        if (L > 0)
        {
            
            const char *lp = memchr(line, '(', L);
            const char *sc = memchr(line, ';', L);
            if (lp && (!sc || lp < sc))
            {
                
                const char *q = lp;
                while (q > line && (isalnum((unsigned char)q[-1]) || q[-1] == '_'))
                    q--; 
                const char *qn = q;
                while (qn < lp && (isalnum((unsigned char)*qn) || *qn == '_'))
                    qn++;
                if (qn > q)
                {
                    size_t nlen = (size_t)(qn - q);
                    
                    char *nm = (char *)xmalloc(nlen + 1);
                    memcpy(nm, q, nlen);
                    nm[nlen] = '\0';
                    Symbol s = (Symbol){0};
                    s.kind = SYM_FUNC;
                    s.name = nm;
                    s.backend_name = s.name;
                    s.is_extern = 1;
                    s.abi = "C";
                    static Type ti32 = {.kind = TY_I32};
                    s.sig.ret = &ti32;
                    s.sig.params = NULL;
                    s.sig.param_count = 0;
                    int varargs = 0;
                    for (size_t i = 0; i + 2 < L; i++)
                    {
                        if (line[i] == '.' && line[i + 1] == '.' && line[i + 2] == '.')
                        {
                            varargs = 1;
                            break;
                        }
                    }
                    s.sig.is_varargs = varargs;

                    symtab_add(syms, s);
                }
            }
        }
        if (p < end)
            p++; 
    }
}

static int resolve_include_path(const char *name, char **include_dirs,
                                int dir_count, char *out, size_t outsz)
{
    
    if (strchr(name, '/') || strchr(name, '\\'))
    {
        snprintf(out, outsz, "%s", name);
        return 0;
    }
    for (int i = 0; i < dir_count; i++)
    {
        try_form_path(out, outsz, include_dirs[i], name);
        FILE *f = fopen(out, "rb");
        if (f)
        {
            fclose(f);
            return 0;
        }
    }
    return 1;
}

int chance_process_includes_and_scan(const char *source_path,
                                     const char *source_buf, int source_len,
                                     char **include_dirs, int dir_count,
                                     SymTable *syms)
{
    
    
    const char *p = source_buf, *end = source_buf + source_len;
    char path[1024];
    (void)source_path;
    while (p < end)
    {
        
        const char *line = p;
        while (p < end && *p != '\n')
            p++;
        size_t L = (size_t)(p - line);
        const char *nl = p < end ? p + 1 : p;
        
        const char *s = line;
        while (s < line + L && (*s == ' ' || *s == '\t'))
            s++;
        if (s < line + L && *s == '#')
        {
            s++;
            while (s < line + L && (*s == ' ' || *s == '\t'))
                s++;
            if ((size_t)(line + L - s) >= 7 && strncmp(s, "include", 7) == 0)
            {
                s += 7;
                while (s < line + L && (*s == ' ' || *s == '\t'))
                    s++;
                if (s < line + L && (*s == '<' || *s == '\"'))
                {
                    char q = *s;
                    s++;
                    const char *name = s;
                    while (s < line + L && *s != (q == '<' ? '>' : '\"'))
                        s++;
                    size_t nlen = (size_t)(s - name);
                    if (nlen > 0)
                    {
                        char *inc = (char *)xmalloc(nlen + 1);
                        memcpy(inc, name, nlen);
                        inc[nlen] = '\0';
                        if (resolve_include_path(inc, include_dirs, dir_count, path,
                                                 sizeof(path)) == 0)
                        {
                            int hlen = 0;
                            char *hbuf = read_all_file(path, &hlen);
                            if (hbuf)
                            {
                                scan_header_for_prototypes(hbuf, hlen, syms);
                                free(hbuf);
                            }
                        }
                        free(inc);
                    }
                }
            }
        }
        p = nl;
    }
    return 0;
}
