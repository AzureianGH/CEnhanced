#include "includes.h"
#include "ast.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    if (!dirs || !count || !dir)
        return;
    int c = *count;
    *dirs = (char **)realloc(*dirs, sizeof(char *) * (c + 1));
    (*dirs)[c] = xstrdup(dir);
    *count = c + 1;
}

void chance_add_default_include_dirs(char ***dirs, int *count)
{
    // Honor CHANCE_INCLUDE_PATH if set (semicolon or colon separated)
    const char *env = getenv("CHANCE_INCLUDE_PATH");
    if (env)
    {
        const char *p = env;
        while (*p)
        {
            const char *s = p;
            while (*p && *p != ';' && *p != ':')
                p++;
            size_t L = (size_t)(p - s);
            if (L)
            {
                char *seg = (char *)xmalloc(L + 1);
                memcpy(seg, s, L);
                seg[L] = '\0';
                chance_add_include_dir(dirs, count, seg);
                free(seg);
            }
            if (*p)
                p++;
        }
    }
#ifdef _WIN32
    chance_add_include_dir(dirs, count, "C:/msys64/mingw64/include");
    chance_add_include_dir(dirs, count, "C:/mingw64/include");
#else
    chance_add_include_dir(dirs, count, "/usr/include");
    chance_add_include_dir(dirs, count, "/usr/local/include");
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

// Very naive C prototype scanner: handles lines like 'extern int puts(const
// char*);' or 'int printf(const char*, ...);'
static void scan_header_for_prototypes(const char *buf, int len,
                                       SymTable *syms)
{
    const char *p = buf, *end = buf + len;
    while (p < end)
    {
        // skip whitespace and preprocessor lines
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
        // look for identifier (return type ignored) then function name ident '('
        // ... ');' Skip potential storage/class specifiers
        const char *line = p;
        while (p < end && *p != '\n' && *p != ';')
            p++; // crude: one decl per line
        size_t L = (size_t)(p - line);
        if (L > 0)
        {
            // find '(' and previous word as function name
            const char *lp = memchr(line, '(', L);
            const char *sc = memchr(line, ';', L);
            if (lp && (!sc || lp < sc))
            {
                // backtrack to get name
                const char *q = lp;
                while (q > line && (isalnum((unsigned char)q[-1]) || q[-1] == '_'))
                    q--; // start of name
                const char *qn = q;
                while (qn < lp && (isalnum((unsigned char)*qn) || *qn == '_'))
                    qn++;
                if (qn > q)
                {
                    size_t nlen = (size_t)(qn - q);
                    // Build symbol
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
            p++; // consume delimiter
    }
}

static int resolve_include_path(const char *name, char **include_dirs,
                                int dir_count, char *out, size_t outsz)
{
    // If name is absolute or relative with slash, use as-is
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
    // Walk lines, find #include <...> or #include "..." and scan those headers
    // for prototypes
    const char *p = source_buf, *end = source_buf + source_len;
    char path[1024];
    (void)source_path;
    while (p < end)
    {
        // consume spaces
        const char *line = p;
        while (p < end && *p != '\n')
            p++;
        size_t L = (size_t)(p - line);
        const char *nl = p < end ? p + 1 : p;
        // match pattern ^\s*#\s*include\s*[<"]name[>"]
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
