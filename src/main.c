#include "ast.h"
#include "includes.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
    fprintf(stderr,
            "  -o <file>         Output executable path (default a.exe)\n");
    fprintf(stderr, "  -S                Emit pseudo-asm alongside exe (.S)\n");
    fprintf(stderr, "  -c [obj] | --no-link [obj]\n");
    fprintf(stderr, "                    Compile only; do not link (emit object). Optional obj output path.\n");
    fprintf(stderr, "  --freestanding    Freestanding mode (no default libs)\n");
    fprintf(stderr, "  -m32|-m64         Target bitness (currently -m64 only)\n");
    fprintf(
        stderr,
        "  --asm-syntax <s>  Assembly syntax: intel|att|nasm (default intel)\n");
    fprintf(stderr,
            "  -I <dir>          Add include search directory for #include <>\n");
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
    int emit_asm = 0;
    int no_link = 0; // -c / --no-link
    int freestanding = 0;
    int m32 = 0;
    AsmSyntax asm_syntax = ASM_INTEL;
#ifdef _WIN32
    TargetOS target_os = OS_WINDOWS;
#else
    TargetOS target_os = OS_LINUX;
#endif
    // Separate CHance and object inputs
    const char **ce_inputs = NULL;
    int ce_count = 0, ce_cap = 0;
    const char **obj_inputs = NULL;
    int obj_count = 0, obj_cap = 0;
    // include paths
    char **include_dirs = NULL;
    int include_dir_count = 0;
    const char *obj_override = NULL; // optional object path after -c/--no-link
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
            printf("chancec: CHance Compiler version 0.1.0\n");
            printf("chancec: CE language standard: H25\n");
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
            emit_asm = 1;
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
        if (strcmp(argv[i], "--freestanding") == 0)
        {
            freestanding = 1;
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
    if (ce_count == 0 && obj_count == 0)
    {
        usage(argv[0]);
        return 2;
    }
    if (m32)
    {
        fprintf(stderr, "Error: -m32 not implemented yet\n");
        return 2;
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

    int rc = 0;
    // Determine if we need a final link step combining multiple inputs
    int multi_link = (!no_link && (obj_count > 0 || ce_count > 1));

    // Container for temporary objects when merging or linking
    char **temp_objs = NULL;
    int to_cnt = 0, to_cap = 0;
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
        // Strip preprocessor lines for the CHance parser, but still scan #include
        // headers to populate externs
        int stripped_len = 0;
        char *stripped = chance_strip_preprocessor_lines(src, len, &stripped_len);
        SourceBuffer sb = {stripped ? stripped : src,
                           stripped ? stripped_len : len, input};
        Parser *ps = parser_create(sb);
        // Set up semantics and scan includes before parsing to populate symbols for
        // known C prototypes
        SemaContext *sc = sema_create();
        chance_process_includes_and_scan(input, src, len, include_dirs,
                                         include_dir_count, sc->syms);
        Node *unit = parse_unit(ps);
        // Semantic analysis (strict typing)
        // Export externs parsed from 'extend from ...'
        parser_export_externs(ps, sc->syms);
        int serr = sema_check_unit(sc, unit);
        if (serr)
        {
            sema_destroy(sc);
            ast_free(unit);
            parser_destroy(ps);
            if (stripped)
                free(stripped);
            free(src);
            rc = 1;
            break;
        }
        // Compute per-file or temporary object name if needed
        char objOut[1024] = {0};
        if (no_link || multi_link)
        {
            if (no_link && obj_override)
            {
                // Create a temporary object per CE to merge later
                char dir[512], base[512];
                split_path(input, dir, sizeof(dir), base, sizeof(base));
#ifdef _WIN32
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s\\%s.co.tmp.obj", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.co.tmp.obj", base);
#else
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s/%s.co.tmp.o", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.co.tmp.o", base);
#endif
                // Note: join handles empty dir by omitting separator
            }
            else if (no_link)
            {
                char dir[512], base[512];
                split_path(input, dir, sizeof(dir), base, sizeof(base));
#ifdef _WIN32
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s\\%s.obj", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.obj", base);
#else
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s/%s.o", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.o", base);
#endif
            }
            else /* multi_link */
            {
                char dir[512], base[512];
                split_path(input, dir, sizeof(dir), base, sizeof(base));
#ifdef _WIN32
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s\\%s.co.tmp.obj", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.co.tmp.obj", base);
#else
                if (dir[0])
                    snprintf(objOut, sizeof(objOut), "%s/%s.co.tmp.o", dir, base);
                else
                    snprintf(objOut, sizeof(objOut), "%s.co.tmp.o", base);
#endif
            }
        }
        CodegenOptions co = {.freestanding = freestanding != 0,
                             .m32 = m32 != 0,
                             .emit_asm = emit_asm != 0,
                             .no_link = (no_link || multi_link) != 0,
                             .asm_syntax = asm_syntax,
                             .output_path = out,
                             .obj_output_path = (no_link || multi_link) ? objOut : NULL,
                             .os = target_os};
        rc = codegen_coff_x64_write_exe(unit, &co);

        sema_destroy(sc);
        ast_free(unit);
        parser_destroy(ps);
        if (stripped)
            free(stripped);
        free(src);
        if (rc)
            break;
        if ((no_link && obj_override) || multi_link)
        {
            // collect temp object for merge
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
    }
    // free include dirs
    for (int i = 0; i < include_dir_count; i++)
        free(include_dirs[i]);
    free(include_dirs);
    free((void *)ce_inputs);
    free((void *)obj_inputs);
    return rc;
fail:
    for (int i = 0; i < include_dir_count; i++)
        free(include_dirs[i]);
    free(include_dirs);
    free((void *)ce_inputs);
    free((void *)obj_inputs);
    return 2;
}
