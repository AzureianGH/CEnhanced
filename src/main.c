#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
// sema
int sema_eval_const_i32(Node *expr);
SemaContext *sema_create(void);
void sema_destroy(SemaContext *sc);
int sema_check_unit(SemaContext *sc, Node *unit);

static void usage(const char *prog){
    fprintf(stderr, "Usage: %s [options] input.ce\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -o <file>         Output executable path (default a.exe)\n");
    fprintf(stderr, "  -S                Emit pseudo-asm alongside exe (.S)\n");
    fprintf(stderr, "  --freestanding    Freestanding mode (no default libs)\n");
    fprintf(stderr, "  -m32|-m64         Target bitness (currently -m64 only)\n");
}

static char *read_all(const char *path, int *out_len){
    FILE *f = fopen(path, "rb");
    if(!f){ perror("fopen"); return NULL; }
    fseek(f, 0, SEEK_END); long sz = ftell(f); fseek(f, 0, SEEK_SET);
    char *buf = (char*)xmalloc((size_t)sz+1);
    if(fread(buf, 1, sz, f)!=(size_t)sz){ perror("fread"); fclose(f); free(buf); return NULL; }
    fclose(f); buf[sz]='\0'; if(out_len) *out_len = (int)sz; return buf;
}

int main(int argc, char **argv){
    const char *out = "a.exe";
    int emit_asm = 0;
    int freestanding = 0;
    int m32 = 0;
    AsmSyntax asm_syntax = ASM_INTEL;
    #ifdef _WIN32
    TargetOS target_os = OS_WINDOWS;
    #else
    TargetOS target_os = OS_LINUX;
    #endif
    const char *input = NULL;

    for(int i=1;i<argc;i++){
        if(strcmp(argv[i], "-o")==0 && i+1<argc){ out = argv[++i]; continue; }
        if(strcmp(argv[i], "-S")==0){ emit_asm=1; continue; }
    if(strcmp(argv[i], "--asm-syntax")==0 && i+1<argc){
            const char *arg = argv[++i];
            if(strcmp(arg, "intel")==0) asm_syntax = ASM_INTEL;
            else if(strcmp(arg, "att")==0) asm_syntax = ASM_ATT;
            else if(strcmp(arg, "nasm")==0) asm_syntax = ASM_NASM;
            else { fprintf(stderr, "Unknown asm syntax '%s' (use intel|att|nasm)\n", arg); return 2; }
            continue;
        }
        if(strcmp(argv[i], "--freestanding")==0){ freestanding=1; continue; }
        if(strcmp(argv[i], "-m32")==0){ m32=1; continue; }
        if(strcmp(argv[i], "-m64")==0){ m32=0; continue; }
        if(strcmp(argv[i], "--target-os")==0 && i+1<argc){
            const char *os = argv[++i];
            if(strcmp(os, "windows")==0) target_os = OS_WINDOWS;
            else if(strcmp(os, "linux")==0) target_os = OS_LINUX;
            else { fprintf(stderr, "Unknown --target-os '%s' (use windows|linux)\n", os); return 2; }
            continue;
        }
        if(argv[i][0]=='-'){ usage(argv[0]); return 2; }
        input = argv[i];
    }
    if(!input){ usage(argv[0]); return 2; }
    if(m32){ fprintf(stderr, "Error: -m32 not implemented yet\n"); return 2; }

    int len=0; char *src = read_all(input, &len);
    if(!src) return 1;
    SourceBuffer sb = { src, len, input };
    Parser *ps = parser_create(sb);
    Node *unit = parse_unit(ps);
    // Semantic analysis (strict typing)
    SemaContext *sc = sema_create();
    // Export externs parsed from 'extend from ...'
    parser_export_externs(ps, sc->syms);
    int serr = sema_check_unit(sc, unit);
    if(serr){ sema_destroy(sc); ast_free(unit); parser_destroy(ps); free(src); return 1; }
    CodegenOptions co = { .freestanding = freestanding != 0, .m32 = m32 != 0, .emit_asm = emit_asm != 0, .asm_syntax = asm_syntax, .output_path = out, .os = target_os };
    int rc = codegen_coff_x64_write_exe(unit, &co);
    sema_destroy(sc);
    ast_free(unit);
    parser_destroy(ps);
    free(src);
    return rc;
}
