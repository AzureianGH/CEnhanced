#include "driver_cli.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>

void usage(const char *prog)
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
  fprintf(stderr, "  -bslash           Select BSlash backend for assembly/object/executable output\n");
  fprintf(stderr, "  --target-os <os>  Backend target OS: windows|linux|macos\n");
  fprintf(
      stderr,
      "  --asm-syntax <s>  Assembly syntax: intel|att|nasm (default intel)\n");
  fprintf(stderr, "  --chancecodec <path>\n");
  fprintf(stderr, "                    Override ChanceCode CLI executable path "
                  "(default: auto-detect or PATH)\n");
  fprintf(stderr, "  --chs <path>      Override CHS assembler executable path "
                  "(default: auto-detect or PATH)\n");
  fprintf(stderr, "  --cc <path>       Override host C compiler used for link and x86 assembly fallback (default cc)\n");
        fprintf(stderr, "  --entry <symbol>  Set entry symbol passed to linker (e.g., _start)\n");
  fprintf(stderr,
          "  -sr:<path>       Load .ce/.cclib for symbols only (no codegen)\n");
  fprintf(stderr,
          "  -I <dir>          Add include search directory for #include <>\n");
  fprintf(stderr, "  -Nno-formatting  Disable formatting guidance notes\n");
  fprintf(stderr,
          "  -d, --debug      Forward debug mode to toolchain stages\n");
  fprintf(stderr,
          "  -v, --verbose    Enable verbose diagnostics and progress\n");
  fprintf(stderr,
          "  -vd             Verbose deep + forward deep debug to toolchain\n");
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
  fprintf(stderr,
          "  -H26             Compile in H26 language mode\n");
  fprintf(stderr,
          "  -H27             Compile in H27 language mode (default)\n");
  fprintf(stderr, "  --library         Emit a .cclib library instead of "
                  "compiling/linking\n");
}

int ends_with_icase(const char *s, const char *suf)
{
  if (!s || !suf)
    return 0;
  size_t ls = strlen(s);
  size_t lu = strlen(suf);
  if (lu > ls)
    return 0;
  const char *p = s + (ls - lu);
  for (size_t i = 0; i < lu; ++i)
  {
    unsigned char a = (unsigned char)p[i];
    unsigned char b = (unsigned char)suf[i];
    if (tolower(a) != tolower(b))
      return 0;
  }
  return 1;
}

int is_ce_source_arg(const char *path)
{
  return path && (ends_with_icase(path, ".ce") || ends_with_icase(path, ".ceproj"));
}

int is_object_file_arg(const char *path)
{
  return path && (ends_with_icase(path, ".o") || ends_with_icase(path, ".obj"));
}

int equals_icase(const char *a, const char *b)
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