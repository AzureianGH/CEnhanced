// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.section __TEXT,__text,regular,pure_instructions

.globl _main
.p2align 2
_main:
    movz w0, #0x3039
    ret

