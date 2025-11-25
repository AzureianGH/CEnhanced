// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.globl _Test_Chance_test_chance
.p2align 2
_Test_Chance_test_chance:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    movz w0, #0x43
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

