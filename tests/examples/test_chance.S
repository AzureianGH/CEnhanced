// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.file 0 "/Users/nathanhornby/Documents/GitHub/CEnhanced/tests/examples" "test_chance.ce"
.file 1 "/Users/nathanhornby/Documents/GitHub/CEnhanced/tests/examples/test_chance.ce"

.section __TEXT,__text,regular,pure_instructions

.globl _Test_Chance_test_chance
.p2align 2
_Test_Chance_test_chance:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 18 9
    .loc 1 18 5
    movz w0, #0x43
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

