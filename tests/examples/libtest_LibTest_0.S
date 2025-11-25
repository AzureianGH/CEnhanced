// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.globl _LibTest_test_fun
.p2align 2
_LibTest_test_fun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    str w0, [sp, #0]
    ldr w9, [sp, #0]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #8]
    ldr w0, [sp, #8]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_other_fun
.p2align 2
_LibTest_other_fun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    sub sp, sp, #16
    mov x10, sp
    str x10, [sp, #40]
    ldr x9, [sp, #40]
    str x9, [sp, #16]
    movz w9, #0xa
    str w9, [sp, #24]
    ldr x9, [sp, #16]
    ldr w10, [sp, #24]
    str w10, [x9]
    ldr x9, [sp, #16]
    str x9, [sp, #40]
    ldr x9, [sp, #40]
    movz x10, #0x4, lsl #0
    add x9, x9, x10
    str x9, [sp, #40]
    ldr x9, [sp, #40]
    str x9, [sp, #40]
    movz w9, #0x14
    strb w9, [sp, #32]
    ldr x9, [sp, #40]
    ldrb w10, [sp, #32]
    strb w10, [x9]
    ldr x0, [sp, #16]
    add sp, sp, #16
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

