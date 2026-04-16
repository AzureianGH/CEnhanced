// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.section __TEXT,__text,regular,pure_instructions

.globl _LibTest_test_fun_i32
.p2align 2
_LibTest_test_fun_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    ldr w9, [x27, #0]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #8]
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_other_fun
.p2align 2
_LibTest_other_fun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #8]
    ldr x9, [x27, #8]
    str x9, [x27, #0]
    ldr x9, [x27, #0]
    movz w10, #0xa
    str w10, [x9]
    ldr x9, [x27, #0]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    movz w10, #0x14
    strb w10, [x9]
    ldr x0, [x27, #0]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

