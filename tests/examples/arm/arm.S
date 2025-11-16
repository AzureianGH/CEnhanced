// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _ArmTest_testfun
.extern _ArmTest_overflowi8
.extern _printf
.extern _ArmTest_testLiteral

.globl _ArmTest_testfun
.p2align 2
_ArmTest_testfun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    strh w0, [sp, #0]
    strh w1, [sp, #8]
    ldrsh w9, [sp, #0]
    ldrsh w10, [sp, #8]
    add w9, w9, w10
    sxth w9, w9
    strh w9, [sp, #16]
    ldrsh w9, [sp, #16]
    sxth w9, w9
    str w9, [sp, #16]
    ldr w0, [sp, #16]
    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret

.globl _ArmTest_overflowi8
.p2align 2
_ArmTest_overflowi8:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    movz w9, #0x78
    strb w9, [sp, #0]
    movz w9, #0xa
    strb w9, [sp, #8]
    ldrsb w9, [sp, #0]
    ldrsb w10, [sp, #8]
    add w9, w9, w10
    sxtb w9, w9
    strb w9, [sp, #24]
    ldrsb w9, [sp, #24]
    strb w9, [sp, #16]
    ldrsb w0, [sp, #16]
    add sp, sp, #48
    ldp x29, x30, [sp], #16
    ret

.globl _main
.p2align 2
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #96
    movz w0, #0x5
    movz w1, #0x5
    bl _ArmTest_testfun
    str w0, [sp, #40]
    ldr w9, [sp, #40]
    str w9, [sp, #0]
    movz w0, #0xa
    movz w1, #0x14
    bl _ArmTest_testfun
    str w0, [sp, #72]
    ldr w9, [sp, #0]
    ldr w10, [sp, #72]
    add w9, w9, w10
    str w9, [sp, #64]
    bl _ArmTest_overflowi8
    strb w0, [sp, #72]
    ldrsb w9, [sp, #72]
    sxtb w9, w9
    str w9, [sp, #72]
    adrp x0, _main____str0@PAGE
    add x0, x0, _main____str0@PAGEOFF
    movz w1, #0x2a
    adrp x2, _main____str1@PAGE
    add x2, x2, _main____str1@PAGEOFF
    ldr w3, [sp, #64]
    ldr w4, [sp, #72]
    sub sp, sp, #32
    sxtw x1, w1
    str x1, [sp]
    str x2, [sp, #8]
    sxtw x3, w3
    str x3, [sp, #16]
    sxtw x4, w4
    str x4, [sp, #24]
    bl _printf
    add sp, sp, #32
    str w0, [sp, #40]
    movz w9, #0x64
    str w9, [sp, #8]
    add x9, sp, #8
    str x9, [sp, #40]
    ldr x9, [sp, #40]
    str x9, [sp, #16]
    ldr x9, [sp, #16]
    ldr w10, [x9]
    str w10, [sp, #48]
    adrp x0, _main____str2@PAGE
    add x0, x0, _main____str2@PAGEOFF
    ldr w1, [sp, #48]
    sub sp, sp, #16
    sxtw x1, w1
    str x1, [sp]
    bl _printf
    add sp, sp, #16
    str w0, [sp, #40]
    movz w9, #0x301
    strh w9, [sp, #24]
    add x9, sp, #24
    str x9, [sp, #40]
    ldr x9, [sp, #40]
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    mov w9, wzr
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    ldrsb w10, [x9]
    strb w10, [sp, #48]
    ldr x9, [sp, #32]
    str x9, [sp, #56]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [sp, #64]
    ldr x9, [sp, #56]
    ldr x10, [sp, #64]
    add x9, x9, x10
    str x9, [sp, #56]
    ldr x9, [sp, #56]
    str x9, [sp, #56]
    ldr x9, [sp, #56]
    ldrsb w10, [x9]
    strb w10, [sp, #56]
    adrp x0, _main____str3@PAGE
    add x0, x0, _main____str3@PAGEOFF
    ldrsb w1, [sp, #48]
    ldrsb w2, [sp, #56]
    sub sp, sp, #16
    sxtw x1, w1
    str x1, [sp]
    sxtw x2, w2
    str x2, [sp, #8]
    bl _printf
    add sp, sp, #16
    str w0, [sp, #40]
    bl _ArmTest_testLiteral
    str w0, [sp, #40]
