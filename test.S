// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _ArmTest_testfun
.extern _ArmTest_overflowi8
.extern _printf

.globl _ArmTest_testfun
.p2align 2
_ArmTest_testfun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    strh w0, [sp, #0]
    strh w1, [sp, #8]
    ldrsh w9, [sp, #0]
    ldrsh w10, [sp, #8]
    add w9, w9, w10
    sxth w9, w9
    sxth w9, w9
    mov w0, w9
    add sp, sp, #16
    ldp x29, x30, [sp], #16
    ret

.globl _ArmTest_overflowi8
.p2align 2
_ArmTest_overflowi8:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    movz w9, #0x78
    strb w9, [sp, #0]
    movz w9, #0xa
    strb w9, [sp, #8]
    ldrsb w9, [sp, #0]
    ldrsb w10, [sp, #8]
    add w9, w9, w10
    sxtb w9, w9
    strb w9, [sp, #16]
    ldrsb w0, [sp, #16]
    add sp, sp, #32
    ldp x29, x30, [sp], #16
    ret

.globl _main
.p2align 2
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    movz w0, #0x5
    movz w1, #0x5
    bl _ArmTest_testfun
    mov w9, w0
    str w9, [sp, #0]
    movz w0, #0xa
    movz w1, #0x14
    bl _ArmTest_testfun
    ldr w9, [sp, #0]
    mov w10, w0
    add w9, w9, w10
    bl _ArmTest_overflowi8
    adrp x0, _main____str0@PAGE
    add x0, x0, _main____str0@PAGEOFF
    movz w1, #0x2a
    adrp x2, _main____str1@PAGE
    add x2, x2, _main____str1@PAGEOFF
    mov w3, w9
    mov w4, w0
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
    movz w0, #0x7b
    add sp, sp, #16
    ldp x29, x30, [sp], #16
    ret


.section __TEXT,__cstring
_main____str0:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x41, 0x52, 0x4d, 0x20, 0x57, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0x20, 0x4e, 0x75, 0x6d, 0x20, 0x25, 0x64, 0x2c, 0x20, 0x25, 0x73, 0x2c, 0x20, 0x25, 0x64, 0x2c, 0x20, 0x25, 0x64, 0x0a, 0x00
_main____str1:
    .byte 0x4f, 0x74, 0x68, 0x65, 0x72, 0x20, 0x53, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x00
