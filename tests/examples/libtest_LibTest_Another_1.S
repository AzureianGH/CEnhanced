// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _printf

.globl _LibTest_Another_other_test_fun
.p2align 2
_LibTest_Another_other_test_fun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    str w0, [sp, #0]
    ldr w9, [sp, #0]
    movz w10, #0xa
    add w9, w9, w10
    str w9, [sp, #8]
    ldr w0, [sp, #8]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_get_info
.p2align 2
_LibTest_Another_get_info:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #16
    adrp x0, _LibTest_Another_get_info____str0@PAGE
    add x0, x0, _LibTest_Another_get_info____str0@PAGEOFF
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_test_varargs
.p2align 2
_LibTest_Another_test_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #176
    str w0, [sp, #0]
    str w1, [sp, #8]
    str x0, [sp, #48]
    str x1, [sp, #56]
    str x2, [sp, #64]
    str x3, [sp, #72]
    str x4, [sp, #80]
    str x5, [sp, #88]
    str x6, [sp, #96]
    str x7, [sp, #104]
    adrp x0, _LibTest_Another_test_varargs____str1@PAGE
    add x0, x0, _LibTest_Another_test_varargs____str1@PAGEOFF
    ldr w1, [sp, #0]
    ldr w2, [sp, #8]
    sub sp, sp, #16
    sxtw x1, w1
    str x1, [sp]
    sxtw x2, w2
    str x2, [sp, #8]
    bl _printf
    add sp, sp, #16
    str w0, [sp, #112]
    add x9, sp, #64
    str x9, [sp, #112]
    ldr x9, [sp, #112]
    str x9, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
    mov w9, wzr
    str w9, [sp, #32]
LibTest_Another_test_varargs__while_cond0:
    ldr w9, [sp, #32]
    ldr w10, [sp, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #112]
    ldrb w9, [sp, #112]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #112]
    ldrb w9, [sp, #112]
    cmp w9, wzr
    b.ne LibTest_Another_test_varargs__while_body1
    b LibTest_Another_test_varargs__while_end2
LibTest_Another_test_varargs__while_body1:
    ldr x9, [sp, #16]
    ldr w10, [x9]
    str w10, [sp, #112]
    ldr x9, [sp, #16]
    str x9, [sp, #120]
    ldr x9, [sp, #120]
    movz x10, #0x8, lsl #0
    add x9, x9, x10
    str x9, [sp, #120]
    ldr x9, [sp, #120]
    str x9, [sp, #120]
    ldr x9, [sp, #120]
    str x9, [sp, #16]
    ldr w9, [sp, #112]
    str w9, [sp, #40]
    ldr w9, [sp, #24]
    ldr w10, [sp, #40]
    add w9, w9, w10
    str w9, [sp, #112]
    ldr w9, [sp, #112]
    str w9, [sp, #24]
    ldr w9, [sp, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #112]
    ldr w9, [sp, #112]
    str w9, [sp, #32]
    b LibTest_Another_test_varargs__while_cond0
LibTest_Another_test_varargs__while_end2:
    ldr w0, [sp, #24]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_test_diffvarargs
.p2align 2
_LibTest_Another_test_diffvarargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #160
    str x0, [sp, #32]
    str x1, [sp, #40]
    str x2, [sp, #48]
    str x3, [sp, #56]
    str x4, [sp, #64]
    str x5, [sp, #72]
    str x6, [sp, #80]
    str x7, [sp, #88]
    add x9, sp, #32
    str x9, [sp, #96]
    ldr x9, [sp, #96]
    str x9, [sp, #0]
    ldr x9, [sp, #0]
    ldr w10, [x9]
    str w10, [sp, #96]
    ldr x9, [sp, #0]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    movz x10, #0x8, lsl #0
    add x9, x9, x10
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #0]
    ldr w9, [sp, #96]
    str w9, [sp, #8]
    ldr x9, [sp, #0]
    ldr x10, [x9]
    str x10, [sp, #96]
    ldr x9, [sp, #0]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    movz x10, #0x8, lsl #0
    add x9, x9, x10
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #0]
    ldr x9, [sp, #96]
    str x9, [sp, #16]
    ldr x9, [sp, #0]
    ldr x10, [x9]
    str x10, [sp, #96]
    ldr x9, [sp, #0]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    movz x10, #0x8, lsl #0
    add x9, x9, x10
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #104]
    ldr x9, [sp, #104]
    str x9, [sp, #0]
    ldr x9, [sp, #96]
    str x9, [sp, #24]
    adrp x0, _LibTest_Another_test_diffvarargs____str2@PAGE
    add x0, x0, _LibTest_Another_test_diffvarargs____str2@PAGEOFF
    ldr w1, [sp, #8]
    ldr d0, [sp, #16]
    fmov x2, d0
    ldr x3, [sp, #24]
    sub sp, sp, #32
    sxtw x1, w1
    str x1, [sp]
    str x2, [sp, #8]
    str x3, [sp, #16]
    bl _printf
    add sp, sp, #32
    str w0, [sp, #96]
    mov w0, wzr
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret


.section __TEXT,__cstring
_LibTest_Another_get_info____str0:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0x00
_LibTest_Another_test_varargs____str1:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x63, 0x6f, 0x75, 0x6e, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e, 0x75, 0x73, 0x65, 0x64, 0x3d, 0x25, 0x64, 0x0a, 0x00
_LibTest_Another_test_diffvarargs____str2:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x64, 0x69, 0x66, 0x66, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x3a, 0x20, 0x66, 0x69, 0x72, 0x73, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x3d, 0x25, 0x66, 0x2c, 0x20, 0x74, 0x68, 0x69, 0x72, 0x64, 0x3d, 0x25, 0x73, 0x0a, 0x00
