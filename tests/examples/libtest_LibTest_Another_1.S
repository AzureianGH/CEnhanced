// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.extern _printf
.extern _Std_IO_print_ptr_to_char

.section __TEXT,__text,regular,pure_instructions

.globl _LibTest_Another_other_test_fun_i32
.p2align 2
_LibTest_Another_other_test_fun_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    ldr w9, [x27, #0]
    movz w10, #0xa
    add w9, w9, w10
    str w9, [x27, #8]
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_get_info
.p2align 2
_LibTest_Another_get_info:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    adrp x0, __LibTest_Another_get_info____str0@PAGE
    add x0, x0, __LibTest_Another_get_info____str0@PAGEOFF
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_test_varargs_i32_i32
.p2align 2
_LibTest_Another_test_varargs_i32_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #128
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    add x9, x27, #56
    str x15, [x9]
    adrp x0, __LibTest_Another_test_varargs_i32_i32____str1@PAGE
    add x0, x0, __LibTest_Another_test_varargs_i32_i32____str1@PAGEOFF
    ldr w1, [x27, #0]
    ldr w2, [x27, #8]
    sub sp, sp, #16
    ldr w9, [x27, #0]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr w9, [x27, #8]
    sxtw x9, w9
    str x9, [sp, #8]
    mov x15, sp
    bl _printf
    add sp, sp, #16
    str w0, [x27, #64]
    add x10, x27, #56
    ldr x9, [x10]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    str x9, [x27, #16]
    mov w9, wzr
    str w9, [x27, #24]
    mov w9, wzr
    str w9, [x27, #32]
LLibTest_Another_test_varargs_i32_i32__while_cond0:
    ldr w9, [x27, #32]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #64]
    ldrb w9, [x27, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #64]
    ldrb w9, [x27, #64]
    cmp w9, wzr
    b.ne LLibTest_Another_test_varargs_i32_i32__while_body1
    b LLibTest_Another_test_varargs_i32_i32__while_end2
LLibTest_Another_test_varargs_i32_i32__while_body1:
    ldr x9, [x27, #16]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    movz x10, #0x3
    add x9, x9, x10
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    movz x10, #0xfffc
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    ldr w10, [x9]
    str w10, [x27, #64]
    ldr x9, [x27, #48]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #16]
    ldr w9, [x27, #64]
    str w9, [x27, #40]
    ldr w9, [x27, #24]
    ldr w10, [x27, #40]
    add w9, w9, w10
    str w9, [x27, #64]
    ldr w9, [x27, #64]
    str w9, [x27, #24]
LLibTest_Another_test_varargs_i32_i32__while_post3:
    ldr w9, [x27, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #64]
    ldr w9, [x27, #64]
    str w9, [x27, #32]
    b LLibTest_Another_test_varargs_i32_i32__while_cond0
LLibTest_Another_test_varargs_i32_i32__while_end2:
    ldr w0, [x27, #24]
    mov sp, x27
    add sp, sp, #128
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_test_diffvarargs_i32
.p2align 2
_LibTest_Another_test_diffvarargs_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #144
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #64
    str x15, [x9]
    add x10, x27, #64
    ldr x9, [x10]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0x3
    add x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0xfffc
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #24]
    ldr x9, [x27, #24]
    ldr w10, [x9]
    str w10, [x27, #72]
    ldr x9, [x27, #24]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #8]
    ldr w9, [x27, #72]
    str w9, [x27, #16]
    ldr x9, [x27, #8]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldr x10, [x9]
    str x10, [x27, #72]
    ldr x9, [x27, #40]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #8]
    ldr d0, [x27, #72]
    str d0, [x27, #32]
    ldr x9, [x27, #8]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #72]
    ldr x9, [x27, #72]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldr x10, [x9]
    str x10, [x27, #72]
    ldr x9, [x27, #56]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #80]
    ldr x9, [x27, #80]
    str x9, [x27, #8]
    ldr x9, [x27, #72]
    str x9, [x27, #48]
    adrp x0, __LibTest_Another_test_diffvarargs_i32____str2@PAGE
    add x0, x0, __LibTest_Another_test_diffvarargs_i32____str2@PAGEOFF
    ldr w1, [x27, #16]
    ldr d0, [x27, #32]
    ldr x2, [x27, #48]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #72]
    mov w0, wzr
    mov sp, x27
    add sp, sp, #144
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest
.p2align 2
_LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #80
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    str x2, [x27, #16]
    ldr x9, [x27, #0]
    ldr w10, [x9]
    str w10, [x27, #24]
    ldr x9, [x27, #8]
    ldr w10, [x9]
    str w10, [x27, #32]
    ldr w9, [x27, #24]
    ldr w10, [x27, #32]
    add w9, w9, w10
    str w9, [x27, #24]
    ldr x9, [x27, #16]
    ldr w10, [x9]
    str w10, [x27, #32]
    ldr w9, [x27, #24]
    ldr w10, [x27, #32]
    add w9, w9, w10
    str w9, [x27, #24]
    ldr w0, [x27, #24]
    mov sp, x27
    add sp, sp, #80
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret


.section __TEXT,__cstring
__LibTest_Another_get_info____str0:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0x00
__LibTest_Another_test_varargs_i32_i32____str1:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x63, 0x6f, 0x75, 0x6e, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e, 0x75, 0x73, 0x65, 0x64, 0x3d, 0x25, 0x64, 0x0a, 0x00
__LibTest_Another_test_diffvarargs_i32____str2:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x64, 0x69, 0x66, 0x66, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x3a, 0x20, 0x66, 0x69, 0x72, 0x73, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x3d, 0x25, 0x6c, 0x66, 0x2c, 0x20, 0x74, 0x68, 0x69, 0x72, 0x64, 0x3d, 0x25, 0x73, 0x0a, 0x00
