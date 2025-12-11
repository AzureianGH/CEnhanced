// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.extern _Std_IO_print_ptr_to_char
.extern _Std_Memory_alloc_i32
.extern _Std_Memory_memset_ptr_to_void_u8_i32
.extern _write
.extern _read
.extern _Std_String_strlen_ptr_to_char
.extern _Std_Memory_dealloc_ptr_to_void
.extern _Test_Chance_test_chance
.extern _test
.extern _LibTest_test_fun_i32
.extern _LibTest_Another_other_test_fun_i32
.extern _LibTest_Another_get_info
.extern _LibTest_Another_test_varargs_i32_i32
.extern _LibTest_Another_test_diffvarargs_i32
.extern _LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest
.extern _Std_IO_printnl_ptr_to_char
.extern _exit


.section __DATA,__data
.p2align 2
_Test_Program_total_test_count:
    .long 0x00000000

.p2align 3
_Test_Program_globalElement:
    .zero 48

.section __TEXT,__text,regular,pure_instructions

.p2align 2
__cc_hidden_fn1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    movz w0, #0x8
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    movz w0, #0x8
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn3:
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

.p2align 2
__cc_hidden_fn4:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    adrp x0, ___cc_hidden_fn4____str0@PAGE
    add x0, x0, ___cc_hidden_fn4____str0@PAGEOFF
    adrp x1, ___cc_hidden_fn4____str1@PAGE
    add x1, x1, ___cc_hidden_fn4____str1@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #0]
    ldr w9, [x27, #0]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #0]
    ldrb w0, [x27, #0]
    adrp x1, ___cc_hidden_fn4____str2@PAGE
    add x1, x1, ___cc_hidden_fn4____str2@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, ___cc_hidden_fn4____str3@PAGE
    add x0, x0, ___cc_hidden_fn4____str3@PAGEOFF
    adrp x1, ___cc_hidden_fn4____str4@PAGE
    add x1, x1, ___cc_hidden_fn4____str4@PAGEOFF
    movz w2, #0x2a
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #0]
    ldr w0, [x27, #0]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn6:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    strb w0, [x27, #0]
    str x1, [x27, #8]
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x9]
    str w10, [x27, #32]
    ldr w9, [x27, #32]
    str w9, [x27, #16]
    movz w9, #0x1
    str w9, [x27, #24]
    ldr w9, [x27, #16]
    ldr w10, [x27, #24]
    add w9, w9, w10
    str w9, [x27, #32]
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x27, #32]
    str w10, [x9]
    ldrsb w9, [x27, #0]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    ldrb w9, [x27, #32]
    cmp w9, wzr
    b.ne Lcc_label_f4_0
    b Lcc_label_f4_1
Lcc_label_f4_0:
    adrp x0, ___cc_hidden_fn6____str5@PAGE
    add x0, x0, ___cc_hidden_fn6____str5@PAGEOFF
    ldr x1, [x27, #8]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #32]
    b Lcc_label_f4_2
Lcc_label_f4_1:
    adrp x0, ___cc_hidden_fn6____str6@PAGE
    add x0, x0, ___cc_hidden_fn6____str6@PAGEOFF
    ldr x1, [x27, #8]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #32]
    bl __cc_hidden_fn7
Lcc_label_f4_2:
    mov w0, wzr
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _main
.p2align 2
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #3072
    mov x27, sp
    mov x9, xzr
    str x9, [x27, #0]
    movz w0, #0x80
    bl _Std_Memory_alloc_i32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #0]
    ldr x9, [x27, #0]
    ldr x10, [x27, #0]
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str7@PAGE
    add x1, x1, __main____str7@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #0]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str8@PAGE
    add x1, x1, __main____str8@PAGEOFF
    bl __cc_hidden_fn6
    ldr x0, [x27, #0]
    mov w1, wzr
    movz w2, #0x80
    bl _Std_Memory_memset_ptr_to_void_u8_i32
    str x0, [x27, #1104]
    movz w9, #0x1
    strb w9, [x27, #8]
    mov w9, wzr
    str w9, [x27, #16]
Lcc_label_f5_0:
    ldr w9, [x27, #16]
    movz w10, #0x80
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_1
    b Lcc_label_f5_2
Lcc_label_f5_1:
    ldr x9, [x27, #0]
    str x9, [x27, #1104]
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_3
    b Lcc_label_f5_4
Lcc_label_f5_3:
    mov w9, wzr
    strb w9, [x27, #8]
Lcc_label_f5_4:
Lcc_label_f5_5:
    ldr w9, [x27, #16]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #16]
    b Lcc_label_f5_0
Lcc_label_f5_2:
    ldrsb w0, [x27, #8]
    adrp x1, __main____str9@PAGE
    add x1, x1, __main____str9@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str10@PAGE
    add x1, x1, __main____str10@PAGEOFF
    movz w2, #0x14
    bl _write
    str w0, [x27, #1104]
    mov w0, wzr
    ldr x1, [x27, #0]
    movz w2, #0x7f
    bl _read
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #24]
    bl __cc_hidden_fn4
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x1a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str11@PAGE
    add x1, x1, __main____str11@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    ldr w9, [x27, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_6
    b Lcc_label_f5_7
Lcc_label_f5_6:
    ldr x9, [x27, #32]
    str x9, [x27, #1104]
    ldr w9, [x27, #24]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrsb w10, [x9]
    strb w10, [x27, #1104]
    ldrsb w9, [x27, #1104]
    movz w10, #0xa
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_8
    b Lcc_label_f5_9
Lcc_label_f5_8:
    ldr x9, [x27, #32]
    str x9, [x27, #1104]
    ldr w9, [x27, #24]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    str w9, [x27, #24]
    ldr w9, [x27, #24]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
Lcc_label_f5_9:
    ldr w9, [x27, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_10
    b Lcc_label_f5_11
Lcc_label_f5_10:
    ldr x9, [x27, #32]
    str x9, [x27, #1104]
    ldr w9, [x27, #24]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrsb w10, [x9]
    strb w10, [x27, #1104]
    ldrsb w9, [x27, #1104]
    movz w10, #0xd
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_12
    b Lcc_label_f5_11
Lcc_label_f5_12:
    movz w11, #0x1
    strb w11, [x27, #1104]
    b Lcc_label_f5_13
Lcc_label_f5_11:
Lcc_label_f5_13:
    ldrsb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_14
    b Lcc_label_f5_15
Lcc_label_f5_14:
    ldr x9, [x27, #32]
    str x9, [x27, #1104]
    ldr w9, [x27, #24]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    str w9, [x27, #24]
    ldr w9, [x27, #24]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
Lcc_label_f5_15:
    b Lcc_label_f5_16
Lcc_label_f5_7:
    adrp x0, __main____str12@PAGE
    add x0, x0, __main____str12@PAGEOFF
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    ldr x9, [x27, #32]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
Lcc_label_f5_16:
    ldr x0, [x27, #32]
    bl _Std_String_strlen_ptr_to_char
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #24]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str13@PAGE
    add x1, x1, __main____str13@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __main____str14@PAGE
    add x9, x9, __main____str14@PAGEOFF
    str x9, [x27, #40]
    ldr x0, [x27, #32]
    ldr x1, [x27, #40]
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_17
    b Lcc_label_f5_18
Lcc_label_f5_17:
    movz w0, #0x1
    adrp x1, __main____str15@PAGE
    add x1, x1, __main____str15@PAGEOFF
    bl __cc_hidden_fn6
    b Lcc_label_f5_19
Lcc_label_f5_18:
    movz w0, #0x1
    adrp x1, __main____str16@PAGE
    add x1, x1, __main____str16@PAGEOFF
    bl __cc_hidden_fn6
Lcc_label_f5_19:
    adrp x0, __main____str17@PAGE
    add x0, x0, __main____str17@PAGEOFF
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    ldr x0, [x27, #0]
    bl _Std_Memory_dealloc_ptr_to_void
    bl _Test_Program_test_overflow
    strh w0, [x27, #1104]
    ldrsh w9, [x27, #1104]
    strh w9, [x27, #48]
    adrp x11, __main____str18@PAGE
    add x11, x11, __main____str18@PAGEOFF
    str x11, [x27, #1104]
    ldrsh w9, [x27, #48]
    sxth w9, w9
    str w9, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    ldrsh w9, [x27, #48]
    sxth w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x9c40
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str19@PAGE
    add x1, x1, __main____str19@PAGEOFF
    bl __cc_hidden_fn6
    bl __cc_hidden_fn8
    strb w0, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x40
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str20@PAGE
    add x1, x1, __main____str20@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str21@PAGE
    add x1, x1, __main____str21@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str22@PAGE
    add x1, x1, __main____str22@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str23@PAGE
    add x1, x1, __main____str23@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str24@PAGE
    add x1, x1, __main____str24@PAGEOFF
    bl __cc_hidden_fn6
    adrp x11, __main____str25@PAGE
    add x11, x11, __main____str25@PAGEOFF
    str x11, [x27, #1104]
    bl _Test_Chance_test_chance
    str w0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    adrp x11, __main____str26@PAGE
    add x11, x11, __main____str26@PAGEOFF
    str x11, [x27, #1104]
    bl _test
    str w0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #56]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #56]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x3
    strb w10, [x9]
    ldr x9, [x27, #56]
    movz w10, #0x2c
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #64]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #64]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x7b
    strb w10, [x9]
    ldr x9, [x27, #64]
    movz w10, #0x14d
    str w10, [x9]
    ldr x9, [x27, #56]
    str x9, [x27, #72]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #80]
    ldr x9, [x27, #72]
    str x9, [x27, #88]
    ldr x9, [x27, #80]
    str x9, [x27, #96]
    ldr x9, [x27, #88]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #104]
    ldr x9, [x27, #96]
    ldr w10, [x27, #104]
    str w10, [x9]
    ldr x9, [x27, #88]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    strb w9, [x27, #112]
    ldr x9, [x27, #96]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x27, #112]
    strb w10, [x9]
    ldr x9, [x27, #80]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str27@PAGE
    add x1, x1, __main____str27@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0x7b
    str w9, [x27, #120]
    add x9, x27, #120
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #128]
    add x9, x27, #128
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldr x10, [x9]
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x7b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str28@PAGE
    add x1, x1, __main____str28@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x10
    bl _Std_Memory_alloc_i32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #136]
    ldr x9, [x27, #136]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #144]
    bl __cc_hidden_fn9
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #152]
    ldr x9, [x27, #152]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #160]
    ldr x9, [x27, #144]
    ldr w10, [x27, #160]
    str w10, [x9]
    ldr x9, [x27, #152]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    strb w9, [x27, #168]
    ldr x9, [x27, #144]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x27, #168]
    strb w10, [x9]
    ldr x9, [x27, #136]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #176]
    ldr x9, [x27, #64]
    str x9, [x27, #184]
    ldr x9, [x27, #184]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #192]
    ldr x9, [x27, #176]
    ldr w10, [x27, #192]
    str w10, [x9]
    ldr x9, [x27, #184]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    strb w9, [x27, #200]
    ldr x9, [x27, #176]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x27, #200]
    strb w10, [x9]
    ldr x9, [x27, #136]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str29@PAGE
    add x1, x1, __main____str29@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #136]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x7b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str30@PAGE
    add x1, x1, __main____str30@PAGEOFF
    bl __cc_hidden_fn6
    ldr x0, [x27, #136]
    bl _Std_Memory_dealloc_ptr_to_void
    ldr x9, [x27, #72]
    str x9, [x27, #208]
    adrp x0, __main____str31@PAGE
    add x0, x0, __main____str31@PAGEOFF
    ldr x1, [x27, #208]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    movz w9, #0x3
    str w9, [x27, #216]
    ldr x9, [x27, #72]
    movz w10, #0x2d
    str w10, [x9]
    ldr x9, [x27, #72]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2d
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str32@PAGE
    add x1, x1, __main____str32@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str33@PAGE
    add x1, x1, __main____str33@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str34@PAGE
    add x1, x1, __main____str34@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, __main____str35@PAGE
    add x0, x0, __main____str35@PAGEOFF
    adrp x1, __main____str36@PAGE
    add x1, x1, __main____str36@PAGEOFF
    movz w2, #0x8
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    bl _differentsig
    bl _differentsig
    movz w0, #0xa
    bl __cc_hidden_fn10
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x1d2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str37@PAGE
    add x1, x1, __main____str37@PAGEOFF
    bl __cc_hidden_fn6
    adrp x11, __main____str38@PAGE
    add x11, x11, __main____str38@PAGEOFF
    str x11, [x27, #1104]
    movz w0, #0x29
    bl _LibTest_test_fun_i32
    str w0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    adrp x11, __main____str39@PAGE
    add x11, x11, __main____str39@PAGEOFF
    str x11, [x27, #1104]
    movz w0, #0x32
    bl _LibTest_Another_other_test_fun_i32
    str w0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    movz w12, #0xf5c3
    movk w12, #0x4048, lsl #16
    fmov s0, w12
    str s0, [x27, #224]
    movz x12, #0xde1b
    movk x12, #0x4283, lsl #16
    movk x12, #0xc0ca, lsl #32
    movk x12, #0x3ff3, lsl #48
    fmov d0, x12
    str d0, [x27, #232]
    add x9, x27, #224
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #240]
    ldr x9, [x27, #240]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w12, #0xf5c3
    movk w12, #0x40c8, lsl #16
    fmov s0, w12
    str s0, [x9]
    movz w0, #0x1
    adrp x1, __main____str40@PAGE
    add x1, x1, __main____str40@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #240]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr s0, [x27, #1104]
    movz w12, #0xf5c3
    movk w12, #0x40c8, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str41@PAGE
    add x1, x1, __main____str41@PAGEOFF
    bl __cc_hidden_fn6
    ldr d0, [x27, #232]
    movz x12, #0xde1b
    movk x12, #0x4283, lsl #16
    movk x12, #0xc0ca, lsl #32
    movk x12, #0x3ff3, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str42@PAGE
    add x1, x1, __main____str42@PAGEOFF
    bl __cc_hidden_fn6
    adrp x11, __main____str43@PAGE
    add x11, x11, __main____str43@PAGEOFF
    str x11, [x27, #1104]
    ldr w9, [x27, #224]
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #1120]
    ldr d0, [x27, #232]
    ldr d1, [x27, #1120]
    fadd d0, d0, d1
    str d0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr d0, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    ldr w9, [x27, #224]
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #1112]
    ldr d0, [x27, #232]
    ldr d1, [x27, #1112]
    fadd d0, d0, d1
    str d0, [x27, #1104]
    ldr d0, [x27, #1104]
    movz x12, #0x45c3
    movk x12, #0xe9ee, lsl #16
    movk x12, #0xeea, lsl #32
    movk x12, #0x401e, lsl #48
    fmov d1, x12
    fsub d0, d0, d1
    str d0, [x27, #1104]
    ldr d0, [x27, #1104]
    str d0, [x27, #248]
    ldr d0, [x27, #248]
    mov x12, xzr
    fmov d1, x12
    fcmp d0, d1
    cset w9, lt
    strb w9, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_20
    b Lcc_label_f5_21
Lcc_label_f5_20:
    mov x12, xzr
    fmov d0, x12
    ldr d1, [x27, #248]
    fsub d0, d0, d1
    str d0, [x27, #1104]
    ldr d0, [x27, #1104]
    str d0, [x27, #248]
Lcc_label_f5_21:
    ldr d0, [x27, #248]
    movz x12, #0xed8d
    movk x12, #0xa0b5, lsl #16
    movk x12, #0xc6f7, lsl #32
    movk x12, #0x3eb0, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, lt
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str44@PAGE
    add x1, x1, __main____str44@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #56]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr x9, [x27, #64]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldrb w10, [x9]
    strb w10, [x27, #1112]
    ldrb w9, [x27, #1112]
    uxtb w9, w9
    str w9, [x27, #1112]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #1112]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0xa8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str45@PAGE
    add x1, x1, __main____str45@PAGEOFF
    bl __cc_hidden_fn6
    movz x12, #0xda72
    movk x12, #0xb8f8, lsl #16
    movk x12, #0x9adb, lsl #32
    movk x12, #0x3fbf, lsl #48
    fmov d0, x12
    str d0, [x27, #256]
    movz x12, #0x999a
    movk x12, #0x9999, lsl #16
    movk x12, #0x9999, lsl #32
    movk x12, #0x3fb9, lsl #48
    fmov d0, x12
    str d0, [x27, #264]
    movz w12, #0xcccd
    movk w12, #0x3dcc, lsl #16
    fmov s0, w12
    str s0, [x27, #272]
    ldr d0, [x27, #264]
    movz x12, #0x999a
    movk x12, #0x9999, lsl #16
    movk x12, #0x9999, lsl #32
    movk x12, #0x3fb9, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_22
    b Lcc_label_f5_23
Lcc_label_f5_22:
    ldr s0, [x27, #272]
    movz w12, #0xcccd
    movk w12, #0x3dcc, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_24
    b Lcc_label_f5_23
Lcc_label_f5_24:
    movz w11, #0x1
    strb w11, [x27, #1104]
    b Lcc_label_f5_25
Lcc_label_f5_23:
Lcc_label_f5_25:
    ldrsb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_26
    b Lcc_label_f5_27
Lcc_label_f5_26:
    ldr d0, [x27, #256]
    movz x12, #0xda72
    movk x12, #0xb8f8, lsl #16
    movk x12, #0x9adb, lsl #32
    movk x12, #0x3fbf, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_28
    b Lcc_label_f5_27
Lcc_label_f5_28:
    movz w11, #0x1
    strb w11, [x27, #1104]
    b Lcc_label_f5_29
Lcc_label_f5_27:
Lcc_label_f5_29:
    ldrsb w0, [x27, #1104]
    adrp x1, __main____str46@PAGE
    add x1, x1, __main____str46@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #56]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr x9, [x27, #64]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldrb w10, [x9]
    strb w10, [x27, #1112]
    ldrb w9, [x27, #1112]
    uxtb w9, w9
    str w9, [x27, #1112]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #1112]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #280]
    ldr w9, [x27, #280]
    movz w10, #0xa8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str47@PAGE
    add x1, x1, __main____str47@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str48@PAGE
    add x1, x1, __main____str48@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str49@PAGE
    add x1, x1, __main____str49@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str50@PAGE
    add x1, x1, __main____str50@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str51@PAGE
    add x1, x1, __main____str51@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #288]
    mov w9, wzr
    str w9, [x27, #296]
Lcc_label_f5_30:
    ldr w9, [x27, #296]
    movz w10, #0xa
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_31
    b Lcc_label_f5_32
Lcc_label_f5_31:
    ldr w9, [x27, #296]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_33
    b Lcc_label_f5_34
Lcc_label_f5_33:
    b Lcc_label_f5_32
Lcc_label_f5_34:
    ldr w9, [x27, #288]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #288]
Lcc_label_f5_35:
    ldr w9, [x27, #296]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #296]
    b Lcc_label_f5_30
Lcc_label_f5_32:
    ldr w9, [x27, #288]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str52@PAGE
    add x1, x1, __main____str52@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #304]
    mov w9, wzr
    str w9, [x27, #312]
Lcc_label_f5_36:
    ldr w9, [x27, #312]
    movz w10, #0x6
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_37
    b Lcc_label_f5_38
Lcc_label_f5_37:
    ldr w9, [x27, #312]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_39
    b Lcc_label_f5_40
Lcc_label_f5_39:
    b Lcc_label_f5_41
Lcc_label_f5_40:
    ldr w9, [x27, #304]
    ldr w10, [x27, #312]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #304]
Lcc_label_f5_41:
    ldr w9, [x27, #312]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #312]
    b Lcc_label_f5_36
Lcc_label_f5_38:
    ldr w9, [x27, #304]
    movz w10, #0xd
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str53@PAGE
    add x1, x1, __main____str53@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #320]
    mov w9, wzr
    str w9, [x27, #328]
Lcc_label_f5_42:
    ldr w9, [x27, #328]
    movz w10, #0x3
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_43
    b Lcc_label_f5_44
Lcc_label_f5_43:
    ldr w9, [x27, #320]
    ldr w10, [x27, #328]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #320]
Lcc_label_f5_45:
    ldr w9, [x27, #328]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #328]
    b Lcc_label_f5_42
Lcc_label_f5_44:
    ldr w9, [x27, #320]
    movz w10, #0x3
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str54@PAGE
    add x1, x1, __main____str54@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #336]
    mov w9, wzr
    str w9, [x27, #344]
Lcc_label_f5_46:
    ldr w9, [x27, #344]
    movz w10, #0x6
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_47
    b Lcc_label_f5_48
Lcc_label_f5_47:
    ldr w9, [x27, #344]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #344]
    ldr w9, [x27, #344]
    movz w10, #0x2
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_49
    b Lcc_label_f5_50
Lcc_label_f5_49:
    b Lcc_label_f5_46
Lcc_label_f5_50:
    ldr w9, [x27, #336]
    ldr w10, [x27, #344]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #336]
    ldr w9, [x27, #344]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_51
    b Lcc_label_f5_52
Lcc_label_f5_51:
    b Lcc_label_f5_48
Lcc_label_f5_52:
    b Lcc_label_f5_46
Lcc_label_f5_48:
    ldr w9, [x27, #336]
    movz w10, #0x9
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str55@PAGE
    add x1, x1, __main____str55@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0x3039
    strh w9, [x27, #352]
    movz w0, #0x11
    bl _Std_Memory_alloc_i32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #360]
    ldr x0, [x27, #360]
    mov w1, wzr
    movz w2, #0x11
    bl _Std_Memory_memset_ptr_to_void_u8_i32
    str x0, [x27, #1104]
    movz w9, #0xf
    str w9, [x27, #368]
Lcc_label_f5_53:
    ldr w9, [x27, #368]
    mov w10, wzr
    cmp w9, w10
    cset w11, ge
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_54
    b Lcc_label_f5_55
Lcc_label_f5_54:
    ldrh w9, [x27, #352]
    ldr w10, [x27, #368]
    lsr w9, w9, w10
    uxth w9, w9
    strh w9, [x27, #1104]
    ldrh w9, [x27, #1104]
    movz w10, #0x1
    and w9, w9, w10
    uxth w9, w9
    strh w9, [x27, #1104]
    ldrh w9, [x27, #1104]
    strh w9, [x27, #376]
    ldr x9, [x27, #360]
    str x9, [x27, #1104]
    movz w9, #0xf
    ldr w10, [x27, #368]
    sub w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    sxtw x9, w9
    str x9, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldrh w9, [x27, #376]
    uxth w9, w9
    str w9, [x27, #1112]
    ldr w9, [x27, #1112]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1112]
    ldrb w9, [x27, #1112]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1112]
    ldrb w9, [x27, #1112]
    cmp w9, wzr
    b.ne Lcc_label_f5_56
    b Lcc_label_f5_57
Lcc_label_f5_56:
    movz w9, #0x31
    strb w9, [x27, #384]
    b Lcc_label_f5_58
Lcc_label_f5_57:
    movz w9, #0x30
    strb w9, [x27, #384]
Lcc_label_f5_58:
    ldr x9, [x27, #1104]
    ldrsb w10, [x27, #384]
    strb w10, [x9]
Lcc_label_f5_59:
    ldr w9, [x27, #368]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #368]
    b Lcc_label_f5_53
Lcc_label_f5_55:
    ldr x0, [x27, #360]
    adrp x1, __main____str56@PAGE
    add x1, x1, __main____str56@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str57@PAGE
    add x1, x1, __main____str57@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str58@PAGE
    add x1, x1, __main____str58@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str59@PAGE
    add x1, x1, __main____str59@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str60@PAGE
    add x1, x1, __main____str60@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str61@PAGE
    add x1, x1, __main____str61@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str62@PAGE
    add x1, x1, __main____str62@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str63@PAGE
    add x1, x1, __main____str63@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str64@PAGE
    add x1, x1, __main____str64@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str65@PAGE
    add x1, x1, __main____str65@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str66@PAGE
    add x1, x1, __main____str66@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str67@PAGE
    add x1, x1, __main____str67@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str68@PAGE
    add x1, x1, __main____str68@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, __main____str69@PAGE
    add x0, x0, __main____str69@PAGEOFF
    adrp x1, __main____str70@PAGE
    add x1, x1, __main____str70@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str71@PAGE
    add x1, x1, __main____str71@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str72@PAGE
    add x1, x1, __main____str72@PAGEOFF
    bl __cc_hidden_fn6
    b Lcc_label_f5_60
Lcc_label_f5_60:
    movz w0, #0x1
    adrp x1, __main____str73@PAGE
    add x1, x1, __main____str73@PAGEOFF
    bl __cc_hidden_fn6
    b Lcc_label_f5_61
Lcc_label_f5_62:
    mov w0, wzr
    adrp x1, __main____str74@PAGE
    add x1, x1, __main____str74@PAGEOFF
    bl __cc_hidden_fn6
Lcc_label_f5_61:
    movz w0, #0x1
    adrp x1, __main____str75@PAGE
    add x1, x1, __main____str75@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __main____str76@PAGE
    add x9, x9, __main____str76@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrsb w10, [x9]
    strb w10, [x27, #1104]
    ldrsb w9, [x27, #1104]
    sxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str77@PAGE
    add x1, x1, __main____str77@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, __main____str78@PAGE
    add x0, x0, __main____str78@PAGEOFF
    adrp x1, __main____str79@PAGE
    add x1, x1, __main____str79@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str80@PAGE
    add x1, x1, __main____str80@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str81@PAGE
    add x1, x1, __main____str81@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __main____str82@PAGE
    add x9, x9, __main____str82@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrsb w10, [x9]
    strb w10, [x27, #1104]
    ldrsb w9, [x27, #1104]
    sxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str83@PAGE
    add x1, x1, __main____str83@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __main____str84@PAGE
    add x9, x9, __main____str84@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrsb w10, [x9]
    strb w10, [x27, #1104]
    ldrsb w9, [x27, #1104]
    sxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str85@PAGE
    add x1, x1, __main____str85@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #392]
    movz w11, #0x1
    str w11, [x27, #1104]
    ldr w9, [x27, #392]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #1112]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #1112]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str86@PAGE
    add x1, x1, __main____str86@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, __main____str87@PAGE
    add x0, x0, __main____str87@PAGEOFF
    adrp x1, __main____str88@PAGE
    add x1, x1, __main____str88@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str89@PAGE
    add x1, x1, __main____str89@PAGEOFF
    bl __cc_hidden_fn6
    adrp x0, __main____str90@PAGE
    add x0, x0, __main____str90@PAGEOFF
    adrp x1, __main____str91@PAGE
    add x1, x1, __main____str91@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str92@PAGE
    add x1, x1, __main____str92@PAGEOFF
    bl __cc_hidden_fn6
    bl _LibTest_Another_get_info
    str x0, [x27, #1104]
    ldr x0, [x27, #1104]
    adrp x1, __main____str93@PAGE
    add x1, x1, __main____str93@PAGEOFF
    bl __cc_hidden_fn5
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str94@PAGE
    add x1, x1, __main____str94@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x5
    movz w1, #0x3039
    movz w2, #0xa
    movz w3, #0x14
    movz w4, #0x1e
    movz w5, #0x28
    movz w6, #0x32
    sub sp, sp, #48
    sxtw x2, w2
    str x2, [sp, #0]
    sxtw x3, w3
    str x3, [sp, #8]
    sxtw x4, w4
    str x4, [sp, #16]
    sxtw x5, w5
    str x5, [sp, #24]
    sxtw x6, w6
    str x6, [sp, #32]
    bl _LibTest_Another_test_varargs_i32_i32
    add sp, sp, #48
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x96
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str95@PAGE
    add x1, x1, __main____str95@PAGEOFF
    bl __cc_hidden_fn6
    movz w11, #0xa
    str w11, [x27, #1104]
    movz w11, #0x4d
    str w11, [x27, #1112]
    movz w9, #0xf5c3
    movk w9, #0x4048, lsl #16
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #1120]
    ldr w0, [x27, #1104]
    ldr w1, [x27, #1112]
    ldr d0, [x27, #1120]
    fmov x2, d0
    adrp x3, __main____str96@PAGE
    add x3, x3, __main____str96@PAGEOFF
    sub sp, sp, #32
    sxtw x1, w1
    str x1, [sp, #0]
    str x2, [sp, #8]
    str x3, [sp, #16]
    bl _LibTest_Another_test_diffvarargs_i32
    add sp, sp, #32
    mov w0, wzr
    movz w1, #0x58
    movz x12, #0xf790
    movk x12, #0x95aa, lsl #16
    movk x12, #0xbf09, lsl #32
    movk x12, #0x4005, lsl #48
    fmov d0, x12
    fmov x2, d0
    adrp x3, __main____str97@PAGE
    add x3, x3, __main____str97@PAGEOFF
    sub sp, sp, #32
    sxtw x1, w1
    str x1, [sp, #0]
    str x2, [sp, #8]
    str x3, [sp, #16]
    bl _Test_Program_internal_diffvarargs_i32
    add sp, sp, #32
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #400]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0xa
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x14
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x1e
    str w10, [x9]
    ldr x9, [x27, #400]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr x9, [x27, #400]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldr w10, [x9]
    str w10, [x27, #1112]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #1112]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr x9, [x27, #400]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldr w10, [x9]
    str w10, [x27, #1112]
    ldr w9, [x27, #1104]
    ldr w10, [x27, #1112]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str98@PAGE
    add x1, x1, __main____str98@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str99@PAGE
    add x1, x1, __main____str99@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x20
    bl _Std_Memory_alloc_i32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #408]
    ldr x9, [x27, #408]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x64
    str x10, [x9]
    ldr x9, [x27, #408]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0xc8
    str x10, [x9]
    ldr x9, [x27, #408]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x10
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x12c
    str x10, [x9]
    ldr x9, [x27, #408]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x18
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x190
    str x10, [x9]
    ldr x9, [x27, #408]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldr x10, [x9]
    str x10, [x27, #1104]
    ldr x9, [x27, #408]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldr x10, [x9]
    str x10, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #408]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x10
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldr x10, [x9]
    str x10, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #408]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x18
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    ldr x10, [x9]
    str x10, [x27, #1112]
    ldr x9, [x27, #1104]
    ldr x10, [x27, #1112]
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x3e8
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str100@PAGE
    add x1, x1, __main____str100@PAGEOFF
    bl __cc_hidden_fn6
    ldr x0, [x27, #408]
    bl _Std_Memory_dealloc_ptr_to_void
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #416]
    ldr x9, [x27, #416]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #416]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #416]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x9
    strb w10, [x9]
    ldr x9, [x27, #416]
    movz w10, #0x51
    str w10, [x9]
    ldr x0, [x27, #416]
    bl __cc_hidden_fn11
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str101@PAGE
    add x1, x1, __main____str101@PAGEOFF
    bl __cc_hidden_fn6
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #424]
    ldr x0, [x27, #416]
    bl __cc_hidden_fn12
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #432]
    ldr x9, [x27, #424]
    str x9, [x27, #440]
    ldr x9, [x27, #432]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #448]
    ldr x9, [x27, #440]
    ldr w10, [x27, #448]
    str w10, [x9]
    ldr x9, [x27, #432]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    strb w9, [x27, #456]
    ldr x9, [x27, #440]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x27, #456]
    strb w10, [x9]
    ldr x9, [x27, #424]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x9
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_63
    b Lcc_label_f5_64
Lcc_label_f5_63:
    ldr x9, [x27, #424]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_65
    b Lcc_label_f5_64
Lcc_label_f5_65:
    movz w11, #0x1
    strb w11, [x27, #1104]
    b Lcc_label_f5_66
Lcc_label_f5_64:
Lcc_label_f5_66:
    ldrsb w0, [x27, #1104]
    adrp x1, __main____str102@PAGE
    add x1, x1, __main____str102@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __cc_hidden_fn13@PAGE
    add x9, x9, __cc_hidden_fn13@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #464]
    adrp x9, __cc_hidden_fn13@PAGE
    add x9, x9, __cc_hidden_fn13@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #472]
    movz w0, #0xf
    movz w1, #0x1b
    ldr x16, [x27, #464]
    blr x16
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str103@PAGE
    add x1, x1, __main____str103@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x14
    movz w1, #0x16
    ldr x16, [x27, #472]
    blr x16
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str104@PAGE
    add x1, x1, __main____str104@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #464]
    str x9, [x27, #480]
    ldr x9, [x27, #480]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str105@PAGE
    add x1, x1, __main____str105@PAGEOFF
    bl __cc_hidden_fn6
    ldr x9, [x27, #480]
    str x9, [x27, #488]
    movz w0, #0x7
    movz w1, #0x23
    ldr x16, [x27, #488]
    blr x16
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str106@PAGE
    add x1, x1, __main____str106@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __cc_hidden_fn11@PAGE
    add x9, x9, __cc_hidden_fn11@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #496]
    ldr x0, [x27, #416]
    ldr x16, [x27, #496]
    blr x16
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str107@PAGE
    add x1, x1, __main____str107@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0xc
    str w9, [x27, #504]
    ldr w9, [x27, #504]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_67
    b Lcc_label_f5_68
Lcc_label_f5_67:
    mov w9, wzr
    str w9, [x27, #512]
    b Lcc_label_f5_69
Lcc_label_f5_68:
    ldr w0, [x27, #504]
    bl __cc_hidden_fn3
    str w0, [x27, #1112]
    ldr w9, [x27, #504]
    ldr w10, [x27, #1112]
    mul w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #512]
Lcc_label_f5_69:
    ldr w9, [x27, #512]
    movz w10, #0x9c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str108@PAGE
    add x1, x1, __main____str108@PAGEOFF
    bl __cc_hidden_fn6
    mov w9, wzr
    str w9, [x27, #520]
    ldr w9, [x27, #520]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_70
    b Lcc_label_f5_71
Lcc_label_f5_70:
    mov w9, wzr
    str w9, [x27, #528]
    b Lcc_label_f5_72
Lcc_label_f5_71:
    ldr w0, [x27, #520]
    bl __cc_hidden_fn3
    str w0, [x27, #1112]
    ldr w9, [x27, #520]
    ldr w10, [x27, #1112]
    mul w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #528]
Lcc_label_f5_72:
    ldr w9, [x27, #528]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str109@PAGE
    add x1, x1, __main____str109@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __cc_hidden_fn3@PAGE
    add x9, x9, __cc_hidden_fn3@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #536]
    adrp x11, __main____str110@PAGE
    add x11, x11, __main____str110@PAGEOFF
    str x11, [x27, #1104]
    movz w0, #0x5
    ldr x16, [x27, #536]
    blr x16
    str w0, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    adrp x0, __main____str111@PAGE
    add x0, x0, __main____str111@PAGEOFF
    adrp x1, __main____str112@PAGE
    add x1, x1, __main____str112@PAGEOFF
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #544]
    ldr x9, [x27, #544]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #544]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #544]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x1
    strb w10, [x9]
    ldr x9, [x27, #544]
    movz w10, #0xa
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #552]
    ldr x9, [x27, #552]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #552]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #552]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz w10, #0x2
    strb w10, [x9]
    ldr x9, [x27, #552]
    movz w10, #0x14
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #560]
    ldr x9, [x27, #560]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #560]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #560]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz w10, #0x3
    strb w10, [x9]
    ldr x9, [x27, #560]
    movz w10, #0x1e
    str w10, [x9]
    ldr x0, [x27, #544]
    ldr x1, [x27, #552]
    ldr x2, [x27, #560]
    bl _LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str113@PAGE
    add x1, x1, __main____str113@PAGEOFF
    bl __cc_hidden_fn6
    movz w11, #0x3
    str w11, [x27, #1104]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #576]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #576]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #576]
    movz w10, #0xb
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #584]
    ldr x9, [x27, #584]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #584]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #584]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #584]
    movz w10, #0x16
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #592]
    ldr x9, [x27, #592]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #592]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #592]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #592]
    movz w10, #0x21
    str w10, [x9]
    ldr w0, [x27, #1104]
    ldr x1, [x27, #576]
    ldr x2, [x27, #584]
    ldr x3, [x27, #592]
    sub sp, sp, #32
    str x1, [sp, #0]
    str x2, [sp, #8]
    str x3, [sp, #16]
    bl _Test_Program_structvarargs_i32
    add sp, sp, #32
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #568]
    adrp x0, __main____str114@PAGE
    add x0, x0, __main____str114@PAGEOFF
    ldr w1, [x27, #568]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #1104]
    ldr w9, [x27, #568]
    movz w10, #0x42
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str115@PAGE
    add x1, x1, __main____str115@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0x22b
    str w9, [x27, #600]
    ldr w9, [x27, #600]
    movz w10, #0x22b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str116@PAGE
    add x1, x1, __main____str116@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x2d
    bl _Test_Program_use_constant_i32
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x91
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str117@PAGE
    add x1, x1, __main____str117@PAGEOFF
    bl __cc_hidden_fn6
    ldr w0, [x27, #600]
    bl _Test_Program_use_constant_i32
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x28f
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str118@PAGE
    add x1, x1, __main____str118@PAGEOFF
    bl __cc_hidden_fn6
    ldr w0, [x27, #600]
    bl _Test_Program_constant_test_i32
    ldr w9, [x27, #600]
    movz w10, #0x22b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str119@PAGE
    add x1, x1, __main____str119@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0xa
    bl _Std_Memory_alloc_i32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #608]
    ldr x0, [x27, #608]
    bl _Test_Program_constant_ptr_ptr_to_u8
    ldr x9, [x27, #608]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str120@PAGE
    add x1, x1, __main____str120@PAGEOFF
    bl __cc_hidden_fn6
    bl __cc_hidden_fn1
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #616]
    movz w9, #0x8
    str w9, [x27, #624]
    ldr w9, [x27, #616]
    ldr w10, [x27, #624]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str121@PAGE
    add x1, x1, __main____str121@PAGEOFF
    bl __cc_hidden_fn6
    bl __cc_hidden_fn2
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #632]
    movz w9, #0x8
    str w9, [x27, #640]
    ldr w9, [x27, #632]
    ldr w10, [x27, #640]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str122@PAGE
    add x1, x1, __main____str122@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #640]
    ldr w10, [x27, #624]
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str123@PAGE
    add x1, x1, __main____str123@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str124@PAGE
    add x1, x1, __main____str124@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str125@PAGE
    add x1, x1, __main____str125@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str126@PAGE
    add x1, x1, __main____str126@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str127@PAGE
    add x1, x1, __main____str127@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str128@PAGE
    add x1, x1, __main____str128@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    adrp x1, __main____str129@PAGE
    add x1, x1, __main____str129@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0xa
    movz w1, #0x14
    bl _default_max__inst__T0_i32
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x14
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str130@PAGE
    add x1, x1, __main____str130@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0xfffb
    movk w0, #0xffff, lsl #16
    movz w1, #0xfffe
    movk w1, #0xffff, lsl #16
    bl __cc_hidden_fn14
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0xfffe
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str131@PAGE
    add x1, x1, __main____str131@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0xf
    movz w1, #0x1b
    bl __cc_hidden_fn15
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str132@PAGE
    add x1, x1, __main____str132@PAGEOFF
    bl __cc_hidden_fn6
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #648]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x6
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0xfffe
    movk w10, #0xffff, lsl #16
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x4
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x6
    strh w10, [x9]
    ldr x9, [x27, #648]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x6
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x8
    strh w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #656]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x3
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0xa
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x14
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x1e
    strb w10, [x9]
    ldr x9, [x27, #656]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x3
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x28
    strb w10, [x9]
    ldr x0, [x27, #648]
    ldr x1, [x27, #656]
    movz w2, #0x4
    bl __cc_hidden_fn16
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x230
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str133@PAGE
    add x1, x1, __main____str133@PAGEOFF
    bl __cc_hidden_fn6
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #664]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x1
    strb w10, [x9]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x2
    strb w10, [x9]
    ldr x9, [x27, #664]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x3
    strb w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #672]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    mov x10, xzr
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x9
    strb w10, [x9]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x1
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x8
    strb w10, [x9]
    ldr x9, [x27, #672]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x2
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz w10, #0x7
    strb w10, [x9]
    ldr x0, [x27, #664]
    ldr x1, [x27, #672]
    movz w2, #0x3
    bl __cc_hidden_fn17
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2e
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str134@PAGE
    add x1, x1, __main____str134@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0x7
    str w9, [x27, #680]
    movz w9, #0xff9c
    movk w9, #0xffff, lsl #16
    str w9, [x27, #688]
    ldr w9, [x27, #680]
    str w9, [x27, #696]
    ldr w9, [x27, #696]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_73
    b Lcc_label_f5_74
Lcc_label_f5_74:
    ldr w9, [x27, #696]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_75
    b Lcc_label_f5_76
Lcc_label_f5_76:
    ldr w9, [x27, #696]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_77
    b Lcc_label_f5_78
Lcc_label_f5_78:
    ldr w9, [x27, #696]
    movz w10, #0x3
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_79
    b Lcc_label_f5_80
Lcc_label_f5_73:
    mov w9, wzr
    str w9, [x27, #688]
    b Lcc_label_f5_81
Lcc_label_f5_75:
Lcc_label_f5_77:
    movz w9, #0xc
    str w9, [x27, #688]
    b Lcc_label_f5_81
Lcc_label_f5_79:
    movz w9, #0x21
    str w9, [x27, #688]
    b Lcc_label_f5_81
Lcc_label_f5_80:
    movz w9, #0x2c
    str w9, [x27, #688]
    b Lcc_label_f5_81
Lcc_label_f5_81:
    ldr w9, [x27, #688]
    movz w10, #0x2c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str135@PAGE
    add x1, x1, __main____str135@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0xffff
    movk w9, #0xffff, lsl #16
    str w9, [x27, #704]
    movz w9, #0x1
    str w9, [x27, #712]
    ldr w9, [x27, #712]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_82
    b Lcc_label_f5_83
Lcc_label_f5_83:
    ldr w9, [x27, #712]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_84
    b Lcc_label_f5_85
Lcc_label_f5_85:
    ldr w9, [x27, #712]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_86
    b Lcc_label_f5_87
Lcc_label_f5_82:
    mov w9, wzr
    str w9, [x27, #704]
    b Lcc_label_f5_88
Lcc_label_f5_84:
    movz w9, #0xa
    str w9, [x27, #704]
Lcc_label_f5_86:
    movz w9, #0x14
    str w9, [x27, #704]
    b Lcc_label_f5_88
Lcc_label_f5_87:
    movz w9, #0xffce
    movk w9, #0xffff, lsl #16
    str w9, [x27, #704]
    b Lcc_label_f5_88
Lcc_label_f5_88:
    ldr w9, [x27, #704]
    movz w10, #0x14
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str136@PAGE
    add x1, x1, __main____str136@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0xfffb
    movk w9, #0xffff, lsl #16
    str w9, [x27, #720]
    ldr w9, [x27, #720]
    str w9, [x27, #728]
    ldr w9, [x27, #728]
    movz w10, #0xfff6
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_89
    b Lcc_label_f5_90
Lcc_label_f5_90:
    ldr w9, [x27, #728]
    movz w10, #0xfffb
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_91
    b Lcc_label_f5_92
Lcc_label_f5_92:
    ldr w9, [x27, #728]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_93
    b Lcc_label_f5_94
Lcc_label_f5_89:
    movz w9, #0x3e8
    str w9, [x27, #736]
    b Lcc_label_f5_95
Lcc_label_f5_91:
    movz w9, #0x1f4
    str w9, [x27, #736]
    b Lcc_label_f5_95
Lcc_label_f5_93:
    mov w9, wzr
    str w9, [x27, #736]
    b Lcc_label_f5_95
Lcc_label_f5_94:
    movz w9, #0x63
    str w9, [x27, #736]
    b Lcc_label_f5_95
Lcc_label_f5_95:
    ldr w9, [x27, #736]
    movz w10, #0x1f4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str137@PAGE
    add x1, x1, __main____str137@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0x7b
    str w9, [x27, #720]
    ldr w9, [x27, #720]
    str w9, [x27, #744]
    ldr w9, [x27, #744]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_96
    b Lcc_label_f5_97
Lcc_label_f5_97:
    ldr w9, [x27, #744]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_98
    b Lcc_label_f5_99
Lcc_label_f5_96:
    mov w9, wzr
    str w9, [x27, #752]
    b Lcc_label_f5_100
Lcc_label_f5_98:
    movz w9, #0x1
    str w9, [x27, #752]
    b Lcc_label_f5_100
Lcc_label_f5_99:
    ldr w9, [x27, #720]
    movz w10, #0x3
    sdiv w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #752]
    b Lcc_label_f5_100
Lcc_label_f5_100:
    ldr w9, [x27, #752]
    movz w10, #0x29
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str138@PAGE
    add x1, x1, __main____str138@PAGEOFF
    bl __cc_hidden_fn6
    movz x12, #0x400c, lsl #48
    fmov d0, x12
    movz x12, #0x4004, lsl #48
    fmov d1, x12
    bl __cc_hidden_fn18
    str x0, [x27, #1104]
    ldr d0, [x27, #1104]
    movz x12, #0x4018, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str139@PAGE
    add x1, x1, __main____str139@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0xa
    movz w1, #0x20
    bl __cc_hidden_fn19
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str140@PAGE
    add x1, x1, __main____str140@PAGEOFF
    bl __cc_hidden_fn6
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #760]
    movz w11, #0x3
    str w11, [x27, #1104]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #784]
    ldr x9, [x27, #784]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #784]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #784]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    str x9, [x27, #1112]
    ldr x9, [x27, #1112]
    movz w10, #0x1
    strb w10, [x9]
    ldr x9, [x27, #784]
    movz w10, #0xa
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #792]
    ldr x9, [x27, #792]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #792]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #792]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    str x9, [x27, #1120]
    ldr x9, [x27, #1120]
    movz w10, #0x2
    strb w10, [x9]
    ldr x9, [x27, #792]
    movz w10, #0x14
    str w10, [x9]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #800]
    ldr x9, [x27, #800]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #800]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #800]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    str x9, [x27, #1128]
    ldr x9, [x27, #1128]
    movz w10, #0x3
    strb w10, [x9]
    ldr x9, [x27, #800]
    movz w10, #0x1e
    str w10, [x9]
    ldr w0, [x27, #1104]
    ldr x1, [x27, #784]
    ldr x2, [x27, #792]
    ldr x3, [x27, #800]
    sub sp, sp, #32
    str x1, [sp, #0]
    str x2, [sp, #8]
    str x3, [sp, #16]
    bl __cc_hidden_fn20
    add sp, sp, #32
    str x0, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #768]
    ldr x9, [x27, #760]
    str x9, [x27, #776]
    ldr x9, [x27, #768]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #808]
    ldr x9, [x27, #776]
    ldr w10, [x27, #808]
    str w10, [x9]
    ldr x9, [x27, #768]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    strb w9, [x27, #816]
    ldr x9, [x27, #776]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x27, #816]
    strb w10, [x9]
    ldr x9, [x27, #760]
    ldr w10, [x9]
    str w10, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_101
    b Lcc_label_f5_102
Lcc_label_f5_101:
    ldr x9, [x27, #760]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    ldrb w10, [x9]
    strb w10, [x27, #1104]
    ldrb w9, [x27, #1104]
    uxtb w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x6
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #1104]
    ldrb w9, [x27, #1104]
    cmp w9, wzr
    b.ne Lcc_label_f5_103
    b Lcc_label_f5_102
Lcc_label_f5_103:
    movz w11, #0x1
    strb w11, [x27, #1104]
    b Lcc_label_f5_104
Lcc_label_f5_102:
Lcc_label_f5_104:
    ldrsb w0, [x27, #1104]
    adrp x1, __main____str141@PAGE
    add x1, x1, __main____str141@PAGEOFF
    bl __cc_hidden_fn6
    movz w9, #0xa
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    str w9, [x27, #832]
    movz w9, #0x5
    str w9, [x27, #840]
    ldr w9, [x27, #832]
    ldr w10, [x27, #840]
    add w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0xf
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str142@PAGE
    add x1, x1, __main____str142@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #848]
    movz w9, #0x3
    str w9, [x27, #856]
    ldr w9, [x27, #848]
    ldr w10, [x27, #856]
    sub w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0xc
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str143@PAGE
    add x1, x1, __main____str143@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #864]
    movz w9, #0x2
    str w9, [x27, #872]
    ldr w9, [x27, #864]
    ldr w10, [x27, #872]
    mul w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x18
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str144@PAGE
    add x1, x1, __main____str144@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #880]
    movz w9, #0x4
    str w9, [x27, #888]
    ldr w9, [x27, #880]
    ldr w10, [x27, #888]
    sdiv w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x6
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str145@PAGE
    add x1, x1, __main____str145@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #896]
    movz w9, #0x4
    str w9, [x27, #904]
    ldr w9, [x27, #896]
    ldr w10, [x27, #904]
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str146@PAGE
    add x1, x1, __main____str146@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #912]
    movz w9, #0x2
    str w9, [x27, #920]
    ldr w9, [x27, #912]
    ldr w10, [x27, #920]
    lsl w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str147@PAGE
    add x1, x1, __main____str147@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #928]
    movz w9, #0x1
    str w9, [x27, #936]
    ldr w9, [x27, #928]
    ldr w10, [x27, #936]
    asr w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str148@PAGE
    add x1, x1, __main____str148@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #944]
    movz w9, #0x6
    str w9, [x27, #952]
    ldr w9, [x27, #944]
    ldr w10, [x27, #952]
    and w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str149@PAGE
    add x1, x1, __main____str149@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #960]
    movz w9, #0x3
    str w9, [x27, #968]
    ldr w9, [x27, #960]
    ldr w10, [x27, #968]
    orr w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #824]
    ldr w9, [x27, #824]
    movz w10, #0x7
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str150@PAGE
    add x1, x1, __main____str150@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #824]
    str w9, [x27, #976]
    movz w9, #0x5
    str w9, [x27, #984]
    ldr w9, [x27, #976]
    ldr w10, [x27, #984]
    eor w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str151@PAGE
    add x1, x1, __main____str151@PAGEOFF
    bl __cc_hidden_fn6
    movz w12, #0x3fc0, lsl #16
    fmov s0, w12
    str s0, [x27, #992]
    ldr s0, [x27, #992]
    str s0, [x27, #1000]
    movz w12, #0x4020, lsl #16
    fmov s0, w12
    str s0, [x27, #1008]
    ldr s0, [x27, #1000]
    ldr s1, [x27, #1008]
    fadd s0, s0, s1
    str s0, [x27, #1104]
    ldr s0, [x27, #1104]
    str s0, [x27, #992]
    ldr s0, [x27, #992]
    movz w12, #0x4080, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str152@PAGE
    add x1, x1, __main____str152@PAGEOFF
    bl __cc_hidden_fn6
    ldr s0, [x27, #992]
    str s0, [x27, #1016]
    movz w12, #0x4000, lsl #16
    fmov s0, w12
    str s0, [x27, #1024]
    ldr s0, [x27, #1016]
    ldr s1, [x27, #1024]
    fmul s0, s0, s1
    str s0, [x27, #1104]
    ldr s0, [x27, #1104]
    str s0, [x27, #992]
    ldr s0, [x27, #992]
    movz w12, #0x4100, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str153@PAGE
    add x1, x1, __main____str153@PAGEOFF
    bl __cc_hidden_fn6
    ldr s0, [x27, #992]
    str s0, [x27, #1032]
    movz w12, #0x4080, lsl #16
    fmov s0, w12
    str s0, [x27, #1040]
    ldr s0, [x27, #1032]
    ldr s1, [x27, #1040]
    fdiv s0, s0, s1
    str s0, [x27, #1104]
    ldr s0, [x27, #1104]
    str s0, [x27, #992]
    ldr s0, [x27, #992]
    movz w12, #0x4000, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str154@PAGE
    add x1, x1, __main____str154@PAGEOFF
    bl __cc_hidden_fn6
    ldr s0, [x27, #992]
    str s0, [x27, #1048]
    movz w12, #0x3f80, lsl #16
    fmov s0, w12
    str s0, [x27, #1056]
    ldr s0, [x27, #1048]
    ldr s1, [x27, #1056]
    fsub s0, s0, s1
    str s0, [x27, #1104]
    ldr s0, [x27, #1104]
    str s0, [x27, #992]
    ldr s0, [x27, #992]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str155@PAGE
    add x1, x1, __main____str155@PAGEOFF
    bl __cc_hidden_fn6
    ldr s0, [x27, #992]
    str s0, [x27, #1064]
    movz w12, #0x4040, lsl #16
    fmov s0, w12
    str s0, [x27, #1072]
    ldr s0, [x27, #1064]
    ldr s1, [x27, #1072]
    fadd s0, s0, s1
    str s0, [x27, #1104]
    ldr s0, [x27, #1104]
    movz w12, #0x4080, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str156@PAGE
    add x1, x1, __main____str156@PAGEOFF
    bl __cc_hidden_fn6
    movz w0, #0x1
    movz w1, #0x2
    movz w2, #0x3
    movz w3, #0x4
    movz w4, #0x5
    movz w5, #0x6
    movz w6, #0x7
    movz w7, #0x8
    sub sp, sp, #144
    movz w9, #0x9
    str w9, [sp, #0]
    movz w9, #0xa
    str w9, [sp, #8]
    movz w9, #0xb
    str w9, [sp, #16]
    movz w9, #0xc
    str w9, [sp, #24]
    movz w9, #0xd
    str w9, [sp, #32]
    movz w9, #0xe
    str w9, [sp, #40]
    movz w9, #0xf
    str w9, [sp, #48]
    movz w9, #0x10
    str w9, [sp, #56]
    movz w9, #0x11
    str w9, [sp, #64]
    movz w9, #0x12
    str w9, [sp, #72]
    movz w9, #0x13
    str w9, [sp, #80]
    movz w9, #0x14
    str w9, [sp, #88]
    movz w9, #0x15
    str w9, [sp, #96]
    movz w9, #0x16
    str w9, [sp, #104]
    movz w9, #0x17
    str w9, [sp, #112]
    movz w9, #0x18
    str w9, [sp, #120]
    movz w9, #0x19
    str w9, [sp, #128]
    movz w9, #0x1a
    str w9, [sp, #136]
    bl __cc_hidden_fn21
    add sp, sp, #144
    str w0, [x27, #1104]
    ldr w9, [x27, #1104]
    str w9, [x27, #1080]
    ldr w9, [x27, #1080]
    movz w10, #0x9180, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str157@PAGE
    add x1, x1, __main____str157@PAGEOFF
    bl __cc_hidden_fn6
    ldr w9, [x27, #1080]
    movz w10, #0x14
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0xfff8
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str158@PAGE
    add x1, x1, __main____str158@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __cc_hidden_fn22@PAGE
    add x9, x9, __cc_hidden_fn22@PAGEOFF
    str x9, [x27, #1104]
    ldr x16, [x27, #1104]
    blr x16
    adrp x9, __cc_hidden_fn22@PAGE
    add x9, x9, __cc_hidden_fn22@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1088]
    ldr x16, [x27, #1088]
    blr x16
    adrp x9, __cc_hidden_fn23@PAGE
    add x9, x9, __cc_hidden_fn23@PAGEOFF
    str x9, [x27, #1104]
    ldr x9, [x27, #1104]
    str x9, [x27, #1096]
    movz w0, #0x29
    ldr x16, [x27, #1096]
    blr x16
    strh w0, [x27, #1104]
    ldrsh w9, [x27, #1104]
    sxth w9, w9
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #1104]
    ldrb w0, [x27, #1104]
    adrp x1, __main____str159@PAGE
    add x1, x1, __main____str159@PAGEOFF
    bl __cc_hidden_fn6
    adrp x9, __cc_hidden_fn24@PAGE
    add x9, x9, __cc_hidden_fn24@PAGEOFF
    str x9, [x27, #1104]
    adrp x9, __cc_hidden_fn25@PAGE
    add x9, x9, __cc_hidden_fn25@PAGEOFF
    str x9, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr x1, [x27, #1112]
    bl __cc_hidden_fn26
    ldr x0, [x27, #1088]
    ldr x1, [x27, #1096]
    bl __cc_hidden_fn26
    adrp x11, __main____str160@PAGE
    add x11, x11, __main____str160@PAGEOFF
    str x11, [x27, #1104]
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x9]
    str w10, [x27, #1112]
    ldr x0, [x27, #1104]
    ldr w1, [x27, #1112]
    bl _Std_IO_printnl_ptr_to_char
    str w0, [x27, #1104]
    ldr w9, [x27, #280]
    movz w10, #0xa8
    sdiv w9, w9, w10
    str w9, [x27, #1104]
    ldr w9, [x27, #1104]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #1104]
    ldr w0, [x27, #1104]
    mov sp, x27
    add sp, sp, #3072
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn26:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    ldr x16, [x27, #0]
    blr x16
    movz w0, #0x2a
    ldr x16, [x27, #8]
    blr x16
    strh w0, [x27, #24]
    ldrsh w9, [x27, #24]
    strh w9, [x27, #16]
    adrp x11, ___cc_hidden_fn26____str161@PAGE
    add x11, x11, ___cc_hidden_fn26____str161@PAGEOFF
    str x11, [x27, #24]
    ldrsh w9, [x27, #16]
    sxth w9, w9
    str w9, [x27, #32]
    ldr x0, [x27, #24]
    ldr w1, [x27, #32]
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #24]
    mov w0, wzr
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn21:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #224
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    str w2, [x27, #16]
    str w3, [x27, #24]
    str w4, [x27, #32]
    str w5, [x27, #40]
    str w6, [x27, #48]
    str w7, [x27, #56]
    ldr w9, [x29, #16]
    str w9, [x27, #64]
    ldr w9, [x29, #24]
    str w9, [x27, #72]
    ldr w9, [x29, #32]
    str w9, [x27, #80]
    ldr w9, [x29, #40]
    str w9, [x27, #88]
    ldr w9, [x29, #48]
    str w9, [x27, #96]
    ldr w9, [x29, #56]
    str w9, [x27, #104]
    ldr w9, [x29, #64]
    str w9, [x27, #112]
    ldr w9, [x29, #72]
    str w9, [x27, #120]
    ldr w9, [x29, #80]
    str w9, [x27, #128]
    ldr w9, [x29, #88]
    str w9, [x27, #136]
    ldr w9, [x29, #96]
    str w9, [x27, #144]
    ldr w9, [x29, #104]
    str w9, [x27, #152]
    ldr w9, [x29, #112]
    str w9, [x27, #160]
    ldr w9, [x29, #120]
    str w9, [x27, #168]
    ldr w9, [x29, #128]
    str w9, [x27, #176]
    ldr w9, [x29, #136]
    str w9, [x27, #184]
    ldr w9, [x29, #144]
    str w9, [x27, #192]
    ldr w9, [x29, #152]
    str w9, [x27, #200]
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #16]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #24]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #32]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #40]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #48]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #56]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #64]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #72]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #80]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #88]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #96]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #104]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #112]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #120]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #128]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #136]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #144]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #152]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #160]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #168]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #176]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #184]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #192]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w9, [x27, #208]
    ldr w10, [x27, #200]
    mul w9, w9, w10
    str w9, [x27, #208]
    ldr w0, [x27, #208]
    mov sp, x27
    add sp, sp, #224
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn19:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    add w9, w9, w10
    str w9, [x27, #16]
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn18:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str d0, [x27, #0]
    str d1, [x27, #8]
    ldr d0, [x27, #0]
    ldr d1, [x27, #8]
    fadd d0, d0, d1
    str d0, [x27, #16]
    ldr x0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cc_hidden_fn20:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #304
    mov x27, sp
    str w0, [x27, #0]
    str x0, [x27, #72]
    str x1, [x27, #80]
    str x2, [x27, #88]
    str x3, [x27, #96]
    str x4, [x27, #104]
    str x5, [x27, #112]
    str x6, [x27, #120]
    str x7, [x27, #128]
    str d0, [x27, #136]
    str d1, [x27, #144]
    str d2, [x27, #152]
    str d3, [x27, #160]
    str d4, [x27, #168]
    str d5, [x27, #176]
    str d6, [x27, #184]
    str d7, [x27, #192]
    add x9, x27, #80
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #8]
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #16]
    ldr x9, [x27, #16]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #16]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    mov w10, wzr
    strb w10, [x9]
    mov w9, wzr
    str w9, [x27, #24]
Lcc_label_f10_0:
    ldr w9, [x27, #24]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #200]
    ldrb w9, [x27, #200]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #200]
    ldrb w9, [x27, #200]
    cmp w9, wzr
    b.ne Lcc_label_f10_1
    b Lcc_label_f10_2
Lcc_label_f10_1:
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #32]
    ldr x9, [x27, #8]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    ldr x10, [x9]
    str x10, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #200]
    ldr x9, [x27, #200]
    str x9, [x27, #8]
