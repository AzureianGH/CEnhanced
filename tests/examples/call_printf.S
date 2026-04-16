// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0

.extern _Std_IO_print_ptr_to_char
.extern __cert__new
.extern __cert__delete
.extern __cert__GC__prep_enter
.extern _Std_Memory_alloc_i32
.extern _Std_Memory_memset_ptr_to_void_u8_i32
.extern _write
.extern _read
.extern _Std_String_strlen_ptr_to_char
.extern _Std_Memory_dealloc_ptr_to_void
.extern _Test_Chance_test_chance
.extern _LibTest_test_fun_i32
.extern _LibTest_Another_other_test_fun_i32
.extern _Std_IO_printnl_ptr_to_char
.extern _LibTest_Another_get_info
.extern _LibTest_Another_test_varargs_i32_i32
.extern _LibTest_Another_test_diffvarargs_i32
.extern _LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest
.extern __cert__GC__prep_exit
.extern _Std_File_write_all_ptr_to_char_ptr_to_void_i32
.extern _Std_File_read_all_ptr_to_char_ptr_to_ptr_to_void_ptr_to_i32
.extern _Std_File_open_ptr_to_char_ptr_to_char
.extern _Std_File_write_ptr_to_void_ptr_to_void_i32
.extern _Std_File_flush_ptr_to_void
.extern _Std_File_close_ptr_to_void
.extern _Std_File_seek_ptr_to_void_i64_i32
.extern _Std_File_tell_ptr_to_void
.extern _Std_File_read_ptr_to_void_ptr_to_void_i32
.extern _remove
.extern _exit


.section __DATA,__data
.p2align 2
_Test_Program_total_test_count:
    .long 0x00000000

.p2align 3
_Test_Program_globalElement:
    .byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00

.p2align 3
_Test_Program_globalctr:
    .quad 0x0000000000000000

.file 0 "/Users/nathanhornby/Documents/GitHub/CEnhanced/tests/examples" "call_printf.ce"
.file 1 "/Users/nathanhornby/Documents/GitHub/CEnhanced/tests/examples/call_printf.ce"

.section __TEXT,__text,regular,pure_instructions

.p2align 2
_Test_Program_alignof_alignprobe:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 127 9
    .loc 1 127 5
    movz w0, #0x8
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_offsetof_alignprobe_payload:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 132 9
    .loc 1 132 5
    movz w0, #0x8
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_inlined_function_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    .loc 1 143 9
    .loc 1 143 13
    .loc 1 143 11
    ldr w9, [x27, #0]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #8]
    .loc 1 143 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_printwrapper:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #96
    mov x27, sp
    .loc 1 156 22
    .loc 1 156 12
    adrp x9, __Test_Program_printwrapper____str0@PAGE
    add x9, x9, __Test_Program_printwrapper____str0@PAGEOFF
    str x9, [x27, #0]
    .loc 1 157 19
    .loc 1 157 12
    ldr x9, [x27, #0]
    str x9, [x27, #8]
    .loc 1 157 28
    .loc 1 157 12
    adrp x9, __Test_Program_printwrapper____str1@PAGE
    add x9, x9, __Test_Program_printwrapper____str1@PAGEOFF
    str x9, [x27, #16]
    ldr x0, [x27, #8]
    ldr x1, [x27, #16]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #64]
    .loc 1 157 47
    .loc 1 157 44
    ldr w9, [x27, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #64]
    .loc 1 157 5
    ldrb w9, [x27, #64]
    strb w9, [x27, #24]
    .loc 1 157 50
    .loc 1 157 5
    adrp x9, __Test_Program_printwrapper____str2@PAGE
    add x9, x9, __Test_Program_printwrapper____str2@PAGEOFF
    str x9, [x27, #32]
    ldrsb w0, [x27, #24]
    ldr x1, [x27, #32]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 158 18
    .loc 1 158 12
    adrp x9, __Test_Program_printwrapper____str3@PAGE
    add x9, x9, __Test_Program_printwrapper____str3@PAGEOFF
    str x9, [x27, #40]
    .loc 1 158 45
    .loc 1 158 12
    adrp x9, __Test_Program_printwrapper____str4@PAGE
    add x9, x9, __Test_Program_printwrapper____str4@PAGEOFF
    str x9, [x27, #48]
    .loc 1 158 55
    .loc 1 158 12
    movz w9, #0x2a
    str w9, [x27, #56]
    ldr x0, [x27, #40]
    ldr x1, [x27, #48]
    ldr w2, [x27, #56]
    sub sp, sp, #16
    ldr x9, [x27, #48]
    str x9, [sp, #0]
    ldr w9, [x27, #56]
    sxtw x9, w9
    str x9, [sp, #8]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #64]
    .loc 1 158 5
    ldr w0, [x27, #64]
    mov sp, x27
    add sp, sp, #96
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_assert_bool_ptr_to_char:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #96
    mov x27, sp
    strb w0, [x27, #0]
    str x1, [x27, #8]
    .loc 1 163 22
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x9]
    str w10, [x27, #64]
    ldr w9, [x27, #64]
    str w9, [x27, #16]
    .loc 1 163 25
    .loc 1 163 22
    movz w9, #0x1
    str w9, [x27, #24]
    ldr w9, [x27, #16]
    ldr w10, [x27, #24]
    add w9, w9, w10
    str w9, [x27, #64]
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x27, #64]
    str w10, [x9]
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x9]
    str w10, [x27, #64]
    .loc 1 164 9
    .loc 1 164 5
    ldrsb w9, [x27, #0]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #64]
    ldrb w9, [x27, #64]
    cmp w9, wzr
    b.ne LTest_Program_assert_bool_ptr_to_char__if_true0
    b LTest_Program_assert_bool_ptr_to_char__if_false1
LTest_Program_assert_bool_ptr_to_char__if_true0:
    .loc 1 166 18
    .loc 1 166 12
    adrp x9, __Test_Program_assert_bool_ptr_to_char____str5@PAGE
    add x9, x9, __Test_Program_assert_bool_ptr_to_char____str5@PAGEOFF
    str x9, [x27, #32]
    .loc 1 166 39
    .loc 1 166 12
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    ldr x0, [x27, #32]
    ldr x1, [x27, #40]
    sub sp, sp, #16
    ldr x9, [x27, #40]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #64]
    .loc 1 164 5
    b LTest_Program_assert_bool_ptr_to_char__if_end2
LTest_Program_assert_bool_ptr_to_char__if_false1:
    .loc 1 170 18
    .loc 1 170 12
    adrp x9, __Test_Program_assert_bool_ptr_to_char____str6@PAGE
    add x9, x9, __Test_Program_assert_bool_ptr_to_char____str6@PAGEOFF
    str x9, [x27, #48]
    .loc 1 170 37
    .loc 1 170 12
    ldr x9, [x27, #8]
    str x9, [x27, #56]
    ldr x0, [x27, #48]
    ldr x1, [x27, #56]
    sub sp, sp, #16
    ldr x9, [x27, #56]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #64]
    .loc 1 171 9
    bl _Test_Program_abort
    .loc 1 164 5
LTest_Program_assert_bool_ptr_to_char__if_end2:
    .loc 1 173 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #96
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_mock_assemble_ptr_to_struct_asmprogram:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 178 9
    .loc 1 178 20
    .loc 1 178 17
    ldr x9, [x27, #0]
    mov x10, xzr
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #8]
    .loc 1 178 5
    ldrb w9, [x27, #8]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #8]
    ldrb w9, [x27, #8]
    cmp w9, wzr
    b.ne LTest_Program_mock_assemble_ptr_to_struct_asmprogram__if_true0
    b LTest_Program_mock_assemble_ptr_to_struct_asmprogram__if_end1
LTest_Program_mock_assemble_ptr_to_struct_asmprogram__if_true0:
    .loc 1 179 13
    .loc 1 179 14
    .loc 1 179 13
    mov w9, wzr
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #8]
    .loc 1 179 9
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
    .loc 1 178 5
LTest_Program_mock_assemble_ptr_to_struct_asmprogram__if_end1:
    .loc 1 180 9
    .loc 1 180 5
    movz w0, #0x7
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program___bundle_inst_TestClass_init_ptr_to_struct_testclass_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    str x0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 189 13
    .loc 1 189 20
    ldr x9, [x27, #0]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    str x9, [x27, #16]
    .loc 1 189 22
    .loc 1 189 20
    ldr w9, [x27, #8]
    str w9, [x27, #24]
    ldr x9, [x27, #16]
    ldr w10, [x27, #24]
    str w10, [x9]
    .loc 1 190 13
    .loc 1 190 26
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    .loc 1 190 36
    .loc 1 190 28
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #48]
    ldr x0, [x27, #48]
    bl __cert__new
    str x0, [x27, #48]
    .loc 1 190 26
    ldr x9, [x27, #48]
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    str x10, [x9]
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program___bundle_inst_TestClass_deinit_ptr_to_struct_testclass:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 194 20
    .loc 1 194 13
    ldr x9, [x27, #0]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    ldr x10, [x9]
    str x10, [x27, #16]
    ldr x0, [x27, #16]
    bl __cert__delete
    ldr x9, [x27, #8]
    mov x10, xzr
    str x10, [x9]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program___bundle_inst_TestClass_Example_ptr_to_struct_testclass_i32
.p2align 2
_Test_Program___bundle_inst_TestClass_Example_ptr_to_struct_testclass_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str x0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 198 13
    .loc 1 198 20
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #16]
    .loc 1 198 22
    .loc 1 198 20
    ldr w9, [x27, #8]
    str w9, [x27, #24]
    ldr x9, [x27, #16]
    ldr w10, [x27, #24]
    str w10, [x9]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program___bundle_static_TestClass_New_i32
.p2align 2
_Test_Program___bundle_static_TestClass_New_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    str w0, [x27, #0]
    .loc 1 202 17
    movz x0, #0x10
    bl __cert__new
    str x0, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    mov x10, xzr
    str x10, [x9]
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    movz w10, #0x1
    str w10, [x9]
    ldr x9, [x27, #8]
    str x9, [x27, #16]
    ldr x9, [x27, #16]
    str x9, [x27, #24]
    .loc 1 202 31
    .loc 1 202 17
    ldr w9, [x27, #0]
    str w9, [x27, #32]
    ldr x0, [x27, #24]
    ldr w1, [x27, #32]
    bl _Test_Program___bundle_inst_TestClass_init_ptr_to_struct_testclass_i32
    .loc 1 202 13
    ldr x0, [x27, #16]
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__cert__entry_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #4080
    sub sp, sp, #4080
    sub sp, sp, #368
    mov x27, sp
    bl __cert__GC__prep_enter
    .loc 1 207 23
    movz x0, #0x10
    bl __cert__new
    str x0, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #0]
    ldr x9, [x27, #0]
    mov x10, xzr
    str x10, [x9]
    ldr x9, [x27, #0]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x1
    str w10, [x9]
    ldr x9, [x27, #0]
    str x9, [x27, #8]
    ldr x9, [x27, #8]
    str x9, [x27, #16]
    .loc 1 207 37
    .loc 1 207 23
    movz w9, #0x5
    str w9, [x27, #24]
    ldr x0, [x27, #16]
    ldr w1, [x27, #24]
    bl _Test_Program___bundle_inst_TestClass_init_ptr_to_struct_testclass_i32
    .loc 1 207 11
    adrp x9, _Test_Program_globalctr@PAGE
    add x9, x9, _Test_Program_globalctr@PAGEOFF
    ldr x10, [x27, #8]
    str x10, [x9]
    adrp x9, _Test_Program_globalctr@PAGE
    add x9, x9, _Test_Program_globalctr@PAGEOFF
    ldr x10, [x9]
    str x10, [x27, #5168]
    .loc 1 213 21
    .loc 1 213 15
    mov x9, xzr
    str x9, [x27, #32]
    .loc 1 215 24
    .loc 1 215 18
    movz w0, #0x80
    bl _Std_Memory_alloc_i32
    str x0, [x27, #5168]
    .loc 1 215 9
    ldr x9, [x27, #5168]
    str x9, [x27, #32]
    .loc 1 217 24
    .loc 1 217 9
    ldr x9, [x27, #32]
    str x9, [x27, #40]
    .loc 1 218 12
    .loc 1 218 28
    .loc 1 218 25
    ldr x9, [x27, #40]
    ldr x10, [x27, #32]
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 218 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #48]
    .loc 1 218 33
    .loc 1 218 5
    adrp x9, ___cert__entry_main____str7@PAGE
    add x9, x9, ___cert__entry_main____str7@PAGEOFF
    str x9, [x27, #56]
    ldrsb w0, [x27, #48]
    ldr x1, [x27, #56]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 220 12
    .loc 1 220 19
    .loc 1 220 16
    ldr x9, [x27, #32]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 220 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #64]
    .loc 1 220 25
    .loc 1 220 5
    adrp x9, ___cert__entry_main____str8@PAGE
    add x9, x9, ___cert__entry_main____str8@PAGEOFF
    str x9, [x27, #72]
    ldrsb w0, [x27, #64]
    ldr x1, [x27, #72]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 222 19
    .loc 1 222 12
    ldr x9, [x27, #32]
    str x9, [x27, #80]
    .loc 1 222 24
    .loc 1 222 12
    mov w9, wzr
    strb w9, [x27, #88]
    .loc 1 222 27
    .loc 1 222 12
    movz w9, #0x80
    str w9, [x27, #96]
    ldr x0, [x27, #80]
    ldrb w1, [x27, #88]
    ldr w2, [x27, #96]
    bl _Std_Memory_memset_ptr_to_void_u8_i32
    str x0, [x27, #5168]
    .loc 1 224 21
    .loc 1 224 10
    movz w9, #0x1
    strb w9, [x27, #104]
    .loc 1 226 18
    .loc 1 226 14
    mov w9, wzr
    str w9, [x27, #112]
    .loc 1 1 1
L__cert__entry_main__while_cond2:
    .loc 1 226 21
    .loc 1 226 25
    .loc 1 226 23
    ldr w9, [x27, #112]
    movz w10, #0x80
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body3
    b L__cert__entry_main__while_end4
L__cert__entry_main__while_body3:
    .loc 1 228 14
    .loc 1 228 25
    ldr x9, [x27, #32]
    str x9, [x27, #5168]
    .loc 1 228 26
    .loc 1 228 25
    ldr w9, [x27, #112]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 228 29
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 228 32
    .loc 1 228 29
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 228 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true6
    b L__cert__entry_main__if_end7
L__cert__entry_main__if_true6:
    .loc 1 230 24
    .loc 1 230 22
    mov w9, wzr
    strb w9, [x27, #104]
    .loc 1 228 9
L__cert__entry_main__if_end7:
    .loc 1 1 1
L__cert__entry_main__while_post5:
    .loc 1 226 34
    .loc 1 226 38
    .loc 1 226 36
    ldr w9, [x27, #112]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 226 32
    ldr w9, [x27, #5168]
    str w9, [x27, #112]
    .loc 1 1 1
    b L__cert__entry_main__while_cond2
L__cert__entry_main__while_end4:
    .loc 1 234 12
    .loc 1 234 5
    ldrsb w9, [x27, #104]
    strb w9, [x27, #120]
    .loc 1 234 22
    .loc 1 234 5
    adrp x9, ___cert__entry_main____str9@PAGE
    add x9, x9, ___cert__entry_main____str9@PAGEOFF
    str x9, [x27, #128]
    ldrsb w0, [x27, #120]
    ldr x1, [x27, #128]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 237 11
    .loc 1 237 5
    movz w9, #0x1
    str w9, [x27, #136]
    .loc 1 237 14
    .loc 1 237 5
    adrp x9, ___cert__entry_main____str10@PAGE
    add x9, x9, ___cert__entry_main____str10@PAGEOFF
    str x9, [x27, #144]
    .loc 1 237 38
    .loc 1 237 5
    movz w9, #0x14
    str w9, [x27, #152]
    ldr w0, [x27, #136]
    ldr x1, [x27, #144]
    ldr w2, [x27, #152]
    bl _write
    str w0, [x27, #5168]
    .loc 1 240 20
    .loc 1 240 15
    mov w9, wzr
    str w9, [x27, #168]
    .loc 1 240 23
    .loc 1 240 15
    ldr x9, [x27, #32]
    str x9, [x27, #176]
    .loc 1 240 38
    .loc 1 240 15
    movz w9, #0x7f
    str w9, [x27, #184]
    ldr w0, [x27, #168]
    ldr x1, [x27, #176]
    ldr w2, [x27, #184]
    bl _read
    str w0, [x27, #5168]
    .loc 1 240 9
    ldr w9, [x27, #5168]
    str w9, [x27, #160]
    .loc 1 243 19
    bl _Test_Program_printwrapper
    str w0, [x27, #5168]
    .loc 1 243 9
    ldr w9, [x27, #5168]
    str w9, [x27, #192]
    .loc 1 245 12
    .loc 1 245 23
    .loc 1 245 20
    ldr w9, [x27, #192]
    movz w10, #0x1a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 245 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #200]
    .loc 1 245 27
    .loc 1 245 5
    adrp x9, ___cert__entry_main____str11@PAGE
    add x9, x9, ___cert__entry_main____str11@PAGEOFF
    str x9, [x27, #208]
    ldrsb w0, [x27, #200]
    ldr x1, [x27, #208]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 247 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #216]
    ldr x9, [x27, #216]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #216]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov x10, xzr
    str x10, [x9]
    .loc 1 248 17
    .loc 1 247 18
    ldr x9, [x27, #216]
    movz w10, #0x7
    str w10, [x9]
    ldr x9, [x27, #216]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 249 20
    .loc 1 247 18
    ldr x9, [x27, #5168]
    adrp x10, ___cert__entry_main____str12@PAGE
    add x10, x10, ___cert__entry_main____str12@PAGEOFF
    str x10, [x9]
    .loc 1 251 31
    adrp x9, __lambda_0@PAGE
    add x9, x9, __lambda_0@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 251 26
    ldr x9, [x27, #5168]
    str x9, [x27, #224]
    .loc 1 258 8
    .loc 1 258 5
    ldr x9, [x27, #216]
    str x9, [x27, #232]
    .loc 1 258 15
    .loc 1 258 5
    mov x9, xzr
    str x9, [x27, #240]
    ldr x0, [x27, #232]
    ldr x1, [x27, #240]
    ldr x16, [x27, #224]
    blr x16
    .loc 1 259 12
    .loc 1 259 17
    ldr x9, [x27, #216]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 259 25
    .loc 1 259 22
    ldr w9, [x27, #5168]
    movz w10, #0x7
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 259 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #248]
    .loc 1 259 28
    .loc 1 259 5
    adrp x9, ___cert__entry_main____str13@PAGE
    add x9, x9, ___cert__entry_main____str13@PAGEOFF
    str x9, [x27, #256]
    ldrsb w0, [x27, #248]
    ldr x1, [x27, #256]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 263 18
    .loc 1 263 12
    ldr x9, [x27, #32]
    str x9, [x27, #264]
    .loc 1 264 9
    .loc 1 264 15
    .loc 1 264 13
    ldr w9, [x27, #160]
    mov w10, wzr
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #5168]
    .loc 1 264 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true8
    b L__cert__entry_main__if_false9
L__cert__entry_main__if_true8:
    .loc 1 266 13
    .loc 1 266 16
    ldr x9, [x27, #264]
    str x9, [x27, #5168]
    .loc 1 266 17
    .loc 1 266 23
    .loc 1 266 21
    ldr w9, [x27, #160]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 266 16
    ldr w9, [x27, #5176]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrsb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 266 29
    .loc 1 266 26
    ldrsb w9, [x27, #5168]
    movz w10, #0xa
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 266 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true11
    b L__cert__entry_main__if_end12
L__cert__entry_main__if_true11:
    .loc 1 266 35
    .loc 1 266 46
    ldr x9, [x27, #264]
    str x9, [x27, #5168]
    .loc 1 266 39
    ldr w9, [x27, #160]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5176]
    ldr w9, [x27, #5176]
    str w9, [x27, #160]
    .loc 1 266 46
    ldr w9, [x27, #160]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #272]
    .loc 1 266 48
    .loc 1 266 46
    mov w9, wzr
    strb w9, [x27, #280]
    ldr x9, [x27, #272]
    ldrsb w10, [x27, #280]
    strb w10, [x9]
    .loc 1 266 9
L__cert__entry_main__if_end12:
    .loc 1 267 13
    .loc 1 267 19
    .loc 1 267 17
    ldr w9, [x27, #160]
    mov w10, wzr
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #5168]
    .loc 1 267 21
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs13
    b L__cert__entry_main__land_false14
L__cert__entry_main__land_rhs13:
    .loc 1 267 24
    .loc 1 267 27
    ldr x9, [x27, #264]
    str x9, [x27, #5168]
    .loc 1 267 28
    .loc 1 267 34
    .loc 1 267 32
    ldr w9, [x27, #160]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 267 27
    ldr w9, [x27, #5176]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrsb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 267 40
    .loc 1 267 37
    ldrsb w9, [x27, #5168]
    movz w10, #0xd
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 267 21
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true15
    b L__cert__entry_main__land_false14
L__cert__entry_main__land_true15:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end16
L__cert__entry_main__land_false14:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end16
L__cert__entry_main__land_end16:
    .loc 1 267 9
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true17
    b L__cert__entry_main__if_end18
L__cert__entry_main__if_true17:
    .loc 1 267 46
    .loc 1 267 57
    ldr x9, [x27, #264]
    str x9, [x27, #5168]
    .loc 1 267 50
    ldr w9, [x27, #160]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5176]
    ldr w9, [x27, #5176]
    str w9, [x27, #160]
    .loc 1 267 57
    ldr w9, [x27, #160]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #288]
    .loc 1 267 59
    .loc 1 267 57
    mov w9, wzr
    strb w9, [x27, #296]
    ldr x9, [x27, #288]
    ldrsb w10, [x27, #296]
    strb w10, [x9]
    .loc 1 267 9
L__cert__entry_main__if_end18:
    .loc 1 264 5
    b L__cert__entry_main__if_end10
L__cert__entry_main__if_false9:
    .loc 1 271 18
    .loc 1 271 12
    adrp x0, ___cert__entry_main____str14@PAGE
    add x0, x0, ___cert__entry_main____str14@PAGEOFF
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 272 9
    .loc 1 272 16
    ldr x9, [x27, #264]
    str x9, [x27, #5168]
    .loc 1 272 13
    .loc 1 272 16
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #304]
    .loc 1 272 18
    .loc 1 272 16
    mov w9, wzr
    strb w9, [x27, #312]
    ldr x9, [x27, #304]
    ldrsb w10, [x27, #312]
    strb w10, [x9]
    .loc 1 264 5
L__cert__entry_main__if_end10:
    .loc 1 276 26
    .loc 1 276 19
    ldr x0, [x27, #264]
    bl _Std_String_strlen_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 276 34
    .loc 1 276 31
    ldr w9, [x27, #5168]
    ldr w10, [x27, #160]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 276 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #320]
    .loc 1 276 39
    .loc 1 276 5
    adrp x9, ___cert__entry_main____str15@PAGE
    add x9, x9, ___cert__entry_main____str15@PAGEOFF
    str x9, [x27, #328]
    ldrsb w0, [x27, #320]
    ldr x1, [x27, #328]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 278 25
    .loc 1 278 12
    adrp x9, ___cert__entry_main____str16@PAGE
    add x9, x9, ___cert__entry_main____str16@PAGEOFF
    str x9, [x27, #336]
    .loc 1 281 16
    .loc 1 281 9
    ldr x9, [x27, #264]
    str x9, [x27, #344]
    .loc 1 281 21
    .loc 1 281 9
    ldr x9, [x27, #336]
    str x9, [x27, #352]
    ldr x0, [x27, #344]
    ldr x1, [x27, #352]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 281 36
    .loc 1 281 33
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 281 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true19
    b L__cert__entry_main__if_false20
L__cert__entry_main__if_true19:
    .loc 1 283 16
    .loc 1 283 9
    movz w9, #0x1
    strb w9, [x27, #360]
    .loc 1 283 22
    .loc 1 283 9
    adrp x9, ___cert__entry_main____str17@PAGE
    add x9, x9, ___cert__entry_main____str17@PAGEOFF
    str x9, [x27, #368]
    ldrsb w0, [x27, #360]
    ldr x1, [x27, #368]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 281 5
    b L__cert__entry_main__if_end21
L__cert__entry_main__if_false20:
    .loc 1 287 16
    .loc 1 287 9
    movz w9, #0x1
    strb w9, [x27, #376]
    .loc 1 287 22
    .loc 1 287 9
    adrp x9, ___cert__entry_main____str18@PAGE
    add x9, x9, ___cert__entry_main____str18@PAGEOFF
    str x9, [x27, #384]
    ldrsb w0, [x27, #376]
    ldr x1, [x27, #384]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 281 5
L__cert__entry_main__if_end21:
    .loc 1 290 14
    .loc 1 290 8
    adrp x0, ___cert__entry_main____str19@PAGE
    add x0, x0, ___cert__entry_main____str19@PAGEOFF
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 291 20
    .loc 1 291 12
    ldr x0, [x27, #32]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 294 14
    bl _Test_Program_test_overflow
    strh w0, [x27, #5168]
    .loc 1 294 9
    ldrsh w9, [x27, #5168]
    strh w9, [x27, #392]
    .loc 1 295 14
    .loc 1 295 8
    adrp x9, ___cert__entry_main____str20@PAGE
    add x9, x9, ___cert__entry_main____str20@PAGEOFF
    str x9, [x27, #400]
    .loc 1 295 44
    .loc 1 295 8
    ldrsh w9, [x27, #392]
    sxth w9, w9
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    strh w9, [x27, #408]
    ldr x0, [x27, #400]
    ldrsh w1, [x27, #408]
    sub sp, sp, #16
    ldrsh w9, [x27, #408]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 296 12
    .loc 1 296 15
    ldrsh w9, [x27, #392]
    sxth w9, w9
    str w9, [x27, #5168]
    .loc 1 296 18
    .loc 1 296 19
    .loc 1 296 18
    mov w9, wzr
    movz w10, #0x63c0
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 296 15
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 296 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #416]
    .loc 1 296 26
    .loc 1 296 5
    adrp x9, ___cert__entry_main____str21@PAGE
    add x9, x9, ___cert__entry_main____str21@PAGEOFF
    str x9, [x27, #424]
    ldrsb w0, [x27, #416]
    ldr x1, [x27, #424]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 299 17
    bl _Test_Program_test_mul_overflow
    strb w0, [x27, #5168]
    .loc 1 299 8
    ldrb w9, [x27, #5168]
    strb w9, [x27, #432]
    .loc 1 300 12
    .loc 1 300 19
    ldrb w9, [x27, #432]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 300 22
    .loc 1 300 19
    ldr w9, [x27, #5168]
    movz w10, #0x40
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 300 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #440]
    .loc 1 300 26
    .loc 1 300 5
    adrp x9, ___cert__entry_main____str22@PAGE
    add x9, x9, ___cert__entry_main____str22@PAGEOFF
    str x9, [x27, #448]
    ldrsb w0, [x27, #440]
    ldr x1, [x27, #448]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 303 15
    .loc 1 303 19
    .loc 1 303 17
    movz w9, #0x6
    movz w10, #0x7
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 303 9
    ldr w9, [x27, #5168]
    str w9, [x27, #456]
    .loc 1 304 12
    .loc 1 304 19
    .loc 1 304 16
    ldr w9, [x27, #456]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 304 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #464]
    .loc 1 304 23
    .loc 1 304 5
    adrp x9, ___cert__entry_main____str23@PAGE
    add x9, x9, ___cert__entry_main____str23@PAGEOFF
    str x9, [x27, #472]
    ldrsb w0, [x27, #464]
    ldr x1, [x27, #472]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 306 15
    .loc 1 306 20
    .loc 1 306 21
    .loc 1 306 20
    movz w11, #0x14
    str w11, [x27, #5168]
    mov w9, wzr
    movz w10, #0x4
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 306 18
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    sdiv w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 306 9
    ldr w9, [x27, #5168]
    str w9, [x27, #480]
    .loc 1 307 12
    .loc 1 307 19
    .loc 1 307 20
    .loc 1 307 19
    mov w9, wzr
    movz w10, #0x5
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 307 16
    ldr w9, [x27, #480]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 307 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #488]
    .loc 1 307 23
    .loc 1 307 5
    adrp x9, ___cert__entry_main____str24@PAGE
    add x9, x9, ___cert__entry_main____str24@PAGEOFF
    str x9, [x27, #496]
    ldrsb w0, [x27, #488]
    ldr x1, [x27, #496]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 309 15
    .loc 1 309 20
    .loc 1 309 18
    movz w9, #0x14
    movz w10, #0x6
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #5168]
    .loc 1 309 9
    ldr w9, [x27, #5168]
    str w9, [x27, #504]
    .loc 1 310 12
    .loc 1 310 19
    .loc 1 310 16
    ldr w9, [x27, #504]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 310 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #512]
    .loc 1 310 22
    .loc 1 310 5
    adrp x9, ___cert__entry_main____str25@PAGE
    add x9, x9, ___cert__entry_main____str25@PAGEOFF
    str x9, [x27, #520]
    ldrsb w0, [x27, #512]
    ldr x1, [x27, #520]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 312 13
    .loc 1 312 29
    .loc 1 312 26
    movz w9, #0xff
    movz w10, #0xff
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 312 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #528]
    .loc 1 312 34
    .loc 1 312 5
    adrp x9, ___cert__entry_main____str26@PAGE
    add x9, x9, ___cert__entry_main____str26@PAGEOFF
    str x9, [x27, #536]
    ldrsb w0, [x27, #528]
    ldr x1, [x27, #536]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 314 14
    .loc 1 314 8
    adrp x9, ___cert__entry_main____str27@PAGE
    add x9, x9, ___cert__entry_main____str27@PAGEOFF
    str x9, [x27, #544]
    .loc 1 314 48
    bl _Test_Chance_test_chance
    str w0, [x27, #5168]
    .loc 1 314 8
    ldr w9, [x27, #5168]
    str w9, [x27, #552]
    ldr x0, [x27, #544]
    ldr w1, [x27, #552]
    sub sp, sp, #16
    ldr w9, [x27, #552]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 318 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #560]
    ldr x9, [x27, #560]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #560]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 319 5
    .loc 1 319 21
    ldr x9, [x27, #560]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #568]
    .loc 1 319 23
    .loc 1 319 21
    movz w9, #0x3
    strb w9, [x27, #576]
    ldr x9, [x27, #568]
    ldrb w10, [x27, #576]
    strb w10, [x9]
    .loc 1 320 5
    .loc 1 320 18
    ldr x9, [x27, #560]
    str x9, [x27, #584]
    .loc 1 320 20
    .loc 1 320 18
    movz w9, #0x2c
    str w9, [x27, #592]
    ldr x9, [x27, #584]
    ldr w10, [x27, #592]
    str w10, [x9]
    .loc 1 322 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #600]
    ldr x9, [x27, #600]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #600]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #600]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 323 23
    .loc 1 322 18
    ldr x9, [x27, #5168]
    movz w10, #0x7b
    strb w10, [x9]
    .loc 1 324 20
    .loc 1 322 18
    ldr x9, [x27, #600]
    movz w10, #0x14d
    str w10, [x9]
    .loc 1 327 26
    .loc 1 327 19
    ldr x9, [x27, #560]
    str x9, [x27, #608]
    .loc 1 329 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #616]
    .loc 1 329 27
    .loc 1 329 18
    ldr x9, [x27, #608]
    str x9, [x27, #624]
    ldr x9, [x27, #616]
    str x9, [x27, #632]
    ldr x9, [x27, #624]
    ldr w10, [x9]
    str w10, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #640]
    ldr x9, [x27, #632]
    ldr w10, [x27, #640]
    str w10, [x9]
    ldr x9, [x27, #624]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    ldrb w9, [x27, #5168]
    strb w9, [x27, #648]
    ldr x9, [x27, #632]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x27, #648]
    strb w10, [x9]
    .loc 1 331 12
    .loc 1 331 18
    ldr x9, [x27, #616]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 331 29
    .loc 1 331 26
    ldr w9, [x27, #5168]
    movz w10, #0x2c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 331 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #656]
    .loc 1 331 33
    .loc 1 331 5
    adrp x9, ___cert__entry_main____str28@PAGE
    add x9, x9, ___cert__entry_main____str28@PAGEOFF
    str x9, [x27, #664]
    ldrsb w0, [x27, #656]
    ldr x1, [x27, #664]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 333 19
    .loc 1 333 9
    movz w9, #0x7b
    str w9, [x27, #672]
    .loc 1 334 21
    add x9, x27, #672
    str x9, [x27, #5168]
    .loc 1 334 10
    ldr x9, [x27, #5168]
    str x9, [x27, #680]
    .loc 1 335 23
    add x9, x27, #680
    str x9, [x27, #5168]
    .loc 1 335 11
    ldr x9, [x27, #5168]
    str x9, [x27, #688]
    .loc 1 336 12
    .loc 1 336 21
    ldr x9, [x27, #688]
    str x9, [x27, #5168]
    .loc 1 336 22
    .loc 1 336 21
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr x10, [x9]
    str x10, [x27, #5168]
    .loc 1 336 24
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 336 25
    .loc 1 336 24
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 336 31
    .loc 1 336 28
    ldr w9, [x27, #5168]
    movz w10, #0x7b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 336 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #696]
    .loc 1 336 36
    .loc 1 336 5
    adrp x9, ___cert__entry_main____str29@PAGE
    add x9, x9, ___cert__entry_main____str29@PAGEOFF
    str x9, [x27, #704]
    ldrsb w0, [x27, #696]
    ldr x1, [x27, #704]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 338 41
    .loc 1 338 64
    .loc 1 338 62
    movz w9, #0x8
    movz w10, #0x2
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 338 35
    ldr w0, [x27, #5168]
    bl _Std_Memory_alloc_i32
    str x0, [x27, #5168]
    .loc 1 338 19
    ldr x9, [x27, #5168]
    str x9, [x27, #712]
    .loc 1 340 5
    .loc 1 340 15
    ldr x9, [x27, #712]
    str x9, [x27, #5168]
    .loc 1 340 12
    .loc 1 340 15
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #720]
    ldr x9, [x27, #720]
    str x9, [x27, #728]
    .loc 1 340 17
    bl _Test_Program_construct_thing
    str x0, [x27, #5168]
    .loc 1 340 15
    ldr x9, [x27, #5168]
    str x9, [x27, #736]
    ldr x9, [x27, #736]
    ldr w10, [x9]
    str w10, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #744]
    ldr x9, [x27, #728]
    ldr w10, [x27, #744]
    str w10, [x9]
    ldr x9, [x27, #736]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    ldrb w9, [x27, #5168]
    strb w9, [x27, #752]
    ldr x9, [x27, #728]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x27, #752]
    strb w10, [x9]
    .loc 1 341 5
    .loc 1 341 15
    ldr x9, [x27, #712]
    str x9, [x27, #5168]
    .loc 1 341 12
    .loc 1 341 15
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #760]
    ldr x9, [x27, #760]
    str x9, [x27, #768]
    .loc 1 341 17
    .loc 1 341 15
    ldr x9, [x27, #600]
    str x9, [x27, #776]
    ldr x9, [x27, #776]
    ldr w10, [x9]
    str w10, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #784]
    ldr x9, [x27, #768]
    ldr w10, [x27, #784]
    str w10, [x9]
    ldr x9, [x27, #776]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    ldrb w9, [x27, #5168]
    strb w9, [x27, #792]
    ldr x9, [x27, #768]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x27, #792]
    strb w10, [x9]
    .loc 1 344 12
    .loc 1 344 18
    ldr x9, [x27, #712]
    str x9, [x27, #5168]
    .loc 1 344 19
    .loc 1 344 18
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 344 22
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 344 33
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 344 36
    .loc 1 344 33
    ldr w9, [x27, #5168]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 344 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #800]
    .loc 1 344 39
    .loc 1 344 5
    adrp x9, ___cert__entry_main____str30@PAGE
    add x9, x9, ___cert__entry_main____str30@PAGEOFF
    str x9, [x27, #808]
    ldrsb w0, [x27, #800]
    ldr x1, [x27, #808]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 345 12
    .loc 1 345 18
    ldr x9, [x27, #712]
    str x9, [x27, #5168]
    .loc 1 345 19
    .loc 1 345 18
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 345 22
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 345 33
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 345 36
    .loc 1 345 33
    ldr w9, [x27, #5168]
    movz w10, #0x7b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 345 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #816]
    .loc 1 345 41
    .loc 1 345 5
    adrp x9, ___cert__entry_main____str31@PAGE
    add x9, x9, ___cert__entry_main____str31@PAGEOFF
    str x9, [x27, #824]
    ldrsb w0, [x27, #816]
    ldr x1, [x27, #824]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 347 20
    .loc 1 347 12
    ldr x0, [x27, #712]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 349 23
    .loc 1 349 10
    ldr x9, [x27, #608]
    str x9, [x27, #832]
    .loc 1 350 14
    .loc 1 350 8
    adrp x9, ___cert__entry_main____str32@PAGE
    add x9, x9, ___cert__entry_main____str32@PAGEOFF
    str x9, [x27, #840]
    .loc 1 350 42
    .loc 1 350 8
    ldr x9, [x27, #832]
    str x9, [x27, #848]
    ldr x0, [x27, #840]
    ldr x1, [x27, #848]
    sub sp, sp, #16
    ldr x9, [x27, #848]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 352 34
    .loc 1 352 16
    movz w9, #0x3
    str w9, [x27, #856]
    .loc 1 354 5
    .loc 1 354 19
    ldr x9, [x27, #608]
    str x9, [x27, #864]
    .loc 1 354 21
    .loc 1 354 19
    movz w9, #0x2d
    str w9, [x27, #872]
    ldr x9, [x27, #864]
    ldr w10, [x27, #872]
    str w10, [x9]
    .loc 1 356 12
    .loc 1 356 18
    ldr x9, [x27, #608]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 356 29
    .loc 1 356 26
    ldr w9, [x27, #5168]
    movz w10, #0x2d
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 356 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #880]
    .loc 1 356 33
    .loc 1 356 5
    adrp x9, ___cert__entry_main____str33@PAGE
    add x9, x9, ___cert__entry_main____str33@PAGEOFF
    str x9, [x27, #888]
    ldrsb w0, [x27, #880]
    ldr x1, [x27, #888]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 358 13
    .loc 1 358 18
    .loc 1 358 15
    movz w9, #0x1
    movz w10, #0x1
    lsl w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 358 9
    ldr w9, [x27, #5168]
    str w9, [x27, #896]
    .loc 1 359 12
    .loc 1 359 17
    .loc 1 359 14
    ldr w9, [x27, #896]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 359 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #904]
    .loc 1 359 20
    .loc 1 359 5
    adrp x9, ___cert__entry_main____str34@PAGE
    add x9, x9, ___cert__entry_main____str34@PAGEOFF
    str x9, [x27, #912]
    ldrsb w0, [x27, #904]
    ldr x1, [x27, #912]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 361 13
    .loc 1 361 18
    .loc 1 361 15
    movz w9, #0x4
    movz w10, #0x1
    asr w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 361 9
    ldr w9, [x27, #5168]
    str w9, [x27, #920]
    .loc 1 362 12
    .loc 1 362 17
    .loc 1 362 14
    ldr w9, [x27, #920]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 362 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #928]
    .loc 1 362 20
    .loc 1 362 5
    adrp x9, ___cert__entry_main____str35@PAGE
    add x9, x9, ___cert__entry_main____str35@PAGEOFF
    str x9, [x27, #936]
    ldrsb w0, [x27, #928]
    ldr x1, [x27, #936]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 364 14
    .loc 1 364 8
    adrp x9, ___cert__entry_main____str36@PAGE
    add x9, x9, ___cert__entry_main____str36@PAGEOFF
    str x9, [x27, #944]
    .loc 1 364 52
    .loc 1 364 8
    adrp x9, ___cert__entry_main____str37@PAGE
    add x9, x9, ___cert__entry_main____str37@PAGEOFF
    str x9, [x27, #952]
    .loc 1 364 74
    .loc 1 364 8
    movz w9, #0x8
    str w9, [x27, #960]
    ldr x0, [x27, #944]
    ldr x1, [x27, #952]
    ldr w2, [x27, #960]
    sub sp, sp, #16
    ldr x9, [x27, #952]
    str x9, [sp, #0]
    ldr w9, [x27, #960]
    sxtw x9, w9
    str x9, [sp, #8]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 366 5
    bl _differentsig
    .loc 1 368 5
    bl _differentsig
    .loc 1 370 21
    .loc 1 370 12
    movz w0, #0xa
    bl _Test_Program_rawthing_i32
    str w0, [x27, #5168]
    .loc 1 370 28
    .loc 1 370 25
    ldr w9, [x27, #5168]
    movz w10, #0x1d2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 370 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #968]
    .loc 1 370 33
    .loc 1 370 5
    adrp x9, ___cert__entry_main____str38@PAGE
    add x9, x9, ___cert__entry_main____str38@PAGEOFF
    str x9, [x27, #976]
    ldrsb w0, [x27, #968]
    ldr x1, [x27, #976]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 372 14
    .loc 1 372 8
    adrp x9, ___cert__entry_main____str39@PAGE
    add x9, x9, ___cert__entry_main____str39@PAGEOFF
    str x9, [x27, #984]
    .loc 1 372 55
    .loc 1 372 46
    movz w0, #0x29
    bl _LibTest_test_fun_i32
    str w0, [x27, #5168]
    .loc 1 372 8
    ldr w9, [x27, #5168]
    str w9, [x27, #992]
    ldr x0, [x27, #984]
    ldr w1, [x27, #992]
    sub sp, sp, #16
    ldr w9, [x27, #992]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 374 14
    .loc 1 374 8
    adrp x9, ___cert__entry_main____str40@PAGE
    add x9, x9, ___cert__entry_main____str40@PAGEOFF
    str x9, [x27, #1000]
    .loc 1 374 66
    .loc 1 374 51
    movz w0, #0x32
    bl _LibTest_Another_other_test_fun_i32
    str w0, [x27, #5168]
    .loc 1 374 8
    ldr w9, [x27, #5168]
    str w9, [x27, #1008]
    ldr x0, [x27, #1000]
    ldr w1, [x27, #1008]
    sub sp, sp, #16
    ldr w9, [x27, #1008]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 376 20
    .loc 1 376 9
    movz w12, #0xf5c3
    movk w12, #0x4048, lsl #16
    fmov s0, w12
    str s0, [x27, #1016]
    .loc 1 377 21
    .loc 1 377 9
    movz x12, #0xde1b
    movk x12, #0x4283, lsl #16
    movk x12, #0xc0ca, lsl #32
    movk x12, #0x3ff3, lsl #48
    fmov d0, x12
    str d0, [x27, #1024]
    .loc 1 378 21
    add x9, x27, #1016
    str x9, [x27, #5168]
    .loc 1 378 10
    ldr x9, [x27, #5168]
    str x9, [x27, #1032]
    .loc 1 379 5
    .loc 1 379 17
    ldr x9, [x27, #1032]
    str x9, [x27, #5168]
    .loc 1 379 14
    .loc 1 379 17
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #1040]
    .loc 1 379 19
    .loc 1 379 17
    movz w12, #0xf5c3
    movk w12, #0x40c8, lsl #16
    fmov s0, w12
    str s0, [x27, #1048]
    ldr x9, [x27, #1040]
    ldr s0, [x27, #1048]
    str s0, [x9]
    .loc 1 380 14
    .loc 1 380 9
    movz w9, #0x4
    str w9, [x27, #1056]
    .loc 1 383 12
    .loc 1 383 18
    .loc 1 383 15
    ldr w9, [x27, #1056]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 383 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1064]
    .loc 1 383 21
    .loc 1 383 5
    adrp x9, ___cert__entry_main____str41@PAGE
    add x9, x9, ___cert__entry_main____str41@PAGEOFF
    str x9, [x27, #1072]
    ldrsb w0, [x27, #1064]
    ldr x1, [x27, #1072]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 384 12
    .loc 1 384 20
    ldr x9, [x27, #1032]
    str x9, [x27, #5168]
    .loc 1 384 21
    .loc 1 384 20
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 384 27
    .loc 1 384 24
    ldr s0, [x27, #5168]
    movz w12, #0xf5c3
    movk w12, #0x40c8, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 384 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1080]
    .loc 1 384 34
    .loc 1 384 5
    adrp x9, ___cert__entry_main____str42@PAGE
    add x9, x9, ___cert__entry_main____str42@PAGEOFF
    str x9, [x27, #1088]
    ldrsb w0, [x27, #1080]
    ldr x1, [x27, #1088]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 385 12
    .loc 1 385 25
    .loc 1 385 22
    ldr d0, [x27, #1024]
    movz x12, #0xde1b
    movk x12, #0x4283, lsl #16
    movk x12, #0xc0ca, lsl #32
    movk x12, #0x3ff3, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 385 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1096]
    .loc 1 385 37
    .loc 1 385 5
    adrp x9, ___cert__entry_main____str43@PAGE
    add x9, x9, ___cert__entry_main____str43@PAGEOFF
    str x9, [x27, #1104]
    ldrsb w0, [x27, #1096]
    ldr x1, [x27, #1104]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 386 14
    .loc 1 386 8
    adrp x9, ___cert__entry_main____str44@PAGE
    add x9, x9, ___cert__entry_main____str44@PAGEOFF
    str x9, [x27, #1112]
    .loc 1 386 66
    .loc 1 386 79
    .loc 1 386 88
    ldr w9, [x27, #1016]
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #5176]
    .loc 1 386 76
    ldr d0, [x27, #1024]
    ldr d1, [x27, #5176]
    fadd d0, d0, d1
    str d0, [x27, #5168]
    .loc 1 386 8
    ldr d0, [x27, #5168]
    str d0, [x27, #1120]
    ldr x0, [x27, #1112]
    ldr d0, [x27, #1120]
    sub sp, sp, #16
    ldr d15, [x27, #1120]
    str d15, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 387 15
    .loc 1 387 28
    .loc 1 387 37
    ldr w9, [x27, #1016]
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #5176]
    .loc 1 387 25
    ldr d0, [x27, #1024]
    ldr d1, [x27, #5176]
    fadd d0, d0, d1
    str d0, [x27, #5168]
    .loc 1 387 9
    ldr d0, [x27, #5168]
    str d0, [x27, #1128]
    .loc 1 388 16
    .loc 1 388 22
    .loc 1 388 20
    ldr d0, [x27, #1128]
    movz x12, #0x45c3
    movk x12, #0xe9ee, lsl #16
    movk x12, #0xeea, lsl #32
    movk x12, #0x401e, lsl #48
    fmov d1, x12
    fsub d0, d0, d1
    str d0, [x27, #5168]
    .loc 1 388 9
    ldr d0, [x27, #5168]
    str d0, [x27, #1136]
    .loc 1 389 9
    .loc 1 389 16
    .loc 1 389 14
    ldr d0, [x27, #1136]
    mov x12, xzr
    fmov d1, x12
    fcmp d0, d1
    cset w9, lt
    strb w9, [x27, #5168]
    .loc 1 389 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true22
    b L__cert__entry_main__if_end23
L__cert__entry_main__if_true22:
    .loc 1 389 28
    .loc 1 389 29
    .loc 1 389 28
    mov x12, xzr
    fmov d0, x12
    ldr d1, [x27, #1136]
    fsub d0, d0, d1
    str d0, [x27, #5168]
    .loc 1 389 26
    ldr d0, [x27, #5168]
    str d0, [x27, #1136]
    .loc 1 389 5
L__cert__entry_main__if_end23:
    .loc 1 390 12
    .loc 1 390 19
    .loc 1 390 17
    ldr d0, [x27, #1136]
    movz x12, #0xed8d
    movk x12, #0xa0b5, lsl #16
    movk x12, #0xc6f7, lsl #32
    movk x12, #0x3eb0, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, lt
    strb w9, [x27, #5168]
    .loc 1 390 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1144]
    .loc 1 390 25
    .loc 1 390 5
    adrp x9, ___cert__entry_main____str45@PAGE
    add x9, x9, ___cert__entry_main____str45@PAGEOFF
    str x9, [x27, #1152]
    ldrsb w0, [x27, #1144]
    ldr x1, [x27, #1152]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 391 14
    .loc 1 391 19
    ldr x9, [x27, #560]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 391 38
    .loc 1 391 46
    ldr x9, [x27, #600]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldrb w10, [x9]
    strb w10, [x27, #5176]
    .loc 1 391 57
    ldrb w9, [x27, #5176]
    uxtb w9, w9
    str w9, [x27, #5176]
    .loc 1 391 35
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 391 69
    .loc 1 391 66
    ldr w9, [x27, #5168]
    movz w10, #0xa8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 391 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1160]
    .loc 1 391 74
    .loc 1 391 5
    adrp x9, ___cert__entry_main____str46@PAGE
    add x9, x9, ___cert__entry_main____str46@PAGEOFF
    str x9, [x27, #1168]
    ldrsb w0, [x27, #1160]
    ldr x1, [x27, #1168]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 393 21
    .loc 1 393 10
    movz x12, #0xda72
    movk x12, #0xb8f8, lsl #16
    movk x12, #0x9adb, lsl #32
    movk x12, #0x3fbf, lsl #48
    fmov d0, x12
    str d0, [x27, #1176]
    .loc 1 394 21
    .loc 1 394 9
    movz x12, #0x999a
    movk x12, #0x9999, lsl #16
    movk x12, #0x9999, lsl #32
    movk x12, #0x3fb9, lsl #48
    fmov d0, x12
    str d0, [x27, #1184]
    .loc 1 395 22
    .loc 1 395 9
    movz w12, #0xcccd
    movk w12, #0x3dcc, lsl #16
    fmov s0, w12
    str s0, [x27, #1192]
    .loc 1 396 13
    .loc 1 396 26
    .loc 1 396 23
    ldr d0, [x27, #1184]
    movz x12, #0x999a
    movk x12, #0x9999, lsl #16
    movk x12, #0x9999, lsl #32
    movk x12, #0x3fb9, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 396 30
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs24
    b L__cert__entry_main__land_false25
L__cert__entry_main__land_rhs24:
    .loc 1 396 33
    .loc 1 396 47
    .loc 1 396 44
    ldr s0, [x27, #1192]
    movz w12, #0xcccd
    movk w12, #0x3dcc, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 396 30
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true26
    b L__cert__entry_main__land_false25
L__cert__entry_main__land_true26:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end27
L__cert__entry_main__land_false25:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end27
L__cert__entry_main__land_end27:
    .loc 1 396 52
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs28
    b L__cert__entry_main__land_false29
L__cert__entry_main__land_rhs28:
    .loc 1 396 55
    .loc 1 396 67
    .loc 1 396 64
    ldr d0, [x27, #1176]
    movz x12, #0xda72
    movk x12, #0xb8f8, lsl #16
    movk x12, #0x9adb, lsl #32
    movk x12, #0x3fbf, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 396 52
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true30
    b L__cert__entry_main__land_false29
L__cert__entry_main__land_true30:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end31
L__cert__entry_main__land_false29:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end31
L__cert__entry_main__land_end31:
    .loc 1 396 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #1200]
    .loc 1 396 87
    .loc 1 396 5
    adrp x9, ___cert__entry_main____str47@PAGE
    add x9, x9, ___cert__entry_main____str47@PAGEOFF
    str x9, [x27, #1208]
    ldrsb w0, [x27, #1200]
    ldr x1, [x27, #1208]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 399 22
    .loc 1 399 27
    ldr x9, [x27, #560]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 399 46
    .loc 1 399 54
    ldr x9, [x27, #600]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldrb w10, [x9]
    strb w10, [x27, #5176]
    .loc 1 399 65
    ldrb w9, [x27, #5176]
    uxtb w9, w9
    str w9, [x27, #5176]
    .loc 1 399 43
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 399 9
    ldr w9, [x27, #5168]
    str w9, [x27, #1216]
    .loc 1 400 12
    .loc 1 400 25
    .loc 1 400 22
    ldr w9, [x27, #1216]
    movz w10, #0xa8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 400 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1224]
    .loc 1 400 30
    .loc 1 400 5
    adrp x9, ___cert__entry_main____str48@PAGE
    add x9, x9, ___cert__entry_main____str48@PAGEOFF
    str x9, [x27, #1232]
    ldrsb w0, [x27, #1224]
    ldr x1, [x27, #1232]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 403 19
    .loc 1 403 28
    .loc 1 403 26
    movz w9, #0xc
    movz w10, #0xa
    and w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 403 9
    ldr w9, [x27, #5168]
    str w9, [x27, #1240]
    .loc 1 404 12
    .loc 1 404 23
    .loc 1 404 20
    ldr w9, [x27, #1240]
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 404 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1248]
    .loc 1 404 31
    .loc 1 404 5
    adrp x9, ___cert__entry_main____str49@PAGE
    add x9, x9, ___cert__entry_main____str49@PAGEOFF
    str x9, [x27, #1256]
    ldrsb w0, [x27, #1248]
    ldr x1, [x27, #1256]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 407 18
    .loc 1 407 27
    .loc 1 407 25
    movz w9, #0xc
    movz w10, #0xa
    orr w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 407 9
    ldr w9, [x27, #5168]
    str w9, [x27, #1264]
    .loc 1 408 12
    .loc 1 408 22
    .loc 1 408 19
    ldr w9, [x27, #1264]
    movz w10, #0xe
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 408 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1272]
    .loc 1 408 30
    .loc 1 408 5
    adrp x9, ___cert__entry_main____str50@PAGE
    add x9, x9, ___cert__entry_main____str50@PAGEOFF
    str x9, [x27, #1280]
    ldrsb w0, [x27, #1272]
    ldr x1, [x27, #1280]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 411 19
    .loc 1 411 28
    .loc 1 411 26
    movz w9, #0xc
    movz w10, #0xa
    eor w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 411 9
    ldr w9, [x27, #5168]
    str w9, [x27, #1288]
    .loc 1 412 12
    .loc 1 412 23
    .loc 1 412 20
    ldr w9, [x27, #1288]
    movz w10, #0x6
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 412 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1296]
    .loc 1 412 31
    .loc 1 412 5
    adrp x9, ___cert__entry_main____str51@PAGE
    add x9, x9, ___cert__entry_main____str51@PAGEOFF
    str x9, [x27, #1304]
    ldrsb w0, [x27, #1296]
    ldr x1, [x27, #1304]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 415 20
    .loc 1 415 19
    movz w9, #0xf
    mvn w9, w9
    str w9, [x27, #5168]
    .loc 1 415 9
    ldr w9, [x27, #5168]
    str w9, [x27, #1312]
    .loc 1 416 12
    .loc 1 416 23
    .loc 1 416 24
    .loc 1 416 23
    mov w9, wzr
    movz w10, #0x10
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 416 20
    ldr w9, [x27, #1312]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 416 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1320]
    .loc 1 416 28
    .loc 1 416 5
    adrp x9, ___cert__entry_main____str52@PAGE
    add x9, x9, ___cert__entry_main____str52@PAGEOFF
    str x9, [x27, #1328]
    ldrsb w0, [x27, #1320]
    ldr x1, [x27, #1328]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 418 25
    .loc 1 418 9
    mov w9, wzr
    str w9, [x27, #1336]
    .loc 1 419 20
    .loc 1 419 14
    mov w9, wzr
    str w9, [x27, #1344]
    .loc 1 1 1
L__cert__entry_main__while_cond32:
    .loc 1 419 23
    .loc 1 419 29
    .loc 1 419 27
    ldr w9, [x27, #1344]
    movz w10, #0xa
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body33
    b L__cert__entry_main__while_end34
L__cert__entry_main__while_body33:
    .loc 1 421 13
    .loc 1 421 20
    .loc 1 421 17
    ldr w9, [x27, #1344]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 421 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true36
    b L__cert__entry_main__if_end37
L__cert__entry_main__if_true36:
    .loc 1 422 13
    b L__cert__entry_main__while_end34
    .loc 1 421 9
L__cert__entry_main__if_end37:
    .loc 1 423 25
    .loc 1 423 41
    .loc 1 423 39
    ldr w9, [x27, #1336]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 423 23
    ldr w9, [x27, #5168]
    str w9, [x27, #1336]
    .loc 1 1 1
L__cert__entry_main__while_post35:
    .loc 1 419 39
    .loc 1 419 45
    .loc 1 419 43
    ldr w9, [x27, #1344]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 419 37
    ldr w9, [x27, #5168]
    str w9, [x27, #1344]
    .loc 1 1 1
    b L__cert__entry_main__while_cond32
L__cert__entry_main__while_end34:
    .loc 1 425 12
    .loc 1 425 29
    .loc 1 425 26
    ldr w9, [x27, #1336]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 425 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1352]
    .loc 1 425 32
    .loc 1 425 5
    adrp x9, ___cert__entry_main____str53@PAGE
    add x9, x9, ___cert__entry_main____str53@PAGEOFF
    str x9, [x27, #1360]
    ldrsb w0, [x27, #1352]
    ldr x1, [x27, #1360]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 427 26
    .loc 1 427 9
    mov w9, wzr
    str w9, [x27, #1368]
    .loc 1 428 21
    .loc 1 428 14
    mov w9, wzr
    str w9, [x27, #1376]
    .loc 1 1 1
L__cert__entry_main__while_cond38:
    .loc 1 428 24
    .loc 1 428 31
    .loc 1 428 29
    ldr w9, [x27, #1376]
    movz w10, #0x6
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body39
    b L__cert__entry_main__while_end40
L__cert__entry_main__while_body39:
    .loc 1 430 13
    .loc 1 430 21
    .loc 1 430 18
    ldr w9, [x27, #1376]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 430 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true42
    b L__cert__entry_main__if_end43
L__cert__entry_main__if_true42:
    .loc 1 431 13
    b L__cert__entry_main__while_post41
    .loc 1 430 9
L__cert__entry_main__if_end43:
    .loc 1 432 26
    .loc 1 432 43
    .loc 1 432 41
    ldr w9, [x27, #1368]
    ldr w10, [x27, #1376]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 432 24
    ldr w9, [x27, #5168]
    str w9, [x27, #1368]
    .loc 1 1 1
L__cert__entry_main__while_post41:
    .loc 1 428 41
    .loc 1 428 48
    .loc 1 428 46
    ldr w9, [x27, #1376]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 428 39
    ldr w9, [x27, #5168]
    str w9, [x27, #1376]
    .loc 1 1 1
    b L__cert__entry_main__while_cond38
L__cert__entry_main__while_end40:
    .loc 1 434 12
    .loc 1 434 30
    .loc 1 434 34
    .loc 1 434 32
    mov w9, wzr
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 434 38
    .loc 1 434 36
    ldr w9, [x27, #5176]
    movz w10, #0x3
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 434 42
    .loc 1 434 40
    ldr w9, [x27, #5176]
    movz w10, #0x4
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 434 46
    .loc 1 434 44
    ldr w9, [x27, #5176]
    movz w10, #0x5
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 434 27
    ldr w9, [x27, #1368]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 434 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1384]
    .loc 1 434 49
    .loc 1 434 5
    adrp x9, ___cert__entry_main____str54@PAGE
    add x9, x9, ___cert__entry_main____str54@PAGEOFF
    str x9, [x27, #1392]
    ldrsb w0, [x27, #1384]
    ldr x1, [x27, #1392]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 436 26
    .loc 1 436 9
    mov w9, wzr
    str w9, [x27, #1400]
    .loc 1 437 25
    .loc 1 437 14
    mov w9, wzr
    str w9, [x27, #1408]
    .loc 1 1 1
L__cert__entry_main__while_cond44:
    .loc 1 437 28
    .loc 1 437 39
    .loc 1 437 37
    ldr w9, [x27, #1408]
    movz w10, #0x3
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body45
    b L__cert__entry_main__while_end46
L__cert__entry_main__while_body45:
    .loc 1 439 26
    .loc 1 439 43
    .loc 1 439 41
    ldr w9, [x27, #1400]
    ldr w10, [x27, #1408]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 439 24
    ldr w9, [x27, #5168]
    str w9, [x27, #1400]
    .loc 1 1 1
L__cert__entry_main__while_post47:
    .loc 1 437 53
    .loc 1 437 64
    .loc 1 437 62
    ldr w9, [x27, #1408]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 437 51
    ldr w9, [x27, #5168]
    str w9, [x27, #1408]
    .loc 1 1 1
    b L__cert__entry_main__while_cond44
L__cert__entry_main__while_end46:
    .loc 1 441 12
    .loc 1 441 30
    .loc 1 441 34
    .loc 1 441 32
    mov w9, wzr
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 441 38
    .loc 1 441 36
    ldr w9, [x27, #5176]
    movz w10, #0x2
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 441 27
    ldr w9, [x27, #1400]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 441 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1416]
    .loc 1 441 41
    .loc 1 441 5
    adrp x9, ___cert__entry_main____str55@PAGE
    add x9, x9, ___cert__entry_main____str55@PAGEOFF
    str x9, [x27, #1424]
    ldrsb w0, [x27, #1416]
    ldr x1, [x27, #1424]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 443 21
    .loc 1 443 9
    mov w9, wzr
    str w9, [x27, #1432]
    .loc 1 444 13
    .loc 1 444 9
    mov w9, wzr
    str w9, [x27, #1440]
    .loc 1 1 1
L__cert__entry_main__while_cond48:
    .loc 1 445 12
    .loc 1 445 16
    .loc 1 445 14
    ldr w9, [x27, #1440]
    movz w10, #0x6
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body49
    b L__cert__entry_main__while_end50
L__cert__entry_main__while_body49:
    .loc 1 447 13
    .loc 1 447 17
    .loc 1 447 15
    ldr w9, [x27, #1440]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 447 11
    ldr w9, [x27, #5168]
    str w9, [x27, #1440]
    .loc 1 448 14
    .loc 1 448 18
    .loc 1 448 16
    ldr w9, [x27, #1440]
    movz w10, #0x2
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #5168]
    .loc 1 448 24
    .loc 1 448 21
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 448 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true51
    b L__cert__entry_main__if_end52
L__cert__entry_main__if_true51:
    .loc 1 449 13
    b L__cert__entry_main__while_cond48
    .loc 1 448 9
L__cert__entry_main__if_end52:
    .loc 1 450 21
    .loc 1 450 33
    .loc 1 450 31
    ldr w9, [x27, #1432]
    ldr w10, [x27, #1440]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 450 19
    ldr w9, [x27, #5168]
    str w9, [x27, #1432]
    .loc 1 451 13
    .loc 1 451 18
    .loc 1 451 15
    ldr w9, [x27, #1440]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 451 9
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true53
    b L__cert__entry_main__if_end54
L__cert__entry_main__if_true53:
    .loc 1 452 13
    b L__cert__entry_main__while_end50
    .loc 1 451 9
L__cert__entry_main__if_end54:
    .loc 1 1 1
    b L__cert__entry_main__while_cond48
L__cert__entry_main__while_end50:
    .loc 1 454 12
    .loc 1 454 25
    .loc 1 454 29
    .loc 1 454 27
    movz w9, #0x1
    movz w10, #0x3
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 454 33
    .loc 1 454 31
    ldr w9, [x27, #5176]
    movz w10, #0x5
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 454 22
    ldr w9, [x27, #1432]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 454 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1448]
    .loc 1 454 36
    .loc 1 454 5
    adrp x9, ___cert__entry_main____str56@PAGE
    add x9, x9, ___cert__entry_main____str56@PAGEOFF
    str x9, [x27, #1456]
    ldrsb w0, [x27, #1448]
    ldr x1, [x27, #1456]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 456 25
    .loc 1 456 9
    movz w9, #0x3039
    strh w9, [x27, #1464]
    .loc 1 458 35
    .loc 1 458 29
    movz w0, #0x11
    bl _Std_Memory_alloc_i32
    str x0, [x27, #5168]
    .loc 1 458 11
    ldr x9, [x27, #5168]
    str x9, [x27, #1472]
    .loc 1 459 19
    .loc 1 459 12
    ldr x9, [x27, #1472]
    str x9, [x27, #1480]
    .loc 1 459 38
    .loc 1 459 12
    mov w9, wzr
    strb w9, [x27, #1488]
    .loc 1 459 41
    .loc 1 459 12
    movz w9, #0x11
    str w9, [x27, #1496]
    ldr x0, [x27, #1480]
    ldrb w1, [x27, #1488]
    ldr w2, [x27, #1496]
    bl _Std_Memory_memset_ptr_to_void_u8_i32
    str x0, [x27, #5168]
    .loc 1 462 18
    .loc 1 462 14
    movz w9, #0xf
    str w9, [x27, #1504]
    .loc 1 1 1
L__cert__entry_main__while_cond55:
    .loc 1 462 22
    .loc 1 462 27
    .loc 1 462 24
    ldr w9, [x27, #1504]
    mov w10, wzr
    cmp w9, w10
    cset w11, ge
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body56
    b L__cert__entry_main__while_end57
L__cert__entry_main__while_body56:
    .loc 1 464 20
    .loc 1 464 37
    .loc 1 464 34
    ldrh w9, [x27, #1464]
    ldr w10, [x27, #1504]
    lsr w9, w9, w10
    uxth w9, w9
    strh w9, [x27, #5168]
    .loc 1 464 42
    .loc 1 464 40
    ldrh w9, [x27, #5168]
    movz w10, #0x1
    and w9, w9, w10
    uxth w9, w9
    strh w9, [x27, #5168]
    .loc 1 464 13
    ldrh w9, [x27, #5168]
    strh w9, [x27, #1512]
    .loc 1 465 9
    .loc 1 465 26
    ldr x9, [x27, #1472]
    str x9, [x27, #5168]
    .loc 1 465 18
    .loc 1 465 23
    .loc 1 465 21
    movz w9, #0xf
    ldr w10, [x27, #1504]
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 465 26
    ldr w9, [x27, #5176]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #1520]
    .loc 1 465 32
    .loc 1 465 36
    ldrh w9, [x27, #1512]
    uxth w9, w9
    str w9, [x27, #5168]
    .loc 1 465 39
    .loc 1 465 36
    ldr w9, [x27, #5168]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 465 28
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__cond_true59
    b L__cert__entry_main__cond_false60
L__cert__entry_main__cond_true59:
    .loc 1 465 42
    .loc 1 465 28
    movz w11, #0x31
    strb w11, [x27, #5168]
    b L__cert__entry_main__cond_end61
L__cert__entry_main__cond_false60:
    .loc 1 465 51
    .loc 1 465 28
    movz w11, #0x30
    strb w11, [x27, #5168]
    b L__cert__entry_main__cond_end61
L__cert__entry_main__cond_end61:
    .loc 1 465 26
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #1528]
    ldr x9, [x27, #1520]
    ldrsb w10, [x27, #1528]
    strb w10, [x9]
    .loc 1 1 1
L__cert__entry_main__while_post58:
    .loc 1 462 34
    .loc 1 462 38
    .loc 1 462 36
    ldr w9, [x27, #1504]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 462 32
    ldr w9, [x27, #5168]
    str w9, [x27, #1504]
    .loc 1 1 1
    b L__cert__entry_main__while_cond55
L__cert__entry_main__while_end57:
    .loc 1 468 19
    .loc 1 468 12
    ldr x9, [x27, #1472]
    str x9, [x27, #1536]
    .loc 1 468 29
    .loc 1 468 12
    adrp x9, ___cert__entry_main____str57@PAGE
    add x9, x9, ___cert__entry_main____str57@PAGEOFF
    str x9, [x27, #1544]
    ldr x0, [x27, #1536]
    ldr x1, [x27, #1544]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 468 52
    .loc 1 468 49
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 468 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1552]
    .loc 1 468 55
    .loc 1 468 5
    adrp x9, ___cert__entry_main____str58@PAGE
    add x9, x9, ___cert__entry_main____str58@PAGEOFF
    str x9, [x27, #1560]
    ldrsb w0, [x27, #1552]
    ldr x1, [x27, #1560]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 470 12
    .loc 1 470 18
    .loc 1 470 15
    movz w9, #0x63
    movz w10, #0x63
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 470 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1568]
    .loc 1 470 22
    .loc 1 470 5
    adrp x9, ___cert__entry_main____str59@PAGE
    add x9, x9, ___cert__entry_main____str59@PAGEOFF
    str x9, [x27, #1576]
    ldrsb w0, [x27, #1568]
    ldr x1, [x27, #1576]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 471 14
    .loc 1 471 21
    .loc 1 471 18
    movz w9, #0x14
    movz w10, #0x16
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 471 29
    .loc 1 471 26
    ldr w9, [x27, #5168]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 471 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1584]
    .loc 1 471 33
    .loc 1 471 5
    adrp x9, ___cert__entry_main____str60@PAGE
    add x9, x9, ___cert__entry_main____str60@PAGEOFF
    str x9, [x27, #1592]
    ldrsb w0, [x27, #1584]
    ldr x1, [x27, #1592]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 472 14
    .loc 1 472 20
    .loc 1 472 17
    movz w9, #0x5
    movz w10, #0x5
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 472 27
    .loc 1 472 24
    ldr w9, [x27, #5168]
    movz w10, #0x19
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 472 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1600]
    .loc 1 472 31
    .loc 1 472 5
    adrp x9, ___cert__entry_main____str61@PAGE
    add x9, x9, ___cert__entry_main____str61@PAGEOFF
    str x9, [x27, #1608]
    ldrsb w0, [x27, #1600]
    ldr x1, [x27, #1608]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 473 12
    .loc 1 473 18
    .loc 1 473 15
    movz w9, #0x63
    movz w10, #0x63
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 473 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1616]
    .loc 1 473 22
    .loc 1 473 5
    adrp x9, ___cert__entry_main____str62@PAGE
    add x9, x9, ___cert__entry_main____str62@PAGEOFF
    str x9, [x27, #1624]
    ldrsb w0, [x27, #1616]
    ldr x1, [x27, #1624]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 474 12
    .loc 1 474 18
    .loc 1 474 15
    movz w9, #0x1e
    movz w10, #0x1e
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 474 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1632]
    .loc 1 474 22
    .loc 1 474 5
    adrp x9, ___cert__entry_main____str63@PAGE
    add x9, x9, ___cert__entry_main____str63@PAGEOFF
    str x9, [x27, #1640]
    ldrsb w0, [x27, #1632]
    ldr x1, [x27, #1640]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 475 12
    .loc 1 475 19
    .loc 1 475 16
    movz w9, #0x1c8
    movz w10, #0x1c8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 475 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1648]
    .loc 1 475 24
    .loc 1 475 5
    adrp x9, ___cert__entry_main____str64@PAGE
    add x9, x9, ___cert__entry_main____str64@PAGEOFF
    str x9, [x27, #1656]
    ldrsb w0, [x27, #1648]
    ldr x1, [x27, #1656]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 477 12
    .loc 1 477 17
    .loc 1 477 14
    movz w9, #0x1
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 477 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1664]
    .loc 1 477 20
    .loc 1 477 5
    adrp x9, ___cert__entry_main____str65@PAGE
    add x9, x9, ___cert__entry_main____str65@PAGEOFF
    str x9, [x27, #1672]
    ldrsb w0, [x27, #1664]
    ldr x1, [x27, #1672]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 478 12
    .loc 1 478 22
    .loc 1 478 26
    .loc 1 478 24
    movz w11, #0x5014
    str w11, [x27, #5168]
    movz w9, #0x2
    movz w10, #0x2710
    mul w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 478 34
    .loc 1 478 38
    .loc 1 478 36
    movz w9, #0x5
    movz w10, #0x64
    mul w9, w9, w10
    str w9, [x27, #5184]
    .loc 1 478 32
    ldr w9, [x27, #5176]
    ldr w10, [x27, #5184]
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 478 44
    .loc 1 478 42
    ldr w9, [x27, #5176]
    mov w10, wzr
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 478 18
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 478 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1680]
    .loc 1 478 48
    .loc 1 478 5
    adrp x9, ___cert__entry_main____str66@PAGE
    add x9, x9, ___cert__entry_main____str66@PAGEOFF
    str x9, [x27, #1688]
    ldrsb w0, [x27, #1680]
    ldr x1, [x27, #1688]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 479 16
    .loc 1 479 8
    adrp x9, ___cert__entry_main____str67@PAGE
    add x9, x9, ___cert__entry_main____str67@PAGEOFF
    str x9, [x27, #1696]
    .loc 1 479 39
    .loc 1 479 8
    adrp x9, ___cert__entry_main____str68@PAGE
    add x9, x9, ___cert__entry_main____str68@PAGEOFF
    str x9, [x27, #1704]
    ldr x0, [x27, #1696]
    ldr x1, [x27, #1704]
    sub sp, sp, #16
    ldr x9, [x27, #1704]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_printnl_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 480 16
    .loc 1 480 8
    adrp x9, ___cert__entry_main____str69@PAGE
    add x9, x9, ___cert__entry_main____str69@PAGEOFF
    str x9, [x27, #1712]
    .loc 1 480 30
    .loc 1 480 8
    movz w9, #0x1c
    str w9, [x27, #1720]
    ldr x0, [x27, #1712]
    ldr w1, [x27, #1720]
    sub sp, sp, #16
    ldr w9, [x27, #1720]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_printnl_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 481 12
    .loc 1 481 19
    .loc 1 481 35
    .loc 1 481 33
    movz w11, #0x40
    str w11, [x27, #5168]
    movz w9, #0x8
    movz w10, #0x8
    mul w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 481 15
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 481 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1728]
    .loc 1 481 39
    .loc 1 481 5
    adrp x9, ___cert__entry_main____str70@PAGE
    add x9, x9, ___cert__entry_main____str70@PAGEOFF
    str x9, [x27, #1736]
    ldrsb w0, [x27, #1728]
    ldr x1, [x27, #1736]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 482 9
    .loc 1 482 26
    .loc 1 482 23
    movz w9, #0x8
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 482 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true62
    b L__cert__entry_main__if_false63
L__cert__entry_main__if_true62:
    .loc 1 484 16
    .loc 1 484 21
    .loc 1 484 18
    movz w9, #0x1
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 484 9
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1744]
    .loc 1 484 24
    .loc 1 484 9
    adrp x9, ___cert__entry_main____str71@PAGE
    add x9, x9, ___cert__entry_main____str71@PAGEOFF
    str x9, [x27, #1752]
    ldrsb w0, [x27, #1744]
    ldr x1, [x27, #1752]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 482 5
    b L__cert__entry_main__if_end64
L__cert__entry_main__if_false63:
    .loc 1 488 16
    .loc 1 488 21
    .loc 1 488 18
    movz w9, #0x1
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 488 9
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1760]
    .loc 1 488 24
    .loc 1 488 9
    adrp x9, ___cert__entry_main____str72@PAGE
    add x9, x9, ___cert__entry_main____str72@PAGEOFF
    str x9, [x27, #1768]
    ldrsb w0, [x27, #1760]
    ldr x1, [x27, #1768]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 482 5
L__cert__entry_main__if_end64:
    .loc 1 495 12
    .loc 1 495 17
    .loc 1 495 14
    movz w9, #0x1
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 495 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1776]
    .loc 1 495 20
    .loc 1 495 5
    adrp x9, ___cert__entry_main____str73@PAGE
    add x9, x9, ___cert__entry_main____str73@PAGEOFF
    str x9, [x27, #1784]
    ldrsb w0, [x27, #1776]
    ldr x1, [x27, #1784]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 500 21
    .loc 1 500 11
    movz w9, #0x3
    strb w9, [x27, #1792]
    .loc 1 502 25
    .loc 1 502 12
    adrp x9, ___cert__entry_main____str74@PAGE
    add x9, x9, ___cert__entry_main____str74@PAGEOFF
    str x9, [x27, #1800]
    .loc 1 503 12
    .loc 1 503 22
    ldr x9, [x27, #1800]
    str x9, [x27, #5168]
    .loc 1 503 23
    .loc 1 503 22
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrsb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 503 26
    ldrsb w9, [x27, #5168]
    sxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 503 29
    .loc 1 503 26
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 503 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1808]
    .loc 1 503 32
    .loc 1 503 5
    adrp x9, ___cert__entry_main____str75@PAGE
    add x9, x9, ___cert__entry_main____str75@PAGEOFF
    str x9, [x27, #1816]
    ldrsb w0, [x27, #1808]
    ldr x1, [x27, #1816]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 504 27
    .loc 1 504 12
    adrp x9, ___cert__entry_main____str76@PAGE
    add x9, x9, ___cert__entry_main____str76@PAGEOFF
    str x9, [x27, #1824]
    .loc 1 505 19
    .loc 1 505 12
    ldr x9, [x27, #1824]
    str x9, [x27, #1832]
    .loc 1 505 33
    .loc 1 505 12
    adrp x9, ___cert__entry_main____str77@PAGE
    add x9, x9, ___cert__entry_main____str77@PAGEOFF
    str x9, [x27, #1840]
    ldr x0, [x27, #1832]
    ldr x1, [x27, #1840]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 505 52
    .loc 1 505 49
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 505 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1848]
    .loc 1 505 55
    .loc 1 505 5
    adrp x9, ___cert__entry_main____str78@PAGE
    add x9, x9, ___cert__entry_main____str78@PAGEOFF
    str x9, [x27, #1856]
    ldrsb w0, [x27, #1848]
    ldr x1, [x27, #1856]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 506 22
    .loc 1 506 9
    movz w9, #0x1fa
    str w9, [x27, #1864]
    .loc 1 507 12
    .loc 1 507 25
    .loc 1 507 23
    ldr w9, [x27, #1864]
    mov w10, wzr
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #5168]
    .loc 1 507 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1872]
    .loc 1 507 28
    .loc 1 507 5
    adrp x9, ___cert__entry_main____str79@PAGE
    add x9, x9, ___cert__entry_main____str79@PAGEOFF
    str x9, [x27, #1880]
    ldrsb w0, [x27, #1872]
    ldr x1, [x27, #1880]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 508 25
    .loc 1 508 12
    adrp x9, ___cert__entry_main____str80@PAGE
    add x9, x9, ___cert__entry_main____str80@PAGEOFF
    str x9, [x27, #1888]
    .loc 1 509 12
    .loc 1 509 22
    ldr x9, [x27, #1888]
    str x9, [x27, #5168]
    .loc 1 509 23
    .loc 1 509 22
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrsb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 509 26
    ldrsb w9, [x27, #5168]
    sxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 509 29
    .loc 1 509 26
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 509 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1896]
    .loc 1 509 32
    .loc 1 509 5
    adrp x9, ___cert__entry_main____str81@PAGE
    add x9, x9, ___cert__entry_main____str81@PAGEOFF
    str x9, [x27, #1904]
    ldrsb w0, [x27, #1896]
    ldr x1, [x27, #1904]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 510 25
    .loc 1 510 12
    adrp x9, ___cert__entry_main____str82@PAGE
    add x9, x9, ___cert__entry_main____str82@PAGEOFF
    str x9, [x27, #1912]
    .loc 1 511 12
    .loc 1 511 22
    ldr x9, [x27, #1912]
    str x9, [x27, #5168]
    .loc 1 511 23
    .loc 1 511 22
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrsb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 511 26
    ldrsb w9, [x27, #5168]
    sxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 511 29
    .loc 1 511 26
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 511 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1920]
    .loc 1 511 32
    .loc 1 511 5
    adrp x9, ___cert__entry_main____str83@PAGE
    add x9, x9, ___cert__entry_main____str83@PAGEOFF
    str x9, [x27, #1928]
    ldrsb w0, [x27, #1920]
    ldr x1, [x27, #1928]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 512 21
    .loc 1 512 9
    mov w9, wzr
    str w9, [x27, #1936]
    .loc 1 513 21
    .loc 1 513 9
    movz w9, #0x1
    str w9, [x27, #1944]
    .loc 1 514 12
    .loc 1 514 25
    .loc 1 514 37
    .loc 1 514 35
    ldr w9, [x27, #1936]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 514 22
    ldr w9, [x27, #1944]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 514 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1952]
    .loc 1 514 40
    .loc 1 514 5
    adrp x9, ___cert__entry_main____str84@PAGE
    add x9, x9, ___cert__entry_main____str84@PAGEOFF
    str x9, [x27, #1960]
    ldrsb w0, [x27, #1952]
    ldr x1, [x27, #1960]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 515 19
    .loc 1 515 12
    adrp x9, ___cert__entry_main____str85@PAGE
    add x9, x9, ___cert__entry_main____str85@PAGEOFF
    str x9, [x27, #1968]
    .loc 1 515 33
    .loc 1 515 12
    adrp x9, ___cert__entry_main____str86@PAGE
    add x9, x9, ___cert__entry_main____str86@PAGEOFF
    str x9, [x27, #1976]
    ldr x0, [x27, #1968]
    ldr x1, [x27, #1976]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 515 44
    .loc 1 515 41
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 515 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #1984]
    .loc 1 515 47
    .loc 1 515 5
    adrp x9, ___cert__entry_main____str87@PAGE
    add x9, x9, ___cert__entry_main____str87@PAGEOFF
    str x9, [x27, #1992]
    ldrsb w0, [x27, #1984]
    ldr x1, [x27, #1992]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 516 27
    .loc 1 516 12
    adrp x9, ___cert__entry_main____str88@PAGE
    add x9, x9, ___cert__entry_main____str88@PAGEOFF
    str x9, [x27, #2000]
    .loc 1 516 38
    .loc 1 516 12
    adrp x9, ___cert__entry_main____str89@PAGE
    add x9, x9, ___cert__entry_main____str89@PAGEOFF
    str x9, [x27, #2008]
    ldr x0, [x27, #2000]
    ldr x1, [x27, #2008]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 516 55
    .loc 1 516 52
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 516 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2016]
    .loc 1 516 58
    .loc 1 516 5
    adrp x9, ___cert__entry_main____str90@PAGE
    add x9, x9, ___cert__entry_main____str90@PAGEOFF
    str x9, [x27, #2024]
    ldrsb w0, [x27, #2016]
    ldr x1, [x27, #2024]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 518 19
    bl _LibTest_Another_get_info
    str x0, [x27, #5168]
    .loc 1 518 12
    ldr x9, [x27, #5168]
    str x9, [x27, #2032]
    .loc 1 518 31
    .loc 1 518 12
    adrp x9, ___cert__entry_main____str91@PAGE
    add x9, x9, ___cert__entry_main____str91@PAGEOFF
    str x9, [x27, #2040]
    ldr x0, [x27, #2032]
    ldr x1, [x27, #2040]
    bl _Test_Program_strcmp_ptr_to_char_ptr_to_char
    str w0, [x27, #5168]
    .loc 1 518 65
    .loc 1 518 62
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 518 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2048]
    .loc 1 518 68
    .loc 1 518 5
    adrp x9, ___cert__entry_main____str92@PAGE
    add x9, x9, ___cert__entry_main____str92@PAGEOFF
    str x9, [x27, #2056]
    ldrsb w0, [x27, #2048]
    ldr x1, [x27, #2056]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 520 36
    .loc 1 520 23
    movz w9, #0x5
    str w9, [x27, #2072]
    .loc 1 520 39
    .loc 1 520 23
    movz w9, #0x3039
    str w9, [x27, #2080]
    .loc 1 520 46
    .loc 1 520 23
    movz w9, #0xa
    str w9, [x27, #2088]
    .loc 1 520 50
    .loc 1 520 23
    movz w9, #0x14
    str w9, [x27, #2096]
    .loc 1 520 54
    .loc 1 520 23
    movz w9, #0x1e
    str w9, [x27, #2104]
    .loc 1 520 58
    .loc 1 520 23
    movz w9, #0x28
    str w9, [x27, #2112]
    .loc 1 520 62
    .loc 1 520 23
    movz w9, #0x32
    str w9, [x27, #2120]
    ldr w0, [x27, #2072]
    ldr w1, [x27, #2080]
    ldr w2, [x27, #2088]
    ldr w3, [x27, #2096]
    ldr w4, [x27, #2104]
    ldr w5, [x27, #2112]
    ldr w6, [x27, #2120]
    sub sp, sp, #48
    ldr w9, [x27, #2088]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr w9, [x27, #2096]
    sxtw x9, w9
    str x9, [sp, #8]
    ldr w9, [x27, #2104]
    sxtw x9, w9
    str x9, [sp, #16]
    ldr w9, [x27, #2112]
    sxtw x9, w9
    str x9, [sp, #24]
    ldr w9, [x27, #2120]
    sxtw x9, w9
    str x9, [sp, #32]
    mov x15, sp
    bl _LibTest_Another_test_varargs_i32_i32
    add sp, sp, #48
    str w0, [x27, #5168]
    .loc 1 520 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2064]
    .loc 1 521 12
    .loc 1 521 27
    .loc 1 521 24
    ldr w9, [x27, #2064]
    movz w10, #0x96
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 521 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2128]
    .loc 1 521 32
    .loc 1 521 5
    adrp x9, ___cert__entry_main____str93@PAGE
    add x9, x9, ___cert__entry_main____str93@PAGEOFF
    str x9, [x27, #2136]
    ldrsb w0, [x27, #2128]
    ldr x1, [x27, #2136]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 524 22
    .loc 1 524 5
    movz w9, #0xa
    str w9, [x27, #2144]
    .loc 1 524 26
    .loc 1 524 5
    movz w9, #0x4d
    str w9, [x27, #2152]
    .loc 1 524 30
    movz w9, #0xf5c3
    movk w9, #0x4048, lsl #16
    fmov s0, w9
    fcvt d0, s0
    fmov x9, d0
    str x9, [x27, #5168]
    .loc 1 524 5
    ldr d0, [x27, #5168]
    str d0, [x27, #2160]
    .loc 1 524 37
    .loc 1 524 5
    adrp x9, ___cert__entry_main____str94@PAGE
    add x9, x9, ___cert__entry_main____str94@PAGEOFF
    str x9, [x27, #2168]
    ldr w0, [x27, #2144]
    ldr w1, [x27, #2152]
    ldr d0, [x27, #2160]
    ldr x2, [x27, #2168]
    sub sp, sp, #32
    ldr w9, [x27, #2152]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr d15, [x27, #2160]
    str d15, [sp, #8]
    ldr x9, [x27, #2168]
    str x9, [sp, #16]
    mov x15, sp
    bl _LibTest_Another_test_diffvarargs_i32
    add sp, sp, #32
    .loc 1 525 26
    .loc 1 525 5
    mov w9, wzr
    str w9, [x27, #2176]
    .loc 1 525 29
    .loc 1 525 5
    movz w9, #0x58
    str w9, [x27, #2184]
    .loc 1 525 33
    .loc 1 525 5
    movz x12, #0xf790
    movk x12, #0x95aa, lsl #16
    movk x12, #0xbf09, lsl #32
    movk x12, #0x4005, lsl #48
    fmov d0, x12
    str d0, [x27, #2192]
    .loc 1 525 42
    .loc 1 525 5
    adrp x9, ___cert__entry_main____str95@PAGE
    add x9, x9, ___cert__entry_main____str95@PAGEOFF
    str x9, [x27, #2200]
    ldr w0, [x27, #2176]
    ldr w1, [x27, #2184]
    ldr d0, [x27, #2192]
    ldr x2, [x27, #2200]
    sub sp, sp, #32
    ldr w9, [x27, #2184]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr d15, [x27, #2192]
    str d15, [sp, #8]
    ldr x9, [x27, #2200]
    str x9, [sp, #16]
    mov x15, sp
    bl _Test_Program_internal_diffvarargs_i32_varargs
    add sp, sp, #32
    .loc 1 527 37
    .loc 1 527 26
    movz w9, #0xc
    str w9, [x27, #2216]
    .loc 1 527 41
    .loc 1 527 26
    movz w9, #0x1
    str w9, [x27, #2224]
    .loc 1 527 44
    .loc 1 527 26
    movz w9, #0x2
    str w9, [x27, #2232]
    .loc 1 527 47
    .loc 1 527 26
    movz w9, #0x3
    str w9, [x27, #2240]
    .loc 1 527 50
    .loc 1 527 26
    movz w9, #0x4
    str w9, [x27, #2248]
    .loc 1 527 53
    .loc 1 527 26
    movz w9, #0x5
    str w9, [x27, #2256]
    .loc 1 527 56
    .loc 1 527 26
    movz w9, #0x6
    str w9, [x27, #2264]
    .loc 1 527 59
    .loc 1 527 26
    movz w9, #0x7
    str w9, [x27, #2272]
    .loc 1 527 62
    .loc 1 527 26
    movz w9, #0x8
    str w9, [x27, #2280]
    .loc 1 527 65
    .loc 1 527 26
    movz w9, #0x9
    str w9, [x27, #2288]
    .loc 1 527 68
    .loc 1 527 26
    movz w9, #0xa
    str w9, [x27, #2296]
    .loc 1 527 72
    .loc 1 527 26
    movz w9, #0xb
    str w9, [x27, #2304]
    .loc 1 527 76
    .loc 1 527 26
    movz w9, #0xc
    str w9, [x27, #2312]
    ldr w0, [x27, #2216]
    ldr w1, [x27, #2224]
    ldr w2, [x27, #2232]
    ldr w3, [x27, #2240]
    ldr w4, [x27, #2248]
    ldr w5, [x27, #2256]
    ldr w6, [x27, #2264]
    ldr w7, [x27, #2272]
    sub sp, sp, #144
    ldr w9, [x27, #2280]
    str w9, [sp, #0]
    ldr w9, [x27, #2288]
    str w9, [sp, #8]
    ldr w9, [x27, #2296]
    str w9, [sp, #16]
    ldr w9, [x27, #2304]
    str w9, [sp, #24]
    ldr w9, [x27, #2312]
    str w9, [sp, #32]
    ldr w9, [x27, #2224]
    sxtw x9, w9
    str x9, [sp, #48]
    ldr w9, [x27, #2232]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr w9, [x27, #2240]
    sxtw x9, w9
    str x9, [sp, #64]
    ldr w9, [x27, #2248]
    sxtw x9, w9
    str x9, [sp, #72]
    ldr w9, [x27, #2256]
    sxtw x9, w9
    str x9, [sp, #80]
    ldr w9, [x27, #2264]
    sxtw x9, w9
    str x9, [sp, #88]
    ldr w9, [x27, #2272]
    sxtw x9, w9
    str x9, [sp, #96]
    ldr w9, [x27, #2280]
    sxtw x9, w9
    str x9, [sp, #104]
    ldr w9, [x27, #2288]
    sxtw x9, w9
    str x9, [sp, #112]
    ldr w9, [x27, #2296]
    sxtw x9, w9
    str x9, [sp, #120]
    ldr w9, [x27, #2304]
    sxtw x9, w9
    str x9, [sp, #128]
    ldr w9, [x27, #2312]
    sxtw x9, w9
    str x9, [sp, #136]
    add x15, sp, #48
    bl _Test_Program_vararg_sum_i32_varargs
    add sp, sp, #144
    str w0, [x27, #5168]
    .loc 1 527 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2208]
    .loc 1 528 12
    .loc 1 528 30
    .loc 1 528 27
    ldr w9, [x27, #2208]
    movz w10, #0x4e
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 528 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2320]
    .loc 1 528 34
    .loc 1 528 5
    adrp x9, ___cert__entry_main____str96@PAGE
    add x9, x9, ___cert__entry_main____str96@PAGEOFF
    str x9, [x27, #2328]
    ldrsb w0, [x27, #2320]
    ldr x1, [x27, #2328]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 530 39
    .loc 1 530 28
    movz w9, #0x6
    str w9, [x27, #2344]
    .loc 1 530 42
    .loc 1 530 28
    movz w9, #0xa
    str w9, [x27, #2352]
    .loc 1 530 46
    .loc 1 530 28
    movz x12, #0x3ff8, lsl #48
    fmov d0, x12
    str d0, [x27, #2360]
    .loc 1 530 51
    .loc 1 530 28
    movz w9, #0x14
    str w9, [x27, #2368]
    .loc 1 530 55
    .loc 1 530 28
    movz x12, #0x4004, lsl #48
    fmov d0, x12
    str d0, [x27, #2376]
    .loc 1 530 60
    .loc 1 530 28
    movz w9, #0x1e
    str w9, [x27, #2384]
    .loc 1 530 64
    .loc 1 530 28
    movz x12, #0x400c, lsl #48
    fmov d0, x12
    str d0, [x27, #2392]
    ldr w0, [x27, #2344]
    ldr w1, [x27, #2352]
    ldr d0, [x27, #2360]
    ldr w2, [x27, #2368]
    ldr d1, [x27, #2376]
    ldr w3, [x27, #2384]
    ldr d2, [x27, #2392]
    sub sp, sp, #48
    ldr w9, [x27, #2352]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr d15, [x27, #2360]
    str d15, [sp, #8]
    ldr w9, [x27, #2368]
    sxtw x9, w9
    str x9, [sp, #16]
    ldr d15, [x27, #2376]
    str d15, [sp, #24]
    ldr w9, [x27, #2384]
    sxtw x9, w9
    str x9, [sp, #32]
    ldr d15, [x27, #2392]
    str d15, [sp, #40]
    mov x15, sp
    bl _Test_Program_vararg_mix_i32_varargs
    add sp, sp, #48
    str w0, [x27, #5168]
    .loc 1 530 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2336]
    .loc 1 531 12
    .loc 1 531 32
    .loc 1 531 29
    ldr w9, [x27, #2336]
    movz w10, #0x42
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 531 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2400]
    .loc 1 531 36
    .loc 1 531 5
    adrp x9, ___cert__entry_main____str97@PAGE
    add x9, x9, ___cert__entry_main____str97@PAGEOFF
    str x9, [x27, #2408]
    ldrsb w0, [x27, #2400]
    ldr x1, [x27, #2408]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 533 12
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2416]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 533 27
    .loc 1 533 12
    ldr x9, [x27, #5168]
    movz w10, #0xa
    str w10, [x9]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 533 31
    .loc 1 533 12
    ldr x9, [x27, #5168]
    movz w10, #0x14
    str w10, [x9]
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 533 35
    .loc 1 533 12
    ldr x9, [x27, #5168]
    movz w10, #0x1e
    str w10, [x9]
    .loc 1 534 21
    .loc 1 534 32
    ldr x9, [x27, #2416]
    str x9, [x27, #5168]
    .loc 1 534 33
    .loc 1 534 32
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 534 38
    .loc 1 534 49
    ldr x9, [x27, #2416]
    str x9, [x27, #5176]
    .loc 1 534 50
    .loc 1 534 49
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5184]
    ldr x9, [x27, #5184]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5184]
    ldr x9, [x27, #5176]
    ldr x10, [x27, #5184]
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldr w10, [x9]
    str w10, [x27, #5176]
    .loc 1 534 36
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 534 55
    .loc 1 534 66
    ldr x9, [x27, #2416]
    str x9, [x27, #5176]
    .loc 1 534 67
    .loc 1 534 66
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5184]
    ldr x9, [x27, #5184]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5184]
    ldr x9, [x27, #5176]
    ldr x10, [x27, #5184]
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldr w10, [x9]
    str w10, [x27, #5176]
    .loc 1 534 53
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 534 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2424]
    .loc 1 535 12
    .loc 1 535 25
    .loc 1 535 22
    ldr w9, [x27, #2424]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 535 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2432]
    .loc 1 535 29
    .loc 1 535 5
    adrp x9, ___cert__entry_main____str98@PAGE
    add x9, x9, ___cert__entry_main____str98@PAGEOFF
    str x9, [x27, #2440]
    ldrsb w0, [x27, #2432]
    ldr x1, [x27, #2440]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 536 12
    .loc 1 536 35
    .loc 1 536 49
    .loc 1 536 47
    movz w11, #0xc
    str w11, [x27, #5168]
    movz w9, #0x4
    movz w10, #0x3
    mul w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 536 32
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 536 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2448]
    .loc 1 536 52
    .loc 1 536 5
    adrp x9, ___cert__entry_main____str99@PAGE
    add x9, x9, ___cert__entry_main____str99@PAGEOFF
    str x9, [x27, #2456]
    ldrsb w0, [x27, #2448]
    ldr x1, [x27, #2456]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 538 36
    .loc 1 538 50
    .loc 1 538 48
    movz w9, #0x8
    movz w10, #0x4
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 538 30
    ldr w0, [x27, #5168]
    bl _Std_Memory_alloc_i32
    str x0, [x27, #5168]
    .loc 1 538 11
    ldr x9, [x27, #5168]
    str x9, [x27, #2464]
    .loc 1 539 5
    .loc 1 539 18
    ldr x9, [x27, #2464]
    str x9, [x27, #5168]
    .loc 1 539 15
    .loc 1 539 18
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2472]
    .loc 1 539 20
    .loc 1 539 18
    movz x9, #0x64
    str x9, [x27, #2480]
    ldr x9, [x27, #2472]
    ldr x10, [x27, #2480]
    str x10, [x9]
    .loc 1 540 5
    .loc 1 540 18
    ldr x9, [x27, #2464]
    str x9, [x27, #5168]
    .loc 1 540 15
    .loc 1 540 18
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2488]
    .loc 1 540 20
    .loc 1 540 18
    movz x9, #0xc8
    str x9, [x27, #2496]
    ldr x9, [x27, #2488]
    ldr x10, [x27, #2496]
    str x10, [x9]
    .loc 1 541 5
    .loc 1 541 18
    ldr x9, [x27, #2464]
    str x9, [x27, #5168]
    .loc 1 541 15
    .loc 1 541 18
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2504]
    .loc 1 541 20
    .loc 1 541 18
    movz x9, #0x12c
    str x9, [x27, #2512]
    ldr x9, [x27, #2504]
    ldr x10, [x27, #2512]
    str x10, [x9]
    .loc 1 542 5
    .loc 1 542 18
    ldr x9, [x27, #2464]
    str x9, [x27, #5168]
    .loc 1 542 15
    .loc 1 542 18
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2520]
    .loc 1 542 20
    .loc 1 542 18
    movz x9, #0x190
    str x9, [x27, #2528]
    ldr x9, [x27, #2520]
    ldr x10, [x27, #2528]
    str x10, [x9]
    .loc 1 543 19
    .loc 1 543 28
    ldr x9, [x27, #2464]
    str x9, [x27, #5168]
    .loc 1 543 29
    .loc 1 543 28
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr x10, [x9]
    str x10, [x27, #5168]
    .loc 1 543 34
    .loc 1 543 43
    ldr x9, [x27, #2464]
    str x9, [x27, #5176]
    .loc 1 543 44
    .loc 1 543 43
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5184]
    ldr x9, [x27, #5184]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5184]
    ldr x9, [x27, #5176]
    ldr x10, [x27, #5184]
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldr x10, [x9]
    str x10, [x27, #5176]
    .loc 1 543 32
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    .loc 1 543 49
    .loc 1 543 58
    ldr x9, [x27, #2464]
    str x9, [x27, #5176]
    .loc 1 543 59
    .loc 1 543 58
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5184]
    ldr x9, [x27, #5184]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5184]
    ldr x9, [x27, #5176]
    ldr x10, [x27, #5184]
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldr x10, [x9]
    str x10, [x27, #5176]
    .loc 1 543 47
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    .loc 1 543 64
    .loc 1 543 73
    ldr x9, [x27, #2464]
    str x9, [x27, #5176]
    .loc 1 543 74
    .loc 1 543 73
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5184]
    ldr x9, [x27, #5184]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #5184]
    ldr x9, [x27, #5176]
    ldr x10, [x27, #5184]
    add x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    ldr x10, [x9]
    str x10, [x27, #5176]
    .loc 1 543 62
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    .loc 1 543 9
    ldr x9, [x27, #5168]
    str x9, [x27, #2536]
    .loc 1 544 12
    .loc 1 544 23
    .loc 1 544 20
    movz w9, #0x3e8
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #2536]
    ldr x10, [x27, #5176]
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 544 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2544]
    .loc 1 544 29
    .loc 1 544 5
    adrp x9, ___cert__entry_main____str100@PAGE
    add x9, x9, ___cert__entry_main____str100@PAGEOFF
    str x9, [x27, #2552]
    ldrsb w0, [x27, #2544]
    ldr x1, [x27, #2552]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 545 20
    .loc 1 545 12
    ldr x0, [x27, #2464]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 547 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2560]
    ldr x9, [x27, #2560]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2560]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #2560]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 548 23
    .loc 1 547 18
    ldr x9, [x27, #5168]
    movz w10, #0x9
    strb w10, [x9]
    .loc 1 549 20
    .loc 1 547 18
    ldr x9, [x27, #2560]
    movz w10, #0x51
    str w10, [x9]
    .loc 1 552 24
    .loc 1 552 12
    adrp x9, ___cert__entry_main____str101@PAGE
    add x9, x9, ___cert__entry_main____str101@PAGEOFF
    str x9, [x27, #2568]
    .loc 1 556 44
    .loc 1 556 31
    ldr x0, [x27, #2560]
    bl _Test_Program_passbystruct_type_kind_21
    str w0, [x27, #5168]
    .loc 1 556 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2576]
    .loc 1 557 12
    .loc 1 557 35
    .loc 1 557 32
    ldr w9, [x27, #2576]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 557 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2584]
    .loc 1 557 39
    .loc 1 557 5
    adrp x9, ___cert__entry_main____str102@PAGE
    add x9, x9, ___cert__entry_main____str102@PAGEOFF
    str x9, [x27, #2592]
    ldrsb w0, [x27, #2584]
    ldr x1, [x27, #2592]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 559 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2600]
    .loc 1 559 45
    .loc 1 559 30
    ldr x0, [x27, #2560]
    bl _Test_Program_returnbystruct_type_kind_21
    str x0, [x27, #5168]
    .loc 1 559 18
    ldr x9, [x27, #5168]
    str x9, [x27, #2608]
    ldr x9, [x27, #2600]
    str x9, [x27, #2616]
    ldr x9, [x27, #2608]
    ldr w10, [x9]
    str w10, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #2624]
    ldr x9, [x27, #2616]
    ldr w10, [x27, #2624]
    str w10, [x9]
    ldr x9, [x27, #2608]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2632]
    ldr x9, [x27, #2616]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x27, #2632]
    strb w10, [x9]
    .loc 1 560 12
    .loc 1 560 22
    ldr x9, [x27, #2600]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 560 33
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 560 36
    .loc 1 560 33
    ldr w9, [x27, #5168]
    movz w10, #0x9
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 560 38
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs65
    b L__cert__entry_main__land_false66
L__cert__entry_main__land_rhs65:
    .loc 1 560 41
    .loc 1 560 51
    ldr x9, [x27, #2600]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 560 62
    .loc 1 560 59
    ldr w9, [x27, #5168]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 560 38
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true67
    b L__cert__entry_main__land_false66
L__cert__entry_main__land_true67:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end68
L__cert__entry_main__land_false66:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end68
L__cert__entry_main__land_end68:
    .loc 1 560 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #2640]
    .loc 1 560 66
    .loc 1 560 5
    adrp x9, ___cert__entry_main____str103@PAGE
    add x9, x9, ___cert__entry_main____str103@PAGEOFF
    str x9, [x27, #2648]
    ldrsb w0, [x27, #2640]
    ldr x1, [x27, #2648]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 562 40
    adrp x9, _Test_Program_funptr_test_i32_i32@PAGE
    add x9, x9, _Test_Program_funptr_test_i32_i32@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 562 10
    ldr x9, [x27, #5168]
    str x9, [x27, #2656]
    .loc 1 564 43
    adrp x9, _Test_Program_funptr_test_i32_i32@PAGE
    add x9, x9, _Test_Program_funptr_test_i32_i32@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 564 29
    ldr x9, [x27, #5168]
    str x9, [x27, #2664]
    .loc 1 566 36
    .loc 1 566 25
    movz w9, #0xf
    str w9, [x27, #2680]
    .loc 1 566 40
    .loc 1 566 25
    movz w9, #0x1b
    str w9, [x27, #2688]
    ldr w0, [x27, #2680]
    ldr w1, [x27, #2688]
    ldr x16, [x27, #2656]
    blr x16
    str w0, [x27, #5168]
    .loc 1 566 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2672]
    .loc 1 567 12
    .loc 1 567 29
    .loc 1 567 26
    ldr w9, [x27, #2672]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 567 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2696]
    .loc 1 567 33
    .loc 1 567 5
    adrp x9, ___cert__entry_main____str104@PAGE
    add x9, x9, ___cert__entry_main____str104@PAGEOFF
    str x9, [x27, #2704]
    ldrsb w0, [x27, #2696]
    ldr x1, [x27, #2704]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 568 38
    .loc 1 568 26
    movz w9, #0x14
    str w9, [x27, #2720]
    .loc 1 568 42
    .loc 1 568 26
    movz w9, #0x16
    str w9, [x27, #2728]
    ldr w0, [x27, #2720]
    ldr w1, [x27, #2728]
    ldr x16, [x27, #2664]
    blr x16
    str w0, [x27, #5168]
    .loc 1 568 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2712]
    .loc 1 569 12
    .loc 1 569 30
    .loc 1 569 27
    ldr w9, [x27, #2712]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 569 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2736]
    .loc 1 569 34
    .loc 1 569 5
    adrp x9, ___cert__entry_main____str105@PAGE
    add x9, x9, ___cert__entry_main____str105@PAGEOFF
    str x9, [x27, #2744]
    ldrsb w0, [x27, #2736]
    ldr x1, [x27, #2744]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 571 23
    .loc 1 571 11
    ldr x9, [x27, #2656]
    str x9, [x27, #2752]
    .loc 1 572 12
    .loc 1 572 25
    .loc 1 572 22
    ldr x9, [x27, #2752]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 572 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2760]
    .loc 1 572 31
    .loc 1 572 5
    adrp x9, ___cert__entry_main____str106@PAGE
    add x9, x9, ___cert__entry_main____str106@PAGEOFF
    str x9, [x27, #2768]
    ldrsb w0, [x27, #2760]
    ldr x1, [x27, #2768]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 574 40
    .loc 1 574 29
    ldr x9, [x27, #2752]
    str x9, [x27, #2776]
    .loc 1 575 35
    .loc 1 575 26
    movz w9, #0x7
    str w9, [x27, #2792]
    .loc 1 575 38
    .loc 1 575 26
    movz w9, #0x23
    str w9, [x27, #2800]
    ldr w0, [x27, #2792]
    ldr w1, [x27, #2800]
    ldr x16, [x27, #2776]
    blr x16
    str w0, [x27, #5168]
    .loc 1 575 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2784]
    .loc 1 576 12
    .loc 1 576 30
    .loc 1 576 27
    ldr w9, [x27, #2784]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 576 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2808]
    .loc 1 576 34
    .loc 1 576 5
    adrp x9, ___cert__entry_main____str107@PAGE
    add x9, x9, ___cert__entry_main____str107@PAGEOFF
    str x9, [x27, #2816]
    ldrsb w0, [x27, #2808]
    ldr x1, [x27, #2816]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 578 49
    adrp x9, _Test_Program_passbystruct_type_kind_21@PAGE
    add x9, x9, _Test_Program_passbystruct_type_kind_21@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 578 33
    ldr x9, [x27, #5168]
    str x9, [x27, #2824]
    .loc 1 579 46
    .loc 1 579 32
    ldr x0, [x27, #2560]
    ldr x16, [x27, #2824]
    blr x16
    str w0, [x27, #5168]
    .loc 1 579 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2832]
    .loc 1 580 12
    .loc 1 580 36
    .loc 1 580 33
    ldr w9, [x27, #2832]
    movz w10, #0x51
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 580 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2840]
    .loc 1 580 40
    .loc 1 580 5
    adrp x9, ___cert__entry_main____str108@PAGE
    add x9, x9, ___cert__entry_main____str108@PAGEOFF
    str x9, [x27, #2848]
    ldrsb w0, [x27, #2840]
    ldr x1, [x27, #2848]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 582 31
    .loc 1 582 12
    movz w9, #0xc
    str w9, [x27, #2856]
    .loc 1 149 9
    .loc 1 149 14
    .loc 1 149 11
    ldr w9, [x27, #2856]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__cond_true69
    b L__cert__entry_main__cond_false70
L__cert__entry_main__cond_true69:
    .loc 1 150 13
    .loc 1 149 11
    mov w11, wzr
    str w11, [x27, #5168]
    b L__cert__entry_main__cond_end71
L__cert__entry_main__cond_false70:
    .loc 1 151 9
    .loc 1 151 30
    .loc 1 151 13
    ldr w0, [x27, #2856]
    bl _Test_Program_inlined_function_i32
    str w0, [x27, #5176]
    .loc 1 151 11
    ldr w9, [x27, #2856]
    ldr w10, [x27, #5176]
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 149 11
    b L__cert__entry_main__cond_end71
L__cert__entry_main__cond_end71:
    .loc 1 582 38
    .loc 1 582 35
    ldr w9, [x27, #5168]
    movz w10, #0x9c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 582 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2864]
    .loc 1 582 43
    .loc 1 582 5
    adrp x9, ___cert__entry_main____str109@PAGE
    add x9, x9, ___cert__entry_main____str109@PAGEOFF
    str x9, [x27, #2872]
    ldrsb w0, [x27, #2864]
    ldr x1, [x27, #2872]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 584 31
    .loc 1 584 12
    mov w9, wzr
    str w9, [x27, #2880]
    .loc 1 149 9
    .loc 1 149 14
    .loc 1 149 11
    ldr w9, [x27, #2880]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__cond_true72
    b L__cert__entry_main__cond_false73
L__cert__entry_main__cond_true72:
    .loc 1 150 13
    .loc 1 149 11
    mov w11, wzr
    str w11, [x27, #5168]
    b L__cert__entry_main__cond_end74
L__cert__entry_main__cond_false73:
    .loc 1 151 9
    .loc 1 151 30
    .loc 1 151 13
    ldr w0, [x27, #2880]
    bl _Test_Program_inlined_function_i32
    str w0, [x27, #5176]
    .loc 1 151 11
    ldr w9, [x27, #2880]
    ldr w10, [x27, #5176]
    mul w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 149 11
    b L__cert__entry_main__cond_end74
L__cert__entry_main__cond_end74:
    .loc 1 584 37
    .loc 1 584 34
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 584 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #2888]
    .loc 1 584 40
    .loc 1 584 5
    adrp x9, ___cert__entry_main____str110@PAGE
    add x9, x9, ___cert__entry_main____str110@PAGEOFF
    str x9, [x27, #2896]
    ldrsb w0, [x27, #2888]
    ldr x1, [x27, #2896]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 586 37
    adrp x9, _Test_Program_inlined_function_i32@PAGE
    add x9, x9, _Test_Program_inlined_function_i32@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 586 24
    ldr x9, [x27, #5168]
    str x9, [x27, #2904]
    .loc 1 588 26
    .loc 1 588 11
    ldr x9, [x27, #2904]
    str x9, [x27, #2912]
    .loc 1 590 40
    .loc 1 590 24
    ldr x9, [x27, #2912]
    str x9, [x27, #2920]
    .loc 1 591 14
    .loc 1 591 8
    adrp x9, ___cert__entry_main____str111@PAGE
    add x9, x9, ___cert__entry_main____str111@PAGEOFF
    str x9, [x27, #2928]
    .loc 1 591 46
    .loc 1 591 32
    movz w0, #0x5
    ldr x16, [x27, #2920]
    blr x16
    str w0, [x27, #5168]
    .loc 1 591 8
    ldr w9, [x27, #5168]
    str w9, [x27, #2936]
    ldr x0, [x27, #2928]
    ldr w1, [x27, #2936]
    sub sp, sp, #16
    ldr w9, [x27, #2936]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 600 14
    .loc 1 600 8
    adrp x9, ___cert__entry_main____str112@PAGE
    add x9, x9, ___cert__entry_main____str112@PAGEOFF
    str x9, [x27, #2944]
    .loc 1 600 41
    .loc 1 600 8
    adrp x9, ___cert__entry_main____str113@PAGE
    add x9, x9, ___cert__entry_main____str113@PAGEOFF
    str x9, [x27, #2952]
    ldr x0, [x27, #2944]
    ldr x1, [x27, #2952]
    sub sp, sp, #16
    ldr x9, [x27, #2952]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 604 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2968]
    ldr x9, [x27, #2968]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2968]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #2968]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 604 37
    .loc 1 604 21
    ldr x9, [x27, #5168]
    movz w10, #0x1
    strb w10, [x9]
    .loc 1 604 51
    .loc 1 604 21
    ldr x9, [x27, #2968]
    movz w10, #0xa
    str w10, [x9]
    .loc 1 603 29
    ldr x9, [x27, #2968]
    str x9, [x27, #2976]
    .loc 1 605 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #2984]
    ldr x9, [x27, #2984]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #2984]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #2984]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 605 37
    .loc 1 605 21
    ldr x9, [x27, #5168]
    movz w10, #0x2
    strb w10, [x9]
    .loc 1 605 51
    .loc 1 605 21
    ldr x9, [x27, #2984]
    movz w10, #0x14
    str w10, [x9]
    .loc 1 603 29
    ldr x9, [x27, #2984]
    str x9, [x27, #2992]
    .loc 1 606 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3000]
    ldr x9, [x27, #3000]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3000]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3000]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 606 37
    .loc 1 606 21
    ldr x9, [x27, #5168]
    movz w10, #0x3
    strb w10, [x9]
    .loc 1 606 51
    .loc 1 606 21
    ldr x9, [x27, #3000]
    movz w10, #0x1e
    str w10, [x9]
    .loc 1 603 29
    ldr x9, [x27, #3000]
    str x9, [x27, #3008]
    ldr x0, [x27, #2976]
    ldr x1, [x27, #2992]
    ldr x2, [x27, #3008]
    bl _LibTest_Another_multistructargs_struct_smalltest_struct_smalltest_struct_smalltest
    str w0, [x27, #5168]
    .loc 1 603 9
    ldr w9, [x27, #5168]
    str w9, [x27, #2960]
    .loc 1 609 12
    .loc 1 609 33
    .loc 1 609 30
    ldr w9, [x27, #2960]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 609 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3016]
    .loc 1 609 37
    .loc 1 609 5
    adrp x9, ___cert__entry_main____str114@PAGE
    add x9, x9, ___cert__entry_main____str114@PAGEOFF
    str x9, [x27, #3024]
    ldrsb w0, [x27, #3016]
    ldr x1, [x27, #3024]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 611 46
    .loc 1 611 32
    movz w9, #0x3
    str w9, [x27, #3040]
    .loc 1 612 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3048]
    ldr x9, [x27, #3048]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3048]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3048]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 612 37
    .loc 1 612 21
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 612 51
    .loc 1 612 21
    ldr x9, [x27, #3048]
    movz w10, #0xb
    str w10, [x9]
    .loc 1 611 32
    ldr x9, [x27, #3048]
    str x9, [x27, #3056]
    .loc 1 613 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3064]
    ldr x9, [x27, #3064]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3064]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3064]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 613 37
    .loc 1 613 21
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 613 51
    .loc 1 613 21
    ldr x9, [x27, #3064]
    movz w10, #0x16
    str w10, [x9]
    .loc 1 611 32
    ldr x9, [x27, #3064]
    str x9, [x27, #3072]
    .loc 1 614 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3080]
    ldr x9, [x27, #3080]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3080]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3080]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 614 37
    .loc 1 614 21
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 614 51
    .loc 1 614 21
    ldr x9, [x27, #3080]
    movz w10, #0x21
    str w10, [x9]
    .loc 1 611 32
    ldr x9, [x27, #3080]
    str x9, [x27, #3088]
    ldr w0, [x27, #3040]
    ldr x1, [x27, #3056]
    ldr x2, [x27, #3072]
    ldr x3, [x27, #3088]
    sub sp, sp, #32
    ldr x9, [x27, #3056]
    str x9, [sp, #0]
    ldr x9, [x27, #3072]
    str x9, [sp, #8]
    ldr x9, [x27, #3088]
    str x9, [sp, #16]
    mov x15, sp
    bl _Test_Program_structvarargs_i32_varargs
    add sp, sp, #32
    str w0, [x27, #5168]
    .loc 1 611 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3032]
    .loc 1 617 14
    .loc 1 617 8
    adrp x9, ___cert__entry_main____str115@PAGE
    add x9, x9, ___cert__entry_main____str115@PAGEOFF
    str x9, [x27, #3096]
    .loc 1 617 45
    .loc 1 617 8
    ldr w9, [x27, #3032]
    str w9, [x27, #3104]
    ldr x0, [x27, #3096]
    ldr w1, [x27, #3104]
    sub sp, sp, #16
    ldr w9, [x27, #3104]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 619 12
    .loc 1 619 36
    .loc 1 619 33
    ldr w9, [x27, #3032]
    movz w10, #0x42
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 619 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3112]
    .loc 1 619 40
    .loc 1 619 5
    adrp x9, ___cert__entry_main____str116@PAGE
    add x9, x9, ___cert__entry_main____str116@PAGEOFF
    str x9, [x27, #3120]
    ldrsb w0, [x27, #3112]
    ldr x1, [x27, #3120]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 621 32
    .loc 1 621 18
    movz w9, #0x22b
    str w9, [x27, #3128]
    .loc 1 622 12
    .loc 1 622 27
    .loc 1 622 24
    ldr w9, [x27, #3128]
    movz w10, #0x22b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 622 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3136]
    .loc 1 622 32
    .loc 1 622 5
    adrp x9, ___cert__entry_main____str117@PAGE
    add x9, x9, ___cert__entry_main____str117@PAGEOFF
    str x9, [x27, #3144]
    ldrsb w0, [x27, #3136]
    ldr x1, [x27, #3144]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 626 25
    .loc 1 626 12
    movz w0, #0x2d
    bl _Test_Program_use_constant_i32
    str w0, [x27, #5168]
    .loc 1 626 32
    .loc 1 626 29
    ldr w9, [x27, #5168]
    movz w10, #0x91
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 626 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3152]
    .loc 1 626 37
    .loc 1 626 5
    adrp x9, ___cert__entry_main____str118@PAGE
    add x9, x9, ___cert__entry_main____str118@PAGEOFF
    str x9, [x27, #3160]
    ldrsb w0, [x27, #3152]
    ldr x1, [x27, #3160]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 627 25
    .loc 1 627 12
    ldr w0, [x27, #3128]
    bl _Test_Program_use_constant_i32
    str w0, [x27, #5168]
    .loc 1 627 48
    .loc 1 627 45
    ldr w9, [x27, #5168]
    movz w10, #0x28f
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 627 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3168]
    .loc 1 627 53
    .loc 1 627 5
    adrp x9, ___cert__entry_main____str119@PAGE
    add x9, x9, ___cert__entry_main____str119@PAGEOFF
    str x9, [x27, #3176]
    ldrsb w0, [x27, #3168]
    ldr x1, [x27, #3176]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 629 19
    .loc 1 629 5
    ldr w0, [x27, #3128]
    bl _Test_Program_constant_test_i32
    .loc 1 630 12
    .loc 1 630 27
    .loc 1 630 24
    ldr w9, [x27, #3128]
    movz w10, #0x22b
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 630 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3184]
    .loc 1 630 32
    .loc 1 630 5
    adrp x9, ___cert__entry_main____str120@PAGE
    add x9, x9, ___cert__entry_main____str120@PAGEOFF
    str x9, [x27, #3192]
    ldrsb w0, [x27, #3184]
    ldr x1, [x27, #3192]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 632 39
    .loc 1 632 33
    movz w0, #0xa
    bl _Std_Memory_alloc_i32
    str x0, [x27, #5168]
    .loc 1 632 18
    ldr x9, [x27, #5168]
    str x9, [x27, #3200]
    .loc 1 635 18
    .loc 1 635 5
    ldr x0, [x27, #3200]
    bl _Test_Program_constant_ptr_ptr_to_u8
    .loc 1 637 12
    .loc 1 637 17
    ldr x9, [x27, #3200]
    str x9, [x27, #5168]
    .loc 1 637 18
    .loc 1 637 17
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 637 21
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 637 24
    .loc 1 637 21
    ldr w9, [x27, #5168]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 637 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3208]
    .loc 1 637 28
    .loc 1 637 5
    adrp x9, ___cert__entry_main____str121@PAGE
    add x9, x9, ___cert__entry_main____str121@PAGEOFF
    str x9, [x27, #3216]
    ldrsb w0, [x27, #3208]
    ldr x1, [x27, #3216]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 639 23
    bl _Test_Program_alignof_alignprobe
    str w0, [x27, #5168]
    .loc 1 639 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3224]
    .loc 1 640 31
    .loc 1 640 9
    movz w9, #0x8
    str w9, [x27, #3232]
    .loc 1 641 12
    .loc 1 641 27
    .loc 1 641 24
    ldr w9, [x27, #3224]
    ldr w10, [x27, #3232]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 641 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3240]
    .loc 1 641 48
    .loc 1 641 5
    adrp x9, ___cert__entry_main____str122@PAGE
    add x9, x9, ___cert__entry_main____str122@PAGEOFF
    str x9, [x27, #3248]
    ldrsb w0, [x27, #3240]
    ldr x1, [x27, #3248]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 642 24
    bl _Test_Program_offsetof_alignprobe_payload
    str w0, [x27, #5168]
    .loc 1 642 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3256]
    .loc 1 643 32
    .loc 1 643 9
    movz w9, #0x8
    str w9, [x27, #3264]
    .loc 1 644 12
    .loc 1 644 28
    .loc 1 644 25
    ldr w9, [x27, #3256]
    ldr w10, [x27, #3264]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 644 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3272]
    .loc 1 644 50
    .loc 1 644 5
    adrp x9, ___cert__entry_main____str123@PAGE
    add x9, x9, ___cert__entry_main____str123@PAGEOFF
    str x9, [x27, #3280]
    ldrsb w0, [x27, #3272]
    ldr x1, [x27, #3280]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 645 13
    .loc 1 645 36
    .loc 1 645 34
    ldr w9, [x27, #3264]
    ldr w10, [x27, #3232]
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #5168]
    .loc 1 645 60
    .loc 1 645 57
    ldr w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 645 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3288]
    .loc 1 645 63
    .loc 1 645 5
    adrp x9, ___cert__entry_main____str124@PAGE
    add x9, x9, ___cert__entry_main____str124@PAGEOFF
    str x9, [x27, #3296]
    ldrsb w0, [x27, #3288]
    ldr x1, [x27, #3296]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 647 12
    .loc 1 647 27
    .loc 1 647 24
    movz w9, #0x1
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 647 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3304]
    .loc 1 647 30
    .loc 1 647 5
    adrp x9, ___cert__entry_main____str125@PAGE
    add x9, x9, ___cert__entry_main____str125@PAGEOFF
    str x9, [x27, #3312]
    ldrsb w0, [x27, #3304]
    ldr x1, [x27, #3312]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 648 12
    .loc 1 648 28
    .loc 1 648 25
    movz w9, #0x2
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 648 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3320]
    .loc 1 648 31
    .loc 1 648 5
    adrp x9, ___cert__entry_main____str126@PAGE
    add x9, x9, ___cert__entry_main____str126@PAGEOFF
    str x9, [x27, #3328]
    ldrsb w0, [x27, #3320]
    ldr x1, [x27, #3328]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 649 12
    .loc 1 649 28
    .loc 1 649 25
    movz w9, #0x4
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 649 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3336]
    .loc 1 649 31
    .loc 1 649 5
    adrp x9, ___cert__entry_main____str127@PAGE
    add x9, x9, ___cert__entry_main____str127@PAGEOFF
    str x9, [x27, #3344]
    ldrsb w0, [x27, #3336]
    ldr x1, [x27, #3344]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 650 12
    .loc 1 650 28
    .loc 1 650 25
    movz w9, #0x8
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 650 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3352]
    .loc 1 650 31
    .loc 1 650 5
    adrp x9, ___cert__entry_main____str128@PAGE
    add x9, x9, ___cert__entry_main____str128@PAGEOFF
    str x9, [x27, #3360]
    ldrsb w0, [x27, #3352]
    ldr x1, [x27, #3360]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 651 12
    .loc 1 651 28
    .loc 1 651 25
    movz w9, #0x4
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 651 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3368]
    .loc 1 651 31
    .loc 1 651 5
    adrp x9, ___cert__entry_main____str129@PAGE
    add x9, x9, ___cert__entry_main____str129@PAGEOFF
    str x9, [x27, #3376]
    ldrsb w0, [x27, #3368]
    ldr x1, [x27, #3376]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 652 12
    .loc 1 652 28
    .loc 1 652 25
    movz w9, #0x8
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 652 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3384]
    .loc 1 652 31
    .loc 1 652 5
    adrp x9, ___cert__entry_main____str130@PAGE
    add x9, x9, ___cert__entry_main____str130@PAGEOFF
    str x9, [x27, #3392]
    ldrsb w0, [x27, #3384]
    ldr x1, [x27, #3392]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 655 24
    .loc 1 655 12
    movz w9, #0xa
    str w9, [x27, #3400]
    .loc 1 655 28
    .loc 1 655 12
    movz w9, #0x14
    str w9, [x27, #3408]
    ldr w0, [x27, #3400]
    ldr w1, [x27, #3408]
    bl _default_max__inst__T0_i32
    str w0, [x27, #5168]
    .loc 1 655 35
    .loc 1 655 32
    ldr w9, [x27, #5168]
    movz w10, #0x14
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 655 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3416]
    .loc 1 655 39
    .loc 1 655 5
    adrp x9, ___cert__entry_main____str131@PAGE
    add x9, x9, ___cert__entry_main____str131@PAGEOFF
    str x9, [x27, #3424]
    ldrsb w0, [x27, #3416]
    ldr x1, [x27, #3424]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 656 21
    .loc 1 656 22
    .loc 1 656 21
    mov w9, wzr
    movz w10, #0x5
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 656 12
    ldr w9, [x27, #5168]
    str w9, [x27, #3432]
    .loc 1 656 25
    .loc 1 656 26
    .loc 1 656 25
    mov w9, wzr
    movz w10, #0x2
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 656 12
    ldr w9, [x27, #5168]
    str w9, [x27, #3440]
    ldr w0, [x27, #3432]
    ldr w1, [x27, #3440]
    bl _max__inst__T0_i32
    str w0, [x27, #5168]
    .loc 1 656 32
    .loc 1 656 33
    .loc 1 656 32
    mov w9, wzr
    movz w10, #0x2
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 656 29
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 656 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3448]
    .loc 1 656 36
    .loc 1 656 5
    adrp x9, ___cert__entry_main____str132@PAGE
    add x9, x9, ___cert__entry_main____str132@PAGEOFF
    str x9, [x27, #3456]
    ldrsb w0, [x27, #3448]
    ldr x1, [x27, #3456]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 657 18
    .loc 1 657 21
    movz w9, #0xf
    uxtb w9, w9
    strb w9, [x27, #5168]
    .loc 1 657 12
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3464]
    .loc 1 657 28
    .loc 1 657 31
    movz w9, #0x1b
    uxth w9, w9
    strh w9, [x27, #5168]
    .loc 1 657 12
    ldrsh w9, [x27, #5168]
    strh w9, [x27, #3472]
    ldrb w0, [x27, #3464]
    ldrsh w1, [x27, #3472]
    bl _other__inst__T0_u8__T1_i16
    str w0, [x27, #5168]
    .loc 1 657 42
    .loc 1 657 39
    ldr w9, [x27, #5168]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 657 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3480]
    .loc 1 657 46
    .loc 1 657 5
    adrp x9, ___cert__entry_main____str133@PAGE
    add x9, x9, ___cert__entry_main____str133@PAGEOFF
    str x9, [x27, #3488]
    ldrsb w0, [x27, #3480]
    ldr x1, [x27, #3488]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 659 12
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3496]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 659 26
    .loc 1 659 27
    .loc 1 659 29
    mov w11, wzr
    strh w11, [x27, #5176]
    movz w9, #0x2
    uxth w9, w9
    strh w9, [x27, #5184]
    .loc 1 659 26
    ldrsh w9, [x27, #5176]
    ldrsh w10, [x27, #5184]
    sub w9, w9, w10
    sxth w9, w9
    strh w9, [x27, #5176]
    .loc 1 659 12
    ldr x9, [x27, #5168]
    ldrsh w10, [x27, #5176]
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 659 38
    .loc 1 659 12
    ldr x9, [x27, #5168]
    movz w10, #0x4
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 659 41
    .loc 1 659 12
    ldr x9, [x27, #5168]
    movz w10, #0x6
    strh w10, [x9]
    ldr x9, [x27, #3496]
    str x9, [x27, #5168]
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 659 44
    .loc 1 659 12
    ldr x9, [x27, #5168]
    movz w10, #0x8
    strh w10, [x9]
    .loc 1 660 11
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3504]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 660 25
    .loc 1 660 11
    ldr x9, [x27, #5168]
    movz w10, #0xa
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 660 29
    .loc 1 660 11
    ldr x9, [x27, #5168]
    movz w10, #0x14
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 660 33
    .loc 1 660 11
    ldr x9, [x27, #5168]
    movz w10, #0x1e
    strb w10, [x9]
    ldr x9, [x27, #3504]
    str x9, [x27, #5168]
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 660 37
    .loc 1 660 11
    ldr x9, [x27, #5168]
    movz w10, #0x28
    strb w10, [x9]
    .loc 1 661 42
    .loc 1 661 20
    ldr x9, [x27, #3496]
    str x9, [x27, #3520]
    .loc 1 661 52
    .loc 1 661 20
    ldr x9, [x27, #3504]
    str x9, [x27, #3528]
    .loc 1 661 63
    .loc 1 661 20
    movz w9, #0x4
    str w9, [x27, #3536]
    ldr x0, [x27, #3520]
    ldr x1, [x27, #3528]
    ldr w2, [x27, #3536]
    bl _dot_sum__inst__T0_i16__T1_u8__T2_i64
    str x0, [x27, #5168]
    .loc 1 661 9
    ldr x9, [x27, #5168]
    str x9, [x27, #3512]
    .loc 1 662 12
    .loc 1 662 24
    .loc 1 662 21
    movz w9, #0x230
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #3512]
    ldr x10, [x27, #5176]
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 662 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3544]
    .loc 1 662 29
    .loc 1 662 5
    adrp x9, ___cert__entry_main____str134@PAGE
    add x9, x9, ___cert__entry_main____str134@PAGEOFF
    str x9, [x27, #3552]
    ldrsb w0, [x27, #3544]
    ldr x1, [x27, #3552]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 664 11
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3560]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 664 26
    .loc 1 664 11
    ldr x9, [x27, #5168]
    movz w10, #0x1
    strb w10, [x9]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 664 29
    .loc 1 664 11
    ldr x9, [x27, #5168]
    movz w10, #0x2
    strb w10, [x9]
    ldr x9, [x27, #3560]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 664 32
    .loc 1 664 11
    ldr x9, [x27, #5168]
    movz w10, #0x3
    strb w10, [x9]
    .loc 1 665 11
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3568]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 665 27
    .loc 1 665 11
    ldr x9, [x27, #5168]
    movz w10, #0x9
    strb w10, [x9]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 665 30
    .loc 1 665 11
    ldr x9, [x27, #5168]
    movz w10, #0x8
    strb w10, [x9]
    ldr x9, [x27, #3568]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 665 33
    .loc 1 665 11
    ldr x9, [x27, #5168]
    movz w10, #0x7
    strb w10, [x9]
    .loc 1 666 44
    .loc 1 666 23
    ldr x9, [x27, #3560]
    str x9, [x27, #3584]
    .loc 1 666 56
    .loc 1 666 23
    ldr x9, [x27, #3568]
    str x9, [x27, #3592]
    .loc 1 666 69
    .loc 1 666 23
    movz w9, #0x3
    str w9, [x27, #3600]
    ldr x0, [x27, #3584]
    ldr x1, [x27, #3592]
    ldr w2, [x27, #3600]
    bl _dot_sum__inst__T0_i8__T1_i8__T2_i32
    str w0, [x27, #5168]
    .loc 1 666 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3576]
    .loc 1 667 12
    .loc 1 667 27
    .loc 1 667 24
    ldr w9, [x27, #3576]
    movz w10, #0x2e
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 667 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3608]
    .loc 1 667 31
    .loc 1 667 5
    adrp x9, ___cert__entry_main____str135@PAGE
    add x9, x9, ___cert__entry_main____str135@PAGEOFF
    str x9, [x27, #3616]
    ldrsb w0, [x27, #3608]
    ldr x1, [x27, #3616]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 669 27
    .loc 1 669 9
    movz w9, #0x7
    str w9, [x27, #3624]
    .loc 1 670 25
    .loc 1 670 26
    .loc 1 670 25
    mov w9, wzr
    movz w10, #0x64
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 670 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3632]
    .loc 1 671 13
    .loc 1 671 5
    ldr w9, [x27, #3624]
    str w9, [x27, #3640]
    .loc 1 673 14
    .loc 1 671 5
    ldr w9, [x27, #3640]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case82
    b L__cert__entry_main__switch_next87
L__cert__entry_main__switch_next87:
    .loc 1 676 14
    .loc 1 671 5
    ldr w9, [x27, #3640]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case83
    b L__cert__entry_main__switch_next88
L__cert__entry_main__switch_next88:
    .loc 1 677 14
    .loc 1 671 5
    ldr w9, [x27, #3640]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case84
    b L__cert__entry_main__switch_next89
L__cert__entry_main__switch_next89:
    .loc 1 680 14
    .loc 1 671 5
    ldr w9, [x27, #3640]
    movz w10, #0x3
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case85
    b L__cert__entry_main__switch_case86
L__cert__entry_main__switch_case82:
    .loc 1 674 29
    .loc 1 674 27
    mov w9, wzr
    str w9, [x27, #3632]
    .loc 1 675 13
    b L__cert__entry_main__switch_end81
    .loc 1 671 5
L__cert__entry_main__switch_case83:
L__cert__entry_main__switch_case84:
    .loc 1 678 29
    .loc 1 678 27
    movz w9, #0xc
    str w9, [x27, #3632]
    .loc 1 679 13
    b L__cert__entry_main__switch_end81
    .loc 1 671 5
L__cert__entry_main__switch_case85:
    .loc 1 681 29
    .loc 1 681 27
    movz w9, #0x21
    str w9, [x27, #3632]
    .loc 1 682 13
    b L__cert__entry_main__switch_end81
    .loc 1 671 5
L__cert__entry_main__switch_case86:
    .loc 1 684 29
    .loc 1 684 27
    movz w9, #0x2c
    str w9, [x27, #3632]
    .loc 1 685 13
    b L__cert__entry_main__switch_end81
    .loc 1 671 5
L__cert__entry_main__switch_end81:
    .loc 1 687 12
    .loc 1 687 29
    .loc 1 687 26
    ldr w9, [x27, #3632]
    movz w10, #0x2c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 687 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3648]
    .loc 1 687 33
    .loc 1 687 5
    adrp x9, ___cert__entry_main____str136@PAGE
    add x9, x9, ___cert__entry_main____str136@PAGEOFF
    str x9, [x27, #3656]
    ldrsb w0, [x27, #3648]
    ldr x1, [x27, #3656]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 689 30
    .loc 1 689 31
    .loc 1 689 30
    mov w9, wzr
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 689 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3664]
    .loc 1 690 13
    .loc 1 690 5
    movz w9, #0x1
    str w9, [x27, #3672]
    .loc 1 692 14
    .loc 1 690 5
    ldr w9, [x27, #3672]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case91
    b L__cert__entry_main__switch_next95
L__cert__entry_main__switch_next95:
    .loc 1 695 14
    .loc 1 690 5
    ldr w9, [x27, #3672]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case92
    b L__cert__entry_main__switch_next96
L__cert__entry_main__switch_next96:
    .loc 1 697 14
    .loc 1 690 5
    ldr w9, [x27, #3672]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__switch_case93
    b L__cert__entry_main__switch_case94
L__cert__entry_main__switch_case91:
    .loc 1 693 34
    .loc 1 693 32
    mov w9, wzr
    str w9, [x27, #3664]
    .loc 1 694 13
    b L__cert__entry_main__switch_end90
    .loc 1 690 5
L__cert__entry_main__switch_case92:
    .loc 1 696 34
    .loc 1 696 32
    movz w9, #0xa
    str w9, [x27, #3664]
    .loc 1 690 5
L__cert__entry_main__switch_case93:
    .loc 1 698 34
    .loc 1 698 32
    movz w9, #0x14
    str w9, [x27, #3664]
    .loc 1 699 13
    b L__cert__entry_main__switch_end90
    .loc 1 690 5
L__cert__entry_main__switch_case94:
    .loc 1 701 34
    .loc 1 701 35
    .loc 1 701 34
    mov w9, wzr
    movz w10, #0x32
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 701 32
    ldr w9, [x27, #5168]
    str w9, [x27, #3664]
    .loc 1 702 13
    b L__cert__entry_main__switch_end90
    .loc 1 690 5
L__cert__entry_main__switch_end90:
    .loc 1 704 12
    .loc 1 704 34
    .loc 1 704 31
    ldr w9, [x27, #3664]
    movz w10, #0x14
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 704 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3680]
    .loc 1 704 38
    .loc 1 704 5
    adrp x9, ___cert__entry_main____str137@PAGE
    add x9, x9, ___cert__entry_main____str137@PAGEOFF
    str x9, [x27, #3688]
    ldrsb w0, [x27, #3680]
    ldr x1, [x27, #3688]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 706 26
    .loc 1 706 27
    .loc 1 706 26
    mov w9, wzr
    movz w10, #0x5
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 706 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3696]
    .loc 1 707 31
    .loc 1 707 24
    ldr w9, [x27, #3696]
    str w9, [x27, #3712]
    .loc 1 709 9
    .loc 1 707 24
    ldr w9, [x27, #3712]
    movz w10, #0xfff6
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__match_arm97
    b L__cert__entry_main__match_next101
L__cert__entry_main__match_next101:
    .loc 1 710 9
    .loc 1 707 24
    ldr w9, [x27, #3712]
    movz w10, #0xfffb
    movk w10, #0xffff, lsl #16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__match_arm98
    b L__cert__entry_main__match_next102
L__cert__entry_main__match_next102:
    .loc 1 711 9
    .loc 1 707 24
    ldr w9, [x27, #3712]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__match_arm99
    b L__cert__entry_main__match_arm100
L__cert__entry_main__match_arm97:
    .loc 1 709 16
    .loc 1 707 24
    movz w9, #0x3e8
    str w9, [x27, #3720]
    b L__cert__entry_main__match_end103
L__cert__entry_main__match_arm98:
    .loc 1 710 15
    .loc 1 707 24
    movz w9, #0x1f4
    str w9, [x27, #3720]
    b L__cert__entry_main__match_end103
L__cert__entry_main__match_arm99:
    .loc 1 711 14
    .loc 1 707 24
    mov w9, wzr
    str w9, [x27, #3720]
    b L__cert__entry_main__match_end103
L__cert__entry_main__match_arm100:
    .loc 1 712 14
    .loc 1 707 24
    movz w9, #0x63
    str w9, [x27, #3720]
    b L__cert__entry_main__match_end103
L__cert__entry_main__match_end103:
    .loc 1 707 9
    ldr w9, [x27, #3720]
    str w9, [x27, #3704]
    .loc 1 714 12
    .loc 1 714 28
    .loc 1 714 25
    ldr w9, [x27, #3704]
    movz w10, #0x1f4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 714 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3728]
    .loc 1 714 33
    .loc 1 714 5
    adrp x9, ___cert__entry_main____str138@PAGE
    add x9, x9, ___cert__entry_main____str138@PAGEOFF
    str x9, [x27, #3736]
    ldrsb w0, [x27, #3728]
    ldr x1, [x27, #3736]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 716 22
    .loc 1 716 20
    movz w9, #0x7b
    str w9, [x27, #3696]
    .loc 1 717 32
    .loc 1 717 25
    ldr w9, [x27, #3696]
    str w9, [x27, #3752]
    .loc 1 719 9
    .loc 1 717 25
    ldr w9, [x27, #3752]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__match_arm104
    b L__cert__entry_main__match_next107
L__cert__entry_main__match_next107:
    .loc 1 720 9
    .loc 1 717 25
    ldr w9, [x27, #3752]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__match_arm105
    b L__cert__entry_main__match_arm106
L__cert__entry_main__match_arm104:
    .loc 1 719 14
    .loc 1 717 25
    mov w9, wzr
    str w9, [x27, #3760]
    b L__cert__entry_main__match_end108
L__cert__entry_main__match_arm105:
    .loc 1 720 14
    .loc 1 717 25
    movz w9, #0x1
    str w9, [x27, #3760]
    b L__cert__entry_main__match_end108
L__cert__entry_main__match_arm106:
    .loc 1 721 14
    .loc 1 721 31
    .loc 1 721 29
    ldr w9, [x27, #3696]
    movz w10, #0x3
    sdiv w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 717 25
    ldr w9, [x27, #5168]
    str w9, [x27, #3760]
    b L__cert__entry_main__match_end108
L__cert__entry_main__match_end108:
    .loc 1 717 9
    ldr w9, [x27, #3760]
    str w9, [x27, #3744]
    .loc 1 723 12
    .loc 1 723 29
    .loc 1 723 26
    ldr w9, [x27, #3744]
    movz w10, #0x29
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 723 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3768]
    .loc 1 723 33
    .loc 1 723 5
    adrp x9, ___cert__entry_main____str139@PAGE
    add x9, x9, ___cert__entry_main____str139@PAGEOFF
    str x9, [x27, #3776]
    ldrsb w0, [x27, #3768]
    ldr x1, [x27, #3776]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 725 48
    .loc 1 725 30
    movz x12, #0x400c, lsl #48
    fmov d0, x12
    str d0, [x27, #3792]
    .loc 1 725 53
    .loc 1 725 30
    movz x12, #0x4004, lsl #48
    fmov d0, x12
    str d0, [x27, #3800]
    ldr d0, [x27, #3792]
    ldr d1, [x27, #3800]
    bl _Test_Program_function_overload_f64_f64
    str x0, [x27, #5168]
    .loc 1 725 9
    ldr d0, [x27, #5168]
    str d0, [x27, #3784]
    .loc 1 726 12
    .loc 1 726 34
    .loc 1 726 31
    ldr d0, [x27, #3784]
    movz x12, #0x4018, lsl #48
    fmov d1, x12
    fcmp d0, d1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 726 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3808]
    .loc 1 726 39
    .loc 1 726 5
    adrp x9, ___cert__entry_main____str140@PAGE
    add x9, x9, ___cert__entry_main____str140@PAGEOFF
    str x9, [x27, #3816]
    ldrsb w0, [x27, #3808]
    ldr x1, [x27, #3816]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 728 46
    .loc 1 728 28
    movz w9, #0xa
    str w9, [x27, #3832]
    .loc 1 728 50
    .loc 1 728 28
    movz w9, #0x20
    str w9, [x27, #3840]
    ldr w0, [x27, #3832]
    ldr w1, [x27, #3840]
    bl _Test_Program_function_overload_i32_i32
    str w0, [x27, #5168]
    .loc 1 728 9
    ldr w9, [x27, #5168]
    str w9, [x27, #3824]
    .loc 1 729 12
    .loc 1 729 32
    .loc 1 729 29
    ldr w9, [x27, #3824]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 729 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3848]
    .loc 1 729 36
    .loc 1 729 5
    adrp x9, ___cert__entry_main____str141@PAGE
    add x9, x9, ___cert__entry_main____str141@PAGEOFF
    str x9, [x27, #3856]
    ldrsb w0, [x27, #3848]
    ldr x1, [x27, #3856]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 731 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3864]
    .loc 1 731 58
    .loc 1 731 40
    movz w9, #0x3
    str w9, [x27, #3888]
    .loc 1 732 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3896]
    ldr x9, [x27, #3896]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3896]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3896]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 732 37
    .loc 1 732 21
    ldr x9, [x27, #5168]
    movz w10, #0x1
    strb w10, [x9]
    .loc 1 732 51
    .loc 1 732 21
    ldr x9, [x27, #3896]
    movz w10, #0xa
    str w10, [x9]
    .loc 1 731 40
    ldr x9, [x27, #3896]
    str x9, [x27, #3904]
    .loc 1 733 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3912]
    ldr x9, [x27, #3912]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3912]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3912]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 733 37
    .loc 1 733 21
    ldr x9, [x27, #5168]
    movz w10, #0x2
    strb w10, [x9]
    .loc 1 733 51
    .loc 1 733 21
    ldr x9, [x27, #3912]
    movz w10, #0x14
    str w10, [x9]
    .loc 1 731 40
    ldr x9, [x27, #3912]
    str x9, [x27, #3920]
    .loc 1 734 21
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #3928]
    ldr x9, [x27, #3928]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #3928]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #3928]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 734 37
    .loc 1 734 21
    ldr x9, [x27, #5168]
    movz w10, #0x3
    strb w10, [x9]
    .loc 1 734 51
    .loc 1 734 21
    ldr x9, [x27, #3928]
    movz w10, #0x1e
    str w10, [x9]
    .loc 1 731 40
    ldr x9, [x27, #3928]
    str x9, [x27, #3936]
    ldr w0, [x27, #3888]
    ldr x1, [x27, #3904]
    ldr x2, [x27, #3920]
    ldr x3, [x27, #3936]
    sub sp, sp, #32
    ldr x9, [x27, #3904]
    str x9, [sp, #0]
    ldr x9, [x27, #3920]
    str x9, [sp, #8]
    ldr x9, [x27, #3936]
    str x9, [sp, #16]
    mov x15, sp
    bl _Test_Program_function_overload_i32_varargs
    add sp, sp, #32
    str x0, [x27, #5168]
    .loc 1 731 18
    ldr x9, [x27, #5168]
    str x9, [x27, #3872]
    ldr x9, [x27, #3864]
    str x9, [x27, #3880]
    ldr x9, [x27, #3872]
    ldr w10, [x9]
    str w10, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3944]
    ldr x9, [x27, #3880]
    ldr w10, [x27, #3944]
    str w10, [x9]
    ldr x9, [x27, #3872]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    ldrb w9, [x27, #5168]
    strb w9, [x27, #3952]
    ldr x9, [x27, #3880]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x27, #3952]
    strb w10, [x9]
    .loc 1 736 12
    .loc 1 736 32
    ldr x9, [x27, #3864]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 736 43
    .loc 1 736 40
    ldr w9, [x27, #5168]
    movz w10, #0x3c
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 736 46
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs112
    b L__cert__entry_main__land_false113
L__cert__entry_main__land_rhs112:
    .loc 1 736 49
    .loc 1 736 69
    ldr x9, [x27, #3864]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldrb w10, [x9]
    strb w10, [x27, #5168]
    .loc 1 736 80
    ldrb w9, [x27, #5168]
    uxtb w9, w9
    str w9, [x27, #5168]
    .loc 1 736 83
    .loc 1 736 80
    ldr w9, [x27, #5168]
    movz w10, #0x6
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 736 46
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true114
    b L__cert__entry_main__land_false113
L__cert__entry_main__land_true114:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end115
L__cert__entry_main__land_false113:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end115
L__cert__entry_main__land_end115:
    .loc 1 736 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #3960]
    .loc 1 736 86
    .loc 1 736 5
    adrp x9, ___cert__entry_main____str142@PAGE
    add x9, x9, ___cert__entry_main____str142@PAGEOFF
    str x9, [x27, #3968]
    ldrsb w0, [x27, #3960]
    ldr x1, [x27, #3968]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 739 23
    .loc 1 739 9
    movz w9, #0xa
    str w9, [x27, #3976]
    .loc 1 740 17
    ldr w9, [x27, #3976]
    str w9, [x27, #3984]
    .loc 1 740 20
    .loc 1 740 17
    movz w9, #0x5
    str w9, [x27, #3992]
    ldr w9, [x27, #3984]
    ldr w10, [x27, #3992]
    add w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 741 12
    .loc 1 741 27
    .loc 1 741 24
    ldr w9, [x27, #3976]
    movz w10, #0xf
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 741 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4000]
    .loc 1 741 31
    .loc 1 741 5
    adrp x9, ___cert__entry_main____str143@PAGE
    add x9, x9, ___cert__entry_main____str143@PAGEOFF
    str x9, [x27, #4008]
    ldrsb w0, [x27, #4000]
    ldr x1, [x27, #4008]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 742 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4016]
    .loc 1 742 20
    .loc 1 742 17
    movz w9, #0x3
    str w9, [x27, #4024]
    ldr w9, [x27, #4016]
    ldr w10, [x27, #4024]
    sub w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 743 12
    .loc 1 743 27
    .loc 1 743 24
    ldr w9, [x27, #3976]
    movz w10, #0xc
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 743 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4032]
    .loc 1 743 31
    .loc 1 743 5
    adrp x9, ___cert__entry_main____str144@PAGE
    add x9, x9, ___cert__entry_main____str144@PAGEOFF
    str x9, [x27, #4040]
    ldrsb w0, [x27, #4032]
    ldr x1, [x27, #4040]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 744 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4048]
    .loc 1 744 20
    .loc 1 744 17
    movz w9, #0x2
    str w9, [x27, #4056]
    ldr w9, [x27, #4048]
    ldr w10, [x27, #4056]
    mul w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 745 12
    .loc 1 745 27
    .loc 1 745 24
    ldr w9, [x27, #3976]
    movz w10, #0x18
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 745 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4064]
    .loc 1 745 31
    .loc 1 745 5
    adrp x9, ___cert__entry_main____str145@PAGE
    add x9, x9, ___cert__entry_main____str145@PAGEOFF
    str x9, [x27, #4072]
    ldrsb w0, [x27, #4064]
    ldr x1, [x27, #4072]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 746 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4080]
    .loc 1 746 20
    .loc 1 746 17
    movz w9, #0x4
    str w9, [x27, #4088]
    ldr w9, [x27, #4080]
    ldr w10, [x27, #4088]
    sdiv w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 747 12
    .loc 1 747 27
    .loc 1 747 24
    ldr w9, [x27, #3976]
    movz w10, #0x6
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 747 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4096]
    .loc 1 747 30
    .loc 1 747 5
    adrp x9, ___cert__entry_main____str146@PAGE
    add x9, x9, ___cert__entry_main____str146@PAGEOFF
    str x9, [x27, #4104]
    ldrsb w0, [x27, #4096]
    ldr x1, [x27, #4104]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 748 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4112]
    .loc 1 748 20
    .loc 1 748 17
    movz w9, #0x4
    str w9, [x27, #4120]
    ldr w9, [x27, #4112]
    ldr w10, [x27, #4120]
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 749 12
    .loc 1 749 27
    .loc 1 749 24
    ldr w9, [x27, #3976]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 749 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4128]
    .loc 1 749 30
    .loc 1 749 5
    adrp x9, ___cert__entry_main____str147@PAGE
    add x9, x9, ___cert__entry_main____str147@PAGEOFF
    str x9, [x27, #4136]
    ldrsb w0, [x27, #4128]
    ldr x1, [x27, #4136]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 750 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4144]
    .loc 1 750 21
    .loc 1 750 17
    movz w9, #0x2
    str w9, [x27, #4152]
    ldr w9, [x27, #4144]
    ldr w10, [x27, #4152]
    lsl w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 751 12
    .loc 1 751 27
    .loc 1 751 24
    ldr w9, [x27, #3976]
    movz w10, #0x8
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 751 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4160]
    .loc 1 751 30
    .loc 1 751 5
    adrp x9, ___cert__entry_main____str148@PAGE
    add x9, x9, ___cert__entry_main____str148@PAGEOFF
    str x9, [x27, #4168]
    ldrsb w0, [x27, #4160]
    ldr x1, [x27, #4168]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 752 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4176]
    .loc 1 752 21
    .loc 1 752 17
    movz w9, #0x1
    str w9, [x27, #4184]
    ldr w9, [x27, #4176]
    ldr w10, [x27, #4184]
    asr w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 753 12
    .loc 1 753 27
    .loc 1 753 24
    ldr w9, [x27, #3976]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 753 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4192]
    .loc 1 753 30
    .loc 1 753 5
    adrp x9, ___cert__entry_main____str149@PAGE
    add x9, x9, ___cert__entry_main____str149@PAGEOFF
    str x9, [x27, #4200]
    ldrsb w0, [x27, #4192]
    ldr x1, [x27, #4200]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 754 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4208]
    .loc 1 754 20
    .loc 1 754 17
    movz w9, #0x6
    str w9, [x27, #4216]
    ldr w9, [x27, #4208]
    ldr w10, [x27, #4216]
    and w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 755 12
    .loc 1 755 27
    .loc 1 755 24
    ldr w9, [x27, #3976]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 755 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4224]
    .loc 1 755 35
    .loc 1 755 5
    adrp x9, ___cert__entry_main____str150@PAGE
    add x9, x9, ___cert__entry_main____str150@PAGEOFF
    str x9, [x27, #4232]
    ldrsb w0, [x27, #4224]
    ldr x1, [x27, #4232]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 756 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4240]
    .loc 1 756 20
    .loc 1 756 17
    movz w9, #0x3
    str w9, [x27, #4248]
    ldr w9, [x27, #4240]
    ldr w10, [x27, #4248]
    orr w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 757 12
    .loc 1 757 27
    .loc 1 757 24
    ldr w9, [x27, #3976]
    movz w10, #0x7
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 757 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4256]
    .loc 1 757 35
    .loc 1 757 5
    adrp x9, ___cert__entry_main____str151@PAGE
    add x9, x9, ___cert__entry_main____str151@PAGEOFF
    str x9, [x27, #4264]
    ldrsb w0, [x27, #4256]
    ldr x1, [x27, #4264]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 758 17
    ldr w9, [x27, #3976]
    str w9, [x27, #4272]
    .loc 1 758 20
    .loc 1 758 17
    movz w9, #0x5
    str w9, [x27, #4280]
    ldr w9, [x27, #4272]
    ldr w10, [x27, #4280]
    eor w9, w9, w10
    str w9, [x27, #5168]
    ldr w9, [x27, #5168]
    str w9, [x27, #3976]
    .loc 1 759 12
    .loc 1 759 27
    .loc 1 759 24
    ldr w9, [x27, #3976]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 759 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4288]
    .loc 1 759 35
    .loc 1 759 5
    adrp x9, ___cert__entry_main____str152@PAGE
    add x9, x9, ___cert__entry_main____str152@PAGEOFF
    str x9, [x27, #4296]
    ldrsb w0, [x27, #4288]
    ldr x1, [x27, #4296]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 761 22
    .loc 1 761 9
    movz w12, #0x3fc0, lsl #16
    fmov s0, w12
    str s0, [x27, #4304]
    .loc 1 762 16
    ldr s0, [x27, #4304]
    str s0, [x27, #4312]
    .loc 1 762 19
    .loc 1 762 16
    movz w12, #0x4020, lsl #16
    fmov s0, w12
    str s0, [x27, #4320]
    ldr s0, [x27, #4312]
    ldr s1, [x27, #4320]
    fadd s0, s0, s1
    str s0, [x27, #5168]
    ldr s0, [x27, #5168]
    str s0, [x27, #4304]
    .loc 1 763 12
    .loc 1 763 26
    .loc 1 763 23
    ldr s0, [x27, #4304]
    movz w12, #0x4080, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 763 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4328]
    .loc 1 763 32
    .loc 1 763 5
    adrp x9, ___cert__entry_main____str153@PAGE
    add x9, x9, ___cert__entry_main____str153@PAGEOFF
    str x9, [x27, #4336]
    ldrsb w0, [x27, #4328]
    ldr x1, [x27, #4336]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 764 16
    ldr s0, [x27, #4304]
    str s0, [x27, #4344]
    .loc 1 764 19
    .loc 1 764 16
    movz w12, #0x4000, lsl #16
    fmov s0, w12
    str s0, [x27, #4352]
    ldr s0, [x27, #4344]
    ldr s1, [x27, #4352]
    fmul s0, s0, s1
    str s0, [x27, #5168]
    ldr s0, [x27, #5168]
    str s0, [x27, #4304]
    .loc 1 765 12
    .loc 1 765 26
    .loc 1 765 23
    ldr s0, [x27, #4304]
    movz w12, #0x4100, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 765 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4360]
    .loc 1 765 32
    .loc 1 765 5
    adrp x9, ___cert__entry_main____str154@PAGE
    add x9, x9, ___cert__entry_main____str154@PAGEOFF
    str x9, [x27, #4368]
    ldrsb w0, [x27, #4360]
    ldr x1, [x27, #4368]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 766 16
    ldr s0, [x27, #4304]
    str s0, [x27, #4376]
    .loc 1 766 19
    .loc 1 766 16
    movz w12, #0x4080, lsl #16
    fmov s0, w12
    str s0, [x27, #4384]
    ldr s0, [x27, #4376]
    ldr s1, [x27, #4384]
    fdiv s0, s0, s1
    str s0, [x27, #5168]
    ldr s0, [x27, #5168]
    str s0, [x27, #4304]
    .loc 1 767 12
    .loc 1 767 26
    .loc 1 767 23
    ldr s0, [x27, #4304]
    movz w12, #0x4000, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 767 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4392]
    .loc 1 767 32
    .loc 1 767 5
    adrp x9, ___cert__entry_main____str155@PAGE
    add x9, x9, ___cert__entry_main____str155@PAGEOFF
    str x9, [x27, #4400]
    ldrsb w0, [x27, #4392]
    ldr x1, [x27, #4400]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 768 16
    ldr s0, [x27, #4304]
    str s0, [x27, #4408]
    .loc 1 768 19
    .loc 1 768 16
    movz w12, #0x3f80, lsl #16
    fmov s0, w12
    str s0, [x27, #4416]
    ldr s0, [x27, #4408]
    ldr s1, [x27, #4416]
    fsub s0, s0, s1
    str s0, [x27, #5168]
    ldr s0, [x27, #5168]
    str s0, [x27, #4304]
    .loc 1 769 12
    .loc 1 769 26
    .loc 1 769 23
    ldr s0, [x27, #4304]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 769 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4424]
    .loc 1 769 32
    .loc 1 769 5
    adrp x9, ___cert__entry_main____str156@PAGE
    add x9, x9, ___cert__entry_main____str156@PAGEOFF
    str x9, [x27, #4432]
    ldrsb w0, [x27, #4424]
    ldr x1, [x27, #4432]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 770 16
    ldr s0, [x27, #4304]
    str s0, [x27, #4440]
    .loc 1 770 19
    .loc 1 770 21
    movz w9, #0x3
    scvtf s0, w9
    fmov w9, s0
    str w9, [x27, #5168]
    .loc 1 770 16
    ldr s0, [x27, #5168]
    str s0, [x27, #4448]
    ldr s0, [x27, #4440]
    ldr s1, [x27, #4448]
    fadd s0, s0, s1
    str s0, [x27, #5168]
    ldr s0, [x27, #5168]
    str s0, [x27, #4304]
    .loc 1 771 12
    .loc 1 771 26
    .loc 1 771 23
    ldr s0, [x27, #4304]
    movz w12, #0x4080, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 771 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4456]
    .loc 1 771 32
    .loc 1 771 5
    adrp x9, ___cert__entry_main____str157@PAGE
    add x9, x9, ___cert__entry_main____str157@PAGEOFF
    str x9, [x27, #4464]
    ldrsb w0, [x27, #4456]
    ldr x1, [x27, #4464]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 775 9
    .loc 1 774 27
    movz w9, #0x1
    str w9, [x27, #4480]
    .loc 1 775 12
    .loc 1 774 27
    movz w9, #0x2
    str w9, [x27, #4488]
    .loc 1 775 15
    .loc 1 774 27
    movz w9, #0x3
    str w9, [x27, #4496]
    .loc 1 776 9
    .loc 1 774 27
    movz w9, #0x4
    str w9, [x27, #4504]
    .loc 1 776 12
    .loc 1 774 27
    movz w9, #0x5
    str w9, [x27, #4512]
    .loc 1 776 15
    .loc 1 774 27
    movz w9, #0x6
    str w9, [x27, #4520]
    .loc 1 777 9
    .loc 1 774 27
    movz w9, #0x7
    str w9, [x27, #4528]
    .loc 1 777 12
    .loc 1 774 27
    movz w9, #0x8
    str w9, [x27, #4536]
    .loc 1 777 15
    .loc 1 774 27
    movz w9, #0x9
    str w9, [x27, #4544]
    .loc 1 778 9
    .loc 1 774 27
    movz w9, #0xa
    str w9, [x27, #4552]
    .loc 1 778 13
    .loc 1 774 27
    movz w9, #0xb
    str w9, [x27, #4560]
    .loc 1 778 17
    .loc 1 774 27
    movz w9, #0xc
    str w9, [x27, #4568]
    .loc 1 779 9
    .loc 1 774 27
    movz w9, #0xd
    str w9, [x27, #4576]
    .loc 1 779 13
    .loc 1 774 27
    movz w9, #0xe
    str w9, [x27, #4584]
    .loc 1 779 17
    .loc 1 774 27
    movz w9, #0xf
    str w9, [x27, #4592]
    .loc 1 780 9
    .loc 1 774 27
    movz w9, #0x10
    str w9, [x27, #4600]
    .loc 1 780 13
    .loc 1 774 27
    movz w9, #0x11
    str w9, [x27, #4608]
    .loc 1 780 17
    .loc 1 774 27
    movz w9, #0x12
    str w9, [x27, #4616]
    .loc 1 781 9
    .loc 1 774 27
    movz w9, #0x13
    str w9, [x27, #4624]
    .loc 1 781 13
    .loc 1 774 27
    movz w9, #0x14
    str w9, [x27, #4632]
    .loc 1 781 17
    .loc 1 774 27
    movz w9, #0x15
    str w9, [x27, #4640]
    .loc 1 782 9
    .loc 1 774 27
    movz w9, #0x16
    str w9, [x27, #4648]
    .loc 1 782 13
    .loc 1 774 27
    movz w9, #0x17
    str w9, [x27, #4656]
    .loc 1 782 17
    .loc 1 774 27
    movz w9, #0x18
    str w9, [x27, #4664]
    .loc 1 783 9
    .loc 1 774 27
    movz w9, #0x19
    str w9, [x27, #4672]
    .loc 1 783 13
    .loc 1 774 27
    movz w9, #0x1a
    str w9, [x27, #4680]
    ldr w0, [x27, #4480]
    ldr w1, [x27, #4488]
    ldr w2, [x27, #4496]
    ldr w3, [x27, #4504]
    ldr w4, [x27, #4512]
    ldr w5, [x27, #4520]
    ldr w6, [x27, #4528]
    ldr w7, [x27, #4536]
    sub sp, sp, #144
    ldr w9, [x27, #4544]
    str w9, [sp, #0]
    ldr w9, [x27, #4552]
    str w9, [sp, #8]
    ldr w9, [x27, #4560]
    str w9, [sp, #16]
    ldr w9, [x27, #4568]
    str w9, [sp, #24]
    ldr w9, [x27, #4576]
    str w9, [sp, #32]
    ldr w9, [x27, #4584]
    str w9, [sp, #40]
    ldr w9, [x27, #4592]
    str w9, [sp, #48]
    ldr w9, [x27, #4600]
    str w9, [sp, #56]
    ldr w9, [x27, #4608]
    str w9, [sp, #64]
    ldr w9, [x27, #4616]
    str w9, [sp, #72]
    ldr w9, [x27, #4624]
    str w9, [sp, #80]
    ldr w9, [x27, #4632]
    str w9, [sp, #88]
    ldr w9, [x27, #4640]
    str w9, [sp, #96]
    ldr w9, [x27, #4648]
    str w9, [sp, #104]
    ldr w9, [x27, #4656]
    str w9, [sp, #112]
    ldr w9, [x27, #4664]
    str w9, [sp, #120]
    ldr w9, [x27, #4672]
    str w9, [sp, #128]
    ldr w9, [x27, #4680]
    str w9, [sp, #136]
    bl _Test_Program_super_args_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32
    add sp, sp, #144
    str w0, [x27, #5168]
    .loc 1 774 9
    ldr w9, [x27, #5168]
    str w9, [x27, #4472]
    .loc 1 785 12
    .loc 1 785 31
    .loc 1 785 32
    .loc 1 785 31
    mov w9, wzr
    movz w10, #0x6e80, lsl #16
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 785 28
    ldr w9, [x27, #4472]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 785 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4688]
    .loc 1 785 44
    .loc 1 785 5
    adrp x9, ___cert__entry_main____str158@PAGE
    add x9, x9, ___cert__entry_main____str158@PAGEOFF
    str x9, [x27, #4696]
    ldrsb w0, [x27, #4688]
    ldr x1, [x27, #4696]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 786 12
    .loc 1 786 30
    .loc 1 786 28
    ldr w9, [x27, #4472]
    movz w10, #0x14
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #5168]
    .loc 1 786 36
    .loc 1 786 37
    .loc 1 786 36
    mov w9, wzr
    movz w10, #0x8
    sub w9, w9, w10
    str w9, [x27, #5176]
    .loc 1 786 33
    ldr w9, [x27, #5168]
    ldr w10, [x27, #5176]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 786 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4704]
    .loc 1 786 40
    .loc 1 786 5
    adrp x9, ___cert__entry_main____str159@PAGE
    add x9, x9, ___cert__entry_main____str159@PAGEOFF
    str x9, [x27, #4712]
    ldrsb w0, [x27, #4704]
    ldr x1, [x27, #4712]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 788 26
    adrp x9, __lambda_1@PAGE
    add x9, x9, __lambda_1@PAGEOFF
    str x9, [x27, #5168]
    ldr x16, [x27, #5168]
    blr x16
    adrp x9, __lambda_1@PAGE
    add x9, x9, __lambda_1@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 788 12
    ldr x9, [x27, #5168]
    str x9, [x27, #4720]
    .loc 1 793 5
    ldr x16, [x27, #4720]
    blr x16
    .loc 1 795 27
    adrp x9, __lambda_2@PAGE
    add x9, x9, __lambda_2@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 795 12
    ldr x9, [x27, #5168]
    str x9, [x27, #4728]
    .loc 1 800 38
    .loc 1 800 25
    movz w0, #0x29
    ldr x16, [x27, #4728]
    blr x16
    strh w0, [x27, #5168]
    .loc 1 800 9
    ldrsh w9, [x27, #5168]
    strh w9, [x27, #4736]
    .loc 1 801 12
    .loc 1 801 26
    ldrsh w9, [x27, #4736]
    sxth w9, w9
    str w9, [x27, #5168]
    .loc 1 801 29
    .loc 1 801 26
    ldr w9, [x27, #5168]
    movz w10, #0x2a
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 801 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4744]
    .loc 1 801 33
    .loc 1 801 5
    adrp x9, ___cert__entry_main____str160@PAGE
    add x9, x9, ___cert__entry_main____str160@PAGEOFF
    str x9, [x27, #4752]
    ldrsb w0, [x27, #4744]
    ldr x1, [x27, #4752]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 803 9
    adrp x9, __lambda_3@PAGE
    add x9, x9, __lambda_3@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 802 5
    ldr x9, [x27, #5168]
    str x9, [x27, #4760]
    .loc 1 807 9
    adrp x9, __lambda_4@PAGE
    add x9, x9, __lambda_4@PAGEOFF
    str x9, [x27, #5168]
    .loc 1 802 5
    ldr x9, [x27, #5168]
    str x9, [x27, #4768]
    ldr x0, [x27, #4760]
    ldr x1, [x27, #4768]
    bl _Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16
    .loc 1 813 17
    .loc 1 813 5
    ldr x9, [x27, #4720]
    str x9, [x27, #4776]
    .loc 1 813 55
    .loc 1 813 5
    ldr x9, [x27, #4728]
    str x9, [x27, #4784]
    ldr x0, [x27, #4776]
    ldr x1, [x27, #4784]
    bl _Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16
    .loc 1 821 5
    bl _Test_Program_file_io_tests
    .loc 1 823 20
    .loc 1 823 9
    movz w9, #0x58
    str w9, [x27, #4792]
    .loc 1 825 12
    .loc 1 825 25
    .loc 1 825 21
    ldr w9, [x27, #4792]
    movz w10, #0x58
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 825 5
    ldrb w9, [x27, #5168]
    strb w9, [x27, #4800]
    .loc 1 825 37
    .loc 1 825 5
    adrp x9, ___cert__entry_main____str161@PAGE
    add x9, x9, ___cert__entry_main____str161@PAGEOFF
    str x9, [x27, #4808]
    ldrsb w0, [x27, #4800]
    ldr x1, [x27, #4808]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 827 25
    sub sp, sp, #96
    mov x10, sp
    str x10, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #4816]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x5
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x6
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x7
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x8
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x9
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xa
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xb
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xc
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xd
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xe
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0xf
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x10
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x11
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x12
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x13
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x14
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x15
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x16
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x17
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x18
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x19
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x1f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x20
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x21
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x22
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x23
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x24
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x25
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x26
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x27
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x28
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x29
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x2f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x30
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x31
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x32
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x33
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x34
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x35
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x36
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x37
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x38
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x39
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x3f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x40
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x41
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x42
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x43
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x44
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x45
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x46
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x47
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x48
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x49
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x4f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x50
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x51
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x52
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    movz w9, #0x53
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x4
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x5
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x6
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x40
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x7
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x8
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x9
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xa
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xb
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xc
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xd
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xe
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0xf
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x10
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcd
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x11
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcc
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x12
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x4c
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x13
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3e
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x14
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcd
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x15
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcc
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x16
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x4c
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x17
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3e
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x18
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x19
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x1a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 828 9
    .loc 1 827 25
    movz w9, #0x1b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x1c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x1d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x1e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x40
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x1f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xbf
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x20
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x21
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x22
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x23
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xbf
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x24
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x25
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x26
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x27
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x28
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcd
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x29
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcc
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x4c
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3e
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x2f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x30
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x9a
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x31
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x99
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x32
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x99
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x33
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3e
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x34
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x35
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x36
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 829 9
    .loc 1 827 25
    movz w9, #0x37
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x38
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x39
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x40
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x33
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x3f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xbf
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x40
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x41
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x42
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x43
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x44
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcd
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x45
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0xcc
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x46
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x4c
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x47
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3e
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x48
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x49
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4a
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4b
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4c
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4d
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4e
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x4f
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x50
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x51
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    mov w10, wzr
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x52
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x80
    strb w10, [x9]
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 830 9
    .loc 1 827 25
    movz w9, #0x53
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x3f
    strb w10, [x9]
    .loc 1 834 12
    .loc 1 834 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 834 21
    .loc 1 834 20
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 32
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 33
    .loc 1 834 32
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 834 39
    .loc 1 834 36
    ldr s0, [x27, #5168]
    mov w12, wzr
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 834 44
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs116
    b L__cert__entry_main__land_false117
L__cert__entry_main__land_rhs116:
    .loc 1 834 47
    .loc 1 834 55
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 834 56
    .loc 1 834 55
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 67
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 68
    .loc 1 834 67
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 834 74
    .loc 1 834 71
    ldr s0, [x27, #5168]
    movz w12, #0x3f40, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 834 44
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true118
    b L__cert__entry_main__land_false117
L__cert__entry_main__land_true118:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end119
L__cert__entry_main__land_false117:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end119
L__cert__entry_main__land_end119:
    .loc 1 834 80
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs120
    b L__cert__entry_main__land_false121
L__cert__entry_main__land_rhs120:
    .loc 1 834 83
    .loc 1 834 91
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 834 92
    .loc 1 834 91
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 103
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 834 104
    .loc 1 834 103
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 834 110
    .loc 1 834 107
    ldr s0, [x27, #5168]
    mov w12, wzr
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 834 80
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true122
    b L__cert__entry_main__land_false121
L__cert__entry_main__land_true122:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end123
L__cert__entry_main__land_false121:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end123
L__cert__entry_main__land_end123:
    .loc 1 834 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4824]
    .loc 1 834 116
    .loc 1 834 5
    adrp x9, ___cert__entry_main____str162@PAGE
    add x9, x9, ___cert__entry_main____str162@PAGEOFF
    str x9, [x27, #4832]
    ldrsb w0, [x27, #4824]
    ldr x1, [x27, #4832]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 835 12
    .loc 1 835 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 835 21
    .loc 1 835 20
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 24
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 29
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 30
    .loc 1 835 29
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 835 36
    .loc 1 835 33
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 835 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs124
    b L__cert__entry_main__land_false125
L__cert__entry_main__land_rhs124:
    .loc 1 835 44
    .loc 1 835 52
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 835 53
    .loc 1 835 52
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 56
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 61
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 62
    .loc 1 835 61
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 835 68
    .loc 1 835 65
    ldr s0, [x27, #5168]
    movz w12, #0xcccd
    movk w12, #0x3e4c, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 835 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true126
    b L__cert__entry_main__land_false125
L__cert__entry_main__land_true126:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end127
L__cert__entry_main__land_false125:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end127
L__cert__entry_main__land_end127:
    .loc 1 835 73
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs128
    b L__cert__entry_main__land_false129
L__cert__entry_main__land_rhs128:
    .loc 1 835 76
    .loc 1 835 84
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 835 85
    .loc 1 835 84
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 88
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 93
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 94
    .loc 1 835 93
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 835 100
    .loc 1 835 97
    ldr s0, [x27, #5168]
    movz w12, #0xcccd
    movk w12, #0x3e4c, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 835 73
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true130
    b L__cert__entry_main__land_false129
L__cert__entry_main__land_true130:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end131
L__cert__entry_main__land_false129:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end131
L__cert__entry_main__land_end131:
    .loc 1 835 105
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs132
    b L__cert__entry_main__land_false133
L__cert__entry_main__land_rhs132:
    .loc 1 835 108
    .loc 1 835 116
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 835 117
    .loc 1 835 116
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 120
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 125
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 835 126
    .loc 1 835 125
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 835 132
    .loc 1 835 129
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 835 105
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true134
    b L__cert__entry_main__land_false133
L__cert__entry_main__land_true134:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end135
L__cert__entry_main__land_false133:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end135
L__cert__entry_main__land_end135:
    .loc 1 835 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4840]
    .loc 1 835 138
    .loc 1 835 5
    adrp x9, ___cert__entry_main____str163@PAGE
    add x9, x9, ___cert__entry_main____str163@PAGEOFF
    str x9, [x27, #4848]
    ldrsb w0, [x27, #4840]
    ldr x1, [x27, #4848]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 836 12
    .loc 1 836 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 836 21
    .loc 1 836 20
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 32
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 33
    .loc 1 836 32
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 836 39
    .loc 1 836 40
    .loc 1 836 39
    mov w12, wzr
    fmov s0, w12
    movz w12, #0x3f40, lsl #16
    fmov s1, w12
    fsub s0, s0, s1
    str s0, [x27, #5176]
    .loc 1 836 36
    ldr s0, [x27, #5168]
    ldr s1, [x27, #5176]
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 836 46
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs136
    b L__cert__entry_main__land_false137
L__cert__entry_main__land_rhs136:
    .loc 1 836 49
    .loc 1 836 57
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 836 58
    .loc 1 836 57
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 69
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 70
    .loc 1 836 69
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 836 76
    .loc 1 836 77
    .loc 1 836 76
    mov w12, wzr
    fmov s0, w12
    movz w12, #0x3333
    movk w12, #0x3f33, lsl #16
    fmov s1, w12
    fsub s0, s0, s1
    str s0, [x27, #5176]
    .loc 1 836 73
    ldr s0, [x27, #5168]
    ldr s1, [x27, #5176]
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 836 46
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true138
    b L__cert__entry_main__land_false137
L__cert__entry_main__land_true138:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end139
L__cert__entry_main__land_false137:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end139
L__cert__entry_main__land_end139:
    .loc 1 836 83
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs140
    b L__cert__entry_main__land_false141
L__cert__entry_main__land_rhs140:
    .loc 1 836 86
    .loc 1 836 94
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 836 95
    .loc 1 836 94
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 106
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 836 107
    .loc 1 836 106
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 836 113
    .loc 1 836 110
    ldr s0, [x27, #5168]
    mov w12, wzr
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 836 83
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true142
    b L__cert__entry_main__land_false141
L__cert__entry_main__land_true142:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end143
L__cert__entry_main__land_false141:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end143
L__cert__entry_main__land_end143:
    .loc 1 836 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4856]
    .loc 1 836 119
    .loc 1 836 5
    adrp x9, ___cert__entry_main____str164@PAGE
    add x9, x9, ___cert__entry_main____str164@PAGEOFF
    str x9, [x27, #4864]
    ldrsb w0, [x27, #4856]
    ldr x1, [x27, #4864]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 837 12
    .loc 1 837 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 837 21
    .loc 1 837 20
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 24
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 29
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 30
    .loc 1 837 29
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 837 36
    .loc 1 837 33
    ldr s0, [x27, #5168]
    movz w12, #0xcccd
    movk w12, #0x3e4c, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 837 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs144
    b L__cert__entry_main__land_false145
L__cert__entry_main__land_rhs144:
    .loc 1 837 44
    .loc 1 837 52
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 837 53
    .loc 1 837 52
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 56
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 61
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 62
    .loc 1 837 61
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 837 68
    .loc 1 837 65
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 837 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true146
    b L__cert__entry_main__land_false145
L__cert__entry_main__land_true146:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end147
L__cert__entry_main__land_false145:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end147
L__cert__entry_main__land_end147:
    .loc 1 837 73
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs148
    b L__cert__entry_main__land_false149
L__cert__entry_main__land_rhs148:
    .loc 1 837 76
    .loc 1 837 84
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 837 85
    .loc 1 837 84
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 88
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 93
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 94
    .loc 1 837 93
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 837 100
    .loc 1 837 97
    ldr s0, [x27, #5168]
    movz w12, #0x999a
    movk w12, #0x3e99, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 837 73
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true150
    b L__cert__entry_main__land_false149
L__cert__entry_main__land_true150:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end151
L__cert__entry_main__land_false149:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end151
L__cert__entry_main__land_end151:
    .loc 1 837 105
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs152
    b L__cert__entry_main__land_false153
L__cert__entry_main__land_rhs152:
    .loc 1 837 108
    .loc 1 837 116
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 837 117
    .loc 1 837 116
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 120
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 125
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 837 126
    .loc 1 837 125
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 837 132
    .loc 1 837 129
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 837 105
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true154
    b L__cert__entry_main__land_false153
L__cert__entry_main__land_true154:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end155
L__cert__entry_main__land_false153:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end155
L__cert__entry_main__land_end155:
    .loc 1 837 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4872]
    .loc 1 837 138
    .loc 1 837 5
    adrp x9, ___cert__entry_main____str165@PAGE
    add x9, x9, ___cert__entry_main____str165@PAGEOFF
    str x9, [x27, #4880]
    ldrsb w0, [x27, #4872]
    ldr x1, [x27, #4880]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 838 12
    .loc 1 838 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 838 21
    .loc 1 838 20
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 32
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 33
    .loc 1 838 32
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 838 39
    .loc 1 838 36
    ldr s0, [x27, #5168]
    movz w12, #0x3f40, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 838 45
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs156
    b L__cert__entry_main__land_false157
L__cert__entry_main__land_rhs156:
    .loc 1 838 48
    .loc 1 838 56
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 838 57
    .loc 1 838 56
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 68
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 69
    .loc 1 838 68
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 838 75
    .loc 1 838 76
    .loc 1 838 75
    mov w12, wzr
    fmov s0, w12
    movz w12, #0x3333
    movk w12, #0x3f33, lsl #16
    fmov s1, w12
    fsub s0, s0, s1
    str s0, [x27, #5176]
    .loc 1 838 72
    ldr s0, [x27, #5168]
    ldr s1, [x27, #5176]
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 838 45
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true158
    b L__cert__entry_main__land_false157
L__cert__entry_main__land_true158:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end159
L__cert__entry_main__land_false157:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end159
L__cert__entry_main__land_end159:
    .loc 1 838 82
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs160
    b L__cert__entry_main__land_false161
L__cert__entry_main__land_rhs160:
    .loc 1 838 85
    .loc 1 838 93
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 838 94
    .loc 1 838 93
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 105
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 838 106
    .loc 1 838 105
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 838 112
    .loc 1 838 109
    ldr s0, [x27, #5168]
    mov w12, wzr
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 838 82
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true162
    b L__cert__entry_main__land_false161
L__cert__entry_main__land_true162:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end163
L__cert__entry_main__land_false161:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end163
L__cert__entry_main__land_end163:
    .loc 1 838 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4888]
    .loc 1 838 118
    .loc 1 838 5
    adrp x9, ___cert__entry_main____str166@PAGE
    add x9, x9, ___cert__entry_main____str166@PAGEOFF
    str x9, [x27, #4896]
    ldrsb w0, [x27, #4888]
    ldr x1, [x27, #4896]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 839 12
    .loc 1 839 20
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 839 21
    .loc 1 839 20
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 24
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 29
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 30
    .loc 1 839 29
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 839 36
    .loc 1 839 33
    ldr s0, [x27, #5168]
    movz w12, #0xcccd
    movk w12, #0x3e4c, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 839 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs164
    b L__cert__entry_main__land_false165
L__cert__entry_main__land_rhs164:
    .loc 1 839 44
    .loc 1 839 52
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 839 53
    .loc 1 839 52
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 56
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 61
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 62
    .loc 1 839 61
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 839 68
    .loc 1 839 65
    ldr s0, [x27, #5168]
    movz w12, #0x3f00, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 839 41
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true166
    b L__cert__entry_main__land_false165
L__cert__entry_main__land_true166:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end167
L__cert__entry_main__land_false165:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end167
L__cert__entry_main__land_end167:
    .loc 1 839 73
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs168
    b L__cert__entry_main__land_false169
L__cert__entry_main__land_rhs168:
    .loc 1 839 76
    .loc 1 839 84
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 839 85
    .loc 1 839 84
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 88
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 93
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 94
    .loc 1 839 93
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 839 100
    .loc 1 839 97
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 839 73
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true170
    b L__cert__entry_main__land_false169
L__cert__entry_main__land_true170:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end171
L__cert__entry_main__land_false169:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end171
L__cert__entry_main__land_end171:
    .loc 1 839 105
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs172
    b L__cert__entry_main__land_false173
L__cert__entry_main__land_rhs172:
    .loc 1 839 108
    .loc 1 839 116
    ldr x9, [x27, #4816]
    str x9, [x27, #5168]
    .loc 1 839 117
    .loc 1 839 116
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x1c
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 120
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0xc
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 125
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    .loc 1 839 126
    .loc 1 839 125
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 839 132
    .loc 1 839 129
    ldr s0, [x27, #5168]
    movz w12, #0x3f80, lsl #16
    fmov s1, w12
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 839 105
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true174
    b L__cert__entry_main__land_false173
L__cert__entry_main__land_true174:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end175
L__cert__entry_main__land_false173:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end175
L__cert__entry_main__land_end175:
    .loc 1 839 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4904]
    .loc 1 839 138
    .loc 1 839 5
    adrp x9, ___cert__entry_main____str167@PAGE
    add x9, x9, ___cert__entry_main____str167@PAGEOFF
    str x9, [x27, #4912]
    ldrsb w0, [x27, #4904]
    ldr x1, [x27, #4912]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 842 29
    .loc 1 842 21
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5168]
    ldr x0, [x27, #5168]
    bl __cert__new
    str x0, [x27, #5168]
    .loc 1 842 10
    ldr x9, [x27, #5168]
    str x9, [x27, #4920]
    .loc 1 843 9
    .loc 1 843 21
    .loc 1 843 18
    ldr x9, [x27, #4920]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 843 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true176
    b L__cert__entry_main__if_end177
L__cert__entry_main__if_true176:
    .loc 1 845 9
    .loc 1 845 21
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 845 18
    .loc 1 845 21
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #4928]
    .loc 1 845 23
    .loc 1 845 21
    movz w9, #0xa
    str w9, [x27, #4936]
    ldr x9, [x27, #4928]
    ldr w10, [x27, #4936]
    str w10, [x9]
    .loc 1 846 9
    .loc 1 846 21
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 846 18
    .loc 1 846 21
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #4944]
    .loc 1 846 23
    .loc 1 846 21
    movz w9, #0x14
    str w9, [x27, #4952]
    ldr x9, [x27, #4944]
    ldr w10, [x27, #4952]
    str w10, [x9]
    .loc 1 847 9
    .loc 1 847 21
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 847 18
    .loc 1 847 21
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #4960]
    .loc 1 847 23
    .loc 1 847 21
    movz w9, #0x1e
    str w9, [x27, #4968]
    ldr x9, [x27, #4960]
    ldr w10, [x27, #4968]
    str w10, [x9]
    .loc 1 848 16
    .loc 1 848 24
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 848 25
    .loc 1 848 24
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 848 31
    .loc 1 848 28
    ldr w9, [x27, #5168]
    movz w10, #0xa
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 848 34
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs178
    b L__cert__entry_main__land_false179
L__cert__entry_main__land_rhs178:
    .loc 1 848 37
    .loc 1 848 45
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 848 46
    .loc 1 848 45
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 848 52
    .loc 1 848 49
    ldr w9, [x27, #5168]
    movz w10, #0x14
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 848 34
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true180
    b L__cert__entry_main__land_false179
L__cert__entry_main__land_true180:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end181
L__cert__entry_main__land_false179:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end181
L__cert__entry_main__land_end181:
    .loc 1 848 55
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs182
    b L__cert__entry_main__land_false183
L__cert__entry_main__land_rhs182:
    .loc 1 848 58
    .loc 1 848 66
    ldr x9, [x27, #4920]
    str x9, [x27, #5168]
    .loc 1 848 67
    .loc 1 848 66
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 848 73
    .loc 1 848 70
    ldr w9, [x27, #5168]
    movz w10, #0x1e
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 848 55
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true184
    b L__cert__entry_main__land_false183
L__cert__entry_main__land_true184:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end185
L__cert__entry_main__land_false183:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end185
L__cert__entry_main__land_end185:
    .loc 1 848 9
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #4976]
    .loc 1 848 77
    .loc 1 848 9
    adrp x9, ___cert__entry_main____str168@PAGE
    add x9, x9, ___cert__entry_main____str168@PAGEOFF
    str x9, [x27, #4984]
    ldrsb w0, [x27, #4976]
    ldr x1, [x27, #4984]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 843 5
L__cert__entry_main__if_end177:
    .loc 1 851 41
    .loc 1 851 31
    movz w9, #0x5
    sxtw x9, w9
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5168]
    ldr x0, [x27, #5168]
    bl __cert__new
    str x0, [x27, #5168]
    .loc 1 851 12
    ldr x9, [x27, #5168]
    str x9, [x27, #4992]
    .loc 1 852 9
    .loc 1 852 29
    .loc 1 852 26
    ldr x9, [x27, #4992]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #5168]
    .loc 1 852 5
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true186
    b L__cert__entry_main__if_end187
L__cert__entry_main__if_true186:
    .loc 1 854 22
    .loc 1 854 18
    mov w9, wzr
    str w9, [x27, #5000]
    .loc 1 1 1
L__cert__entry_main__while_cond188:
    .loc 1 854 25
    .loc 1 854 29
    .loc 1 854 27
    ldr w9, [x27, #5000]
    movz w10, #0x5
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body189
    b L__cert__entry_main__while_end190
L__cert__entry_main__while_body189:
    .loc 1 856 13
    .loc 1 856 33
    ldr x9, [x27, #4992]
    str x9, [x27, #5168]
    .loc 1 856 30
    .loc 1 856 33
    ldr w9, [x27, #5000]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5008]
    .loc 1 856 36
    .loc 1 856 38
    ldr w9, [x27, #5000]
    scvtf s0, w9
    fmov w9, s0
    str w9, [x27, #5168]
    .loc 1 856 48
    .loc 1 856 46
    ldr s0, [x27, #5168]
    movz w12, #0xcccd
    movk w12, #0x3f8c, lsl #16
    fmov s1, w12
    fmul s0, s0, s1
    str s0, [x27, #5168]
    .loc 1 856 33
    ldr s0, [x27, #5168]
    str s0, [x27, #5016]
    ldr x9, [x27, #5008]
    ldr s0, [x27, #5016]
    str s0, [x9]
    .loc 1 1 1
L__cert__entry_main__while_post191:
    .loc 1 854 36
    .loc 1 854 40
    .loc 1 854 38
    ldr w9, [x27, #5000]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 854 34
    ldr w9, [x27, #5168]
    str w9, [x27, #5000]
    .loc 1 1 1
    b L__cert__entry_main__while_cond188
L__cert__entry_main__while_end190:
    .loc 1 858 22
    .loc 1 858 18
    mov w9, wzr
    str w9, [x27, #5024]
    .loc 1 1 1
L__cert__entry_main__while_cond192:
    .loc 1 858 25
    .loc 1 858 29
    .loc 1 858 27
    ldr w9, [x27, #5024]
    movz w10, #0x5
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #5168]
    .loc 1 1 1
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__while_body193
    b L__cert__entry_main__while_end194
L__cert__entry_main__while_body193:
    .loc 1 860 20
    .loc 1 860 36
    ldr x9, [x27, #4992]
    str x9, [x27, #5168]
    .loc 1 860 37
    .loc 1 860 36
    ldr w9, [x27, #5024]
    sxtw x9, w9
    str x9, [x27, #5176]
    ldr x9, [x27, #5176]
    movz x10, #0x4
    mul x9, x9, x10
    str x9, [x27, #5176]
    ldr x9, [x27, #5168]
    ldr x10, [x27, #5176]
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 860 44
    .loc 1 860 46
    ldr w9, [x27, #5024]
    scvtf s0, w9
    fmov w9, s0
    str w9, [x27, #5176]
    .loc 1 860 56
    .loc 1 860 54
    ldr s0, [x27, #5176]
    movz w12, #0xcccd
    movk w12, #0x3f8c, lsl #16
    fmov s1, w12
    fmul s0, s0, s1
    str s0, [x27, #5176]
    .loc 1 860 40
    ldr s0, [x27, #5168]
    ldr s1, [x27, #5176]
    fcmp s0, s1
    cset w9, eq
    strb w9, [x27, #5168]
    .loc 1 860 13
    ldrb w9, [x27, #5168]
    strb w9, [x27, #5032]
    .loc 1 860 62
    .loc 1 860 13
    adrp x9, ___cert__entry_main____str169@PAGE
    add x9, x9, ___cert__entry_main____str169@PAGEOFF
    str x9, [x27, #5040]
    ldrsb w0, [x27, #5032]
    ldr x1, [x27, #5040]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 1 1
L__cert__entry_main__while_post195:
    .loc 1 858 36
    .loc 1 858 40
    .loc 1 858 38
    ldr w9, [x27, #5024]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 858 34
    ldr w9, [x27, #5168]
    str w9, [x27, #5024]
    .loc 1 1 1
    b L__cert__entry_main__while_cond192
L__cert__entry_main__while_end194:
    .loc 1 852 5
L__cert__entry_main__if_end187:
    .loc 1 864 5
    ldr x0, [x27, #4920]
    bl __cert__delete
    mov x9, xzr
    str x9, [x27, #4920]
    .loc 1 865 5
    ldr x0, [x27, #4992]
    bl __cert__delete
    mov x9, xzr
    str x9, [x27, #4992]
    .loc 1 867 9
    .loc 1 867 21
    .loc 1 867 18
    ldr x9, [x27, #4920]
    mov x10, xzr
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 867 26
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs196
    b L__cert__entry_main__land_false197
L__cert__entry_main__land_rhs196:
    .loc 1 867 29
    .loc 1 867 49
    .loc 1 867 46
    ldr x9, [x27, #4992]
    mov x10, xzr
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 867 26
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true198
    b L__cert__entry_main__land_false197
L__cert__entry_main__land_true198:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end199
L__cert__entry_main__land_false197:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end199
L__cert__entry_main__land_end199:
    .loc 1 867 5
    ldrsb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__if_true200
    b L__cert__entry_main__if_false201
L__cert__entry_main__if_true200:
    .loc 1 869 16
    .loc 1 869 9
    movz w9, #0x1
    strb w9, [x27, #5048]
    .loc 1 869 22
    .loc 1 869 9
    adrp x9, ___cert__entry_main____str170@PAGE
    add x9, x9, ___cert__entry_main____str170@PAGEOFF
    str x9, [x27, #5056]
    ldrsb w0, [x27, #5048]
    ldr x1, [x27, #5056]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 867 5
    b L__cert__entry_main__if_end202
L__cert__entry_main__if_false201:
    .loc 1 873 16
    .loc 1 873 9
    mov w9, wzr
    strb w9, [x27, #5064]
    .loc 1 873 23
    .loc 1 873 9
    adrp x9, ___cert__entry_main____str171@PAGE
    add x9, x9, ___cert__entry_main____str171@PAGEOFF
    str x9, [x27, #5072]
    ldrsb w0, [x27, #5064]
    ldr x1, [x27, #5072]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 867 5
L__cert__entry_main__if_end202:
    .loc 1 880 23
    movz x0, #0x10
    bl __cert__new
    str x0, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5088]
    ldr x9, [x27, #5088]
    mov x10, xzr
    str x10, [x9]
    ldr x9, [x27, #5088]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz w10, #0x1
    str w10, [x9]
    ldr x9, [x27, #5088]
    str x9, [x27, #5096]
    ldr x9, [x27, #5096]
    str x9, [x27, #5104]
    .loc 1 880 37
    .loc 1 880 23
    movz w9, #0x3
    str w9, [x27, #5112]
    ldr x0, [x27, #5104]
    ldr w1, [x27, #5112]
    bl _Test_Program___bundle_inst_TestClass_init_ptr_to_struct_testclass_i32
    .loc 1 880 15
    ldr x9, [x27, #5096]
    str x9, [x27, #5080]
    .loc 1 881 5
    .loc 1 881 11
    ldr x9, [x27, #5080]
    str x9, [x27, #5120]
    .loc 1 881 19
    .loc 1 881 11
    movz w9, #0x16
    str w9, [x27, #5128]
    ldr x0, [x27, #5120]
    ldr w1, [x27, #5128]
    bl _Test_Program___bundle_inst_TestClass_Example_ptr_to_struct_testclass_i32
    .loc 1 882 12
    .loc 1 882 18
    ldr x9, [x27, #5080]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 882 23
    .loc 1 882 20
    ldr w9, [x27, #5168]
    movz w10, #0x16
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 882 26
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_rhs205
    b L__cert__entry_main__land_false206
L__cert__entry_main__land_rhs205:
    .loc 1 882 29
    adrp x9, _Test_Program_globalctr@PAGE
    add x9, x9, _Test_Program_globalctr@PAGEOFF
    ldr x10, [x9]
    str x10, [x27, #5168]
    .loc 1 882 39
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    str x9, [x27, #5168]
    ldr x9, [x27, #5168]
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 882 44
    .loc 1 882 41
    ldr w9, [x27, #5168]
    movz w10, #0x5
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #5168]
    .loc 1 882 26
    ldrb w9, [x27, #5168]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #5168]
    ldrb w9, [x27, #5168]
    cmp w9, wzr
    b.ne L__cert__entry_main__land_true207
    b L__cert__entry_main__land_false206
L__cert__entry_main__land_true207:
    movz w11, #0x1
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end208
L__cert__entry_main__land_false206:
    mov w11, wzr
    strb w11, [x27, #5168]
    b L__cert__entry_main__land_end208
L__cert__entry_main__land_end208:
    .loc 1 882 5
    ldrsb w9, [x27, #5168]
    strb w9, [x27, #5136]
    .loc 1 882 47
    .loc 1 882 5
    adrp x9, ___cert__entry_main____str172@PAGE
    add x9, x9, ___cert__entry_main____str172@PAGEOFF
    str x9, [x27, #5144]
    ldrsb w0, [x27, #5136]
    ldr x1, [x27, #5144]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 886 16
    .loc 1 886 8
    adrp x9, ___cert__entry_main____str173@PAGE
    add x9, x9, ___cert__entry_main____str173@PAGEOFF
    str x9, [x27, #5152]
    .loc 1 886 71
    adrp x9, _Test_Program_total_test_count@PAGE
    add x9, x9, _Test_Program_total_test_count@PAGEOFF
    ldr w10, [x9]
    str w10, [x27, #5168]
    .loc 1 886 8
    ldr w9, [x27, #5168]
    str w9, [x27, #5160]
    ldr x0, [x27, #5152]
    ldr w1, [x27, #5160]
    sub sp, sp, #16
    ldr w9, [x27, #5160]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_printnl_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #5168]
    .loc 1 888 5
    bl __cert__GC__prep_exit
    .loc 1 888 10
    .loc 1 888 22
    .loc 1 888 20
    ldr w9, [x27, #1216]
    movz w10, #0xa8
    sdiv w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 888 29
    .loc 1 888 27
    ldr w9, [x27, #5168]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [x27, #5168]
    .loc 1 888 5
    ldr w0, [x27, #5168]
    mov sp, x27
    add sp, sp, #4080
    add sp, sp, #4080
    add sp, sp, #368
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    .loc 1 893 5
    ldr x16, [x27, #0]
    blr x16
    .loc 1 894 17
    .loc 1 894 15
    movz w0, #0x2a
    ldr x16, [x27, #8]
    blr x16
    strh w0, [x27, #40]
    .loc 1 894 9
    ldrsh w9, [x27, #40]
    strh w9, [x27, #16]
    .loc 1 895 14
    .loc 1 895 8
    adrp x9, __Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16____str174@PAGE
    add x9, x9, __Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16____str174@PAGEOFF
    str x9, [x27, #24]
    .loc 1 895 39
    .loc 1 895 8
    ldrsh w9, [x27, #16]
    sxth w9, w9
    str w9, [x27, #40]
    ldr w9, [x27, #40]
    strh w9, [x27, #32]
    ldr x0, [x27, #24]
    ldrsh w1, [x27, #32]
    sub sp, sp, #16
    ldrsh w9, [x27, #32]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #40]
    .loc 1 896 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_file_io_tests:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #976
    mov x27, sp
    .loc 1 904 37
    .loc 1 904 31
    movz w0, #0x4
    bl _Std_Memory_alloc_i32
    str x0, [x27, #576]
    .loc 1 904 9
    ldr x9, [x27, #576]
    str x9, [x27, #0]
    .loc 1 905 5
    .loc 1 905 21
    ldr x9, [x27, #0]
    str x9, [x27, #576]
    .loc 1 905 18
    .loc 1 905 21
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #8]
    .loc 1 905 23
    .loc 1 905 21
    movz w9, #0x1
    strb w9, [x27, #16]
    ldr x9, [x27, #8]
    ldrb w10, [x27, #16]
    strb w10, [x9]
    .loc 1 906 5
    .loc 1 906 21
    ldr x9, [x27, #0]
    str x9, [x27, #576]
    .loc 1 906 18
    .loc 1 906 21
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #24]
    .loc 1 906 23
    .loc 1 906 21
    movz w9, #0x2
    strb w9, [x27, #32]
    ldr x9, [x27, #24]
    ldrb w10, [x27, #32]
    strb w10, [x9]
    .loc 1 907 5
    .loc 1 907 21
    ldr x9, [x27, #0]
    str x9, [x27, #576]
    .loc 1 907 18
    .loc 1 907 21
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #40]
    .loc 1 907 23
    .loc 1 907 21
    movz w9, #0x3
    strb w9, [x27, #48]
    ldr x9, [x27, #40]
    ldrb w10, [x27, #48]
    strb w10, [x9]
    .loc 1 908 5
    .loc 1 908 21
    ldr x9, [x27, #0]
    str x9, [x27, #576]
    .loc 1 908 18
    .loc 1 908 21
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #56]
    .loc 1 908 23
    .loc 1 908 21
    movz w9, #0x4
    strb w9, [x27, #64]
    ldr x9, [x27, #56]
    ldrb w10, [x27, #64]
    strb w10, [x9]
    .loc 1 911 24
    .loc 1 911 14
    adrp x9, __Test_Program_file_io_tests____str175@PAGE
    add x9, x9, __Test_Program_file_io_tests____str175@PAGEOFF
    str x9, [x27, #80]
    .loc 1 911 41
    .loc 1 911 14
    ldr x9, [x27, #0]
    str x9, [x27, #88]
    .loc 1 911 64
    .loc 1 911 14
    movz w9, #0x4
    str w9, [x27, #96]
    ldr x0, [x27, #80]
    ldr x1, [x27, #88]
    ldr w2, [x27, #96]
    bl _Std_File_write_all_ptr_to_char_ptr_to_void_i32
    str w0, [x27, #576]
    .loc 1 910 9
    ldr w9, [x27, #576]
    str w9, [x27, #72]
    .loc 1 912 12
    .loc 1 912 29
    .loc 1 912 26
    ldr w9, [x27, #72]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 912 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #104]
    .loc 1 912 32
    .loc 1 912 5
    adrp x9, __Test_Program_file_io_tests____str176@PAGE
    add x9, x9, __Test_Program_file_io_tests____str176@PAGEOFF
    str x9, [x27, #112]
    ldrsb w0, [x27, #104]
    ldr x1, [x27, #112]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 914 22
    .loc 1 914 11
    mov x9, xzr
    str x9, [x27, #120]
    .loc 1 915 20
    .loc 1 915 9
    mov w9, wzr
    str w9, [x27, #128]
    .loc 1 918 23
    .loc 1 918 14
    adrp x9, __Test_Program_file_io_tests____str177@PAGE
    add x9, x9, __Test_Program_file_io_tests____str177@PAGEOFF
    str x9, [x27, #144]
    .loc 1 918 41
    add x9, x27, #120
    str x9, [x27, #576]
    .loc 1 918 14
    ldr x9, [x27, #576]
    str x9, [x27, #152]
    .loc 1 918 63
    add x9, x27, #128
    str x9, [x27, #576]
    .loc 1 918 14
    ldr x9, [x27, #576]
    str x9, [x27, #160]
    ldr x0, [x27, #144]
    ldr x1, [x27, #152]
    ldr x2, [x27, #160]
    bl _Std_File_read_all_ptr_to_char_ptr_to_ptr_to_void_ptr_to_i32
    str w0, [x27, #576]
    .loc 1 917 9
    ldr w9, [x27, #576]
    str w9, [x27, #136]
    .loc 1 919 12
    .loc 1 919 26
    .loc 1 919 23
    ldr w9, [x27, #136]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 919 28
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_rhs0
    b LTest_Program_file_io_tests__land_false1
LTest_Program_file_io_tests__land_rhs0:
    .loc 1 919 31
    .loc 1 919 43
    .loc 1 919 40
    ldr w9, [x27, #128]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 919 28
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_true2
    b LTest_Program_file_io_tests__land_false1
LTest_Program_file_io_tests__land_true2:
    movz w11, #0x1
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end3
LTest_Program_file_io_tests__land_false1:
    mov w11, wzr
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end3
LTest_Program_file_io_tests__land_end3:
    .loc 1 919 5
    ldrsb w9, [x27, #576]
    strb w9, [x27, #168]
    .loc 1 919 46
    .loc 1 919 5
    adrp x9, __Test_Program_file_io_tests____str178@PAGE
    add x9, x9, __Test_Program_file_io_tests____str178@PAGEOFF
    str x9, [x27, #176]
    ldrsb w0, [x27, #168]
    ldr x1, [x27, #176]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 921 22
    .loc 1 921 9
    ldr x9, [x27, #120]
    str x9, [x27, #184]
    .loc 1 922 12
    .loc 1 922 22
    ldr x9, [x27, #184]
    str x9, [x27, #576]
    .loc 1 922 23
    .loc 1 922 22
    mov w9, wzr
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    .loc 1 922 26
    ldrb w9, [x27, #576]
    uxtb w9, w9
    str w9, [x27, #576]
    .loc 1 922 29
    .loc 1 922 26
    ldr w9, [x27, #576]
    movz w10, #0x1
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 922 31
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_rhs4
    b LTest_Program_file_io_tests__land_false5
LTest_Program_file_io_tests__land_rhs4:
    .loc 1 923 12
    .loc 1 923 22
    ldr x9, [x27, #184]
    str x9, [x27, #576]
    .loc 1 923 23
    .loc 1 923 22
    movz w9, #0x1
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    .loc 1 923 26
    ldrb w9, [x27, #576]
    uxtb w9, w9
    str w9, [x27, #576]
    .loc 1 923 29
    .loc 1 923 26
    ldr w9, [x27, #576]
    movz w10, #0x2
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 922 31
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_true6
    b LTest_Program_file_io_tests__land_false5
LTest_Program_file_io_tests__land_true6:
    movz w11, #0x1
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end7
LTest_Program_file_io_tests__land_false5:
    mov w11, wzr
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end7
LTest_Program_file_io_tests__land_end7:
    .loc 1 923 31
    ldrsb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_rhs8
    b LTest_Program_file_io_tests__land_false9
LTest_Program_file_io_tests__land_rhs8:
    .loc 1 924 12
    .loc 1 924 22
    ldr x9, [x27, #184]
    str x9, [x27, #576]
    .loc 1 924 23
    .loc 1 924 22
    movz w9, #0x2
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    .loc 1 924 26
    ldrb w9, [x27, #576]
    uxtb w9, w9
    str w9, [x27, #576]
    .loc 1 924 29
    .loc 1 924 26
    ldr w9, [x27, #576]
    movz w10, #0x3
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 923 31
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_true10
    b LTest_Program_file_io_tests__land_false9
LTest_Program_file_io_tests__land_true10:
    movz w11, #0x1
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end11
LTest_Program_file_io_tests__land_false9:
    mov w11, wzr
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end11
LTest_Program_file_io_tests__land_end11:
    .loc 1 924 31
    ldrsb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_rhs12
    b LTest_Program_file_io_tests__land_false13
LTest_Program_file_io_tests__land_rhs12:
    .loc 1 925 12
    .loc 1 925 22
    ldr x9, [x27, #184]
    str x9, [x27, #576]
    .loc 1 925 23
    .loc 1 925 22
    movz w9, #0x3
    sxtw x9, w9
    str x9, [x27, #584]
    ldr x9, [x27, #576]
    ldr x10, [x27, #584]
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    .loc 1 925 26
    ldrb w9, [x27, #576]
    uxtb w9, w9
    str w9, [x27, #576]
    .loc 1 925 29
    .loc 1 925 26
    ldr w9, [x27, #576]
    movz w10, #0x4
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 924 31
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_true14
    b LTest_Program_file_io_tests__land_false13
LTest_Program_file_io_tests__land_true14:
    movz w11, #0x1
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end15
LTest_Program_file_io_tests__land_false13:
    mov w11, wzr
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end15
LTest_Program_file_io_tests__land_end15:
    .loc 1 922 5
    ldrsb w9, [x27, #576]
    strb w9, [x27, #192]
    .loc 1 926 12
    .loc 1 922 5
    adrp x9, __Test_Program_file_io_tests____str179@PAGE
    add x9, x9, __Test_Program_file_io_tests____str179@PAGEOFF
    str x9, [x27, #200]
    ldrsb w0, [x27, #192]
    ldr x1, [x27, #200]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 928 20
    .loc 1 928 12
    ldr x0, [x27, #120]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 929 20
    .loc 1 929 12
    ldr x0, [x27, #0]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 936 22
    .loc 1 936 16
    movz w0, #0x8
    bl _Std_Memory_alloc_i32
    str x0, [x27, #576]
    .loc 1 935 19
    ldr x9, [x27, #576]
    str x9, [x27, #208]
    .loc 1 937 5
    .loc 1 937 29
    ldr x9, [x27, #208]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #216]
    .loc 1 937 31
    .loc 1 937 29
    movz w9, #0x7
    strb w9, [x27, #224]
    ldr x9, [x27, #216]
    ldrb w10, [x27, #224]
    strb w10, [x9]
    .loc 1 938 5
    .loc 1 938 28
    ldr x9, [x27, #208]
    str x9, [x27, #232]
    .loc 1 938 30
    .loc 1 938 28
    movz w9, #0x63
    str w9, [x27, #240]
    ldr x9, [x27, #232]
    ldr w10, [x27, #240]
    str w10, [x9]
    .loc 1 940 26
    .loc 1 940 21
    adrp x9, __Test_Program_file_io_tests____str180@PAGE
    add x9, x9, __Test_Program_file_io_tests____str180@PAGEOFF
    str x9, [x27, #256]
    .loc 1 940 45
    .loc 1 940 21
    adrp x9, __Test_Program_file_io_tests____str181@PAGE
    add x9, x9, __Test_Program_file_io_tests____str181@PAGEOFF
    str x9, [x27, #264]
    ldr x0, [x27, #256]
    ldr x1, [x27, #264]
    bl _Std_File_open_ptr_to_char_ptr_to_char
    str x0, [x27, #576]
    .loc 1 940 11
    ldr x9, [x27, #576]
    str x9, [x27, #248]
    .loc 1 941 12
    .loc 1 941 18
    .loc 1 941 15
    ldr x9, [x27, #248]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #576]
    .loc 1 941 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #272]
    .loc 1 941 24
    .loc 1 941 5
    adrp x9, __Test_Program_file_io_tests____str182@PAGE
    add x9, x9, __Test_Program_file_io_tests____str182@PAGEOFF
    str x9, [x27, #280]
    ldrsb w0, [x27, #272]
    ldr x1, [x27, #280]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 943 23
    .loc 1 943 9
    movz w9, #0x8
    str w9, [x27, #288]
    .loc 1 944 23
    .loc 1 944 17
    ldr x9, [x27, #248]
    str x9, [x27, #296]
    .loc 1 944 27
    .loc 1 944 17
    ldr x9, [x27, #208]
    str x9, [x27, #304]
    .loc 1 944 49
    .loc 1 944 17
    ldr w9, [x27, #288]
    str w9, [x27, #312]
    ldr x0, [x27, #296]
    ldr x1, [x27, #304]
    ldr w2, [x27, #312]
    bl _Std_File_write_ptr_to_void_ptr_to_void_i32
    str w0, [x27, #576]
    .loc 1 944 65
    .loc 1 944 62
    ldr w9, [x27, #576]
    ldr w10, [x27, #288]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 944 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #320]
    .loc 1 945 12
    .loc 1 944 5
    adrp x9, __Test_Program_file_io_tests____str183@PAGE
    add x9, x9, __Test_Program_file_io_tests____str183@PAGEOFF
    str x9, [x27, #328]
    ldrsb w0, [x27, #320]
    ldr x1, [x27, #328]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 947 16
    .loc 1 947 10
    ldr x0, [x27, #248]
    bl _Std_File_flush_ptr_to_void
    str w0, [x27, #576]
    .loc 1 948 16
    .loc 1 948 10
    ldr x0, [x27, #248]
    bl _Std_File_close_ptr_to_void
    str w0, [x27, #576]
    .loc 1 953 20
    .loc 1 953 15
    adrp x9, __Test_Program_file_io_tests____str184@PAGE
    add x9, x9, __Test_Program_file_io_tests____str184@PAGEOFF
    str x9, [x27, #336]
    .loc 1 953 39
    .loc 1 953 15
    adrp x9, __Test_Program_file_io_tests____str185@PAGE
    add x9, x9, __Test_Program_file_io_tests____str185@PAGEOFF
    str x9, [x27, #344]
    ldr x0, [x27, #336]
    ldr x1, [x27, #344]
    bl _Std_File_open_ptr_to_char_ptr_to_char
    str x0, [x27, #576]
    .loc 1 953 8
    ldr x9, [x27, #576]
    str x9, [x27, #248]
    .loc 1 954 12
    .loc 1 954 18
    .loc 1 954 15
    ldr x9, [x27, #248]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #576]
    .loc 1 954 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #352]
    .loc 1 954 24
    .loc 1 954 5
    adrp x9, __Test_Program_file_io_tests____str186@PAGE
    add x9, x9, __Test_Program_file_io_tests____str186@PAGEOFF
    str x9, [x27, #360]
    ldrsb w0, [x27, #352]
    ldr x1, [x27, #360]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 957 22
    .loc 1 957 17
    ldr x9, [x27, #248]
    str x9, [x27, #368]
    .loc 1 957 26
    .loc 1 957 17
    mov x9, xzr
    str x9, [x27, #376]
    .loc 1 957 29
    .loc 1 957 17
    movz w9, #0x2
    str w9, [x27, #384]
    ldr x0, [x27, #368]
    ldr x1, [x27, #376]
    ldr w2, [x27, #384]
    bl _Std_File_seek_ptr_to_void_i64_i32
    str w0, [x27, #576]
    .loc 1 957 35
    .loc 1 957 32
    ldr w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 957 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #392]
    .loc 1 957 38
    .loc 1 957 5
    adrp x9, __Test_Program_file_io_tests____str187@PAGE
    add x9, x9, __Test_Program_file_io_tests____str187@PAGEOFF
    str x9, [x27, #400]
    ldrsb w0, [x27, #392]
    ldr x1, [x27, #400]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 958 29
    .loc 1 958 24
    ldr x0, [x27, #248]
    bl _Std_File_tell_ptr_to_void
    str x0, [x27, #576]
    .loc 1 958 9
    ldr x9, [x27, #576]
    str x9, [x27, #408]
    .loc 1 959 12
    .loc 1 959 23
    .loc 1 959 35
    ldr w9, [x27, #288]
    sxtw x9, w9
    str x9, [x27, #584]
    .loc 1 959 20
    ldr x9, [x27, #408]
    ldr x10, [x27, #584]
    cmp x9, x10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 959 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #416]
    .loc 1 959 43
    .loc 1 959 5
    adrp x9, __Test_Program_file_io_tests____str188@PAGE
    add x9, x9, __Test_Program_file_io_tests____str188@PAGEOFF
    str x9, [x27, #424]
    ldrsb w0, [x27, #416]
    ldr x1, [x27, #424]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 962 22
    .loc 1 962 17
    ldr x9, [x27, #248]
    str x9, [x27, #432]
    .loc 1 962 26
    .loc 1 962 17
    mov x9, xzr
    str x9, [x27, #440]
    .loc 1 962 29
    .loc 1 962 17
    mov w9, wzr
    str w9, [x27, #448]
    ldr x0, [x27, #432]
    ldr x1, [x27, #440]
    ldr w2, [x27, #448]
    bl _Std_File_seek_ptr_to_void_i64_i32
    str w0, [x27, #576]
    .loc 1 962 35
    .loc 1 962 32
    ldr w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 962 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #456]
    .loc 1 962 38
    .loc 1 962 5
    adrp x9, __Test_Program_file_io_tests____str189@PAGE
    add x9, x9, __Test_Program_file_io_tests____str189@PAGEOFF
    str x9, [x27, #464]
    ldrsb w0, [x27, #456]
    ldr x1, [x27, #464]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 964 37
    .loc 1 964 31
    ldr w0, [x27, #288]
    bl _Std_Memory_alloc_i32
    str x0, [x27, #576]
    .loc 1 964 11
    ldr x9, [x27, #576]
    str x9, [x27, #472]
    .loc 1 965 22
    .loc 1 965 17
    ldr x9, [x27, #248]
    str x9, [x27, #480]
    .loc 1 965 26
    .loc 1 965 17
    ldr x9, [x27, #472]
    str x9, [x27, #488]
    .loc 1 965 38
    .loc 1 965 17
    ldr w9, [x27, #288]
    str w9, [x27, #496]
    ldr x0, [x27, #480]
    ldr x1, [x27, #488]
    ldr w2, [x27, #496]
    bl _Std_File_read_ptr_to_void_ptr_to_void_i32
    str w0, [x27, #576]
    .loc 1 965 54
    .loc 1 965 51
    ldr w9, [x27, #576]
    ldr w10, [x27, #288]
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 965 5
    ldrb w9, [x27, #576]
    strb w9, [x27, #504]
    .loc 1 966 12
    .loc 1 965 5
    adrp x9, __Test_Program_file_io_tests____str190@PAGE
    add x9, x9, __Test_Program_file_io_tests____str190@PAGEOFF
    str x9, [x27, #512]
    ldrsb w0, [x27, #504]
    ldr x1, [x27, #512]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 968 16
    .loc 1 968 10
    ldr x0, [x27, #248]
    bl _Std_File_close_ptr_to_void
    str w0, [x27, #576]
    .loc 1 970 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #520]
    .loc 1 970 36
    .loc 1 970 18
    ldr x9, [x27, #472]
    str x9, [x27, #528]
    ldr x9, [x27, #520]
    str x9, [x27, #536]
    ldr x9, [x27, #528]
    ldr w10, [x9]
    str w10, [x27, #576]
    ldr w9, [x27, #576]
    str w9, [x27, #544]
    ldr x9, [x27, #536]
    ldr w10, [x27, #544]
    str w10, [x9]
    ldr x9, [x27, #528]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    ldrb w9, [x27, #576]
    strb w9, [x27, #552]
    ldr x9, [x27, #536]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x27, #552]
    strb w10, [x9]
    .loc 1 971 12
    .loc 1 971 26
    ldr x9, [x27, #520]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    str x9, [x27, #576]
    ldr x9, [x27, #576]
    ldrb w10, [x9]
    strb w10, [x27, #576]
    .loc 1 971 37
    ldrb w9, [x27, #576]
    uxtb w9, w9
    str w9, [x27, #576]
    .loc 1 971 40
    .loc 1 971 37
    ldr w9, [x27, #576]
    movz w10, #0x7
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 971 42
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_rhs16
    b LTest_Program_file_io_tests__land_false17
LTest_Program_file_io_tests__land_rhs16:
    .loc 1 972 12
    .loc 1 972 26
    ldr x9, [x27, #520]
    ldr w10, [x9]
    str w10, [x27, #576]
    .loc 1 972 39
    .loc 1 972 36
    ldr w9, [x27, #576]
    movz w10, #0x63
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #576]
    .loc 1 971 42
    ldrb w9, [x27, #576]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #576]
    ldrb w9, [x27, #576]
    cmp w9, wzr
    b.ne LTest_Program_file_io_tests__land_true18
    b LTest_Program_file_io_tests__land_false17
LTest_Program_file_io_tests__land_true18:
    movz w11, #0x1
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end19
LTest_Program_file_io_tests__land_false17:
    mov w11, wzr
    strb w11, [x27, #576]
    b LTest_Program_file_io_tests__land_end19
LTest_Program_file_io_tests__land_end19:
    .loc 1 971 5
    ldrsb w9, [x27, #576]
    strb w9, [x27, #560]
    .loc 1 973 12
    .loc 1 971 5
    adrp x9, __Test_Program_file_io_tests____str191@PAGE
    add x9, x9, __Test_Program_file_io_tests____str191@PAGEOFF
    str x9, [x27, #568]
    ldrsb w0, [x27, #560]
    ldr x1, [x27, #568]
    bl _Test_Program_assert_bool_ptr_to_char
    .loc 1 975 20
    .loc 1 975 12
    ldr x0, [x27, #472]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 976 20
    .loc 1 976 12
    ldr x0, [x27, #208]
    bl _Std_Memory_dealloc_ptr_to_void
    .loc 1 981 12
    .loc 1 981 5
    adrp x0, __Test_Program_file_io_tests____str192@PAGE
    add x0, x0, __Test_Program_file_io_tests____str192@PAGEOFF
    bl _remove
    str w0, [x27, #576]
    .loc 1 982 12
    .loc 1 982 5
    adrp x0, __Test_Program_file_io_tests____str193@PAGE
    add x0, x0, __Test_Program_file_io_tests____str193@PAGEOFF
    bl _remove
    str w0, [x27, #576]
    mov sp, x27
    add sp, sp, #976
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_super_args_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32_i32:
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
    .loc 1 997 9
    .loc 1 997 13
    .loc 1 997 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 17
    .loc 1 997 15
    ldr w9, [x27, #208]
    ldr w10, [x27, #16]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 21
    .loc 1 997 19
    ldr w9, [x27, #208]
    ldr w10, [x27, #24]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 25
    .loc 1 997 23
    ldr w9, [x27, #208]
    ldr w10, [x27, #32]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 29
    .loc 1 997 27
    ldr w9, [x27, #208]
    ldr w10, [x27, #40]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 33
    .loc 1 997 31
    ldr w9, [x27, #208]
    ldr w10, [x27, #48]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 37
    .loc 1 997 35
    ldr w9, [x27, #208]
    ldr w10, [x27, #56]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 41
    .loc 1 997 39
    ldr w9, [x27, #208]
    ldr w10, [x27, #64]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 45
    .loc 1 997 43
    ldr w9, [x27, #208]
    ldr w10, [x27, #72]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 49
    .loc 1 997 47
    ldr w9, [x27, #208]
    ldr w10, [x27, #80]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 53
    .loc 1 997 51
    ldr w9, [x27, #208]
    ldr w10, [x27, #88]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 57
    .loc 1 997 55
    ldr w9, [x27, #208]
    ldr w10, [x27, #96]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 61
    .loc 1 997 59
    ldr w9, [x27, #208]
    ldr w10, [x27, #104]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 65
    .loc 1 997 63
    ldr w9, [x27, #208]
    ldr w10, [x27, #112]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 69
    .loc 1 997 67
    ldr w9, [x27, #208]
    ldr w10, [x27, #120]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 73
    .loc 1 997 71
    ldr w9, [x27, #208]
    ldr w10, [x27, #128]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 77
    .loc 1 997 75
    ldr w9, [x27, #208]
    ldr w10, [x27, #136]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 81
    .loc 1 997 79
    ldr w9, [x27, #208]
    ldr w10, [x27, #144]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 85
    .loc 1 997 83
    ldr w9, [x27, #208]
    ldr w10, [x27, #152]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 89
    .loc 1 997 87
    ldr w9, [x27, #208]
    ldr w10, [x27, #160]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 93
    .loc 1 997 91
    ldr w9, [x27, #208]
    ldr w10, [x27, #168]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 97
    .loc 1 997 95
    ldr w9, [x27, #208]
    ldr w10, [x27, #176]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 101
    .loc 1 997 99
    ldr w9, [x27, #208]
    ldr w10, [x27, #184]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 105
    .loc 1 997 103
    ldr w9, [x27, #208]
    ldr w10, [x27, #192]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 109
    .loc 1 997 107
    ldr w9, [x27, #208]
    ldr w10, [x27, #200]
    mul w9, w9, w10
    str w9, [x27, #208]
    .loc 1 997 5
    ldr w0, [x27, #208]
    mov sp, x27
    add sp, sp, #224
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_function_overload_i32_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1002 9
    .loc 1 1002 13
    .loc 1 1002 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    add w9, w9, w10
    str w9, [x27, #16]
    .loc 1 1002 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_function_overload_f64_f64:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str d0, [x27, #0]
    str d1, [x27, #8]
    .loc 1 1006 9
    .loc 1 1006 13
    .loc 1 1006 11
    ldr d0, [x27, #0]
    ldr d1, [x27, #8]
    fadd d0, d0, d1
    str d0, [x27, #16]
    .loc 1 1006 5
    ldr x0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_function_overload_i32_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #224
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #112
    str x15, [x9]
    .loc 1 1010 20
    add x10, x27, #112
    ldr x9, [x10]
    str x9, [x27, #120]
    .loc 1 1010 13
    ldr x9, [x27, #120]
    str x9, [x27, #8]
    .loc 1 1011 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #16]
    ldr x9, [x27, #16]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #16]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 1012 18
    .loc 1 1012 14
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1 1
LTest_Program_function_overload_i32_varargs__while_cond0:
    .loc 1 1012 21
    .loc 1 1012 25
    .loc 1 1012 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #120]
    .loc 1 1 1
    ldrb w9, [x27, #120]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #120]
    ldrb w9, [x27, #120]
    cmp w9, wzr
    b.ne LTest_Program_function_overload_i32_varargs__while_body1
    b LTest_Program_function_overload_i32_varargs__while_end2
LTest_Program_function_overload_i32_varargs__while_body1:
    .loc 1 1014 22
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #32]
    .loc 1 1014 27
    ldr x9, [x27, #8]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldr x10, [x9]
    str x10, [x27, #120]
    ldr x9, [x27, #56]
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    str x9, [x27, #8]
    .loc 1 1014 22
    ldr x9, [x27, #120]
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    str x9, [x27, #48]
    ldr x9, [x27, #40]
    ldr w10, [x9]
    str w10, [x27, #120]
    ldr w9, [x27, #120]
    str w9, [x27, #64]
    ldr x9, [x27, #48]
    ldr w10, [x27, #64]
    str w10, [x9]
    ldr x9, [x27, #40]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    ldrb w10, [x9]
    strb w10, [x27, #120]
    ldrb w9, [x27, #120]
    strb w9, [x27, #72]
    ldr x9, [x27, #48]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    ldrb w10, [x27, #72]
    strb w10, [x9]
    .loc 1 1015 9
    .loc 1 1015 24
    ldr x9, [x27, #16]
    str x9, [x27, #80]
    .loc 1 1015 26
    .loc 1 1015 33
    ldr x9, [x27, #16]
    ldr w10, [x9]
    str w10, [x27, #120]
    .loc 1 1015 43
    .loc 1 1015 46
    ldr x9, [x27, #32]
    ldr w10, [x9]
    str w10, [x27, #128]
    .loc 1 1015 41
    ldr w9, [x27, #120]
    ldr w10, [x27, #128]
    add w9, w9, w10
    str w9, [x27, #120]
    .loc 1 1015 24
    ldr w9, [x27, #120]
    str w9, [x27, #88]
    ldr x9, [x27, #80]
    ldr w10, [x27, #88]
    str w10, [x9]
    .loc 1 1016 9
    .loc 1 1016 27
    ldr x9, [x27, #16]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #96]
    .loc 1 1016 29
    .loc 1 1016 36
    ldr x9, [x27, #16]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    str x9, [x27, #120]
    ldr x9, [x27, #120]
    ldrb w10, [x9]
    strb w10, [x27, #120]
    .loc 1 1016 49
    .loc 1 1016 52
    ldr x9, [x27, #32]
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    str x9, [x27, #128]
    ldr x9, [x27, #128]
    ldrb w10, [x9]
    strb w10, [x27, #128]
    .loc 1 1016 47
    ldrb w9, [x27, #120]
    ldrb w10, [x27, #128]
    add w9, w9, w10
    uxtb w9, w9
    strb w9, [x27, #120]
    .loc 1 1016 27
    ldrb w9, [x27, #120]
    strb w9, [x27, #104]
    ldr x9, [x27, #96]
    ldrb w10, [x27, #104]
    strb w10, [x9]
    .loc 1 1 1
LTest_Program_function_overload_i32_varargs__while_post3:
    .loc 1 1012 36
    .loc 1 1012 40
    .loc 1 1012 38
    ldr w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #120]
    .loc 1 1012 34
    ldr w9, [x27, #120]
    str w9, [x27, #24]
    .loc 1 1 1
    b LTest_Program_function_overload_i32_varargs__while_cond0
LTest_Program_function_overload_i32_varargs__while_end2:
    .loc 1 1018 12
    .loc 1 1019 9
    .loc 1 1019 5
    ldr x0, [x27, #16]
    mov sp, x27
    add sp, sp, #224
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_use_constant_i32
.p2align 2
_Test_Program_use_constant_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    .loc 1 1024 27
    .loc 1 1024 18
    movz w9, #0x64
    str w9, [x27, #8]
    .loc 1 1025 9
    .loc 1 1025 15
    .loc 1 1025 13
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    add w9, w9, w10
    str w9, [x27, #16]
    .loc 1 1025 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_constant_ptr_ptr_to_u8
.p2align 2
_Test_Program_constant_ptr_ptr_to_u8:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 1030 6
    .loc 1 1030 11
    ldr x9, [x27, #0]
    str x9, [x27, #8]
    .loc 1 1030 13
    .loc 1 1030 11
    movz w9, #0x2a
    strb w9, [x27, #16]
    ldr x9, [x27, #8]
    ldrb w10, [x27, #16]
    strb w10, [x9]
    .loc 1 1031 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_constant_test_i32
.p2align 2
_Test_Program_constant_test_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    ldr w9, [x27, #0]
    str w9, [x27, #8]
    .loc 1 1036 11
    .loc 1 1036 9
    movz w9, #0x14
    str w9, [x27, #8]
    .loc 1 1037 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_internal_diffvarargs_i32_varargs
.p2align 2
_Test_Program_internal_diffvarargs_i32_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #176
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #96
    str x15, [x9]
    .loc 1 1042 20
    add x10, x27, #96
    ldr x9, [x10]
    str x9, [x27, #104]
    .loc 1 1042 13
    ldr x9, [x27, #104]
    str x9, [x27, #8]
    .loc 1 1043 17
    ldr x9, [x27, #8]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #24]
    ldr x9, [x27, #24]
    ldr w10, [x9]
    str w10, [x27, #104]
    ldr x9, [x27, #24]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #8]
    .loc 1 1043 9
    ldr w9, [x27, #104]
    str w9, [x27, #16]
    .loc 1 1044 18
    ldr x9, [x27, #8]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldr x10, [x9]
    str x10, [x27, #104]
    ldr x9, [x27, #40]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #8]
    .loc 1 1044 9
    ldr d0, [x27, #104]
    str d0, [x27, #32]
    .loc 1 1045 20
    ldr x9, [x27, #8]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #104]
    ldr x9, [x27, #104]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldr x10, [x9]
    str x10, [x27, #104]
    ldr x9, [x27, #56]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #112]
    ldr x9, [x27, #112]
    str x9, [x27, #8]
    .loc 1 1045 12
    ldr x9, [x27, #104]
    str x9, [x27, #48]
    .loc 1 1046 14
    .loc 1 1046 8
    adrp x9, __Test_Program_internal_diffvarargs_i32_varargs____str194@PAGE
    add x9, x9, __Test_Program_internal_diffvarargs_i32_varargs____str194@PAGEOFF
    str x9, [x27, #64]
    .loc 1 1046 72
    .loc 1 1046 8
    ldr w9, [x27, #16]
    str w9, [x27, #72]
    .loc 1 1046 79
    .loc 1 1046 8
    ldr d0, [x27, #32]
    str d0, [x27, #80]
    .loc 1 1046 87
    .loc 1 1046 8
    ldr x9, [x27, #48]
    str x9, [x27, #88]
    ldr x0, [x27, #64]
    ldr w1, [x27, #72]
    ldr d0, [x27, #80]
    ldr x2, [x27, #88]
    sub sp, sp, #32
    ldr w9, [x27, #72]
    sxtw x9, w9
    str x9, [sp, #0]
    ldr d15, [x27, #80]
    str d15, [sp, #8]
    ldr x9, [x27, #88]
    str x9, [sp, #16]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #32
    str w0, [x27, #104]
    .loc 1 1047 12
    .loc 1 1048 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #176
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_vararg_sum_i32_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #128
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #48
    str x15, [x9]
    .loc 1 1053 20
    add x10, x27, #48
    ldr x9, [x10]
    str x9, [x27, #56]
    .loc 1 1053 13
    ldr x9, [x27, #56]
    str x9, [x27, #8]
    .loc 1 1054 17
    .loc 1 1054 9
    mov w9, wzr
    str w9, [x27, #16]
    .loc 1 1055 18
    .loc 1 1055 14
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1 1
LTest_Program_vararg_sum_i32_varargs__while_cond0:
    .loc 1 1055 21
    .loc 1 1055 25
    .loc 1 1055 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #56]
    .loc 1 1 1
    ldrb w9, [x27, #56]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #56]
    ldrb w9, [x27, #56]
    cmp w9, wzr
    b.ne LTest_Program_vararg_sum_i32_varargs__while_body1
    b LTest_Program_vararg_sum_i32_varargs__while_end2
LTest_Program_vararg_sum_i32_varargs__while_body1:
    .loc 1 1057 21
    ldr x9, [x27, #8]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldr w10, [x9]
    str w10, [x27, #56]
    ldr x9, [x27, #40]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    str x9, [x27, #8]
    .loc 1 1057 13
    ldr w9, [x27, #56]
    str w9, [x27, #32]
    .loc 1 1058 17
    .loc 1 1058 25
    .loc 1 1058 23
    ldr w9, [x27, #16]
    ldr w10, [x27, #32]
    add w9, w9, w10
    str w9, [x27, #56]
    .loc 1 1058 15
    ldr w9, [x27, #56]
    str w9, [x27, #16]
    .loc 1 1 1
LTest_Program_vararg_sum_i32_varargs__while_post3:
    .loc 1 1055 36
    .loc 1 1055 40
    .loc 1 1055 38
    ldr w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #56]
    .loc 1 1055 34
    ldr w9, [x27, #56]
    str w9, [x27, #24]
    .loc 1 1 1
    b LTest_Program_vararg_sum_i32_varargs__while_cond0
LTest_Program_vararg_sum_i32_varargs__while_end2:
    .loc 1 1060 12
    .loc 1 1061 9
    .loc 1 1061 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #128
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_vararg_mix_i32_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #176
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #64
    str x15, [x9]
    .loc 1 1066 20
    add x10, x27, #64
    ldr x9, [x10]
    str x9, [x27, #72]
    .loc 1 1066 13
    ldr x9, [x27, #72]
    str x9, [x27, #8]
    .loc 1 1067 17
    .loc 1 1067 9
    mov w9, wzr
    str w9, [x27, #16]
    .loc 1 1068 18
    .loc 1 1068 14
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1 1
LTest_Program_vararg_mix_i32_varargs__while_cond0:
    .loc 1 1068 21
    .loc 1 1068 25
    .loc 1 1068 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #72]
    .loc 1 1 1
    ldrb w9, [x27, #72]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #72]
    ldrb w9, [x27, #72]
    cmp w9, wzr
    b.ne LTest_Program_vararg_mix_i32_varargs__while_body1
    b LTest_Program_vararg_mix_i32_varargs__while_end2
LTest_Program_vararg_mix_i32_varargs__while_body1:
    .loc 1 1070 14
    .loc 1 1070 18
    .loc 1 1070 16
    ldr w9, [x27, #24]
    movz w10, #0x2
    mov w11, w9
    sdiv w9, w9, w10
    msub w9, w10, w9, w11
    str w9, [x27, #72]
    .loc 1 1070 24
    .loc 1 1070 21
    ldr w9, [x27, #72]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [x27, #72]
    .loc 1 1070 9
    ldrb w9, [x27, #72]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #72]
    ldrb w9, [x27, #72]
    cmp w9, wzr
    b.ne LTest_Program_vararg_mix_i32_varargs__if_true4
    b LTest_Program_vararg_mix_i32_varargs__if_false5
LTest_Program_vararg_mix_i32_varargs__if_true4:
    .loc 1 1072 25
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
    ldr w10, [x9]
    str w10, [x27, #72]
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
    .loc 1 1072 17
    ldr w9, [x27, #72]
    str w9, [x27, #32]
    .loc 1 1073 21
    .loc 1 1073 29
    .loc 1 1073 27
    ldr w9, [x27, #16]
    ldr w10, [x27, #32]
    add w9, w9, w10
    str w9, [x27, #72]
    .loc 1 1073 19
    ldr w9, [x27, #72]
    str w9, [x27, #16]
    .loc 1 1070 9
    b LTest_Program_vararg_mix_i32_varargs__if_end6
LTest_Program_vararg_mix_i32_varargs__if_false5:
    .loc 1 1077 25
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
    .loc 1 1077 17
    ldr d0, [x27, #72]
    str d0, [x27, #48]
    .loc 1 1078 21
    .loc 1 1078 30
    .loc 1 1078 36
    ldr x9, [x27, #48]
    fmov d0, x9
    fcvtzs w9, d0
    str w9, [x27, #80]
    .loc 1 1078 27
    ldr w9, [x27, #16]
    ldr w10, [x27, #80]
    add w9, w9, w10
    str w9, [x27, #72]
    .loc 1 1078 19
    ldr w9, [x27, #72]
    str w9, [x27, #16]
    .loc 1 1070 9
LTest_Program_vararg_mix_i32_varargs__if_end6:
    .loc 1 1 1
LTest_Program_vararg_mix_i32_varargs__while_post3:
    .loc 1 1068 36
    .loc 1 1068 40
    .loc 1 1068 38
    ldr w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #72]
    .loc 1 1068 34
    ldr w9, [x27, #72]
    str w9, [x27, #24]
    .loc 1 1 1
    b LTest_Program_vararg_mix_i32_varargs__while_cond0
LTest_Program_vararg_mix_i32_varargs__while_end2:
    .loc 1 1081 12
    .loc 1 1082 9
    .loc 1 1082 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #176
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_structvarargs_i32_varargs
.p2align 2
_Test_Program_structvarargs_i32_varargs:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #160
    mov x27, sp
    str w0, [x27, #0]
    add x9, x27, #80
    str x15, [x9]
    .loc 1 1087 20
    add x10, x27, #80
    ldr x9, [x10]
    str x9, [x27, #88]
    .loc 1 1087 13
    ldr x9, [x27, #88]
    str x9, [x27, #8]
    .loc 1 1088 17
    .loc 1 1088 9
    mov w9, wzr
    str w9, [x27, #16]
    .loc 1 1089 18
    .loc 1 1089 14
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1 1
LTest_Program_structvarargs_i32_varargs__while_cond0:
    .loc 1 1089 21
    .loc 1 1089 25
    .loc 1 1089 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #0]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #88]
    .loc 1 1 1
    ldrb w9, [x27, #88]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #88]
    ldrb w9, [x27, #88]
    cmp w9, wzr
    b.ne LTest_Program_structvarargs_i32_varargs__while_body1
    b LTest_Program_structvarargs_i32_varargs__while_end2
LTest_Program_structvarargs_i32_varargs__while_body1:
    .loc 1 1091 22
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #88]
    ldr x9, [x27, #88]
    str x9, [x27, #32]
    .loc 1 1091 27
    ldr x9, [x27, #8]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    movz x10, #0x7
    add x9, x9, x10
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    movz x10, #0xfff8
    movk x10, #0xffff, lsl #16
    movk x10, #0xffff, lsl #32
    movk x10, #0xffff, lsl #48
    and x9, x9, x10
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldr x10, [x9]
    str x10, [x27, #88]
    ldr x9, [x27, #56]
    str x9, [x27, #96]
    ldr x9, [x27, #96]
    movz x10, #0x8
    add x9, x9, x10
    str x9, [x27, #96]
    ldr x9, [x27, #96]
    str x9, [x27, #96]
    ldr x9, [x27, #96]
    str x9, [x27, #8]
    .loc 1 1091 22
    ldr x9, [x27, #88]
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    str x9, [x27, #48]
    ldr x9, [x27, #40]
    ldr w10, [x9]
    str w10, [x27, #88]
    ldr w9, [x27, #88]
    str w9, [x27, #64]
    ldr x9, [x27, #48]
    ldr w10, [x27, #64]
    str w10, [x9]
    ldr x9, [x27, #40]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    ldrb w10, [x9]
    strb w10, [x27, #88]
    ldrb w9, [x27, #88]
    strb w9, [x27, #72]
    ldr x9, [x27, #48]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    str x9, [x27, #88]
    ldr x9, [x27, #88]
    ldrb w10, [x27, #72]
    strb w10, [x9]
    .loc 1 1092 17
    .loc 1 1092 26
    .loc 1 1092 29
    ldr x9, [x27, #32]
    ldr w10, [x9]
    str w10, [x27, #96]
    .loc 1 1092 23
    ldr w9, [x27, #16]
    ldr w10, [x27, #96]
    add w9, w9, w10
    str w9, [x27, #88]
    .loc 1 1092 15
    ldr w9, [x27, #88]
    str w9, [x27, #16]
    .loc 1 1 1
LTest_Program_structvarargs_i32_varargs__while_post3:
    .loc 1 1089 36
    .loc 1 1089 40
    .loc 1 1089 38
    ldr w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #88]
    .loc 1 1089 34
    ldr w9, [x27, #88]
    str w9, [x27, #24]
    .loc 1 1 1
    b LTest_Program_structvarargs_i32_varargs__while_cond0
LTest_Program_structvarargs_i32_varargs__while_end2:
    .loc 1 1094 12
    .loc 1 1095 9
    .loc 1 1095 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #160
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_max_t0_t0:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1100 9
    .loc 1 1100 13
    .loc 1 1100 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #16]
    .loc 1 1100 5
    ldrb w9, [x27, #16]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #16]
    ldrb w9, [x27, #16]
    cmp w9, wzr
    b.ne LTest_Program_max_t0_t0__if_true0
    b LTest_Program_max_t0_t0__if_end1
LTest_Program_max_t0_t0__if_true0:
    .loc 1 1101 13
    .loc 1 1101 9
    ldr w0, [x27, #0]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
    .loc 1 1100 5
LTest_Program_max_t0_t0__if_end1:
    .loc 1 1102 9
    .loc 1 1102 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_default_max_t0_t0
.p2align 2
_Test_Program_default_max_t0_t0:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1107 13
    .loc 1 1107 9
    ldr w9, [x27, #0]
    str w9, [x27, #16]
    .loc 1 1107 16
    .loc 1 1107 9
    ldr w9, [x27, #8]
    str w9, [x27, #24]
    ldr w0, [x27, #16]
    ldr w1, [x27, #24]
    bl _max__inst__T0_T0
    str w0, [x27, #32]
    .loc 1 1107 5
    ldr w0, [x27, #32]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_other_t0_t1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1112 10
    .loc 1 1112 23
    .loc 1 1112 20
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    add w9, w9, w10
    str w9, [x27, #16]
    .loc 1 1112 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #128
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    str w2, [x27, #16]
    .loc 1 1117 20
    .loc 1 1117 12
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1118 18
    .loc 1 1118 14
    mov w9, wzr
    str w9, [x27, #32]
    .loc 1 1 1
LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_cond0:
    .loc 1 1118 21
    .loc 1 1118 25
    .loc 1 1118 23
    ldr w9, [x27, #32]
    ldr w10, [x27, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #40]
    .loc 1 1 1
    ldrb w9, [x27, #40]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #40]
    ldrb w9, [x27, #40]
    cmp w9, wzr
    b.ne LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_body1
    b LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_end2
LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_body1:
    .loc 1 1120 17
    .loc 1 1120 27
    .loc 1 1120 31
    ldr x9, [x27, #0]
    str x9, [x27, #48]
    .loc 1 1120 32
    .loc 1 1120 31
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #48]
    ldr x10, [x27, #56]
    add x9, x9, x10
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    ldr w10, [x9]
    str w10, [x27, #48]
    .loc 1 1120 49
    .loc 1 1120 54
    ldr x9, [x27, #8]
    str x9, [x27, #56]
    .loc 1 1120 55
    .loc 1 1120 54
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #64]
    ldr x9, [x27, #64]
    movz x10, #0x8
    mul x9, x9, x10
    str x9, [x27, #64]
    ldr x9, [x27, #56]
    ldr x10, [x27, #64]
    add x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldr w10, [x9]
    str w10, [x27, #56]
    .loc 1 1120 46
    ldr w9, [x27, #48]
    ldr w10, [x27, #56]
    mul w9, w9, w10
    str w9, [x27, #48]
    .loc 1 1120 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #48]
    add w9, w9, w10
    str w9, [x27, #40]
    .loc 1 1120 15
    ldr w9, [x27, #40]
    str w9, [x27, #24]
    .loc 1 1 1
LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_post3:
    .loc 1 1118 36
    .loc 1 1118 40
    .loc 1 1118 38
    ldr w9, [x27, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #40]
    .loc 1 1118 34
    ldr w9, [x27, #40]
    str w9, [x27, #32]
    .loc 1 1 1
    b LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_cond0
LTest_Program_dot_sum_ptr_to_tleft_ptr_to_tright_i32__while_end2:
    .loc 1 1122 9
    .loc 1 1122 5
    ldr w0, [x27, #24]
    mov sp, x27
    add sp, sp, #128
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_funptr_test_i32_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1127 9
    .loc 1 1127 13
    .loc 1 1127 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    add w9, w9, w10
    str w9, [x27, #16]
    .loc 1 1127 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_passbystruct_type_kind_21:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 1132 9
    .loc 1 1132 12
    ldr x9, [x27, #0]
    ldr w10, [x9]
    str w10, [x27, #8]
    .loc 1 1132 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_returnbystruct_type_kind_21:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 1137 9
    .loc 1 1137 5
    ldr x0, [x27, #0]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_construct_thing:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #64
    mov x27, sp
    .loc 1 1142 18
    sub sp, sp, #16
    mov x10, sp
    str x10, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #0]
    ldr x9, [x27, #0]
    mov w10, wzr
    str w10, [x9]
    ldr x9, [x27, #0]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    mov w10, wzr
    strb w10, [x9]
    .loc 1 1143 5
    .loc 1 1143 22
    ldr x9, [x27, #0]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    movz x10, #0x4
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #8]
    .loc 1 1143 24
    .loc 1 1143 22
    movz w9, #0x5
    strb w9, [x27, #16]
    ldr x9, [x27, #8]
    ldrb w10, [x27, #16]
    strb w10, [x9]
    .loc 1 1144 5
    .loc 1 1144 19
    ldr x9, [x27, #0]
    str x9, [x27, #24]
    .loc 1 1144 21
    .loc 1 1144 19
    movz w9, #0xa
    str w9, [x27, #32]
    ldr x9, [x27, #24]
    ldr w10, [x27, #32]
    str w10, [x9]
    .loc 1 1145 9
    .loc 1 1145 5
    ldr x0, [x27, #0]
    mov sp, x27
    add sp, sp, #64
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_strlen_ptr_to_char:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #80
    mov x27, sp
    str x0, [x27, #0]
    .loc 1 1150 15
    .loc 1 1150 9
    mov w9, wzr
    str w9, [x27, #8]
    .loc 1 1 1
LTest_Program_strlen_ptr_to_char__while_cond0:
    .loc 1 1151 12
    .loc 1 1151 16
    ldr x9, [x27, #0]
    str x9, [x27, #24]
    .loc 1 1151 17
    .loc 1 1151 16
    ldr w9, [x27, #8]
    sxtw x9, w9
    str x9, [x27, #32]
    ldr x9, [x27, #24]
    ldr x10, [x27, #32]
    add x9, x9, x10
    str x9, [x27, #24]
    ldr x9, [x27, #24]
    str x9, [x27, #24]
    ldr x9, [x27, #24]
    ldrsb w10, [x9]
    strb w10, [x27, #24]
    .loc 1 1 1
    ldrsb w9, [x27, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #24]
    ldrb w9, [x27, #24]
    cmp w9, wzr
    b.ne LTest_Program_strlen_ptr_to_char__while_body1
    b LTest_Program_strlen_ptr_to_char__while_end2
LTest_Program_strlen_ptr_to_char__while_body1:
    .loc 1 1153 12
    ldr w9, [x27, #8]
    str w9, [x27, #16]
    ldr w9, [x27, #8]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #24]
    ldr w9, [x27, #24]
    str w9, [x27, #8]
    .loc 1 1 1
    b LTest_Program_strlen_ptr_to_char__while_cond0
LTest_Program_strlen_ptr_to_char__while_end2:
    .loc 1 1155 9
    .loc 1 1155 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #80
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_strcmp_ptr_to_char_ptr_to_char:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #192
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    .loc 1 1160 13
    .loc 1 1160 9
    mov w9, wzr
    str w9, [x27, #16]
    .loc 1 1 1
LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_cond0:
    .loc 1 1161 12
    .loc 1 1161 13
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    .loc 1 1161 14
    .loc 1 1161 13
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    ldrsb w10, [x9]
    strb w10, [x27, #32]
    .loc 1 1161 17
    ldrsb w9, [x27, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    ldrb w9, [x27, #32]
    cmp w9, wzr
    b.ne LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_rhs3
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_false4
LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_rhs3:
    .loc 1 1161 20
    .loc 1 1161 21
    ldr x9, [x27, #8]
    str x9, [x27, #32]
    .loc 1 1161 22
    .loc 1 1161 21
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    ldrsb w10, [x9]
    strb w10, [x27, #32]
    .loc 1 1161 17
    ldrsb w9, [x27, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    ldrb w9, [x27, #32]
    cmp w9, wzr
    b.ne LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_true5
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_false4
LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_true5:
    movz w11, #0x1
    strb w11, [x27, #32]
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_end6
LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_false4:
    mov w11, wzr
    strb w11, [x27, #32]
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_end6
LTest_Program_strcmp_ptr_to_char_ptr_to_char__land_end6:
    .loc 1 1 1
    ldrsb w9, [x27, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    ldrb w9, [x27, #32]
    cmp w9, wzr
    b.ne LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_body1
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_end2
LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_body1:
    .loc 1 1163 13
    .loc 1 1163 14
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    .loc 1 1163 15
    .loc 1 1163 14
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    ldrsb w10, [x9]
    strb w10, [x27, #32]
    .loc 1 1163 21
    .loc 1 1163 22
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    .loc 1 1163 23
    .loc 1 1163 22
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #48]
    ldr x9, [x27, #40]
    ldr x10, [x27, #48]
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldrsb w10, [x9]
    strb w10, [x27, #40]
    .loc 1 1163 18
    ldrsb w9, [x27, #32]
    ldrsb w10, [x27, #40]
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    .loc 1 1163 9
    ldrb w9, [x27, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #32]
    ldrb w9, [x27, #32]
    cmp w9, wzr
    b.ne LTest_Program_strcmp_ptr_to_char_ptr_to_char__if_true7
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__if_end8
LTest_Program_strcmp_ptr_to_char_ptr_to_char__if_true7:
    .loc 1 1165 18
    .loc 1 1165 19
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    .loc 1 1165 20
    .loc 1 1165 19
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    ldrsb w10, [x9]
    strb w10, [x27, #32]
    .loc 1 1165 25
    .loc 1 1165 26
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    .loc 1 1165 27
    .loc 1 1165 26
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #48]
    ldr x9, [x27, #40]
    ldr x10, [x27, #48]
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldrsb w10, [x9]
    strb w10, [x27, #40]
    .loc 1 1165 23
    ldrsb w9, [x27, #32]
    ldrsb w10, [x27, #40]
    sub w9, w9, w10
    sxtb w9, w9
    strb w9, [x27, #32]
    .loc 1 1165 31
    ldrsb w9, [x27, #32]
    sxtb w9, w9
    str w9, [x27, #32]
    .loc 1 1165 13
    ldr w0, [x27, #32]
    mov sp, x27
    add sp, sp, #192
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
    .loc 1 1163 9
LTest_Program_strcmp_ptr_to_char_ptr_to_char__if_end8:
    .loc 1 1167 10
    ldr w9, [x27, #16]
    str w9, [x27, #24]
    ldr w9, [x27, #16]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #32]
    ldr w9, [x27, #32]
    str w9, [x27, #16]
    .loc 1 1 1
    b LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_cond0
LTest_Program_strcmp_ptr_to_char_ptr_to_char__while_end2:
    .loc 1 1169 10
    .loc 1 1169 11
    ldr x9, [x27, #0]
    str x9, [x27, #32]
    .loc 1 1169 12
    .loc 1 1169 11
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #40]
    ldr x9, [x27, #32]
    ldr x10, [x27, #40]
    add x9, x9, x10
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    str x9, [x27, #32]
    ldr x9, [x27, #32]
    ldrsb w10, [x9]
    strb w10, [x27, #32]
    .loc 1 1169 17
    .loc 1 1169 18
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    .loc 1 1169 19
    .loc 1 1169 18
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [x27, #48]
    ldr x9, [x27, #40]
    ldr x10, [x27, #48]
    add x9, x9, x10
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    str x9, [x27, #40]
    ldr x9, [x27, #40]
    ldrsb w10, [x9]
    strb w10, [x27, #40]
    .loc 1 1169 15
    ldrsb w9, [x27, #32]
    ldrsb w10, [x27, #40]
    sub w9, w9, w10
    sxtb w9, w9
    strb w9, [x27, #32]
    .loc 1 1169 23
    ldrsb w9, [x27, #32]
    sxtb w9, w9
    str w9, [x27, #32]
    .loc 1 1169 5
    ldr w0, [x27, #32]
    mov sp, x27
    add sp, sp, #192
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_abort:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 1174 10
    .loc 1 1174 5
    movz w0, #0x1
    bl _exit
    .loc 1 1175 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _Test_Program_test_overflow
.p2align 2
_Test_Program_test_overflow:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    .loc 1 1180 13
    .loc 1 1180 9
    movz w9, #0x7530
    strh w9, [x27, #0]
    .loc 1 1181 13
    .loc 1 1181 9
    movz w9, #0x2710
    strh w9, [x27, #8]
    .loc 1 1182 9
    .loc 1 1182 13
    .loc 1 1182 11
    ldrsh w9, [x27, #0]
    ldrsh w10, [x27, #8]
    add w9, w9, w10
    sxth w9, w9
    strh w9, [x27, #16]
    .loc 1 1182 5
    ldrsh w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_test_mul_overflow:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    .loc 1 1187 12
    .loc 1 1187 8
    movz w9, #0xc8
    strb w9, [x27, #0]
    .loc 1 1188 12
    .loc 1 1188 8
    movz w9, #0xc8
    strb w9, [x27, #8]
    .loc 1 1189 9
    .loc 1 1189 13
    .loc 1 1189 11
    ldrb w9, [x27, #0]
    ldrb w10, [x27, #8]
    mul w9, w9, w10
    uxtb w9, w9
    strb w9, [x27, #16]
    .loc 1 1189 5
    ldrb w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _differentsig
.p2align 2
_differentsig:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 1195 14
    .loc 1 1195 8
    adrp x0, __differentsig____str195@PAGE
    add x0, x0, __differentsig____str195@PAGEOFF
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #0]
    .loc 1 1196 5
    mov w0, wzr
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_Test_Program_rawthing_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    movz w9, #0x1c8
    ldr w10, [x27, #0]
    add w9, w9, w10
    str w9, [x27, #8]
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__lambda_0:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #128
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    .loc 1 252 13
    .loc 1 252 27
    .loc 1 252 24
    ldr x9, [x27, #0]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #48]
    .loc 1 252 9
    ldrb w9, [x27, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #48]
    ldrb w9, [x27, #48]
    cmp w9, wzr
    b.ne L__lambda_0__if_true0
    b L__lambda_0__if_end1
L__lambda_0__if_true0:
    .loc 1 253 22
    .loc 1 253 16
    adrp x9, ___lambda_0____str196@PAGE
    add x9, x9, ___lambda_0____str196@PAGEOFF
    str x9, [x27, #16]
    .loc 1 253 41
    .loc 1 253 53
    ldr x9, [x27, #0]
    ldr w10, [x9]
    str w10, [x27, #48]
    .loc 1 253 16
    ldr w9, [x27, #48]
    str w9, [x27, #24]
    ldr x0, [x27, #16]
    ldr w1, [x27, #24]
    sub sp, sp, #16
    ldr w9, [x27, #24]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #48]
    .loc 1 252 9
L__lambda_0__if_end1:
    .loc 1 254 13
    .loc 1 254 25
    .loc 1 254 22
    ldr x9, [x27, #8]
    mov x10, xzr
    cmp x9, x10
    cset w11, ne
    strb w11, [x27, #48]
    .loc 1 254 9
    ldrb w9, [x27, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #48]
    ldrb w9, [x27, #48]
    cmp w9, wzr
    b.ne L__lambda_0__if_true2
    b L__lambda_0__if_end3
L__lambda_0__if_true2:
    .loc 1 255 22
    .loc 1 255 16
    adrp x9, ___lambda_0____str197@PAGE
    add x9, x9, ___lambda_0____str197@PAGEOFF
    str x9, [x27, #32]
    .loc 1 255 45
    .loc 1 255 16
    ldr x9, [x27, #8]
    str x9, [x27, #40]
    ldr x0, [x27, #32]
    ldr x1, [x27, #40]
    sub sp, sp, #16
    ldr x9, [x27, #40]
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #48]
    .loc 1 254 9
L__lambda_0__if_end3:
    .loc 1 256 9
    mov w0, wzr
    mov sp, x27
    add sp, sp, #128
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _default_max__inst__T0_i32
.p2align 2
_default_max__inst__T0_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1107 13
    .loc 1 1107 9
    ldr w9, [x27, #0]
    str w9, [x27, #16]
    .loc 1 1107 16
    .loc 1 1107 9
    ldr w9, [x27, #8]
    str w9, [x27, #24]
    ldr w0, [x27, #16]
    ldr w1, [x27, #24]
    bl _max__inst__T0_i32
    str w0, [x27, #32]
    .loc 1 1107 5
    ldr w0, [x27, #32]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_max__inst__T0_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1100 9
    .loc 1 1100 13
    .loc 1 1100 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #16]
    .loc 1 1100 5
    ldrb w9, [x27, #16]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #16]
    ldrb w9, [x27, #16]
    cmp w9, wzr
    b.ne Lmax__inst__T0_i32__if_true0
    b Lmax__inst__T0_i32__if_end1
Lmax__inst__T0_i32__if_true0:
    .loc 1 1101 13
    .loc 1 1101 9
    ldr w0, [x27, #0]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
    .loc 1 1100 5
Lmax__inst__T0_i32__if_end1:
    .loc 1 1102 9
    .loc 1 1102 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_other__inst__T0_u8__T1_i16:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    strb w0, [x27, #0]
    strh w1, [x27, #8]
    .loc 1 1112 10
    .loc 1 1112 12
    ldrb w9, [x27, #0]
    uxtb w9, w9
    str w9, [x27, #16]
    .loc 1 1112 23
    .loc 1 1112 25
    ldrsh w9, [x27, #8]
    sxth w9, w9
    str w9, [x27, #24]
    .loc 1 1112 20
    ldr w9, [x27, #16]
    ldr w10, [x27, #24]
    add w9, w9, w10
    str w9, [x27, #16]
    .loc 1 1112 5
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_dot_sum__inst__T0_i16__T1_u8__T2_i64:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #112
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    str w2, [x27, #16]
    .loc 1 1117 20
    .loc 1 1117 12
    mov x9, xzr
    str x9, [x27, #24]
    .loc 1 1118 18
    .loc 1 1118 14
    mov w9, wzr
    str w9, [x27, #32]
    .loc 1 1 1
Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_cond0:
    .loc 1 1118 21
    .loc 1 1118 25
    .loc 1 1118 23
    ldr w9, [x27, #32]
    ldr w10, [x27, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #40]
    .loc 1 1 1
    ldrb w9, [x27, #40]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #40]
    ldrb w9, [x27, #40]
    cmp w9, wzr
    b.ne Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_body1
    b Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_end2
Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_body1:
    .loc 1 1120 17
    .loc 1 1120 27
    .loc 1 1120 31
    ldr x9, [x27, #0]
    str x9, [x27, #48]
    .loc 1 1120 32
    .loc 1 1120 31
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    movz x10, #0x2
    mul x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #48]
    ldr x10, [x27, #56]
    add x9, x9, x10
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    ldrsh w10, [x9]
    strh w10, [x27, #48]
    .loc 1 1120 35
    ldrsh w9, [x27, #48]
    sxth x9, w9
    str x9, [x27, #48]
    .loc 1 1120 49
    .loc 1 1120 54
    ldr x9, [x27, #8]
    str x9, [x27, #56]
    .loc 1 1120 55
    .loc 1 1120 54
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #64]
    ldr x9, [x27, #56]
    ldr x10, [x27, #64]
    add x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldrb w10, [x9]
    strb w10, [x27, #56]
    .loc 1 1120 58
    ldrb w9, [x27, #56]
    uxtb w9, w9
    str x9, [x27, #56]
    .loc 1 1120 46
    ldr x9, [x27, #48]
    ldr x10, [x27, #56]
    mul x9, x9, x10
    str x9, [x27, #48]
    .loc 1 1120 23
    ldr x9, [x27, #24]
    ldr x10, [x27, #48]
    add x9, x9, x10
    str x9, [x27, #40]
    .loc 1 1120 15
    ldr x9, [x27, #40]
    str x9, [x27, #24]
    .loc 1 1 1
Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_post3:
    .loc 1 1118 36
    .loc 1 1118 40
    .loc 1 1118 38
    ldr w9, [x27, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #40]
    .loc 1 1118 34
    ldr w9, [x27, #40]
    str w9, [x27, #32]
    .loc 1 1 1
    b Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_cond0
Ldot_sum__inst__T0_i16__T1_u8__T2_i64__while_end2:
    .loc 1 1122 9
    .loc 1 1122 5
    ldr x0, [x27, #24]
    mov sp, x27
    add sp, sp, #112
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_dot_sum__inst__T0_i8__T1_i8__T2_i32:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #112
    mov x27, sp
    str x0, [x27, #0]
    str x1, [x27, #8]
    str w2, [x27, #16]
    .loc 1 1117 20
    .loc 1 1117 12
    mov w9, wzr
    str w9, [x27, #24]
    .loc 1 1118 18
    .loc 1 1118 14
    mov w9, wzr
    str w9, [x27, #32]
    .loc 1 1 1
Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_cond0:
    .loc 1 1118 21
    .loc 1 1118 25
    .loc 1 1118 23
    ldr w9, [x27, #32]
    ldr w10, [x27, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [x27, #40]
    .loc 1 1 1
    ldrb w9, [x27, #40]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #40]
    ldrb w9, [x27, #40]
    cmp w9, wzr
    b.ne Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_body1
    b Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_end2
Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_body1:
    .loc 1 1120 17
    .loc 1 1120 27
    .loc 1 1120 31
    ldr x9, [x27, #0]
    str x9, [x27, #48]
    .loc 1 1120 32
    .loc 1 1120 31
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #56]
    ldr x9, [x27, #48]
    ldr x10, [x27, #56]
    add x9, x9, x10
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    str x9, [x27, #48]
    ldr x9, [x27, #48]
    ldrsb w10, [x9]
    strb w10, [x27, #48]
    .loc 1 1120 35
    ldrsb w9, [x27, #48]
    sxtb w9, w9
    str w9, [x27, #48]
    .loc 1 1120 49
    .loc 1 1120 54
    ldr x9, [x27, #8]
    str x9, [x27, #56]
    .loc 1 1120 55
    .loc 1 1120 54
    ldr w9, [x27, #32]
    sxtw x9, w9
    str x9, [x27, #64]
    ldr x9, [x27, #56]
    ldr x10, [x27, #64]
    add x9, x9, x10
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    str x9, [x27, #56]
    ldr x9, [x27, #56]
    ldrsb w10, [x9]
    strb w10, [x27, #56]
    .loc 1 1120 58
    ldrsb w9, [x27, #56]
    sxtb w9, w9
    str w9, [x27, #56]
    .loc 1 1120 46
    ldr w9, [x27, #48]
    ldr w10, [x27, #56]
    mul w9, w9, w10
    str w9, [x27, #48]
    .loc 1 1120 23
    ldr w9, [x27, #24]
    ldr w10, [x27, #48]
    add w9, w9, w10
    str w9, [x27, #40]
    .loc 1 1120 15
    ldr w9, [x27, #40]
    str w9, [x27, #24]
    .loc 1 1 1
Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_post3:
    .loc 1 1118 36
    .loc 1 1118 40
    .loc 1 1118 38
    ldr w9, [x27, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [x27, #40]
    .loc 1 1118 34
    ldr w9, [x27, #40]
    str w9, [x27, #32]
    .loc 1 1 1
    b Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_cond0
Ldot_sum__inst__T0_i8__T1_i8__T2_i32__while_end2:
    .loc 1 1122 9
    .loc 1 1122 5
    ldr w0, [x27, #24]
    mov sp, x27
    add sp, sp, #112
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__lambda_1:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 789 18
    .loc 1 789 12
    adrp x0, ___lambda_1____str198@PAGE
    add x0, x0, ___lambda_1____str198@PAGEOFF
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #0]
    .loc 1 790 9
    mov w0, wzr
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__lambda_2:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    .loc 1 796 18
    .loc 1 796 12
    adrp x9, ___lambda_2____str199@PAGE
    add x9, x9, ___lambda_2____str199@PAGEOFF
    str x9, [x27, #8]
    .loc 1 796 58
    .loc 1 796 12
    ldr w9, [x27, #0]
    str w9, [x27, #16]
    ldr x0, [x27, #8]
    ldr w1, [x27, #16]
    sub sp, sp, #16
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #24]
    .loc 1 797 14
    .loc 1 797 16
    ldr w9, [x27, #0]
    uxth w9, w9
    strh w9, [x27, #24]
    .loc 1 797 26
    .loc 1 797 24
    ldrsh w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    sxth w9, w9
    strh w9, [x27, #24]
    .loc 1 797 9
    ldrsh w0, [x27, #24]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__lambda_3:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #16
    mov x27, sp
    .loc 1 804 22
    .loc 1 804 16
    adrp x0, ___lambda_3____str200@PAGE
    add x0, x0, ___lambda_3____str200@PAGEOFF
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    str w0, [x27, #0]
    .loc 1 805 13
    mov w0, wzr
    mov sp, x27
    add sp, sp, #16
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
__lambda_4:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    .loc 1 808 22
    .loc 1 808 16
    adrp x9, ___lambda_4____str201@PAGE
    add x9, x9, ___lambda_4____str201@PAGEOFF
    str x9, [x27, #8]
    .loc 1 808 65
    .loc 1 808 16
    ldr w9, [x27, #0]
    str w9, [x27, #16]
    ldr x0, [x27, #8]
    ldr w1, [x27, #16]
    sub sp, sp, #16
    ldr w9, [x27, #16]
    sxtw x9, w9
    str x9, [sp, #0]
    mov x15, sp
    bl _Std_IO_print_ptr_to_char
    add sp, sp, #16
    str w0, [x27, #24]
    .loc 1 809 18
    .loc 1 809 20
    ldr w9, [x27, #0]
    uxth w9, w9
    strh w9, [x27, #24]
    .loc 1 809 30
    .loc 1 809 28
    ldrsh w9, [x27, #24]
    movz w10, #0x1
    add w9, w9, w10
    sxth w9, w9
    strh w9, [x27, #24]
    .loc 1 809 13
    ldrsh w0, [x27, #24]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.p2align 2
_max__inst__T0_T0:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #48
    mov x27, sp
    str w0, [x27, #0]
    str w1, [x27, #8]
    .loc 1 1100 9
    .loc 1 1100 13
    .loc 1 1100 11
    ldr w9, [x27, #0]
    ldr w10, [x27, #8]
    cmp w9, w10
    cset w11, gt
    strb w11, [x27, #16]
    .loc 1 1100 5
    ldrb w9, [x27, #16]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [x27, #16]
    ldrb w9, [x27, #16]
    cmp w9, wzr
    b.ne Lmax__inst__T0_T0__if_true0
    b Lmax__inst__T0_T0__if_end1
Lmax__inst__T0_T0__if_true0:
    .loc 1 1101 13
    .loc 1 1101 9
    ldr w0, [x27, #0]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret
    .loc 1 1100 5
Lmax__inst__T0_T0__if_end1:
    .loc 1 1102 9
    .loc 1 1102 5
    ldr w0, [x27, #8]
    mov sp, x27
    add sp, sp, #48
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret

.globl _main
.p2align 2
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    stp x27, x28, [sp, #-16]!
    sub sp, sp, #32
    mov x27, sp
    str w0, [x27, #0]
    str x1, [x27, #8]
    bl __cert__entry_main
    str w0, [x27, #16]
    ldr w0, [x27, #16]
    mov sp, x27
    add sp, sp, #32
    ldp x27, x28, [sp], #16
    ldp x29, x30, [sp], #16
    ret


.section __TEXT,__cstring
__Test_Program_printwrapper____str0:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x00
__Test_Program_printwrapper____str1:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x00
__Test_Program_printwrapper____str2:
    .byte 0x5f, 0x5f, 0x46, 0x55, 0x4e, 0x43, 0x54, 0x49, 0x4f, 0x4e, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x00
__Test_Program_printwrapper____str3:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x25, 0x73, 0x21, 0x20, 0x4e, 0x75, 0x6d, 0x62, 0x65, 0x72, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
__Test_Program_printwrapper____str4:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0x00
__Test_Program_assert_bool_ptr_to_char____str5:
    .byte 0x54, 0x45, 0x53, 0x54, 0x20, 0x50, 0x41, 0x53, 0x53, 0x45, 0x44, 0x3a, 0x20, 0x25, 0x73, 0x0a, 0x00
__Test_Program_assert_bool_ptr_to_char____str6:
    .byte 0x54, 0x45, 0x53, 0x54, 0x20, 0x46, 0x41, 0x49, 0x4c, 0x3a, 0x20, 0x25, 0x73, 0x0a, 0x00
___cert__entry_main____str7:
    .byte 0x76, 0x61, 0x72, 0x20, 0x69, 0x6e, 0x66, 0x65, 0x72, 0x72, 0x65, 0x64, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x6f, 0x72, 0x69, 0x67, 0x69, 0x6e, 0x61, 0x6c, 0x20, 0x62, 0x75, 0x66, 0x66, 0x65, 0x72, 0x00
___cert__entry_main____str8:
    .byte 0x22, 0x62, 0x75, 0x66, 0x22, 0x20, 0x61, 0x6c, 0x6c, 0x6f, 0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x00
___cert__entry_main____str9:
    .byte 0x6d, 0x65, 0x6d, 0x73, 0x65, 0x74, 0x20, 0x74, 0x6f, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0x00
___cert__entry_main____str10:
    .byte 0x57, 0x68, 0x61, 0x74, 0x27, 0x73, 0x20, 0x79, 0x6f, 0x75, 0x72, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x3f, 0x20, 0x3e, 0x20, 0x00
___cert__entry_main____str11:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x20, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x65, 0x64, 0x20, 0x6c, 0x65, 0x6e, 0x67, 0x74, 0x68, 0x00
___cert__entry_main____str12:
    .byte 0x61, 0x6c, 0x69, 0x61, 0x73, 0x20, 0x74, 0x65, 0x73, 0x74, 0x00
___cert__entry_main____str13:
    .byte 0x43, 0x43, 0x44, 0x69, 0x61, 0x67, 0x6e, 0x6f, 0x73, 0x74, 0x69, 0x63, 0x43, 0x61, 0x6c, 0x6c, 0x62, 0x61, 0x63, 0x6b, 0x20, 0x61, 0x6c, 0x69, 0x61, 0x73, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str14:
    .byte 0x4e, 0x6f, 0x20, 0x69, 0x6e, 0x70, 0x75, 0x74, 0x20, 0x72, 0x65, 0x61, 0x64, 0x2c, 0x20, 0x75, 0x73, 0x69, 0x6e, 0x67, 0x20, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x0a, 0x00
___cert__entry_main____str15:
    .byte 0x73, 0x74, 0x72, 0x6c, 0x65, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x72, 0x65, 0x61, 0x64, 0x00
___cert__entry_main____str16:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0x00
___cert__entry_main____str17:
    .byte 0x55, 0x73, 0x65, 0x72, 0x20, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x65, 0x64, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x00
___cert__entry_main____str18:
    .byte 0x55, 0x73, 0x65, 0x72, 0x20, 0x64, 0x69, 0x64, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x20, 0x28, 0x6e, 0x6f, 0x74, 0x20, 0x72, 0x65, 0x71, 0x75, 0x69, 0x72, 0x65, 0x64, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x74, 0x65, 0x73, 0x74, 0x73, 0x29, 0x00
___cert__entry_main____str19:
    .byte 0x46, 0x72, 0x65, 0x65, 0x69, 0x6e, 0x67, 0x20, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x2e, 0x2e, 0x2e, 0x0a, 0x00
___cert__entry_main____str20:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str21:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x2d, 0x32, 0x35, 0x35, 0x33, 0x36, 0x00
___cert__entry_main____str22:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x75, 0x6c, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x36, 0x34, 0x00
___cert__entry_main____str23:
    .byte 0x36, 0x20, 0x2a, 0x20, 0x37, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str24:
    .byte 0x32, 0x30, 0x20, 0x2f, 0x20, 0x2d, 0x34, 0x20, 0x3d, 0x3d, 0x20, 0x2d, 0x35, 0x00
___cert__entry_main____str25:
    .byte 0x32, 0x30, 0x20, 0x25, 0x20, 0x36, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x00
___cert__entry_main____str26:
    .byte 0x30, 0x78, 0x46, 0x46, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x35, 0x35, 0x00
___cert__entry_main____str27:
    .byte 0x43, 0x61, 0x6c, 0x6c, 0x69, 0x6e, 0x67, 0x20, 0x74, 0x65, 0x73, 0x74, 0x5f, 0x63, 0x68, 0x61, 0x6e, 0x63, 0x65, 0x28, 0x29, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str28:
    .byte 0x64, 0x65, 0x72, 0x65, 0x66, 0x2e, 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x34, 0x00
___cert__entry_main____str29:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x64, 0x65, 0x72, 0x65, 0x66, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x32, 0x33, 0x00
___cert__entry_main____str30:
    .byte 0x61, 0x64, 0x64, 0x70, 0x74, 0x72, 0x5b, 0x30, 0x5d, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x35, 0x00
___cert__entry_main____str31:
    .byte 0x61, 0x64, 0x64, 0x70, 0x74, 0x72, 0x5b, 0x31, 0x5d, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x32, 0x33, 0x00
___cert__entry_main____str32:
    .byte 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x76, 0x69, 0x61, 0x20, 0x69, 0x6e, 0x74, 0x70, 0x74, 0x72, 0x3a, 0x20, 0x25, 0x70, 0x0a, 0x00
___cert__entry_main____str33:
    .byte 0x73, 0x65, 0x74, 0x74, 0x65, 0x72, 0x2f, 0x67, 0x65, 0x74, 0x74, 0x65, 0x72, 0x20, 0x76, 0x69, 0x61, 0x20, 0x3d, 0x3e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str34:
    .byte 0x31, 0x20, 0x3c, 0x3c, 0x20, 0x31, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x00
___cert__entry_main____str35:
    .byte 0x34, 0x20, 0x3e, 0x3e, 0x20, 0x31, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x00
___cert__entry_main____str36:
    .byte 0x74, 0x79, 0x70, 0x65, 0x6f, 0x66, 0x28, 0x29, 0x20, 0x3d, 0x20, 0x25, 0x73, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x6f, 0x66, 0x28, 0x29, 0x20, 0x3d, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str37:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x43, 0x68, 0x61, 0x6e, 0x63, 0x65, 0x2e, 0x53, 0x6d, 0x61, 0x6c, 0x6c, 0x54, 0x65, 0x73, 0x74, 0x3a, 0x3a, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x00
___cert__entry_main____str38:
    .byte 0x72, 0x61, 0x77, 0x74, 0x68, 0x69, 0x6e, 0x67, 0x28, 0x31, 0x30, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x36, 0x36, 0x00
___cert__entry_main____str39:
    .byte 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str40:
    .byte 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str41:
    .byte 0x61, 0x63, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x00
___cert__entry_main____str42:
    .byte 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x70, 0x74, 0x72, 0x5b, 0x30, 0x5d, 0x20, 0x3d, 0x3d, 0x20, 0x36, 0x2e, 0x32, 0x38, 0x66, 0x00
___cert__entry_main____str43:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x2e, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x00
___cert__entry_main____str44:
    .byte 0x45, 0x78, 0x70, 0x65, 0x63, 0x74, 0x20, 0x61, 0x64, 0x64, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x6f, 0x66, 0x20, 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x2b, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x76, 0x61, 0x6c, 0x20, 0x3d, 0x20, 0x25, 0x6c, 0x66, 0x0a, 0x00
___cert__entry_main____str45:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x2b, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x76, 0x61, 0x6c, 0x20, 0x7e, 0x3d, 0x20, 0x37, 0x2e, 0x35, 0x31, 0x34, 0x35, 0x36, 0x38, 0x00
___cert__entry_main____str46:
    .byte 0x53, 0x75, 0x6d, 0x20, 0x6f, 0x66, 0x20, 0x76, 0x61, 0x72, 0x69, 0x2e, 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x61, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x36, 0x38, 0x00
___cert__entry_main____str47:
    .byte 0x66, 0x31, 0x32, 0x38, 0x2c, 0x20, 0x66, 0x36, 0x34, 0x2c, 0x20, 0x66, 0x33, 0x32, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x00
___cert__entry_main____str48:
    .byte 0x66, 0x69, 0x6e, 0x61, 0x6c, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x65, 0x71, 0x75, 0x61, 0x6c, 0x73, 0x20, 0x31, 0x36, 0x38, 0x00
___cert__entry_main____str49:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x41, 0x4e, 0x44, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str50:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x4f, 0x52, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str51:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x58, 0x4f, 0x52, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str52:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x4e, 0x4f, 0x54, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str53:
    .byte 0x62, 0x72, 0x65, 0x61, 0x6b, 0x20, 0x65, 0x78, 0x69, 0x74, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0x20, 0x65, 0x61, 0x72, 0x6c, 0x79, 0x00
___cert__entry_main____str54:
    .byte 0x63, 0x6f, 0x6e, 0x74, 0x69, 0x6e, 0x75, 0x65, 0x20, 0x73, 0x6b, 0x69, 0x70, 0x73, 0x20, 0x69, 0x74, 0x65, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x75, 0x74, 0x20, 0x73, 0x74, 0x69, 0x6c, 0x6c, 0x20, 0x61, 0x64, 0x76, 0x61, 0x6e, 0x63, 0x65, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0x00
___cert__entry_main____str55:
    .byte 0x27, 0x76, 0x61, 0x72, 0x27, 0x20, 0x69, 0x6e, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x69, 0x6e, 0x73, 0x69, 0x64, 0x65, 0x20, 0x66, 0x6f, 0x72, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0x20, 0x69, 0x6e, 0x69, 0x74, 0x00
___cert__entry_main____str56:
    .byte 0x62, 0x72, 0x65, 0x61, 0x6b, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x63, 0x6f, 0x6e, 0x74, 0x69, 0x6e, 0x75, 0x65, 0x20, 0x63, 0x6f, 0x6f, 0x70, 0x65, 0x72, 0x61, 0x74, 0x65, 0x20, 0x69, 0x6e, 0x20, 0x77, 0x68, 0x69, 0x6c, 0x65, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0x00
___cert__entry_main____str57:
    .byte 0x30, 0x30, 0x31, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x31, 0x31, 0x30, 0x30, 0x31, 0x00
___cert__entry_main____str58:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x72, 0x65, 0x70, 0x72, 0x65, 0x73, 0x65, 0x6e, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x6f, 0x66, 0x20, 0x31, 0x32, 0x33, 0x34, 0x35, 0x20, 0x69, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str59:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x2d, 0x6c, 0x69, 0x6b, 0x65, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x65, 0x78, 0x70, 0x61, 0x6e, 0x64, 0x73, 0x20, 0x74, 0x6f, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x00
___cert__entry_main____str60:
    .byte 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x2d, 0x73, 0x74, 0x79, 0x6c, 0x65, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x65, 0x78, 0x70, 0x61, 0x6e, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str61:
    .byte 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x61, 0x72, 0x67, 0x75, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x73, 0x74, 0x61, 0x79, 0x20, 0x70, 0x61, 0x72, 0x65, 0x6e, 0x74, 0x68, 0x65, 0x73, 0x69, 0x7a, 0x65, 0x64, 0x00
___cert__entry_main____str62:
    .byte 0x23, 0x69, 0x66, 0x2f, 0x23, 0x65, 0x6c, 0x73, 0x65, 0x20, 0x62, 0x72, 0x61, 0x6e, 0x63, 0x68, 0x20, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str63:
    .byte 0x23, 0x65, 0x6c, 0x69, 0x66, 0x20, 0x62, 0x72, 0x61, 0x6e, 0x63, 0x68, 0x20, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str64:
    .byte 0x23, 0x75, 0x6e, 0x64, 0x65, 0x66, 0x20, 0x63, 0x6c, 0x65, 0x61, 0x72, 0x73, 0x20, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x67, 0x75, 0x61, 0x72, 0x64, 0x73, 0x00
___cert__entry_main____str65:
    .byte 0x5f, 0x5f, 0x43, 0x48, 0x41, 0x4e, 0x43, 0x45, 0x5f, 0x5f, 0x20, 0x62, 0x75, 0x69, 0x6c, 0x74, 0x69, 0x6e, 0x20, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x64, 0x00
___cert__entry_main____str66:
    .byte 0x61, 0x67, 0x67, 0x72, 0x65, 0x67, 0x61, 0x74, 0x65, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x6e, 0x65, 0x6e, 0x74, 0x73, 0x00
___cert__entry_main____str67:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0x43, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x3a, 0x20, 0x25, 0x73, 0x00
___cert__entry_main____str68:
    .byte 0x32, 0x2e, 0x35, 0x2e, 0x30, 0x00
___cert__entry_main____str69:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0x20, 0x48, 0x25, 0x64, 0x00
___cert__entry_main____str70:
    .byte 0x5f, 0x5f, 0x50, 0x4f, 0x49, 0x4e, 0x54, 0x45, 0x52, 0x5f, 0x57, 0x49, 0x44, 0x54, 0x48, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x6f, 0x66, 0x28, 0x76, 0x6f, 0x69, 0x64, 0x2a, 0x29, 0x00
___cert__entry_main____str71:
    .byte 0x5f, 0x5f, 0x49, 0x53, 0x36, 0x34, 0x42, 0x49, 0x54, 0x5f, 0x5f, 0x20, 0x72, 0x65, 0x66, 0x6c, 0x65, 0x63, 0x74, 0x73, 0x20, 0x36, 0x34, 0x2d, 0x62, 0x69, 0x74, 0x00
___cert__entry_main____str72:
    .byte 0x5f, 0x5f, 0x49, 0x53, 0x36, 0x34, 0x42, 0x49, 0x54, 0x5f, 0x5f, 0x20, 0x72, 0x65, 0x66, 0x6c, 0x65, 0x63, 0x74, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x36, 0x34, 0x2d, 0x62, 0x69, 0x74, 0x00
___cert__entry_main____str73:
    .byte 0x5f, 0x5f, 0x4d, 0x41, 0x43, 0x48, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0x00
___cert__entry_main____str74:
    .byte 0x74, 0x65, 0x73, 0x74, 0x73, 0x2f, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x73, 0x2f, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x66, 0x2e, 0x63, 0x65, 0x00
___cert__entry_main____str75:
    .byte 0x5f, 0x5f, 0x46, 0x49, 0x4c, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00
___cert__entry_main____str76:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x50, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x00
___cert__entry_main____str77:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x50, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0x00
___cert__entry_main____str78:
    .byte 0x5f, 0x5f, 0x4d, 0x4f, 0x44, 0x55, 0x4c, 0x45, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x6d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x00
___cert__entry_main____str79:
    .byte 0x5f, 0x5f, 0x4c, 0x49, 0x4e, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x76, 0x65, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00
___cert__entry_main____str80:
    .byte 0x41, 0x70, 0x72, 0x20, 0x30, 0x36, 0x20, 0x32, 0x30, 0x32, 0x36, 0x00
___cert__entry_main____str81:
    .byte 0x5f, 0x5f, 0x44, 0x41, 0x54, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00
___cert__entry_main____str82:
    .byte 0x31, 0x36, 0x3a, 0x31, 0x36, 0x3a, 0x35, 0x38, 0x00
___cert__entry_main____str83:
    .byte 0x5f, 0x5f, 0x54, 0x49, 0x4d, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x00
___cert__entry_main____str84:
    .byte 0x5f, 0x5f, 0x43, 0x4f, 0x55, 0x4e, 0x54, 0x45, 0x52, 0x5f, 0x5f, 0x20, 0x69, 0x6e, 0x63, 0x72, 0x65, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x73, 0x65, 0x71, 0x75, 0x65, 0x6e, 0x74, 0x69, 0x61, 0x6c, 0x6c, 0x79, 0x00
___cert__entry_main____str85:
    .byte 0x6d, 0x61, 0x69, 0x6e, 0x00
___cert__entry_main____str86:
    .byte 0x6d, 0x61, 0x69, 0x6e, 0x00
___cert__entry_main____str87:
    .byte 0x5f, 0x5f, 0x46, 0x55, 0x4e, 0x43, 0x54, 0x49, 0x4f, 0x4e, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x6d, 0x61, 0x69, 0x6e, 0x00
___cert__entry_main____str88:
    .byte 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x00
___cert__entry_main____str89:
    .byte 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x00
___cert__entry_main____str90:
    .byte 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x20, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x20, 0x6d, 0x65, 0x72, 0x67, 0x69, 0x6e, 0x67, 0x00
___cert__entry_main____str91:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0x00
___cert__entry_main____str92:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str93:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x31, 0x35, 0x30, 0x00
___cert__entry_main____str94:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x74, 0x65, 0x73, 0x74, 0x00
___cert__entry_main____str95:
    .byte 0x69, 0x6e, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x20, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x74, 0x65, 0x73, 0x74, 0x00
___cert__entry_main____str96:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x5f, 0x73, 0x75, 0x6d, 0x20, 0x68, 0x61, 0x6e, 0x64, 0x6c, 0x65, 0x73, 0x20, 0x3e, 0x38, 0x20, 0x61, 0x72, 0x67, 0x73, 0x00
___cert__entry_main____str97:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x5f, 0x6d, 0x69, 0x78, 0x20, 0x68, 0x61, 0x6e, 0x64, 0x6c, 0x65, 0x73, 0x20, 0x69, 0x6e, 0x74, 0x2b, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x20, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x00
___cert__entry_main____str98:
    .byte 0x66, 0x69, 0x78, 0x65, 0x64, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x36, 0x30, 0x00
___cert__entry_main____str99:
    .byte 0x66, 0x69, 0x78, 0x65, 0x64, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x20, 0x69, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str100:
    .byte 0x64, 0x79, 0x6e, 0x61, 0x6d, 0x69, 0x63, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x31, 0x30, 0x30, 0x30, 0x00
___cert__entry_main____str101:
    .byte 0x74, 0x65, 0x73, 0x74, 0x00
___cert__entry_main____str102:
    .byte 0x70, 0x61, 0x73, 0x73, 0x62, 0x79, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x38, 0x31, 0x00
___cert__entry_main____str103:
    .byte 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x62, 0x79, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x69, 0x6e, 0x70, 0x75, 0x74, 0x00
___cert__entry_main____str104:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str105:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x76, 0x69, 0x61, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str106:
    .byte 0x72, 0x61, 0x77, 0x20, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x00
___cert__entry_main____str107:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x76, 0x69, 0x61, 0x20, 0x72, 0x61, 0x77, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str108:
    .byte 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x38, 0x31, 0x00
___cert__entry_main____str109:
    .byte 0x69, 0x6e, 0x6c, 0x69, 0x6e, 0x61, 0x62, 0x6c, 0x65, 0x5f, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x31, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x35, 0x36, 0x00
___cert__entry_main____str110:
    .byte 0x69, 0x6e, 0x6c, 0x69, 0x6e, 0x61, 0x62, 0x6c, 0x65, 0x5f, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x30, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x30, 0x00
___cert__entry_main____str111:
    .byte 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x69, 0x73, 0x20, 0x25, 0x64, 0x21, 0x0a, 0x00
___cert__entry_main____str112:
    .byte 0x41, 0x72, 0x63, 0x68, 0x20, 0x28, 0x6e, 0x6f, 0x6e, 0x2d, 0x78, 0x38, 0x36, 0x29, 0x20, 0x69, 0x73, 0x20, 0x25, 0x73, 0x2e, 0x0a, 0x00
___cert__entry_main____str113:
    .byte 0x61, 0x72, 0x6d, 0x36, 0x34, 0x00
___cert__entry_main____str114:
    .byte 0x6d, 0x75, 0x6c, 0x74, 0x69, 0x20, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x61, 0x72, 0x67, 0x73, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x36, 0x30, 0x00
___cert__entry_main____str115:
    .byte 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x5f, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x3d, 0x20, 0x25, 0x64, 0x0a, 0x00
___cert__entry_main____str116:
    .byte 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x36, 0x36, 0x00
___cert__entry_main____str117:
    .byte 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x69, 0x73, 0x20, 0x35, 0x35, 0x35, 0x00
___cert__entry_main____str118:
    .byte 0x75, 0x73, 0x65, 0x5f, 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x28, 0x34, 0x35, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x34, 0x35, 0x00
___cert__entry_main____str119:
    .byte 0x75, 0x73, 0x65, 0x5f, 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x28, 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x5f, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x36, 0x35, 0x35, 0x00
___cert__entry_main____str120:
    .byte 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x72, 0x65, 0x6d, 0x61, 0x69, 0x6e, 0x73, 0x20, 0x35, 0x35, 0x35, 0x20, 0x61, 0x66, 0x74, 0x65, 0x72, 0x20, 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x00
___cert__entry_main____str121:
    .byte 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x5f, 0x70, 0x74, 0x72, 0x20, 0x6d, 0x6f, 0x64, 0x69, 0x66, 0x69, 0x65, 0x64, 0x20, 0x66, 0x69, 0x72, 0x73, 0x74, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x74, 0x6f, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str122:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x41, 0x6c, 0x69, 0x67, 0x6e, 0x50, 0x72, 0x6f, 0x62, 0x65, 0x29, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x68, 0x65, 0x6c, 0x70, 0x65, 0x72, 0x00
___cert__entry_main____str123:
    .byte 0x6f, 0x66, 0x66, 0x73, 0x65, 0x74, 0x6f, 0x66, 0x28, 0x41, 0x6c, 0x69, 0x67, 0x6e, 0x50, 0x72, 0x6f, 0x62, 0x65, 0x2c, 0x20, 0x2e, 0x70, 0x61, 0x79, 0x6c, 0x6f, 0x61, 0x64, 0x29, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x68, 0x65, 0x6c, 0x70, 0x65, 0x72, 0x00
___cert__entry_main____str124:
    .byte 0x6f, 0x66, 0x66, 0x73, 0x65, 0x74, 0x6f, 0x66, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x72, 0x65, 0x73, 0x70, 0x65, 0x63, 0x74, 0x73, 0x20, 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x00
___cert__entry_main____str125:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x69, 0x38, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x00
___cert__entry_main____str126:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x69, 0x31, 0x36, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x00
___cert__entry_main____str127:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x69, 0x33, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x00
___cert__entry_main____str128:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x69, 0x36, 0x34, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x38, 0x00
___cert__entry_main____str129:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x66, 0x33, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x00
___cert__entry_main____str130:
    .byte 0x61, 0x6c, 0x69, 0x67, 0x6e, 0x6f, 0x66, 0x28, 0x66, 0x36, 0x34, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x38, 0x00
___cert__entry_main____str131:
    .byte 0x64, 0x65, 0x66, 0x61, 0x75, 0x6c, 0x74, 0x5f, 0x6d, 0x61, 0x78, 0x28, 0x31, 0x30, 0x2c, 0x20, 0x32, 0x30, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x30, 0x00
___cert__entry_main____str132:
    .byte 0x64, 0x65, 0x66, 0x61, 0x75, 0x6c, 0x74, 0x5f, 0x6d, 0x61, 0x78, 0x28, 0x2d, 0x35, 0x2c, 0x20, 0x2d, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x2d, 0x32, 0x00
___cert__entry_main____str133:
    .byte 0x6f, 0x74, 0x68, 0x65, 0x72, 0x28, 0x31, 0x35, 0x75, 0x38, 0x2c, 0x20, 0x32, 0x37, 0x69, 0x31, 0x36, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str134:
    .byte 0x64, 0x6f, 0x74, 0x5f, 0x73, 0x75, 0x6d, 0x3c, 0x69, 0x31, 0x36, 0x2c, 0x75, 0x38, 0x2c, 0x69, 0x36, 0x34, 0x3e, 0x28, 0x64, 0x6f, 0x74, 0x5f, 0x6c, 0x65, 0x66, 0x74, 0x2c, 0x20, 0x64, 0x6f, 0x74, 0x5f, 0x72, 0x69, 0x67, 0x68, 0x74, 0x2c, 0x20, 0x34, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x35, 0x36, 0x30, 0x00
___cert__entry_main____str135:
    .byte 0x64, 0x6f, 0x74, 0x5f, 0x73, 0x75, 0x6d, 0x3c, 0x69, 0x38, 0x2c, 0x69, 0x38, 0x2c, 0x69, 0x33, 0x32, 0x3e, 0x28, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x5f, 0x6c, 0x65, 0x66, 0x74, 0x2c, 0x20, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x5f, 0x72, 0x69, 0x67, 0x68, 0x74, 0x2c, 0x20, 0x33, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x36, 0x00
___cert__entry_main____str136:
    .byte 0x73, 0x77, 0x69, 0x74, 0x63, 0x68, 0x20, 0x64, 0x65, 0x66, 0x61, 0x75, 0x6c, 0x74, 0x20, 0x62, 0x75, 0x63, 0x6b, 0x65, 0x74, 0x20, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x65, 0x64, 0x20, 0x77, 0x68, 0x65, 0x6e, 0x20, 0x6e, 0x6f, 0x20, 0x63, 0x61, 0x73, 0x65, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x00
___cert__entry_main____str137:
    .byte 0x73, 0x77, 0x69, 0x74, 0x63, 0x68, 0x20, 0x66, 0x61, 0x6c, 0x6c, 0x74, 0x68, 0x72, 0x6f, 0x75, 0x67, 0x68, 0x20, 0x65, 0x78, 0x65, 0x63, 0x75, 0x74, 0x65, 0x73, 0x20, 0x73, 0x75, 0x62, 0x73, 0x65, 0x71, 0x75, 0x65, 0x6e, 0x74, 0x20, 0x63, 0x61, 0x73, 0x65, 0x20, 0x77, 0x68, 0x65, 0x6e, 0x20, 0x62, 0x72, 0x65, 0x61, 0x6b, 0x20, 0x6f, 0x6d, 0x69, 0x74, 0x74, 0x65, 0x64, 0x00
___cert__entry_main____str138:
    .byte 0x6d, 0x61, 0x74, 0x63, 0x68, 0x20, 0x63, 0x68, 0x6f, 0x6f, 0x73, 0x65, 0x73, 0x20, 0x65, 0x78, 0x61, 0x63, 0x74, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x20, 0x61, 0x72, 0x6d, 0x00
___cert__entry_main____str139:
    .byte 0x6d, 0x61, 0x74, 0x63, 0x68, 0x20, 0x66, 0x61, 0x6c, 0x6c, 0x73, 0x20, 0x62, 0x61, 0x63, 0x6b, 0x20, 0x74, 0x6f, 0x20, 0x77, 0x69, 0x6c, 0x64, 0x63, 0x61, 0x72, 0x64, 0x20, 0x61, 0x72, 0x6d, 0x00
___cert__entry_main____str140:
    .byte 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x6c, 0x6f, 0x61, 0x64, 0x28, 0x66, 0x36, 0x34, 0x2c, 0x20, 0x66, 0x36, 0x34, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x36, 0x2e, 0x30, 0x00
___cert__entry_main____str141:
    .byte 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x6c, 0x6f, 0x61, 0x64, 0x28, 0x69, 0x33, 0x32, 0x2c, 0x20, 0x69, 0x33, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str142:
    .byte 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x6c, 0x6f, 0x61, 0x64, 0x28, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x54, 0x43, 0x2e, 0x53, 0x6d, 0x61, 0x6c, 0x6c, 0x54, 0x65, 0x73, 0x74, 0x29, 0x20, 0x73, 0x75, 0x6d, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x6c, 0x79, 0x00
___cert__entry_main____str143:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2b, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str144:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2d, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str145:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2a, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str146:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2f, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str147:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x25, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str148:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x3c, 0x3c, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str149:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x3e, 0x3e, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str150:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x26, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str151:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x7c, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str152:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x5e, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x00
___cert__entry_main____str153:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2b, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x33, 0x32, 0x00
___cert__entry_main____str154:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2a, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x33, 0x32, 0x00
___cert__entry_main____str155:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2f, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x33, 0x32, 0x00
___cert__entry_main____str156:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x2d, 0x3d, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x33, 0x32, 0x00
___cert__entry_main____str157:
    .byte 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x75, 0x6e, 0x64, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x33, 0x32, 0x00
___cert__entry_main____str158:
    .byte 0x73, 0x75, 0x70, 0x65, 0x72, 0x5f, 0x61, 0x72, 0x67, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x64, 0x75, 0x63, 0x74, 0x20, 0x69, 0x73, 0x20, 0x2d, 0x31, 0x38, 0x35, 0x33, 0x38, 0x38, 0x32, 0x33, 0x36, 0x38, 0x00
___cert__entry_main____str159:
    .byte 0x73, 0x75, 0x70, 0x65, 0x72, 0x5f, 0x61, 0x72, 0x67, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x64, 0x75, 0x63, 0x74, 0x20, 0x6d, 0x6f, 0x64, 0x20, 0x32, 0x30, 0x20, 0x69, 0x73, 0x20, 0x31, 0x32, 0x00
___cert__entry_main____str160:
    .byte 0x6c, 0x61, 0x6d, 0x62, 0x64, 0x61, 0x20, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x20, 0x34, 0x32, 0x00
___cert__entry_main____str161:
    .byte 0x75, 0x6e, 0x73, 0x69, 0x67, 0x6e, 0x65, 0x64, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0x20, 0x38, 0x38, 0x75, 0x20, 0x3d, 0x3d, 0x20, 0x38, 0x38, 0x20, 0x61, 0x73, 0x20, 0x75, 0x33, 0x32, 0x00
___cert__entry_main____str162:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x30, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str163:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x30, 0x20, 0x63, 0x6f, 0x6c, 0x6f, 0x72, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str164:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x31, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str165:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x31, 0x20, 0x63, 0x6f, 0x6c, 0x6f, 0x72, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str166:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x32, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str167:
    .byte 0x76, 0x65, 0x72, 0x74, 0x65, 0x78, 0x20, 0x32, 0x20, 0x63, 0x6f, 0x6c, 0x6f, 0x72, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str168:
    .byte 0x6e, 0x65, 0x77, 0x20, 0x69, 0x6e, 0x74, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str169:
    .byte 0x6e, 0x65, 0x77, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0x00
___cert__entry_main____str170:
    .byte 0x64, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x20, 0x73, 0x65, 0x74, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x20, 0x74, 0x6f, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x00
___cert__entry_main____str171:
    .byte 0x64, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x20, 0x64, 0x69, 0x64, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x73, 0x65, 0x74, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x20, 0x74, 0x6f, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x00
___cert__entry_main____str172:
    .byte 0x42, 0x75, 0x6e, 0x64, 0x6c, 0x65, 0x20, 0x54, 0x65, 0x73, 0x74, 0x73, 0x00
___cert__entry_main____str173:
    .byte 0x41, 0x6c, 0x6c, 0x20, 0x74, 0x65, 0x73, 0x74, 0x73, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x6c, 0x65, 0x74, 0x65, 0x64, 0x20, 0x73, 0x75, 0x63, 0x63, 0x65, 0x73, 0x73, 0x66, 0x75, 0x6c, 0x6c, 0x79, 0x21, 0x20, 0x25, 0x64, 0x20, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x20, 0x61, 0x73, 0x73, 0x65, 0x72, 0x74, 0x73, 0x2e, 0x00
__Test_Program_lambda_test_ptr_to_fun_0_____void_ptr_to_fun_1_____i16____str174:
    .byte 0x4c, 0x61, 0x6d, 0x62, 0x64, 0x61, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
__Test_Program_file_io_tests____str175:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_file_io_tests____str176:
    .byte 0x77, 0x72, 0x69, 0x74, 0x65, 0x5f, 0x61, 0x6c, 0x6c, 0x20, 0x77, 0x72, 0x6f, 0x74, 0x65, 0x20, 0x34, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x00
__Test_Program_file_io_tests____str177:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_file_io_tests____str178:
    .byte 0x72, 0x65, 0x61, 0x64, 0x5f, 0x61, 0x6c, 0x6c, 0x20, 0x72, 0x65, 0x61, 0x64, 0x20, 0x34, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x00
__Test_Program_file_io_tests____str179:
    .byte 0x72, 0x65, 0x61, 0x64, 0x5f, 0x61, 0x6c, 0x6c, 0x20, 0x70, 0x72, 0x65, 0x73, 0x65, 0x72, 0x76, 0x65, 0x64, 0x20, 0x62, 0x79, 0x74, 0x65, 0x20, 0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x73, 0x00
__Test_Program_file_io_tests____str180:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_file_io_tests____str181:
    .byte 0x77, 0x62, 0x00
__Test_Program_file_io_tests____str182:
    .byte 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x77, 0x72, 0x69, 0x74, 0x65, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x20, 0x68, 0x61, 0x6e, 0x64, 0x6c, 0x65, 0x00
__Test_Program_file_io_tests____str183:
    .byte 0x77, 0x72, 0x69, 0x74, 0x65, 0x20, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x00
__Test_Program_file_io_tests____str184:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_file_io_tests____str185:
    .byte 0x72, 0x62, 0x00
__Test_Program_file_io_tests____str186:
    .byte 0x6f, 0x70, 0x65, 0x6e, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x72, 0x65, 0x61, 0x64, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x65, 0x64, 0x20, 0x68, 0x61, 0x6e, 0x64, 0x6c, 0x65, 0x00
__Test_Program_file_io_tests____str187:
    .byte 0x73, 0x65, 0x65, 0x6b, 0x20, 0x74, 0x6f, 0x20, 0x65, 0x6e, 0x64, 0x20, 0x73, 0x75, 0x63, 0x63, 0x65, 0x65, 0x64, 0x65, 0x64, 0x00
__Test_Program_file_io_tests____str188:
    .byte 0x65, 0x6e, 0x64, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x00
__Test_Program_file_io_tests____str189:
    .byte 0x73, 0x65, 0x65, 0x6b, 0x20, 0x74, 0x6f, 0x20, 0x73, 0x74, 0x61, 0x72, 0x74, 0x20, 0x73, 0x75, 0x63, 0x63, 0x65, 0x65, 0x64, 0x65, 0x64, 0x00
__Test_Program_file_io_tests____str190:
    .byte 0x72, 0x65, 0x61, 0x64, 0x20, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x00
__Test_Program_file_io_tests____str191:
    .byte 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x72, 0x6f, 0x75, 0x6e, 0x64, 0x2d, 0x74, 0x72, 0x69, 0x70, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x00
__Test_Program_file_io_tests____str192:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_file_io_tests____str193:
    .byte 0x66, 0x69, 0x6c, 0x65, 0x5f, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x2e, 0x62, 0x69, 0x6e, 0x00
__Test_Program_internal_diffvarargs_i32_varargs____str194:
    .byte 0x69, 0x6e, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x5f, 0x64, 0x69, 0x66, 0x66, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x3a, 0x20, 0x66, 0x69, 0x72, 0x73, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x3d, 0x25, 0x6c, 0x66, 0x2c, 0x20, 0x74, 0x68, 0x69, 0x72, 0x64, 0x3d, 0x25, 0x73, 0x0a, 0x00
__differentsig____str195:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x73, 0x69, 0x67, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x21, 0x0a, 0x00
___lambda_0____str196:
    .byte 0x44, 0x69, 0x61, 0x67, 0x20, 0x63, 0x6f, 0x64, 0x65, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___lambda_0____str197:
    .byte 0x44, 0x69, 0x61, 0x67, 0x20, 0x75, 0x73, 0x65, 0x72, 0x64, 0x61, 0x74, 0x61, 0x3a, 0x20, 0x25, 0x70, 0x0a, 0x00
___lambda_1____str198:
    .byte 0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20, 0x61, 0x20, 0x74, 0x65, 0x73, 0x74, 0x20, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x21, 0x0a, 0x00
___lambda_2____str199:
    .byte 0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20, 0x61, 0x20, 0x74, 0x65, 0x73, 0x74, 0x20, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x61, 0x72, 0x67, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0x00
___lambda_3____str200:
    .byte 0x4c, 0x61, 0x6d, 0x62, 0x64, 0x61, 0x20, 0x74, 0x65, 0x73, 0x74, 0x20, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x41, 0x20, 0x69, 0x6e, 0x76, 0x6f, 0x6b, 0x65, 0x64, 0x21, 0x0a, 0x00
___lambda_4____str201:
    .byte 0x4c, 0x61, 0x6d, 0x62, 0x64, 0x61, 0x20, 0x74, 0x65, 0x73, 0x74, 0x20, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x42, 0x20, 0x69, 0x6e, 0x76, 0x6f, 0x6b, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x25, 0x64, 0x21, 0x0a, 0x00
