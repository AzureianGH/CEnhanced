// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _Std_String_strlen

.globl _Std_String_strcmp
.p2align 2
_Std_String_strcmp:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #224
    str x0, [sp, #0]
    str x1, [sp, #8]
    mov w9, wzr
    str w9, [sp, #16]
    ldr x9, [sp, #0]
    str x9, [sp, #24]
    ldr x9, [sp, #8]
    str x9, [sp, #32]
Std_String_strcmp__while_cond0:
    ldr x9, [sp, #24]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_String_strcmp__land_rhs3
    b Std_String_strcmp__land_false4
Std_String_strcmp__land_rhs3:
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_String_strcmp__land_true5
    b Std_String_strcmp__land_false4
Std_String_strcmp__land_true5:
    movz w11, #0x1
    str w11, [sp, #48]
    b Std_String_strcmp__land_end6
Std_String_strcmp__land_false4:
Std_String_strcmp__land_end6:
    ldr w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_String_strcmp__while_body1
    b Std_String_strcmp__while_end2
Std_String_strcmp__while_body1:
    ldr x9, [sp, #24]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr x9, [sp, #32]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #56]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #48]
    ldr w10, [sp, #56]
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_String_strcmp__if_true7
    b Std_String_strcmp__if_end8
Std_String_strcmp__if_true7:
    ldr x9, [sp, #24]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr x9, [sp, #32]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #56]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #48]
    ldr w10, [sp, #56]
    sub w9, w9, w10
    str w9, [sp, #48]
    ldr w0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_String_strcmp__if_end8:
    ldr w9, [sp, #16]
    str w9, [sp, #40]
    ldr w9, [sp, #16]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    str w9, [sp, #16]
    b Std_String_strcmp__while_cond0
Std_String_strcmp__while_end2:
    ldr x9, [sp, #24]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr x9, [sp, #32]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #56]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #48]
    ldr w10, [sp, #56]
    sub w9, w9, w10
    str w9, [sp, #48]
    ldr w0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_String_strlen
.p2align 2
_Std_String_strlen:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #80
    str x0, [sp, #0]
    mov w9, wzr
    str w9, [sp, #8]
Std_String_strlen__while_cond0:
    ldr x9, [sp, #0]
    str x9, [sp, #24]
    ldr w9, [sp, #8]
    sxtw x9, w9
    str x9, [sp, #32]
    ldr x9, [sp, #24]
    ldr x10, [sp, #32]
    add x9, x9, x10
    str x9, [sp, #24]
    ldr x9, [sp, #24]
    str x9, [sp, #24]
    ldr x9, [sp, #24]
    ldrsb w10, [x9]
    strb w10, [sp, #24]
    ldrsb w9, [sp, #24]
    sxtb w9, w9
    str w9, [sp, #24]
    ldr w9, [sp, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #24]
    ldrb w9, [sp, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #24]
    ldrb w9, [sp, #24]
    cmp w9, wzr
    b.ne Std_String_strlen__while_body1
    b Std_String_strlen__while_end2
Std_String_strlen__while_body1:
    ldr w9, [sp, #8]
    str w9, [sp, #16]
    ldr w9, [sp, #8]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #24]
    ldr w9, [sp, #24]
    str w9, [sp, #8]
    b Std_String_strlen__while_cond0
Std_String_strlen__while_end2:
    ldr w0, [sp, #8]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_String_strcpy
.p2align 2
_Std_String_strcpy:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #112
    str x0, [sp, #0]
    str x1, [sp, #8]
    mov w9, wzr
    str w9, [sp, #16]
Std_String_strcpy__while_cond0:
    ldr x9, [sp, #8]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_String_strcpy__while_body1
    b Std_String_strcpy__while_end2
Std_String_strcpy__while_body1:
    ldr x9, [sp, #0]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x9, [sp, #8]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
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
    ldrsb w9, [sp, #56]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #56]
    uxtb w9, w9
    strb w9, [sp, #56]
    ldrsb w9, [sp, #56]
    strb w9, [sp, #24]
    ldr x9, [sp, #48]
    ldrsb w10, [sp, #24]
    strb w10, [x9]
    ldrsb w9, [sp, #24]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr w9, [sp, #16]
    str w9, [sp, #32]
    ldr w9, [sp, #16]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    str w9, [sp, #16]
    b Std_String_strcpy__while_cond0
Std_String_strcpy__while_end2:
    ldr x9, [sp, #0]
    str x9, [sp, #48]
    ldr w9, [sp, #16]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    mov w9, wzr
    uxtb w9, w9
    strb w9, [sp, #56]
    ldrsb w9, [sp, #56]
    strb w9, [sp, #40]
    ldr x9, [sp, #48]
    ldrsb w10, [sp, #40]
    strb w10, [x9]
    ldrsb w9, [sp, #40]
    sxtb w9, w9
    str w9, [sp, #48]
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_String_strcat
.p2align 2
_Std_String_strcat:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #128
    str x0, [sp, #0]
    str x1, [sp, #8]
    ldr x0, [sp, #0]
    bl _Std_String_strlen
    str w0, [sp, #56]
    ldr w9, [sp, #56]
    str w9, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
Std_String_strcat__while_cond0:
    ldr x9, [sp, #8]
    str x9, [sp, #56]
    ldr w9, [sp, #24]
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
    ldrsb w9, [sp, #56]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #56]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    cmp w9, wzr
    b.ne Std_String_strcat__while_body1
    b Std_String_strcat__while_end2
Std_String_strcat__while_body1:
    ldr x9, [sp, #0]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
    ldr w10, [sp, #24]
    add w9, w9, w10
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    sxtw x9, w9
    str x9, [sp, #64]
    ldr x9, [sp, #56]
    ldr x10, [sp, #64]
    add x9, x9, x10
    str x9, [sp, #56]
    ldr x9, [sp, #56]
    str x9, [sp, #56]
    ldr x9, [sp, #8]
    str x9, [sp, #64]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #72]
    ldr x9, [sp, #64]
    ldr x10, [sp, #72]
    add x9, x9, x10
    str x9, [sp, #64]
    ldr x9, [sp, #64]
    str x9, [sp, #64]
    ldr x9, [sp, #64]
    ldrsb w10, [x9]
    strb w10, [sp, #64]
    ldrsb w9, [sp, #64]
    sxtb w9, w9
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    uxtb w9, w9
    strb w9, [sp, #64]
    ldrsb w9, [sp, #64]
    strb w9, [sp, #32]
    ldr x9, [sp, #56]
    ldrsb w10, [sp, #32]
    strb w10, [x9]
    ldrsb w9, [sp, #32]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr w9, [sp, #24]
    str w9, [sp, #40]
    ldr w9, [sp, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #56]
    ldr w9, [sp, #56]
    str w9, [sp, #24]
    b Std_String_strcat__while_cond0
Std_String_strcat__while_end2:
    ldr x9, [sp, #0]
    str x9, [sp, #56]
    ldr w9, [sp, #16]
    ldr w10, [sp, #24]
    add w9, w9, w10
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    sxtw x9, w9
    str x9, [sp, #64]
    ldr x9, [sp, #56]
    ldr x10, [sp, #64]
    add x9, x9, x10
    str x9, [sp, #56]
    ldr x9, [sp, #56]
    str x9, [sp, #56]
    mov w9, wzr
    uxtb w9, w9
    strb w9, [sp, #64]
    ldrsb w9, [sp, #64]
    strb w9, [sp, #48]
    ldr x9, [sp, #56]
    ldrsb w10, [sp, #48]
    strb w10, [x9]
    ldrsb w9, [sp, #48]
    sxtb w9, w9
    str w9, [sp, #56]
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_String_strchr
.p2align 2
_Std_String_strchr:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #112
    str x0, [sp, #0]
    strb w1, [sp, #8]
    mov w9, wzr
    str w9, [sp, #16]
Std_String_strchr__while_cond0:
    ldr x9, [sp, #0]
    str x9, [sp, #32]
    ldr w9, [sp, #16]
    sxtw x9, w9
    str x9, [sp, #40]
    ldr x9, [sp, #32]
    ldr x10, [sp, #40]
    add x9, x9, x10
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    ldrsb w10, [x9]
    strb w10, [sp, #32]
    ldrsb w9, [sp, #32]
    sxtb w9, w9
    str w9, [sp, #32]
    ldr w9, [sp, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    cmp w9, wzr
    b.ne Std_String_strchr__while_body1
    b Std_String_strchr__while_end2
Std_String_strchr__while_body1:
    ldr x9, [sp, #0]
    str x9, [sp, #32]
    ldr w9, [sp, #16]
    sxtw x9, w9
    str x9, [sp, #40]
    ldr x9, [sp, #32]
    ldr x10, [sp, #40]
    add x9, x9, x10
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    ldrsb w10, [x9]
    strb w10, [sp, #32]
    ldrsb w9, [sp, #32]
    sxtb w9, w9
    str w9, [sp, #32]
    ldrsb w9, [sp, #8]
    sxtb w9, w9
    str w9, [sp, #40]
    ldr w9, [sp, #32]
    ldr w10, [sp, #40]
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    cmp w9, wzr
    b.ne Std_String_strchr__if_true3
    b Std_String_strchr__if_end4
Std_String_strchr__if_true3:
    ldr x9, [sp, #0]
    str x9, [sp, #32]
    ldr w9, [sp, #16]
    sxtw x9, w9
    str x9, [sp, #40]
    ldr x9, [sp, #32]
    ldr x10, [sp, #40]
    add x9, x9, x10
    str x9, [sp, #32]
    ldr x9, [sp, #32]
    str x9, [sp, #32]
    ldr x0, [sp, #32]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_String_strchr__if_end4:
    ldr w9, [sp, #16]
    str w9, [sp, #24]
    ldr w9, [sp, #16]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #32]
    ldr w9, [sp, #32]
    str w9, [sp, #16]
    b Std_String_strchr__while_cond0
Std_String_strchr__while_end2:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #32]
    ldr x0, [sp, #32]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_String_strstr
.p2align 2
_Std_String_strstr:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #240
    str x0, [sp, #0]
    str x1, [sp, #8]
    ldr x0, [sp, #0]
    bl _Std_String_strlen
    str w0, [sp, #64]
    ldr w9, [sp, #64]
    str w9, [sp, #16]
    ldr x0, [sp, #8]
    bl _Std_String_strlen
    str w0, [sp, #64]
    ldr w9, [sp, #64]
    str w9, [sp, #24]
    ldr w9, [sp, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__if_true0
    b Std_String_strstr__if_end1
Std_String_strstr__if_true0:
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_String_strstr__if_end1:
    mov w9, wzr
    str w9, [sp, #32]
Std_String_strstr__while_cond2:
    ldr w9, [sp, #16]
    ldr w10, [sp, #24]
    sub w9, w9, w10
    str w9, [sp, #72]
    ldr w9, [sp, #32]
    ldr w10, [sp, #72]
    cmp w9, w10
    cset w11, le
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__while_body3
    b Std_String_strstr__while_end4
Std_String_strstr__while_body3:
    mov w9, wzr
    str w9, [sp, #40]
Std_String_strstr__while_cond5:
    ldr w9, [sp, #40]
    ldr w10, [sp, #24]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__land_rhs8
    b Std_String_strstr__land_false9
Std_String_strstr__land_rhs8:
    ldr x9, [sp, #0]
    str x9, [sp, #64]
    ldr w9, [sp, #32]
    ldr w10, [sp, #40]
    add w9, w9, w10
    str w9, [sp, #72]
    ldr w9, [sp, #72]
    sxtw x9, w9
    str x9, [sp, #72]
    ldr x9, [sp, #64]
    ldr x10, [sp, #72]
    add x9, x9, x10
    str x9, [sp, #64]
    ldr x9, [sp, #64]
    str x9, [sp, #64]
    ldr x9, [sp, #64]
    ldrsb w10, [x9]
    strb w10, [sp, #64]
    ldrsb w9, [sp, #64]
    sxtb w9, w9
    str w9, [sp, #64]
    ldr x9, [sp, #8]
    str x9, [sp, #72]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #80]
    ldr x9, [sp, #72]
    ldr x10, [sp, #80]
    add x9, x9, x10
    str x9, [sp, #72]
    ldr x9, [sp, #72]
    str x9, [sp, #72]
    ldr x9, [sp, #72]
    ldrsb w10, [x9]
    strb w10, [sp, #72]
    ldrsb w9, [sp, #72]
    sxtb w9, w9
    str w9, [sp, #72]
    ldr w9, [sp, #64]
    ldr w10, [sp, #72]
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__land_true10
    b Std_String_strstr__land_false9
Std_String_strstr__land_true10:
    movz w11, #0x1
    str w11, [sp, #64]
    b Std_String_strstr__land_end11
Std_String_strstr__land_false9:
Std_String_strstr__land_end11:
    ldr w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__while_body6
    b Std_String_strstr__while_end7
Std_String_strstr__while_body6:
    ldr w9, [sp, #40]
    str w9, [sp, #48]
    ldr w9, [sp, #40]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    str w9, [sp, #40]
    b Std_String_strstr__while_cond5
Std_String_strstr__while_end7:
    ldr w9, [sp, #40]
    ldr w10, [sp, #24]
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #64]
    ldrb w9, [sp, #64]
    cmp w9, wzr
    b.ne Std_String_strstr__if_true12
    b Std_String_strstr__if_end13
Std_String_strstr__if_true12:
    ldr x9, [sp, #0]
    str x9, [sp, #64]
    ldr w9, [sp, #32]
    sxtw x9, w9
    str x9, [sp, #72]
    ldr x9, [sp, #64]
    ldr x10, [sp, #72]
    add x9, x9, x10
    str x9, [sp, #64]
    ldr x9, [sp, #64]
    str x9, [sp, #64]
    ldr x0, [sp, #64]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_String_strstr__if_end13:
    ldr w9, [sp, #32]
    str w9, [sp, #56]
    ldr w9, [sp, #32]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    str w9, [sp, #32]
    b Std_String_strstr__while_cond2
Std_String_strstr__while_end4:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #64]
    ldr x0, [sp, #64]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

