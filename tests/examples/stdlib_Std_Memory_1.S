// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _Std_Memory_memset
.extern _Std_Memory_memmove
.extern _malloc
.extern _free

.globl _Std_Memory_memcpy
.p2align 2
_Std_Memory_memcpy:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #112
    str x0, [sp, #0]
    str x1, [sp, #8]
    str w2, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
    ldr x9, [sp, #0]
    str x9, [sp, #32]
    ldr x9, [sp, #8]
    str x9, [sp, #40]
Std_Memory_memcpy__while_cond0:
    ldr w9, [sp, #24]
    ldr w10, [sp, #16]
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
    b.ne Std_Memory_memcpy__while_body1
    b Std_Memory_memcpy__while_end2
Std_Memory_memcpy__while_body1:
    ldr x9, [sp, #32]
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
    ldr x9, [sp, #40]
    str x9, [sp, #72]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #80]
    ldr x9, [sp, #72]
    ldr x10, [sp, #80]
    add x9, x9, x10
    str x9, [sp, #72]
    ldr x9, [sp, #72]
    str x9, [sp, #72]
    ldr x9, [sp, #72]
    ldrb w10, [x9]
    strb w10, [sp, #72]
    ldrb w9, [sp, #72]
    strb w9, [sp, #48]
    ldr x9, [sp, #64]
    ldrb w10, [sp, #48]
    strb w10, [x9]
    ldr w9, [sp, #24]
    str w9, [sp, #56]
    ldr w9, [sp, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #64]
    ldr w9, [sp, #64]
    str w9, [sp, #24]
    b Std_Memory_memcpy__while_cond0
Std_Memory_memcpy__while_end2:
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_memset
.p2align 2
_Std_Memory_memset:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #112
    str x0, [sp, #0]
    strb w1, [sp, #8]
    str w2, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
    ldr x9, [sp, #0]
    str x9, [sp, #32]
Std_Memory_memset__while_cond0:
    ldr w9, [sp, #24]
    ldr w10, [sp, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    cmp w9, wzr
    b.ne Std_Memory_memset__while_body1
    b Std_Memory_memset__while_end2
Std_Memory_memset__while_body1:
    ldr x9, [sp, #32]
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
    ldrb w9, [sp, #8]
    strb w9, [sp, #40]
    ldr x9, [sp, #56]
    ldrb w10, [sp, #40]
    strb w10, [x9]
    ldr w9, [sp, #24]
    str w9, [sp, #48]
    ldr w9, [sp, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #56]
    ldr w9, [sp, #56]
    str w9, [sp, #24]
    b Std_Memory_memset__while_cond0
Std_Memory_memset__while_end2:
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_memcmp
.p2align 2
_Std_Memory_memcmp:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #160
    str x0, [sp, #0]
    str x1, [sp, #8]
    str w2, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
    ldr x9, [sp, #0]
    str x9, [sp, #32]
    ldr x9, [sp, #8]
    str x9, [sp, #40]
Std_Memory_memcmp__while_cond0:
    ldr w9, [sp, #24]
    ldr w10, [sp, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #56]
    ldrb w9, [sp, #56]
    cmp w9, wzr
    b.ne Std_Memory_memcmp__while_body1
    b Std_Memory_memcmp__while_end2
Std_Memory_memcmp__while_body1:
    ldr x9, [sp, #32]
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
    ldrb w10, [x9]
    strb w10, [sp, #56]
    ldr x9, [sp, #40]
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
    ldrb w10, [x9]
    strb w10, [sp, #64]
    ldrb w9, [sp, #56]
    ldrb w10, [sp, #64]
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
    b.ne Std_Memory_memcmp__if_true3
    b Std_Memory_memcmp__if_end4
Std_Memory_memcmp__if_true3:
    ldr x9, [sp, #32]
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
    ldrb w10, [x9]
    strb w10, [sp, #56]
    ldrb w9, [sp, #56]
    uxtb w9, w9
    str w9, [sp, #56]
    ldr x9, [sp, #40]
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
    ldrb w10, [x9]
    strb w10, [sp, #64]
    ldrb w9, [sp, #64]
    uxtb w9, w9
    str w9, [sp, #64]
    ldr w9, [sp, #56]
    ldr w10, [sp, #64]
    sub w9, w9, w10
    str w9, [sp, #56]
    ldr w0, [sp, #56]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_Memory_memcmp__if_end4:
    ldr w9, [sp, #24]
    str w9, [sp, #48]
    ldr w9, [sp, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #56]
    ldr w9, [sp, #56]
    str w9, [sp, #24]
    b Std_Memory_memcmp__while_cond0
Std_Memory_memcmp__while_end2:
    mov w0, wzr
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_memmove
.p2align 2
_Std_Memory_memmove:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #224
    str x0, [sp, #0]
    str x1, [sp, #8]
    str w2, [sp, #16]
    ldr x9, [sp, #0]
    str x9, [sp, #24]
    ldr x9, [sp, #8]
    str x9, [sp, #32]
    ldr x9, [sp, #24]
    ldr x10, [sp, #32]
    cmp x9, x10
    cset w11, lo
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    cmp w9, wzr
    b.ne Std_Memory_memmove__if_true0
    b Std_Memory_memmove__if_false1
Std_Memory_memmove__if_true0:
    mov w9, wzr
    str w9, [sp, #40]
Std_Memory_memmove__while_cond3:
    ldr w9, [sp, #40]
    ldr w10, [sp, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    cmp w9, wzr
    b.ne Std_Memory_memmove__while_body4
    b Std_Memory_memmove__while_end5
Std_Memory_memmove__while_body4:
    ldr x9, [sp, #24]
    str x9, [sp, #88]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #96]
    ldr x9, [sp, #88]
    ldr x10, [sp, #96]
    add x9, x9, x10
    str x9, [sp, #88]
    ldr x9, [sp, #88]
    str x9, [sp, #88]
    ldr x9, [sp, #32]
    str x9, [sp, #96]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #104]
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    add x9, x9, x10
    str x9, [sp, #96]
    ldr x9, [sp, #96]
    str x9, [sp, #96]
    ldr x9, [sp, #96]
    ldrb w10, [x9]
    strb w10, [sp, #96]
    ldrb w9, [sp, #96]
    strb w9, [sp, #48]
    ldr x9, [sp, #88]
    ldrb w10, [sp, #48]
    strb w10, [x9]
    ldr w9, [sp, #40]
    str w9, [sp, #56]
    ldr w9, [sp, #40]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #88]
    ldr w9, [sp, #88]
    str w9, [sp, #40]
    b Std_Memory_memmove__while_cond3
Std_Memory_memmove__while_end5:
    b Std_Memory_memmove__if_end2
Std_Memory_memmove__if_false1:
    ldr x9, [sp, #24]
    ldr x10, [sp, #32]
    cmp x9, x10
    cset w11, hi
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    cmp w9, wzr
    b.ne Std_Memory_memmove__if_true6
    b Std_Memory_memmove__if_end7
Std_Memory_memmove__if_true6:
    ldr w9, [sp, #16]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [sp, #88]
    ldr w9, [sp, #88]
    str w9, [sp, #64]
Std_Memory_memmove__while_cond8:
    ldr w9, [sp, #64]
    mov w10, wzr
    cmp w9, w10
    cset w11, ge
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #88]
    ldrb w9, [sp, #88]
    cmp w9, wzr
    b.ne Std_Memory_memmove__while_body9
    b Std_Memory_memmove__while_end10
Std_Memory_memmove__while_body9:
    ldr x9, [sp, #24]
    str x9, [sp, #88]
    ldr w9, [sp, #64]
    sxtw x9, w9
    str x9, [sp, #96]
    ldr x9, [sp, #88]
    ldr x10, [sp, #96]
    add x9, x9, x10
    str x9, [sp, #88]
    ldr x9, [sp, #88]
    str x9, [sp, #88]
    ldr x9, [sp, #32]
    str x9, [sp, #96]
    ldr w9, [sp, #64]
    sxtw x9, w9
    str x9, [sp, #104]
    ldr x9, [sp, #96]
    ldr x10, [sp, #104]
    add x9, x9, x10
    str x9, [sp, #96]
    ldr x9, [sp, #96]
    str x9, [sp, #96]
    ldr x9, [sp, #96]
    ldrb w10, [x9]
    strb w10, [sp, #96]
    ldrb w9, [sp, #96]
    strb w9, [sp, #72]
    ldr x9, [sp, #88]
    ldrb w10, [sp, #72]
    strb w10, [x9]
    ldr w9, [sp, #64]
    str w9, [sp, #80]
    ldr w9, [sp, #64]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [sp, #88]
    ldr w9, [sp, #88]
    str w9, [sp, #64]
    b Std_Memory_memmove__while_cond8
Std_Memory_memmove__while_end10:
Std_Memory_memmove__if_end7:
Std_Memory_memmove__if_end2:
    ldr x0, [sp, #0]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_memchr
.p2align 2
_Std_Memory_memchr:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #128
    str x0, [sp, #0]
    strb w1, [sp, #8]
    str w2, [sp, #16]
    mov w9, wzr
    str w9, [sp, #24]
    ldr x9, [sp, #0]
    str x9, [sp, #32]
Std_Memory_memchr__while_cond0:
    ldr w9, [sp, #24]
    ldr w10, [sp, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_Memory_memchr__while_body1
    b Std_Memory_memchr__while_end2
Std_Memory_memchr__while_body1:
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    ldrb w10, [x9]
    strb w10, [sp, #48]
    ldrb w9, [sp, #48]
    ldrb w10, [sp, #8]
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_Memory_memchr__if_true3
    b Std_Memory_memchr__if_end4
Std_Memory_memchr__if_true3:
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_Memory_memchr__if_end4:
    ldr w9, [sp, #24]
    str w9, [sp, #40]
    ldr w9, [sp, #24]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    str w9, [sp, #24]
    b Std_Memory_memchr__while_cond0
Std_Memory_memchr__while_end2:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #48]
    ldr x0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_memrchr
.p2align 2
_Std_Memory_memrchr:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #128
    str x0, [sp, #0]
    strb w1, [sp, #8]
    str w2, [sp, #16]
    ldr w9, [sp, #16]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    str w9, [sp, #24]
    ldr x9, [sp, #0]
    str x9, [sp, #32]
Std_Memory_memrchr__while_cond0:
    ldr w9, [sp, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, ge
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_Memory_memrchr__while_body1
    b Std_Memory_memrchr__while_end2
Std_Memory_memrchr__while_body1:
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    ldrb w10, [x9]
    strb w10, [sp, #48]
    ldrb w9, [sp, #48]
    ldrb w10, [sp, #8]
    cmp w9, w10
    cset w11, eq
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #48]
    ldrb w9, [sp, #48]
    cmp w9, wzr
    b.ne Std_Memory_memrchr__if_true3
    b Std_Memory_memrchr__if_end4
Std_Memory_memrchr__if_true3:
    ldr x9, [sp, #32]
    str x9, [sp, #48]
    ldr w9, [sp, #24]
    sxtw x9, w9
    str x9, [sp, #56]
    ldr x9, [sp, #48]
    ldr x10, [sp, #56]
    add x9, x9, x10
    str x9, [sp, #48]
    ldr x9, [sp, #48]
    str x9, [sp, #48]
    ldr x0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_Memory_memrchr__if_end4:
    ldr w9, [sp, #24]
    str w9, [sp, #40]
    ldr w9, [sp, #24]
    movz w10, #0x1
    sub w9, w9, w10
    str w9, [sp, #48]
    ldr w9, [sp, #48]
    str w9, [sp, #24]
    b Std_Memory_memrchr__while_cond0
Std_Memory_memrchr__while_end2:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #48]
    ldr x0, [sp, #48]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_bzero
.p2align 2
_Std_Memory_bzero:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    str x0, [sp, #0]
    str w1, [sp, #8]
    ldr x0, [sp, #0]
    mov w1, wzr
    ldr w2, [sp, #8]
    bl _Std_Memory_memset
    str x0, [sp, #16]
    ldr x0, [sp, #16]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_bcopy
.p2align 2
_Std_Memory_bcopy:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #64
    str x0, [sp, #0]
    str x1, [sp, #8]
    str w2, [sp, #16]
    ldr x0, [sp, #8]
    ldr x1, [sp, #0]
    ldr w2, [sp, #16]
    bl _Std_Memory_memmove
    str x0, [sp, #24]
    ldr x0, [sp, #24]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_alloc
.p2align 2
_Std_Memory_alloc:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    str w0, [sp, #0]
    ldr w0, [sp, #0]
    bl _malloc
    str x0, [sp, #8]
    ldr x0, [sp, #8]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_dealloc
.p2align 2
_Std_Memory_dealloc:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #32
    str x0, [sp, #0]
    ldr x0, [sp, #0]
    bl _free
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_realloc
.p2align 2
_Std_Memory_realloc:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #80
    str x0, [sp, #0]
    str w1, [sp, #8]
    ldr w0, [sp, #8]
    bl _malloc
    str x0, [sp, #24]
    ldr x9, [sp, #24]
    str x9, [sp, #16]
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #32]
    ldr x9, [sp, #16]
    ldr x10, [sp, #32]
    cmp x9, x10
    cset w11, eq
    strb w11, [sp, #24]
    ldrb w9, [sp, #24]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #24]
    ldrb w9, [sp, #24]
    cmp w9, wzr
    b.ne Std_Memory_realloc__if_true0
    b Std_Memory_realloc__if_end1
Std_Memory_realloc__if_true0:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #24]
    ldr x0, [sp, #24]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_Memory_realloc__if_end1:
    ldr x0, [sp, #16]
    ldr x1, [sp, #0]
    ldr w2, [sp, #8]
    bl _Std_Memory_memmove
    str x0, [sp, #24]
    ldr x0, [sp, #0]
    bl _free
    ldr x0, [sp, #16]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_cleanalloc
.p2align 2
_Std_Memory_cleanalloc:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #80
    str w0, [sp, #0]
    str w1, [sp, #8]
    ldr w9, [sp, #0]
    ldr w10, [sp, #8]
    mul w9, w9, w10
    str w9, [sp, #32]
    ldr w9, [sp, #32]
    str w9, [sp, #16]
    ldr w0, [sp, #16]
    bl _malloc
    str x0, [sp, #32]
    ldr x9, [sp, #32]
    str x9, [sp, #24]
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #40]
    ldr x9, [sp, #24]
    ldr x10, [sp, #40]
    cmp x9, x10
    cset w11, eq
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #32]
    ldrb w9, [sp, #32]
    cmp w9, wzr
    b.ne Std_Memory_cleanalloc__if_true0
    b Std_Memory_cleanalloc__if_end1
Std_Memory_cleanalloc__if_true0:
    mov w9, wzr
    uxtw x9, w9
    str x9, [sp, #32]
    ldr x0, [sp, #32]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret
Std_Memory_cleanalloc__if_end1:
    ldr x0, [sp, #24]
    mov w1, wzr
    ldr w2, [sp, #16]
    bl _Std_Memory_memset
    str x0, [sp, #32]
    ldr x0, [sp, #24]
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

.globl _Std_Memory_swap
.p2align 2
_Std_Memory_swap:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #144
    str x0, [sp, #0]
    str x1, [sp, #8]
    str w2, [sp, #16]
    ldr x9, [sp, #0]
    str x9, [sp, #24]
    ldr x9, [sp, #8]
    str x9, [sp, #32]
    mov w9, wzr
    str w9, [sp, #40]
Std_Memory_swap__while_cond0:
    ldr w9, [sp, #40]
    ldr w10, [sp, #16]
    cmp w9, w10
    cset w11, lt
    strb w11, [sp, #80]
    ldrb w9, [sp, #80]
    mov w10, wzr
    cmp w9, w10
    cset w11, ne
    strb w11, [sp, #80]
    ldrb w9, [sp, #80]
    cmp w9, wzr
    b.ne Std_Memory_swap__while_body1
    b Std_Memory_swap__while_end2
Std_Memory_swap__while_body1:
    ldr x9, [sp, #24]
    str x9, [sp, #80]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #88]
    ldr x9, [sp, #80]
    ldr x10, [sp, #88]
    add x9, x9, x10
    str x9, [sp, #80]
    ldr x9, [sp, #80]
    str x9, [sp, #80]
    ldr x9, [sp, #80]
    ldrb w10, [x9]
    strb w10, [sp, #80]
    ldrb w9, [sp, #80]
    strb w9, [sp, #48]
    ldr x9, [sp, #24]
    str x9, [sp, #80]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #88]
    ldr x9, [sp, #80]
    ldr x10, [sp, #88]
    add x9, x9, x10
    str x9, [sp, #80]
    ldr x9, [sp, #80]
    str x9, [sp, #80]
    ldr x9, [sp, #32]
    str x9, [sp, #88]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #96]
    ldr x9, [sp, #88]
    ldr x10, [sp, #96]
    add x9, x9, x10
    str x9, [sp, #88]
    ldr x9, [sp, #88]
    str x9, [sp, #88]
    ldr x9, [sp, #88]
    ldrb w10, [x9]
    strb w10, [sp, #88]
    ldrb w9, [sp, #88]
    strb w9, [sp, #56]
    ldr x9, [sp, #80]
    ldrb w10, [sp, #56]
    strb w10, [x9]
    ldr x9, [sp, #32]
    str x9, [sp, #80]
    ldr w9, [sp, #40]
    sxtw x9, w9
    str x9, [sp, #88]
    ldr x9, [sp, #80]
    ldr x10, [sp, #88]
    add x9, x9, x10
    str x9, [sp, #80]
    ldr x9, [sp, #80]
    str x9, [sp, #80]
    ldrb w9, [sp, #48]
    strb w9, [sp, #64]
    ldr x9, [sp, #80]
    ldrb w10, [sp, #64]
    strb w10, [x9]
    ldr w9, [sp, #40]
    str w9, [sp, #72]
    ldr w9, [sp, #40]
    movz w10, #0x1
    add w9, w9, w10
    str w9, [sp, #80]
    ldr w9, [sp, #80]
    str w9, [sp, #40]
    b Std_Memory_swap__while_cond0
Std_Memory_swap__while_end2:
    mov sp, x29
    ldp x29, x30, [sp], #16
    ret

