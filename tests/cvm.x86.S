// ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern strcmp
.extern __cert__runtime_error_ex
.extern __cert__malloc

.section __TEXT,__const
.balign 4
__cert__OBJ_TAG_I64:
    .long 0x00000001

.section __TEXT,__const
.balign 4
__cert__OBJ_TAG_F64:
    .long 0x00000002

.section __TEXT,__const
.balign 4
__cert__OBJ_TAG_PTR:
    .long 0x00000003

.section __TEXT,__const
.balign 4
__cert__ERR_INVALID_CAST_NULL:
    .long 0x000003e9

.section __TEXT,__const
.balign 4
__cert__ERR_INVALID_CAST_TYPE:
    .long 0x000003ea

.section __TEXT,__const
.balign 4
__cert__ERR_INVALID_CAST_KIND:
    .long 0x000003eb

.section __TEXT,__const
.balign 4
__cert__ERR_NULL_REFERENCE:
    .long 0x7eadc0de

.data
.balign 8
__cert__unbox_ptr_fallback:
    .space 1

.text

.globl __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
__cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov dword ptr [rbp-8], edi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    mov qword ptr [rbp-32], rcx
    mov qword ptr [rbp-40], r8
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    lea rax, [rip + __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char____str0]
    mov r11, rax
    mov rax, qword ptr [rbp-16]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-24]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-32]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    push r11
    push r10
    push rax
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    mov rax, qword ptr [rsp + 0]
    mov r9, rax
    call __cert__runtime_error_ex
    add rsp, 48
    xor eax, eax
    leave
    ret

.globl __cert__box_i64
__cert__box_i64:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, 40
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call __cert__malloc
    add rsp, 16
    mov qword ptr [rbp-24], rax
    mov qword ptr [rbp-32], rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_I64]
    mov dword ptr [rbp-36], eax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-36]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 16
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 24
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0000000000000000
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 32
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    xor rax, rax
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    leave
    ret

.globl __cert__box_f64
__cert__box_f64:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    movsd qword ptr [rbp-8], xmm0
    mov qword ptr [rbp-16], rdi
    mov rax, 40
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call __cert__malloc
    add rsp, 16
    mov qword ptr [rbp-24], rax
    mov qword ptr [rbp-32], rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_F64]
    mov dword ptr [rbp-36], eax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-36]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 16
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 24
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 32
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    xor rax, rax
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    leave
    ret

.globl __cert__box_ptr
__cert__box_ptr:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, 40
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call __cert__malloc
    add rsp, 16
    mov qword ptr [rbp-24], rax
    mov qword ptr [rbp-32], rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_PTR]
    mov dword ptr [rbp-36], eax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-36]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 16
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 24
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0000000000000000
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 32
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-24]
    leave
    ret

.globl __cert__unbox_i64
__cert__unbox_i64:
    push rbp
    mov rbp, rsp
    sub rsp, 192
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    mov qword ptr [rbp-32], rcx
    mov qword ptr [rbp-40], r8
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_0
    jmp __cc_label_f4_1
__cc_label_f4_0:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_NULL]
    mov dword ptr [rbp-44], eax
    lea rax, [rip + __cert__unbox_i64____str1]
    mov qword ptr [rbp-56], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-64], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-72], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-80], rax
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    mov rax, qword ptr [rbp-56]
    mov r11, rax
    mov rax, qword ptr [rbp-64]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-72]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-80]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0
    movsxd rax, eax
    leave
    ret
__cc_label_f4_1:
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-88], rax
    mov rax, qword ptr [rbp-88]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_I64]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_2
    jmp __cc_label_f4_3
__cc_label_f4_2:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_KIND]
    mov dword ptr [rbp-92], eax
    lea rax, [rip + __cert__unbox_i64____str2]
    mov qword ptr [rbp-104], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-112], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-120], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-128], rax
    movsxd rax, dword ptr [rbp-92]
    mov r10, rax
    mov rax, qword ptr [rbp-104]
    mov r11, rax
    mov rax, qword ptr [rbp-112]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-120]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-128]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0
    movsxd rax, eax
    leave
    ret
__cc_label_f4_3:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_4
    jmp __cc_label_f4_5
__cc_label_f4_4:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_6
    jmp __cc_label_f4_5
__cc_label_f4_6:
    mov rax, 1
    jmp __cc_label_f4_7
__cc_label_f4_5:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f4_7
__cc_label_f4_7:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_8
    jmp __cc_label_f4_9
__cc_label_f4_8:
    mov rax, qword ptr [rbp-88]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov qword ptr [rbp-136], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-144], rax
    mov rax, qword ptr [rbp-136]
    mov r11, rax
    mov rax, qword ptr [rbp-144]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_10
    jmp __cc_label_f4_9
__cc_label_f4_10:
    mov rax, 1
    jmp __cc_label_f4_11
__cc_label_f4_9:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f4_11
__cc_label_f4_11:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f4_12
    jmp __cc_label_f4_13
__cc_label_f4_12:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_TYPE]
    mov dword ptr [rbp-148], eax
    lea rax, [rip + __cert__unbox_i64____str3]
    mov qword ptr [rbp-160], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-168], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-176], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-184], rax
    movsxd rax, dword ptr [rbp-148]
    mov r11, rax
    mov rax, qword ptr [rbp-160]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-168]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-176]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-184]
    push r11
    push r10
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0
    movsxd rax, eax
    add rsp, 16
    leave
    ret
__cc_label_f4_13:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 16
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    leave
    ret

.globl __cert__unbox_f64
__cert__unbox_f64:
    push rbp
    mov rbp, rsp
    sub rsp, 192
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    mov qword ptr [rbp-32], rcx
    mov qword ptr [rbp-40], r8
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_0
    jmp __cc_label_f5_1
__cc_label_f5_0:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_NULL]
    mov dword ptr [rbp-44], eax
    lea rax, [rip + __cert__unbox_f64____str4]
    mov qword ptr [rbp-56], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-64], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-72], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-80], rax
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    mov rax, qword ptr [rbp-56]
    mov r11, rax
    mov rax, qword ptr [rbp-64]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-72]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-80]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0x0000000000000000
    leave
    ret
__cc_label_f5_1:
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-88], rax
    mov rax, qword ptr [rbp-88]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_F64]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_2
    jmp __cc_label_f5_3
__cc_label_f5_2:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_KIND]
    mov dword ptr [rbp-92], eax
    lea rax, [rip + __cert__unbox_f64____str5]
    mov qword ptr [rbp-104], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-112], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-120], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-128], rax
    movsxd rax, dword ptr [rbp-92]
    mov r10, rax
    mov rax, qword ptr [rbp-104]
    mov r11, rax
    mov rax, qword ptr [rbp-112]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-120]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-128]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0x0000000000000000
    leave
    ret
__cc_label_f5_3:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_4
    jmp __cc_label_f5_5
__cc_label_f5_4:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_6
    jmp __cc_label_f5_5
__cc_label_f5_6:
    mov rax, 1
    jmp __cc_label_f5_7
__cc_label_f5_5:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f5_7
__cc_label_f5_7:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_8
    jmp __cc_label_f5_9
__cc_label_f5_8:
    mov rax, qword ptr [rbp-88]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov qword ptr [rbp-136], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-144], rax
    mov rax, qword ptr [rbp-136]
    mov r11, rax
    mov rax, qword ptr [rbp-144]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_10
    jmp __cc_label_f5_9
__cc_label_f5_10:
    mov rax, 1
    jmp __cc_label_f5_11
__cc_label_f5_9:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f5_11
__cc_label_f5_11:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f5_12
    jmp __cc_label_f5_13
__cc_label_f5_12:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_TYPE]
    mov dword ptr [rbp-148], eax
    lea rax, [rip + __cert__unbox_f64____str6]
    mov qword ptr [rbp-160], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-168], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-176], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-184], rax
    movsxd rax, dword ptr [rbp-148]
    mov r11, rax
    mov rax, qword ptr [rbp-160]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-168]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-176]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-184]
    push r11
    push r10
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    mov rax, 0x0000000000000000
    add rsp, 16
    leave
    ret
__cc_label_f5_13:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 24
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    leave
    ret

.globl __cert__unbox_ptr
__cert__unbox_ptr:
    push rbp
    mov rbp, rsp
    sub rsp, 192
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    mov qword ptr [rbp-32], rcx
    mov qword ptr [rbp-40], r8
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_0
    jmp __cc_label_f6_1
__cc_label_f6_0:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_NULL]
    mov dword ptr [rbp-44], eax
    lea rax, [rip + __cert__unbox_ptr____str7]
    mov qword ptr [rbp-56], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-64], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-72], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-80], rax
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    mov rax, qword ptr [rbp-56]
    mov r11, rax
    mov rax, qword ptr [rbp-64]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-72]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-80]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    lea rax, [rip + __cert__unbox_ptr_fallback]
    leave
    ret
__cc_label_f6_1:
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-88], rax
    mov rax, qword ptr [rbp-88]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    movsxd rax, dword ptr [rip + __cert__OBJ_TAG_PTR]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_2
    jmp __cc_label_f6_3
__cc_label_f6_2:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_KIND]
    mov dword ptr [rbp-92], eax
    lea rax, [rip + __cert__unbox_ptr____str8]
    mov qword ptr [rbp-104], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-112], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-120], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-128], rax
    movsxd rax, dword ptr [rbp-92]
    mov r10, rax
    mov rax, qword ptr [rbp-104]
    mov r11, rax
    mov rax, qword ptr [rbp-112]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-120]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-128]
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    lea rax, [rip + __cert__unbox_ptr_fallback]
    leave
    ret
__cc_label_f6_3:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_4
    jmp __cc_label_f6_5
__cc_label_f6_4:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_6
    jmp __cc_label_f6_5
__cc_label_f6_6:
    mov rax, 1
    jmp __cc_label_f6_7
__cc_label_f6_5:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f6_7
__cc_label_f6_7:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_8
    jmp __cc_label_f6_9
__cc_label_f6_8:
    mov rax, qword ptr [rbp-88]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov qword ptr [rbp-136], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-144], rax
    mov rax, qword ptr [rbp-136]
    mov r11, rax
    mov rax, qword ptr [rbp-144]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_10
    jmp __cc_label_f6_9
__cc_label_f6_10:
    mov rax, 1
    jmp __cc_label_f6_11
__cc_label_f6_9:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f6_11
__cc_label_f6_11:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f6_12
    jmp __cc_label_f6_13
__cc_label_f6_12:
    movsxd rax, dword ptr [rip + __cert__ERR_INVALID_CAST_TYPE]
    mov dword ptr [rbp-148], eax
    lea rax, [rip + __cert__unbox_ptr____str9]
    mov qword ptr [rbp-160], rax
    mov rax, qword ptr [rbp-24]
    mov qword ptr [rbp-168], rax
    mov rax, qword ptr [rbp-32]
    mov qword ptr [rbp-176], rax
    mov rax, qword ptr [rbp-40]
    mov qword ptr [rbp-184], rax
    movsxd rax, dword ptr [rbp-148]
    mov r11, rax
    mov rax, qword ptr [rbp-160]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-168]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-176]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-184]
    push r11
    push r10
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    call __cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char
    add rsp, 48
    lea rax, [rip + __cert__unbox_ptr_fallback]
    add rsp, 16
    leave
    ret
__cc_label_f6_13:
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 32
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    leave
    ret

.globl __cert__object_is_type
__cert__object_is_type:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f7_0
    jmp __cc_label_f7_1
__cc_label_f7_1:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f7_0
    jmp __cc_label_f7_2
__cc_label_f7_0:
    mov rax, 1
    jmp __cc_label_f7_3
__cc_label_f7_2:
    mov r10, rax
    mov rax, 0
    jmp __cc_label_f7_3
__cc_label_f7_3:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f7_4
    jmp __cc_label_f7_5
__cc_label_f7_4:
    mov rax, 0
    push r10
    add rsp, 8
    leave
    ret
__cc_label_f7_5:
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-24], rax
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f7_6
    jmp __cc_label_f7_7
__cc_label_f7_6:
    mov rax, 0
    leave
    ret
__cc_label_f7_7:
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov qword ptr [rbp-32], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-40], rax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    push r10
    push rax
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call strcmp
    add rsp, 16
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne __cc_label_f7_8
    jmp __cc_label_f7_9
__cc_label_f7_8:
    mov rax, 1
    leave
    ret
__cc_label_f7_9:
    mov rax, 0
    leave
    ret

.globl __cert__null_deref
__cert__null_deref:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    movsxd rax, dword ptr [rip + __cert__ERR_NULL_REFERENCE]
    mov r10, rax
    lea rax, [rip + __cert__null_deref____str10]
    mov r11, rax
    lea rax, [rip + __cert__null_deref____str11]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-16]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-24]
    push r11
    push r10
    push rax
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    mov rax, qword ptr [rsp + 0]
    mov r9, rax
    call __cert__runtime_error_ex
    add rsp, 48
    xor eax, eax
    leave
    ret

.globl __cert__null_deref_fallback
__cert__null_deref_fallback:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov qword ptr [rbp-24], rdx
    movsxd rax, dword ptr [rip + __cert__ERR_NULL_REFERENCE]
    mov r10, rax
    lea rax, [rip + __cert__null_deref_fallback____str12]
    mov r11, rax
    lea rax, [rip + __cert__null_deref_fallback____str13]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    push r11
    mov r11, rax
    mov rax, qword ptr [rbp-16]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-24]
    push r11
    push r10
    push rax
    movsxd rax, dword ptr [rsp + 40]
    mov rdi, rax
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov rax, qword ptr [rsp + 24]
    mov rdx, rax
    mov rax, qword ptr [rsp + 16]
    mov rcx, rax
    mov rax, qword ptr [rsp + 8]
    mov r8, rax
    mov rax, qword ptr [rsp + 0]
    mov r9, rax
    call __cert__runtime_error_ex
    add rsp, 48
    lea rax, [rip + __cert__unbox_ptr_fallback]
    leave
    ret

.section __TEXT,__const
.balign 1
__cert__object_invalid_cast_i32_ptr_to_char_ptr_to_char_u64_ptr_to_char____str0:
    .byte 0x49, 0x6e, 0x76, 0x61, 0x6c, 0x69, 0x64, 0x43, 0x61, 0x73, 0x74, 0

.balign 1
__cert__unbox_i64____str1:
    .byte 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x20, 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0

.balign 1
__cert__unbox_i64____str2:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6b, 0x69, 0x6e, 0x64, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x69, 0x6e, 0x74, 0x65, 0x67, 0x65, 0x72, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0

.balign 1
__cert__unbox_i64____str3:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x20, 0x74, 0x79, 0x70, 0x65, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0

.balign 1
__cert__unbox_f64____str4:
    .byte 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x20, 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0

.balign 1
__cert__unbox_f64____str5:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6b, 0x69, 0x6e, 0x64, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x69, 0x6e, 0x67, 0x2d, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0

.balign 1
__cert__unbox_f64____str6:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x20, 0x74, 0x79, 0x70, 0x65, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0

.balign 1
__cert__unbox_ptr____str7:
    .byte 0x63, 0x61, 0x6e, 0x6e, 0x6f, 0x74, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x20, 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0

.balign 1
__cert__unbox_ptr____str8:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6b, 0x69, 0x6e, 0x64, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x75, 0x6e, 0x62, 0x6f, 0x78, 0

.balign 1
__cert__unbox_ptr____str9:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x20, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x20, 0x74, 0x79, 0x70, 0x65, 0x20, 0x6d, 0x69, 0x73, 0x6d, 0x61, 0x74, 0x63, 0x68, 0

.balign 1
__cert__null_deref____str10:
    .byte 0x4e, 0x75, 0x6c, 0x6c, 0x52, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0

.balign 1
__cert__null_deref____str11:
    .byte 0x41, 0x74, 0x74, 0x65, 0x6d, 0x70, 0x74, 0x65, 0x64, 0x20, 0x74, 0x6f, 0x20, 0x64, 0x65, 0x72, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x20, 0x72, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0

.balign 1
__cert__null_deref_fallback____str12:
    .byte 0x4e, 0x75, 0x6c, 0x6c, 0x52, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0

.balign 1
__cert__null_deref_fallback____str13:
    .byte 0x41, 0x74, 0x74, 0x65, 0x6d, 0x70, 0x74, 0x65, 0x64, 0x20, 0x74, 0x6f, 0x20, 0x64, 0x65, 0x72, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0x20, 0x72, 0x65, 0x66, 0x65, 0x72, 0x65, 0x6e, 0x63, 0x65, 0


.att_syntax prefix
