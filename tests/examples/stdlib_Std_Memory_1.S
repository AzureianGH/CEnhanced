# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern malloc
.extern free

.text

.globl Std_Memory_memcpy
Std_Memory_memcpy:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov dword ptr [rbp-24], edx
    mov rax, 0
    mov dword ptr [rbp-28], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-40], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-48], rax
Std_Memory_memcpy__while_cond0:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memcpy__while_body1
    jmp Std_Memory_memcpy__while_end2
Std_Memory_memcpy__while_body1:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-48]
    mov r11, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-52], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_Memory_memcpy__while_cond0
Std_Memory_memcpy__while_end2:
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Std_Memory_memset
Std_Memory_memset:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov byte ptr [rbp-16], sil
    mov dword ptr [rbp-24], edx
    mov rax, 0
    mov dword ptr [rbp-28], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-40], rax
Std_Memory_memset__while_cond0:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memset__while_body1
    jmp Std_Memory_memset__while_end2
Std_Memory_memset__while_body1:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    movzx eax, byte ptr [rbp-16]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-44], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_Memory_memset__while_cond0
Std_Memory_memset__while_end2:
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Std_Memory_memcmp
Std_Memory_memcmp:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov dword ptr [rbp-24], edx
    mov rax, 0
    mov dword ptr [rbp-28], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-40], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-48], rax
Std_Memory_memcmp__while_cond0:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memcmp__while_body1
    jmp Std_Memory_memcmp__while_end2
Std_Memory_memcmp__while_body1:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-48]
    mov r11, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
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
    jne Std_Memory_memcmp__if_true3
    jmp Std_Memory_memcmp__if_end4
Std_Memory_memcmp__if_true3:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-48]
    mov r11, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    leave
    ret
Std_Memory_memcmp__if_end4:
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-52], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_Memory_memcmp__while_cond0
Std_Memory_memcmp__while_end2:
    mov rax, 0
    leave
    ret

.globl Std_Memory_memmove
Std_Memory_memmove:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov dword ptr [rbp-24], edx
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-32], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-40], rax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setb al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memmove__if_true0
    jmp Std_Memory_memmove__if_false1
Std_Memory_memmove__if_true0:
    mov rax, 0
    mov dword ptr [rbp-44], eax
Std_Memory_memmove__while_cond3:
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memmove__while_body4
    jmp Std_Memory_memmove__while_end5
Std_Memory_memmove__while_body4:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov r11, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-44]
    mov dword ptr [rbp-48], eax
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-44], eax
    jmp Std_Memory_memmove__while_cond3
Std_Memory_memmove__while_end5:
    jmp Std_Memory_memmove__if_end2
Std_Memory_memmove__if_false1:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    seta al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memmove__if_true6
    jmp Std_Memory_memmove__if_end7
Std_Memory_memmove__if_true6:
    movsxd rax, dword ptr [rbp-24]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-52], eax
Std_Memory_memmove__while_cond8:
    movsxd rax, dword ptr [rbp-52]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setge al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memmove__while_body9
    jmp Std_Memory_memmove__while_end10
Std_Memory_memmove__while_body9:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-52]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov r11, rax
    movsxd rax, dword ptr [rbp-52]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-52]
    mov dword ptr [rbp-56], eax
    movsxd rax, dword ptr [rbp-52]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-52], eax
    jmp Std_Memory_memmove__while_cond8
Std_Memory_memmove__while_end10:
Std_Memory_memmove__if_end7:
Std_Memory_memmove__if_end2:
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Std_Memory_memchr
Std_Memory_memchr:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov byte ptr [rbp-16], sil
    mov dword ptr [rbp-24], edx
    mov rax, 0
    mov dword ptr [rbp-28], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-40], rax
Std_Memory_memchr__while_cond0:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memchr__while_body1
    jmp Std_Memory_memchr__while_end2
Std_Memory_memchr__while_body1:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov r10, rax
    movzx eax, byte ptr [rbp-16]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memchr__if_true3
    jmp Std_Memory_memchr__if_end4
Std_Memory_memchr__if_true3:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    leave
    ret
Std_Memory_memchr__if_end4:
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-44], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_Memory_memchr__while_cond0
Std_Memory_memchr__while_end2:
    mov rax, 0
    leave
    ret

.globl Std_Memory_memrchr
Std_Memory_memrchr:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov byte ptr [rbp-16], sil
    mov dword ptr [rbp-24], edx
    movsxd rax, dword ptr [rbp-24]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-40], rax
Std_Memory_memrchr__while_cond0:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setge al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memrchr__while_body1
    jmp Std_Memory_memrchr__while_end2
Std_Memory_memrchr__while_body1:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov r10, rax
    movzx eax, byte ptr [rbp-16]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_memrchr__if_true3
    jmp Std_Memory_memrchr__if_end4
Std_Memory_memrchr__if_true3:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    leave
    ret
Std_Memory_memrchr__if_end4:
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-44], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_Memory_memrchr__while_cond0
Std_Memory_memrchr__while_end2:
    mov rax, 0
    leave
    ret

.globl Std_Memory_bzero
Std_Memory_bzero:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov dword ptr [rbp-16], esi
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 0x0
    mov r11, rax
    movsxd rax, dword ptr [rbp-16]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    movzx eax, byte ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call Std_Memory_memset
    add rsp, 32
    leave
    ret

.globl Std_Memory_bcopy
Std_Memory_bcopy:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov dword ptr [rbp-24], edx
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call Std_Memory_memmove
    add rsp, 32
    leave
    ret

.globl Std_Memory_alloc
Std_Memory_alloc:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    movsxd rax, dword ptr [rbp-8]
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call malloc
    add rsp, 16
    leave
    ret

.globl Std_Memory_dealloc
Std_Memory_dealloc:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov rax, qword ptr [rbp-8]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call free
    add rsp, 16
    xor eax, eax
    leave
    ret

.globl Std_Memory_realloc
Std_Memory_realloc:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov dword ptr [rbp-16], esi
    movsxd rax, dword ptr [rbp-16]
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call malloc
    add rsp, 16
    mov qword ptr [rbp-24], rax
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_realloc__if_true0
    jmp Std_Memory_realloc__if_end1
Std_Memory_realloc__if_true0:
    mov rax, 0
    leave
    ret
Std_Memory_realloc__if_end1:
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    movsxd rax, dword ptr [rbp-16]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call Std_Memory_memmove
    add rsp, 32
    mov rax, qword ptr [rbp-8]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call free
    add rsp, 16
    mov rax, qword ptr [rbp-24]
    leave
    ret

.globl Std_Memory_cleanalloc
Std_Memory_cleanalloc:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov dword ptr [rbp-8], edi
    mov dword ptr [rbp-16], esi
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-16]
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    movsxd rax, dword ptr [rbp-20]
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call malloc
    add rsp, 16
    mov qword ptr [rbp-32], rax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_cleanalloc__if_true0
    jmp Std_Memory_cleanalloc__if_end1
Std_Memory_cleanalloc__if_true0:
    mov rax, 0
    leave
    ret
Std_Memory_cleanalloc__if_end1:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, 0x0
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    movzx eax, byte ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call Std_Memory_memset
    add rsp, 32
    mov rax, qword ptr [rbp-32]
    leave
    ret

.globl Std_Memory_swap
Std_Memory_swap:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov dword ptr [rbp-24], edx
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-32], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-40], rax
    mov rax, 0
    mov dword ptr [rbp-44], eax
Std_Memory_swap__while_cond0:
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_Memory_swap__while_body1
    jmp Std_Memory_swap__while_end2
Std_Memory_swap__while_body1:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov byte ptr [rbp-45], al
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov r11, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-44]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    movzx eax, byte ptr [rbp-45]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-44]
    mov dword ptr [rbp-52], eax
    movsxd rax, dword ptr [rbp-44]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-44], eax
    jmp Std_Memory_swap__while_cond0
Std_Memory_swap__while_end2:
    xor eax, eax
    leave
    ret


.att_syntax prefix
