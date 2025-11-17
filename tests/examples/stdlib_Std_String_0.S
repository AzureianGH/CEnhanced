# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.text

.globl Std_String_strcmp
Std_String_strcmp:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, 0
    mov dword ptr [rbp-20], eax
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-32], rax
    mov rax, qword ptr [rbp-16]
    mov qword ptr [rbp-40], rax
Std_String_strcmp__while_cond0:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strcmp__land_rhs3
    jmp Std_String_strcmp__land_false4
Std_String_strcmp__land_rhs3:
    mov rax, qword ptr [rbp-40]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strcmp__land_true5
    jmp Std_String_strcmp__land_false4
Std_String_strcmp__land_true5:
    mov rax, 1
    jmp Std_String_strcmp__land_end6
Std_String_strcmp__land_false4:
    mov r10, rax
    mov rax, 0
Std_String_strcmp__land_end6:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_String_strcmp__while_body1
    jmp Std_String_strcmp__while_end2
Std_String_strcmp__while_body1:
    mov rax, qword ptr [rbp-32]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r11, rax
    mov rax, qword ptr [rbp-40]
    push r10
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
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
    jne Std_String_strcmp__if_true7
    jmp Std_String_strcmp__if_end8
Std_String_strcmp__if_true7:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    leave
    ret
Std_String_strcmp__if_end8:
    movsxd rax, dword ptr [rbp-20]
    mov dword ptr [rbp-44], eax
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    jmp Std_String_strcmp__while_cond0
Std_String_strcmp__while_end2:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-40]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    leave
    ret

.globl Std_String_strlen
Std_String_strlen:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov rax, 0
    mov dword ptr [rbp-12], eax
Std_String_strlen__while_cond0:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-12]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strlen__while_body1
    jmp Std_String_strlen__while_end2
Std_String_strlen__while_body1:
    movsxd rax, dword ptr [rbp-12]
    mov dword ptr [rbp-16], eax
    movsxd rax, dword ptr [rbp-12]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-12], eax
    jmp Std_String_strlen__while_cond0
Std_String_strlen__while_end2:
    movsxd rax, dword ptr [rbp-12]
    leave
    ret

.globl Std_String_strcpy
Std_String_strcpy:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, 0
    mov dword ptr [rbp-20], eax
Std_String_strcpy__while_cond0:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strcpy__while_body1
    jmp Std_String_strcpy__while_end2
Std_String_strcpy__while_body1:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-20]
    mov dword ptr [rbp-24], eax
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    jmp Std_String_strcpy__while_cond0
Std_String_strcpy__while_end2:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Std_String_strcat
Std_String_strcat:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, qword ptr [rbp-8]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call Std_String_strlen
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    mov rax, 0
    mov dword ptr [rbp-24], eax
Std_String_strcat__while_cond0:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    movsxd rax, dword ptr [rbp-24]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strcat__while_body1
    jmp Std_String_strcat__while_end2
Std_String_strcat__while_body1:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movsxd rax, dword ptr [rbp-24]
    mov dword ptr [rbp-28], eax
    movsxd rax, dword ptr [rbp-24]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-24], eax
    jmp Std_String_strcat__while_cond0
Std_String_strcat__while_end2:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Std_String_strchr
Std_String_strchr:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov byte ptr [rbp-16], sil
    mov rax, 0
    mov dword ptr [rbp-20], eax
Std_String_strchr__while_cond0:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
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
    jne Std_String_strchr__while_body1
    jmp Std_String_strchr__while_end2
Std_String_strchr__while_body1:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    movzx eax, byte ptr [rbp-16]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
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
    jne Std_String_strchr__if_true3
    jmp Std_String_strchr__if_end4
Std_String_strchr__if_true3:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    leave
    ret
Std_String_strchr__if_end4:
    movsxd rax, dword ptr [rbp-20]
    mov dword ptr [rbp-24], eax
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    jmp Std_String_strchr__while_cond0
Std_String_strchr__while_end2:
    mov rax, 0
    leave
    ret

.globl Std_String_strstr
Std_String_strstr:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, qword ptr [rbp-8]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call Std_String_strlen
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    mov rax, qword ptr [rbp-16]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call Std_String_strlen
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-24], eax
    movsxd rax, dword ptr [rbp-24]
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
    jne Std_String_strstr__if_true0
    jmp Std_String_strstr__if_end1
Std_String_strstr__if_true0:
    mov rax, qword ptr [rbp-8]
    leave
    ret
Std_String_strstr__if_end1:
    mov rax, 0
    mov dword ptr [rbp-28], eax
Std_String_strstr__while_cond2:
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setle al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_String_strstr__while_body3
    jmp Std_String_strstr__while_end4
Std_String_strstr__while_body3:
    mov rax, 0
    mov dword ptr [rbp-32], eax
Std_String_strstr__while_cond5:
    movsxd rax, dword ptr [rbp-32]
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
    jne Std_String_strstr__land_rhs8
    jmp Std_String_strstr__land_false9
Std_String_strstr__land_rhs8:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    mov r11, rax
    movsxd rax, dword ptr [rbp-32]
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov r11, rax
    movsxd rax, dword ptr [rbp-32]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
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
    jne Std_String_strstr__land_true10
    jmp Std_String_strstr__land_false9
Std_String_strstr__land_true10:
    mov rax, 1
    jmp Std_String_strstr__land_end11
Std_String_strstr__land_false9:
    mov r10, rax
    mov rax, 0
Std_String_strstr__land_end11:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_String_strstr__while_body6
    jmp Std_String_strstr__while_end7
Std_String_strstr__while_body6:
    movsxd rax, dword ptr [rbp-32]
    mov dword ptr [rbp-36], eax
    movsxd rax, dword ptr [rbp-32]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-32], eax
    jmp Std_String_strstr__while_cond5
Std_String_strstr__while_end7:
    movsxd rax, dword ptr [rbp-32]
    mov r11, rax
    movsxd rax, dword ptr [rbp-24]
    mov rbx, rax
    mov rax, r11
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Std_String_strstr__if_true12
    jmp Std_String_strstr__if_end13
Std_String_strstr__if_true12:
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    movsxd rax, dword ptr [rbp-28]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    leave
    ret
Std_String_strstr__if_end13:
    movsxd rax, dword ptr [rbp-28]
    mov dword ptr [rbp-40], eax
    movsxd rax, dword ptr [rbp-28]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    jmp Std_String_strstr__while_cond2
Std_String_strstr__while_end4:
    mov rax, 0
    leave
    ret


.att_syntax prefix
