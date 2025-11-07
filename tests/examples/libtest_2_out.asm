; ChanceCode NASM-style x86-64 backend output

extern printf

section .text

global LibTest_Another_other_test_fun
LibTest_Another_other_test_fun:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword [rbp-8], ecx
    movsxd rax, dword [rbp-8]
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

global LibTest_Another_get_info
LibTest_Another_get_info:
    lea rax, [rel LibTest_Another_get_info____str0]
    ret

global LibTest_Another_test_varargs
LibTest_Another_test_varargs:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov dword [rbp-8], ecx
    mov dword [rbp-16], edx
    lea rax, [rel LibTest_Another_test_varargs____str1]
    mov r10, rax
    movsxd rax, dword [rbp-8]
    mov r11, rax
    movsxd rax, dword [rbp-16]
    push r10
    push r11
    push rax
    sub rsp, 40
    mov rax, qword [rsp + 56]
    mov rcx, rax
    mov qword [rsp + 0], rcx
    movsxd rax, dword [rsp + 48]
    mov rdx, rax
    mov qword [rsp + 8], rdx
    movsxd rax, dword [rsp + 40]
    mov r8, rax
    mov qword [rsp + 16], r8
    call printf
    add rsp, 40
    add rsp, 24
    movsxd rax, eax
    lea rax, [rbp+32]
    mov qword [rbp-24], rax
    mov rax, 0
    mov dword [rbp-28], eax
    mov rax, 0
    mov dword [rbp-32], eax
LibTest_Another_test_varargs__while_cond0:
    movsxd rax, dword [rbp-32]
    mov r10, rax
    movsxd rax, dword [rbp-8]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    setne al
    movzx eax, al
    cmp rax, 0
    jne LibTest_Another_test_varargs__while_body1
    jmp LibTest_Another_test_varargs__while_end2
LibTest_Another_test_varargs__while_body1:
    mov rax, qword [rbp-24]
    mov rcx, rax
    movsxd rax, dword [rcx]
    mov r10, rax
    mov rax, qword [rbp-24]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov qword [rbp-24], rax
    mov rax, r10
    mov dword [rbp-36], eax
    movsxd rax, dword [rbp-28]
    mov r10, rax
    movsxd rax, dword [rbp-36]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword [rbp-28], eax
    movsxd rax, dword [rbp-28]
    movsxd rax, dword [rbp-32]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword [rbp-32], eax
    movsxd rax, dword [rbp-32]
    jmp LibTest_Another_test_varargs__while_cond0
LibTest_Another_test_varargs__while_end2:
    mov rax, qword [rbp-24]
    mov r10, rax
    movsxd rax, dword [rbp-28]
    leave
    ret

section .rodata
align 1
LibTest_Another_get_info____str0:
    db 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0

align 1
LibTest_Another_test_varargs____str1:
    db 0x74, 0x65, 0x73, 0x74, 0x5f, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x63, 0x6f, 0x75, 0x6e, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e, 0x75, 0x73, 0x65, 0x64, 0x3d, 0x25, 0x64, 0x0a, 0

