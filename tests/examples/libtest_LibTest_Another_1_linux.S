# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern printf

.text

.globl LibTest_Another_other_test_fun
LibTest_Another_other_test_fun:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

.globl LibTest_Another_get_info
LibTest_Another_get_info:
    lea rax, [rip + LibTest_Another_get_info____str0]
    ret

.globl LibTest_Another_test_varargs
LibTest_Another_test_varargs:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov dword ptr [rbp-8], edi
    mov dword ptr [rbp-16], esi
    mov qword ptr [rbp + 16], rdi
    mov qword ptr [rbp + 24], rsi
    mov qword ptr [rbp + 32], rdx
    mov qword ptr [rbp + 40], rcx
    mov qword ptr [rbp + 48], r8
    mov qword ptr [rbp + 56], r9
    lea rax, [rip + LibTest_Another_test_varargs____str1]
    mov r10, rax
    movsxd rax, dword ptr [rbp-8]
    mov r11, rax
    movsxd rax, dword ptr [rbp-16]
    push r10
    push r11
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 40]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 32]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    movsxd rax, dword ptr [rsp + 24]
    mov rdx, rax
    mov qword ptr [rsp + 16], rdx
    xor eax, eax
    call printf
    add rsp, 48
    movsxd rax, eax
    lea rax, [rbp+32]
    mov qword ptr [rbp-24], rax
    mov rax, 0
    mov dword ptr [rbp-28], eax
    mov rax, 0
    mov dword ptr [rbp-32], eax
LibTest_Another_test_varargs__while_cond0:
    movsxd rax, dword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-8]
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
    jne LibTest_Another_test_varargs__while_body1
    jmp LibTest_Another_test_varargs__while_end2
LibTest_Another_test_varargs__while_body1:
    mov rax, qword ptr [rbp-24]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-24]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov qword ptr [rbp-24], rax
    mov rax, r10
    mov dword ptr [rbp-36], eax
    movsxd rax, dword ptr [rbp-28]
    mov r10, rax
    movsxd rax, dword ptr [rbp-36]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-28], eax
    movsxd rax, dword ptr [rbp-32]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-32], eax
    jmp LibTest_Another_test_varargs__while_cond0
LibTest_Another_test_varargs__while_end2:
    mov rax, qword ptr [rbp-24]
    mov r10, rax
    movsxd rax, dword ptr [rbp-28]
    leave
    ret

.globl LibTest_Another_test_diffvarargs
LibTest_Another_test_diffvarargs:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp + 16], rdi
    mov qword ptr [rbp + 24], rsi
    mov qword ptr [rbp + 32], rdx
    mov qword ptr [rbp + 40], rcx
    mov qword ptr [rbp + 48], r8
    mov qword ptr [rbp + 56], r9
    lea rax, [rbp+16]
    mov qword ptr [rbp-8], rax
    mov rax, qword ptr [rbp-8]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov qword ptr [rbp-8], rax
    mov rax, r10
    mov dword ptr [rbp-12], eax
    mov rax, qword ptr [rbp-8]
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov qword ptr [rbp-8], rax
    mov rax, r10
    mov qword ptr [rbp-24], rax
    mov rax, qword ptr [rbp-8]
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov qword ptr [rbp-8], rax
    mov rax, r10
    mov qword ptr [rbp-32], rax
    lea rax, [rip + LibTest_Another_test_diffvarargs____str2]
    mov r10, rax
    movsxd rax, dword ptr [rbp-12]
    mov r11, rax
    mov rax, qword ptr [rbp-24]
    push r10
    mov r10, rax
    mov rax, qword ptr [rbp-32]
    push r11
    push r10
    push rax
    sub rsp, 32
    mov rax, qword ptr [rsp + 56]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 48]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    movsd xmm0, qword ptr [rsp + 40]
    movsd qword ptr [rsp + 16], xmm0
    mov rax, qword ptr [rsp + 32]
    mov rdx, rax
    mov qword ptr [rsp + 24], rdx
    mov eax, 1
    call printf
    add rsp, 64
    movsxd rax, eax
    mov rax, qword ptr [rbp-8]
    xor eax, eax
    leave
    ret

.section .rodata
.balign 1
LibTest_Another_get_info____str0:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0

.balign 1
LibTest_Another_test_varargs____str1:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x63, 0x61, 0x6c, 0x6c, 0x65, 0x64, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x63, 0x6f, 0x75, 0x6e, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x75, 0x6e, 0x75, 0x73, 0x65, 0x64, 0x3d, 0x25, 0x64, 0x0a, 0

.balign 1
LibTest_Another_test_diffvarargs____str2:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x64, 0x69, 0x66, 0x66, 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x3a, 0x20, 0x66, 0x69, 0x72, 0x73, 0x74, 0x3d, 0x25, 0x64, 0x2c, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x3d, 0x25, 0x66, 0x2c, 0x20, 0x74, 0x68, 0x69, 0x72, 0x64, 0x3d, 0x25, 0x73, 0x0a, 0


.att_syntax prefix
