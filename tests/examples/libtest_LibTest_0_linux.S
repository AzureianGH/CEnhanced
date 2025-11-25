# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.text

.globl LibTest_test_fun
LibTest_test_fun:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

.globl LibTest_other_fun
LibTest_other_fun:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-8], rax
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 10
    mov dword ptr [rbp-12], eax
    movsxd rax, dword ptr [rbp-12]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x14
    mov byte ptr [rbp-13], al
    movzx eax, byte ptr [rbp-13]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-8]
    leave
    ret


.att_syntax prefix
