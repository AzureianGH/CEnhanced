# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern printf
.extern malloc

.text

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, 4
