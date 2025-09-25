.intel_syntax noprefix
.section .text
.globl main
main:
  push rbp
  mov rbp, rsp
  sub rsp,       40
  mov eax, 0
  jmp .Lret1
.Lret1:
  mov rsp, rbp
  pop rbp
  ret
