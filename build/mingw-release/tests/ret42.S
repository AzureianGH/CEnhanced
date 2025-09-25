.intel_syntax noprefix
.section .text
.globl main
main:
  push rbp
  mov rbp, rsp
  sub rsp,       40
  mov eax, 41
  push rax
  mov eax, 1
  mov rcx, rax
  pop rax
  add rax, rcx
  jmp .Lret1
.Lret1:
  mov rsp, rbp
  pop rbp
  ret
