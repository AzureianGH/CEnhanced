.intel_syntax noprefix
.section .text
.globl test_chance
test_chance:
  push rbp
  mov rbp, rsp
  sub rsp,       40
  mov eax, 67
  mov eax, eax
  jmp .Lret9
.Lret9:
  mov rsp, rbp
  pop rbp
  ret
