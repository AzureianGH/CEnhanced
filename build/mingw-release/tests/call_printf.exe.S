.intel_syntax noprefix
.section .text
.globl main
main:
  sub rsp, 40
  lea rcx, [rel .Lstr0]
  lea rdx, [rel .Lstr1]
  xor eax, eax
  call printf
  add rsp, 40
  ret
.section .rdata,"dr"
.Lstr0:
  .asciz "Hello, %s!\\n"
.Lstr1:
  .asciz "CHance"
