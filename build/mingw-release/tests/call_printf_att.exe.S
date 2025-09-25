.section .text
.globl main
main:
  subq $40, %rsp
  leaq .Lstr0(%rip), %rcx
  leaq .Lstr1(%rip), %rdx
  xorl %eax, %eax
  call printf@PLT
  addq $40, %rsp
  ret
.section .rdata,"dr"
.Lstr0:
  .asciz "Hello, %s!\\n"
.Lstr1:
  .asciz "CHance"
