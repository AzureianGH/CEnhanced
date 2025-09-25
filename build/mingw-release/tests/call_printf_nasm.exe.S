default rel
section .text
extern printf
global main
main:
  sub rsp, 40
  lea rcx, [rel .Lstr0]
  lea rdx, [rel .Lstr1]
  xor eax, eax
  call printf
  add rsp, 40
  ret
section .rdata align=1
.Lstr0: db 'H','e','l','l','o',',',' ','%','s','!',\'\','n',0
.Lstr1: db 'C','H','a','n','c','e',0
