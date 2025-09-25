.intel_syntax noprefix
.section .text
.globl printwrapper
printwrapper:
  push rbp
  mov rbp, rsp
  sub rsp,       40
  lea rax, [rip + .Lstr0]
  push rax
  lea rax, [rip + .Lstr1]
  push rax
  mov eax, 42
  push rax
  pop rax
  mov r8, rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  jmp .Lret1
.Lret1:
  mov rsp, rbp
  pop rbp
  ret
.globl main
main:
  push rbp
  mov rbp, rsp
  sub rsp,       72
  mov eax, 128
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call malloc
  mov [rbp-8], rax
  mov eax, 1
  push rax
  lea rax, [rip + .Lstr2]
  push rax
  mov eax, 20
  push rax
  pop rax
  mov r8, rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call write
  mov eax, 0
  push rax
  mov rax, [rbp-8]
  push rax
  mov eax, 127
  push rax
  pop rax
  mov r8, rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call read
  mov dword ptr [rbp-12], eax
  lea rax, [rip + .Lstr3]
  push rax
  xor eax, eax
  call printwrapper
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  mov rax, [rbp-8]
  mov [rbp-20], rax
  mov eax, dword ptr [rbp-12]
  push rax
  mov eax, 0
  mov rcx, rax
  pop rax
  cmp rax, rcx
  setg al
  movzx eax, al
  cmp eax, 0
  je .Lelse1
  mov rax, [rbp-20]
  mov rdx, rax
  mov eax, dword ptr [rbp-12]
  push rax
  mov eax, 1
  mov rcx, rax
  pop rax
  sub rax, rcx
  add rax, rdx
  mov rdx, rax
  mov eax, 0
  mov byte ptr [rdx], al
  jmp .Lend1
.Lelse1:
  mov rax, [rbp-20]
  mov rdx, rax
  mov eax, 0
  add rax, rdx
  mov rdx, rax
  mov eax, 0
  mov byte ptr [rdx], al
.Lend1:
  lea rax, [rip + .Lstr4]
  push rax
  mov rax, [rbp-20]
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  lea rax, [rip + .Lstr5]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  mov rax, [rbp-8]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call free
  mov eax, 0
  jmp .Lret2
.Lret2:
  mov rsp, rbp
  pop rbp
  ret
.section .rdata,"dr"
.Lstr0:
  .asciz "Hello, %s! Number: %d\n"
.Lstr1:
  .asciz "CHance"
.Lstr2:
  .asciz "What's your name? > "
.Lstr3:
  .asciz "Read %d bytes\n"
.Lstr4:
  .asciz "Hello, %s!\n"
.Lstr5:
  .asciz "Freeing memory...\n"
