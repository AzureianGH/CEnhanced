.intel_syntax noprefix
.section .text
.globl main
main:
  push rbp
  mov rbp, rsp
  sub rsp,       32
  mov eax, 128
  mov rcx, rax
  xor eax, eax
  call malloc
  mov [rbp-8], rax
  mov eax, 1
  mov rcx, rax
  lea rdx, [rel .Lstr0]
  mov eax, 20
  mov r8, rax
  xor eax, eax
  call write
  mov eax, 0
  mov rcx, rax
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, 127
  mov r8, rax
  xor eax, eax
  call read
  mov dword ptr [rbp-12], eax
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
