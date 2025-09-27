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
  mov eax, 1
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
  lea rax, [rip + .Lstr4]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  mov rax, [rbp-20]
  mov rdx, rax
  mov eax, 0
  add rax, rdx
  mov rdx, rax
  mov eax, 0
  mov byte ptr [rdx], al
.Lend1:
  lea rax, [rip + .Lstr5]
  push rax
  mov rax, [rbp-20]
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  lea rax, [rip + .Lstr6]
  push rax
  mov rax, [rbp-20]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call strlen
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  lea rax, [rip + .Lstr1]
  mov [rbp-28], rax
  mov rax, [rbp-20]
  push rax
  mov rax, [rbp-28]
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call strcmp
  push rax
  mov eax, 0
  mov rcx, rax
  pop rax
  cmp rax, rcx
  sete al
  movzx eax, al
  cmp eax, 0
  je .Lelse2
  lea rax, [rip + .Lstr7]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  jmp .Lend2
.Lelse2:
  lea rax, [rip + .Lstr8]
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
.Lend2:
  lea rax, [rip + .Lstr9]
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
  xor eax, eax
  call test_overflow
  mov word ptr [rbp-30], ax
  lea rax, [rip + .Lstr10]
  push rax
  movsx eax, word ptr [rbp-30]
  push rax
  pop rax
  mov rdx, rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call printf
  mov eax, 0
  jmp .Lret2
.Lret2:
  mov rsp, rbp
  pop rbp
  ret
.globl strlen
strlen:
  push rbp
  mov rbp, rsp
  sub rsp,       56
  mov [rbp-8], rcx
  mov eax, 0
  mov dword ptr [rbp-12], eax
.Lwhile3:
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, dword ptr [rbp-12]
  add rax, rdx
  movzx eax, byte ptr [rax]
  cmp eax, 0
  je .Lendw3
  mov ecx, dword ptr [rbp-12]
  mov eax, ecx
  add ecx, 1
  mov dword ptr [rbp-12], ecx
  jmp .Lwhile3
.Lendw3:
  mov eax, dword ptr [rbp-12]
  jmp .Lret3
.Lret3:
  mov rsp, rbp
  pop rbp
  ret
.globl strcmp
strcmp:
  push rbp
  mov rbp, rsp
  sub rsp,       72
  mov [rbp-8], rcx
  mov [rbp-16], rdx
  mov eax, 0
  mov dword ptr [rbp-20], eax
.Lwhile4:
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  cmp eax, 0
  je .Lland_false5
  mov rax, [rbp-16]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  cmp eax, 0
  je .Lland_false5
  mov eax, 1
  jmp .Lland_end5
.Lland_false5:
  xor eax, eax
.Lland_end5:
  cmp eax, 0
  je .Lendw4
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  push rax
  mov rax, [rbp-16]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  mov rcx, rax
  pop rax
  cmp rax, rcx
  setne al
  movzx eax, al
  cmp eax, 0
  je .Lelse6
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  push rax
  mov rax, [rbp-16]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  mov rcx, rax
  pop rax
  sub rax, rcx
  jmp .Lret4
  jmp .Lend6
.Lelse6:
.Lend6:
  mov ecx, dword ptr [rbp-20]
  mov eax, ecx
  add ecx, 1
  mov dword ptr [rbp-20], ecx
  jmp .Lwhile4
.Lendw4:
  mov rax, [rbp-8]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  push rax
  mov rax, [rbp-16]
  mov rdx, rax
  mov eax, dword ptr [rbp-20]
  add rax, rdx
  movzx eax, byte ptr [rax]
  mov rcx, rax
  pop rax
  sub rax, rcx
  jmp .Lret4
.Lret4:
  mov rsp, rbp
  pop rbp
  ret
.globl abort
abort:
  push rbp
  mov rbp, rsp
  sub rsp,       40
  mov eax, 1
  push rax
  pop rax
  mov rcx, rax
  xor eax, eax
  call exit
  jmp .Lret5
.Lret5:
  mov rsp, rbp
  pop rbp
  ret
.globl test_overflow
test_overflow:
  push rbp
  mov rbp, rsp
  sub rsp,       56
  mov eax, 30000
  mov word ptr [rbp-2], ax
  mov eax, 10000
  mov word ptr [rbp-4], ax
  movsx eax, word ptr [rbp-2]
  push rax
  movsx eax, word ptr [rbp-4]
  mov rcx, rax
  pop rax
  add rax, rcx
  jmp .Lret6
.Lret6:
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
  .asciz "No input read, using empty string\n"
.Lstr5:
  .asciz "Hello, %s!\n"
.Lstr6:
  .asciz "String length: %d\n"
.Lstr7:
  .asciz "You entered the secret name!\n"
.Lstr8:
  .asciz "You did not enter the secret name.\n"
.Lstr9:
  .asciz "Freeing memory...\n"
.Lstr10:
  .asciz "Overflow test result (should be -25536): %d\n"
