# ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern printf
.extern write
.extern read
.extern malloc
.extern free
.extern exit
.extern test
.extern test_chance
.extern Std_Memory_alloc
.extern Std_Memory_memset
.extern Std_String_strlen
.extern Std_Memory_dealloc
.extern Test_Chance_test_chance
.extern LibTest_test_fun
.extern LibTest_Another_other_test_fun
.extern LibTest_Another_get_info
.extern LibTest_Another_test_varargs
.extern LibTest_Another_test_diffvarargs

.data
.balign 4
Test_Program_total_test_count:
    .long 0x00000000

.data
.balign 8
Test_Program_globalElement:
    .space 44

.text

.globl Test_Program_inlined_function
Test_Program_inlined_function:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

.globl Test_Program_printwrapper
Test_Program_printwrapper:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    lea rax, [rip + Test_Program_printwrapper____str0]
    mov qword ptr [rbp-8], rax
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    lea rax, [rip + Test_Program_printwrapper____str1]
    push r10
    push rax
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 16
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + Test_Program_printwrapper____str2]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    lea rax, [rip + Test_Program_printwrapper____str3]
    mov r10, rax
    lea rax, [rip + Test_Program_printwrapper____str4]
    mov r11, rax
    mov rax, 42
    push r10
    push r11
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 40]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 32]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    movsxd rax, dword ptr [rsp + 24]
    mov rdx, rax
    mov qword ptr [rsp + 16], rdx
    xor eax, eax
    call printf
    add rsp, 48
    movsxd rax, eax
    leave
    ret

.globl Test_Program_assert
Test_Program_assert:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov byte ptr [rbp-8], dil
    mov qword ptr [rbp-16], rsi
    movzx eax, byte ptr [rbp-8]
    movsx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_assert__if_true0
    jmp Test_Program_assert__if_false1
Test_Program_assert__if_true0:
    lea rax, [rip + Test_Program_assert____str5]
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    push r10
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 32
    movsxd rax, eax
    jmp Test_Program_assert__if_end2
Test_Program_assert__if_false1:
    lea rax, [rip + Test_Program_assert____str6]
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    push r10
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 32
    movsxd rax, eax
    call Test_Program_abort
Test_Program_assert__if_end2:
    xor eax, eax
    leave
    ret

.globl main
main:
    push rbp
    mov rbp, rsp
    sub rsp, 608
    xor rax, rax
    mov qword ptr [rbp-8], rax
    mov rax, 128
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call Std_Memory_alloc
    add rsp, 16
    mov qword ptr [rbp-8], rax
    mov rax, qword ptr [rbp-8]
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str7]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 0x0
    mov r11, rax
    mov rax, 128
    push r10
    push r11
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    movzx eax, byte ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call Std_Memory_memset
    add rsp, 32
    mov rax, 1
    mov byte ptr [rbp-9], al
    mov rax, 0
    mov dword ptr [rbp-16], eax
main__while_cond0:
    movsxd rax, dword ptr [rbp-16]
    mov r10, rax
    mov rax, 128
    mov rax, r10
    cmp rax, 128
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__while_body1
    jmp main__while_end2
main__while_body1:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-16]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true4
    jmp main__if_end5
main__if_true4:
    mov rax, 0
    mov byte ptr [rbp-9], al
    movzx eax, byte ptr [rbp-9]
    movsx eax, al
main__if_end5:
main__while_post3:
    movsxd rax, dword ptr [rbp-16]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-16], eax
    movsxd rax, dword ptr [rbp-16]
    jmp main__while_cond0
main__while_end2:
    movzx eax, byte ptr [rbp-9]
    movsx eax, al
    mov r10, rax
    lea rax, [rip + main____str8]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, 1
    mov r10, rax
    lea rax, [rip + main____str9]
    mov r11, rax
    mov rax, 20
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 24]
    mov rdi, rax
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call write
    add rsp, 32
    movsxd rax, eax
    mov rax, 0
    mov r10, rax
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    mov rax, 127
    push r10
    push r11
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 24]
    mov rdi, rax
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdx, rax
    call read
    add rsp, 32
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    call Test_Program_printwrapper
    movsxd rax, eax
    mov dword ptr [rbp-24], eax
    movsxd rax, dword ptr [rbp-24]
    mov r10, rax
    mov rax, 26
    mov rax, r10
    cmp rax, 26
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str10]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, qword ptr [rbp-8]
    mov qword ptr [rbp-32], rax
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setg al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true6
    jmp main__if_false7
main__if_true6:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 10
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true9
    jmp main__if_end10
main__if_true9:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    movsx eax, al
    mov byte ptr [rbp-33], al
    movzx eax, byte ptr [rbp-33]
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-33]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
main__if_end10:
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setg al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_rhs11
    jmp main__land_false12
main__land_rhs11:
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 13
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_true13
    jmp main__land_false12
main__land_true13:
    mov rax, 1
    jmp main__land_end14
main__land_false12:
    mov r10, rax
    mov rax, 0
main__land_end14:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true15
    jmp main__if_end16
main__if_true15:
    mov rax, qword ptr [rbp-32]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    push r10
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov r10, rax
    mov rax, 0
    movsx eax, al
    mov byte ptr [rbp-34], al
    movzx eax, byte ptr [rbp-34]
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-34]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
main__if_end16:
    jmp main__if_end8
main__if_false7:
    lea rax, [rip + main____str11]
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    xor eax, eax
    call printf
    add rsp, 24
    movsxd rax, eax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    movsx eax, al
    mov byte ptr [rbp-35], al
    movzx eax, byte ptr [rbp-35]
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-35]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
main__if_end8:
    mov rax, qword ptr [rbp-32]
    push rax
    mov rax, qword ptr [rsp + 0]
    mov rdi, rax
    call Std_String_strlen
    add rsp, 8
    movsxd rax, eax
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str12]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str13]
    mov qword ptr [rbp-48], rax
    mov rax, qword ptr [rbp-32]
    mov r10, rax
    mov rax, qword ptr [rbp-48]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true17
    jmp main__if_false18
main__if_true17:
    mov rax, 1
    mov r10, rax
    lea rax, [rip + main____str14]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    jmp main__if_end19
main__if_false18:
    mov rax, 1
    mov r10, rax
    lea rax, [rip + main____str15]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
main__if_end19:
    lea rax, [rip + main____str16]
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    xor eax, eax
    call printf
    add rsp, 24
    movsxd rax, eax
    mov rax, qword ptr [rbp-8]
    push rax
    mov rax, qword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_dealloc
    add rsp, 8
    sub rsp, 8
    call Test_Program_test_overflow
    add rsp, 8
    movsx eax, ax
    mov word ptr [rbp-50], ax
    lea rax, [rip + main____str17]
    mov r10, rax
    movsx eax, word ptr [rbp-50]
    push r10
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsx eax, word ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    movsx eax, word ptr [rbp-50]
    movsx eax, ax
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 25536
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str18]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    sub rsp, 8
    call Test_Program_test_mul_overflow
    add rsp, 8
    movzx eax, al
    mov byte ptr [rbp-51], al
    movzx eax, byte ptr [rbp-51]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, 64
    mov rax, r10
    cmp rax, 64
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str19]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 6
    mov r10, rax
    mov rax, 7
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-56], eax
    movsxd rax, dword ptr [rbp-56]
    mov r10, rax
    mov rax, 42
    mov rax, r10
    cmp rax, 42
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str20]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 20
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cqo
    idiv rbx
    movsxd rax, eax
    mov dword ptr [rbp-60], eax
    movsxd rax, dword ptr [rbp-60]
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str21]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 20
    mov r10, rax
    mov rax, 6
    mov rbx, rax
    mov rax, r10
    cqo
    idiv rbx
    mov rax, rdx
    movsxd rax, eax
    mov dword ptr [rbp-64], eax
    movsxd rax, dword ptr [rbp-64]
    mov r10, rax
    mov rax, 2
    mov rax, r10
    cmp rax, 2
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str22]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 255
    mov r10, rax
    mov rax, 255
    mov rax, r10
    cmp rax, 255
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str23]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str24]
    push rax
    call Test_Chance_test_chance
    movsxd rax, eax
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    lea rax, [rip + main____str25]
    push rax
    call test
    movsxd rax, eax
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-72], rax
    mov rax, qword ptr [rbp-72]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-72]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-72]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x3
    mov byte ptr [rbp-73], al
    movzx eax, byte ptr [rbp-73]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-73]
    mov rax, qword ptr [rbp-72]
    mov r10, rax
    mov rax, 44
    mov dword ptr [rbp-80], eax
    movsxd rax, dword ptr [rbp-80]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    movsxd rax, dword ptr [rbp-80]
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-88], rax
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x7b
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-88]
    mov r10, rax
    mov rax, 333
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-72]
    mov qword ptr [rbp-96], rax
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-104], rax
    mov rax, qword ptr [rbp-104]
    mov r10, rax
    mov rax, qword ptr [rbp-96]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-104]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-96]
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-104]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, 44
    mov rax, r10
    cmp rax, 44
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str26]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 123
    mov dword ptr [rbp-108], eax
    lea rax, [rbp-108]
    mov qword ptr [rbp-120], rax
    lea rax, [rbp-120]
    mov qword ptr [rbp-128], rax
    mov rax, qword ptr [rbp-128]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, 123
    mov rax, r10
    cmp rax, 123
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str27]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 5
    mov r10, rax
    mov rax, 2
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_alloc
    add rsp, 8
    mov qword ptr [rbp-136], rax
    mov rax, qword ptr [rbp-136]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov qword ptr [rbp-144], rax
    sub rsp, 8
    call Test_Program_construct_thing
    add rsp, 8
    mov qword ptr [rbp-152], rax
    mov rax, qword ptr [rbp-152]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov dword ptr [rbp-156], eax
    mov rax, qword ptr [rbp-144]
    mov r10, rax
    movsxd rax, dword ptr [rbp-156]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-152]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov byte ptr [rbp-157], al
    mov rax, qword ptr [rbp-144]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    movzx eax, byte ptr [rbp-157]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-144]
    mov rax, qword ptr [rbp-136]
    mov r10, rax
    mov rax, 1
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov qword ptr [rbp-168], rax
    mov rax, qword ptr [rbp-88]
    mov qword ptr [rbp-176], rax
    mov rax, qword ptr [rbp-176]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov dword ptr [rbp-180], eax
    mov rax, qword ptr [rbp-168]
    mov r10, rax
    movsxd rax, dword ptr [rbp-180]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-176]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov byte ptr [rbp-181], al
    mov rax, qword ptr [rbp-168]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    movzx eax, byte ptr [rbp-181]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-168]
    mov rax, qword ptr [rbp-136]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, 5
    mov rax, r10
    cmp rax, 5
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str28]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-136]
    mov r10, rax
    mov rax, 1
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, 123
    mov rax, r10
    cmp rax, 123
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str29]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-136]
    push rax
    mov rax, qword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_dealloc
    add rsp, 8
    mov rax, qword ptr [rbp-96]
    mov qword ptr [rbp-192], rax
    lea rax, [rip + main____str30]
    mov r10, rax
    mov rax, qword ptr [rbp-192]
    push r10
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    mov rax, 3
    mov dword ptr [rbp-196], eax
    mov rax, qword ptr [rbp-96]
    mov r10, rax
    mov rax, 45
    mov dword ptr [rbp-200], eax
    movsxd rax, dword ptr [rbp-200]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    movsxd rax, dword ptr [rbp-200]
    mov rax, qword ptr [rbp-96]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, 45
    mov rax, r10
    cmp rax, 45
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str31]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 1
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    mov rcx, rbx
    shl rax, cl
    movsxd rax, eax
    mov dword ptr [rbp-204], eax
    movsxd rax, dword ptr [rbp-204]
    mov r10, rax
    mov rax, 2
    mov rax, r10
    cmp rax, 2
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str32]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 4
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    mov rcx, rbx
    sar rax, cl
    movsxd rax, eax
    mov dword ptr [rbp-208], eax
    movsxd rax, dword ptr [rbp-208]
    mov r10, rax
    mov rax, 2
    mov rax, r10
    cmp rax, 2
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str33]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str34]
    mov r10, rax
    lea rax, [rip + main____str35]
    mov r11, rax
    mov rax, 5
    push r10
    push r11
    push rax
    sub rsp, 32
    mov rax, qword ptr [rsp + 48]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 40]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    movsxd rax, dword ptr [rsp + 32]
    mov rdx, rax
    mov qword ptr [rsp + 16], rdx
    xor eax, eax
    call printf
    add rsp, 56
    movsxd rax, eax
    sub rsp, 8
    call differentsig
    add rsp, 8
    sub rsp, 8
    call differentsig
    add rsp, 8
    mov rax, 10
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Test_Program_rawthing
    add rsp, 8
    movsxd rax, eax
    mov r10, rax
    mov rax, 466
    mov rax, r10
    cmp rax, 466
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str36]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str37]
    mov r10, rax
    mov rax, 41
    push r10
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call LibTest_test_fun
    add rsp, 16
    movsxd rax, eax
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    lea rax, [rip + main____str38]
    mov r10, rax
    mov rax, 50
    push r10
    push rax
    sub rsp, 8
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    call LibTest_Another_other_test_fun
    add rsp, 16
    movsxd rax, eax
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 24]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 40
    movsxd rax, eax
    mov eax, 0x4048f5c3
    mov dword ptr [rbp-212], eax
    mov rax, 0x3ff3c0ca4283de1b
    mov qword ptr [rbp-224], rax
    lea rax, [rbp-212]
    mov qword ptr [rbp-232], rax
    mov rax, qword ptr [rbp-232]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov eax, 0x40c8f5c3
    mov dword ptr [rbp-236], eax
    mov eax, dword ptr [rbp-236]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov eax, dword ptr [rbp-236]
    mov rax, 4
    mov dword ptr [rbp-240], eax
    movsxd rax, dword ptr [rbp-240]
    mov r10, rax
    mov rax, 4
    mov rax, r10
    cmp rax, 4
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str39]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-232]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov eax, dword ptr [rcx]
    mov r10, rax
    mov eax, 0x40c8f5c3
    movd xmm1, eax
    mov rax, r10
    movd xmm0, eax
    cmpss xmm0, xmm1, 0
    movd eax, xmm0
    and eax, 1
    mov r10, rax
    lea rax, [rip + main____str40]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-224]
    mov r10, rax
    mov rax, 0x3ff3c0ca4283de1b
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    cmpsd xmm0, xmm1, 0
    movq rax, xmm0
    and eax, 1
    mov r10, rax
    lea rax, [rip + main____str41]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str42]
    mov r10, rax
    mov rax, qword ptr [rbp-224]
    mov r11, rax
    mov eax, dword ptr [rbp-212]
    movd xmm0, eax
    cvtss2sd xmm0, xmm0
    movq rax, xmm0
    movq xmm1, rax
    mov rax, r11
    movq xmm0, rax
    addsd xmm0, xmm1
    movq rax, xmm0
    push r10
    push rax
    sub rsp, 24
    mov rax, qword ptr [rsp + 32]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsd xmm0, qword ptr [rsp + 24]
    movsd qword ptr [rsp + 8], xmm0
    mov eax, 1
    call printf
    add rsp, 40
    movsxd rax, eax
    mov rax, qword ptr [rbp-224]
    mov r10, rax
    mov eax, dword ptr [rbp-212]
    movd xmm0, eax
    cvtss2sd xmm0, xmm0
    movq rax, xmm0
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    addsd xmm0, xmm1
    movq rax, xmm0
    mov qword ptr [rbp-248], rax
    mov rax, qword ptr [rbp-248]
    mov r10, rax
    mov rax, 0x401e0eeae9ee45c3
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    subsd xmm0, xmm1
    movq rax, xmm0
    mov qword ptr [rbp-256], rax
    mov rax, qword ptr [rbp-256]
    mov r10, rax
    mov rax, 0x0000000000000000
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    cmpsd xmm0, xmm1, 1
    movq rax, xmm0
    and eax, 1
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true20
    jmp main__if_end21
main__if_true20:
    mov rax, 0x0000000000000000
    mov r10, rax
    mov rax, qword ptr [rbp-256]
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    subsd xmm0, xmm1
    movq rax, xmm0
    mov qword ptr [rbp-256], rax
    mov rax, qword ptr [rbp-256]
main__if_end21:
    mov rax, qword ptr [rbp-256]
    mov r10, rax
    mov rax, 0x3eb0c6f7a0b5ed8d
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    cmpsd xmm0, xmm1, 1
    movq rax, xmm0
    and eax, 1
    mov r10, rax
    lea rax, [rip + main____str43]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-72]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-88]
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov r10, rax
    mov rax, 168
    mov rax, r10
    cmp rax, 168
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str44]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0x3fbf9adbb8f8da72
    mov qword ptr [rbp-264], rax
    mov rax, 0x3fb999999999999a
    mov qword ptr [rbp-272], rax
    mov eax, 0x3dcccccd
    mov dword ptr [rbp-276], eax
    mov rax, qword ptr [rbp-272]
    mov r10, rax
    mov rax, 0x3fb999999999999a
    movq xmm1, rax
    mov rax, r10
    movq xmm0, rax
    cmpsd xmm0, xmm1, 0
    movq rax, xmm0
    and eax, 1
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_rhs22
    jmp main__land_false23
main__land_rhs22:
    mov eax, dword ptr [rbp-276]
    mov r10, rax
    mov eax, 0x3dcccccd
    movd xmm1, eax
    mov rax, r10
    movd xmm0, eax
    cmpss xmm0, xmm1, 0
    movd eax, xmm0
    and eax, 1
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_true24
    jmp main__land_false23
main__land_true24:
    mov rax, 1
    jmp main__land_end25
main__land_false23:
    mov r10, rax
    mov rax, 0
main__land_end25:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_rhs26
    jmp main__land_false27
main__land_rhs26:
    mov rax, qword ptr [rbp-264]
    mov r11, rax
    mov rax, 0x3fbf9adbb8f8da72
    movq xmm1, rax
    mov rax, r11
    movq xmm0, rax
    cmpsd xmm0, xmm1, 0
    movq rax, xmm0
    and eax, 1
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_true28
    jmp main__land_false27
main__land_true28:
    mov rax, 1
    jmp main__land_end29
main__land_false27:
    mov r11, rax
    mov rax, 0
main__land_end29:
    push r10
    mov r10, rax
    lea rax, [rip + main____str45]
    push r11
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-72]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-88]
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-280], eax
    movsxd rax, dword ptr [rbp-280]
    mov r10, rax
    mov rax, 168
    mov rax, r10
    cmp rax, 168
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str46]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 12
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rax, r10
    and rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-284], eax
    movsxd rax, dword ptr [rbp-284]
    mov r10, rax
    mov rax, 8
    mov rax, r10
    cmp rax, 8
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str47]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 12
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rax, r10
    or rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-288], eax
    movsxd rax, dword ptr [rbp-288]
    mov r10, rax
    mov rax, 14
    mov rax, r10
    cmp rax, 14
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str48]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 12
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rax, r10
    xor rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-292], eax
    movsxd rax, dword ptr [rbp-292]
    mov r10, rax
    mov rax, 6
    mov rax, r10
    cmp rax, 6
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str49]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 15
    not rax
    movsxd rax, eax
    mov dword ptr [rbp-296], eax
    movsxd rax, dword ptr [rbp-296]
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 16
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str50]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0
    mov dword ptr [rbp-300], eax
    mov rax, 0
    mov dword ptr [rbp-304], eax
main__while_cond30:
    movsxd rax, dword ptr [rbp-304]
    mov r10, rax
    mov rax, 10
    mov rax, r10
    cmp rax, 10
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__while_body31
    jmp main__while_end32
main__while_body31:
    movsxd rax, dword ptr [rbp-304]
    mov r10, rax
    mov rax, 5
    mov rax, r10
    cmp rax, 5
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true34
    jmp main__if_end35
main__if_true34:
    jmp main__while_end32
main__if_end35:
    movsxd rax, dword ptr [rbp-300]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-300], eax
    movsxd rax, dword ptr [rbp-300]
main__while_post33:
    movsxd rax, dword ptr [rbp-304]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-304], eax
    movsxd rax, dword ptr [rbp-304]
    jmp main__while_cond30
main__while_end32:
    movsxd rax, dword ptr [rbp-300]
    mov r10, rax
    mov rax, 5
    mov rax, r10
    cmp rax, 5
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str51]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0
    mov dword ptr [rbp-308], eax
    mov rax, 0
    mov dword ptr [rbp-312], eax
main__while_cond36:
    movsxd rax, dword ptr [rbp-312]
    mov r10, rax
    mov rax, 6
    mov rax, r10
    cmp rax, 6
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__while_body37
    jmp main__while_end38
main__while_body37:
    movsxd rax, dword ptr [rbp-312]
    mov r10, rax
    mov rax, 2
    mov rax, r10
    cmp rax, 2
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true40
    jmp main__if_end41
main__if_true40:
    jmp main__while_post39
main__if_end41:
    movsxd rax, dword ptr [rbp-308]
    mov r10, rax
    movsxd rax, dword ptr [rbp-312]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-308], eax
    movsxd rax, dword ptr [rbp-308]
main__while_post39:
    movsxd rax, dword ptr [rbp-312]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-312], eax
    movsxd rax, dword ptr [rbp-312]
    jmp main__while_cond36
main__while_end38:
    movsxd rax, dword ptr [rbp-308]
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov r11, rax
    mov rax, 3
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str52]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0
    mov dword ptr [rbp-316], eax
    mov rax, 0
    mov dword ptr [rbp-320], eax
main__while_cond42:
    movsxd rax, dword ptr [rbp-320]
    mov r10, rax
    mov rax, 6
    mov rax, r10
    cmp rax, 6
    setl al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__while_body43
    jmp main__while_end44
main__while_body43:
    movsxd rax, dword ptr [rbp-320]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-320], eax
    movsxd rax, dword ptr [rbp-320]
    movsxd rax, dword ptr [rbp-320]
    mov r10, rax
    mov rax, 2
    mov rbx, rax
    mov rax, r10
    cqo
    idiv rbx
    mov rax, rdx
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true45
    jmp main__if_end46
main__if_true45:
    jmp main__while_cond42
main__if_end46:
    movsxd rax, dword ptr [rbp-316]
    mov r10, rax
    movsxd rax, dword ptr [rbp-320]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-316], eax
    movsxd rax, dword ptr [rbp-316]
    movsxd rax, dword ptr [rbp-320]
    mov r10, rax
    mov rax, 5
    mov rax, r10
    cmp rax, 5
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true47
    jmp main__if_end48
main__if_true47:
    jmp main__while_end44
main__if_end48:
    jmp main__while_cond42
main__while_end44:
    movsxd rax, dword ptr [rbp-316]
    mov r10, rax
    mov rax, 1
    mov r11, rax
    mov rax, 3
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov r11, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str53]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0x3039
    mov word ptr [rbp-322], ax
    mov rax, 17
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_alloc
    add rsp, 8
    mov qword ptr [rbp-336], rax
    mov rax, qword ptr [rbp-336]
    mov r10, rax
    mov rax, 0x0
    mov r11, rax
    mov rax, 17
    push r10
    push r11
    push rax
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    movzx eax, byte ptr [rsp + 8]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdx, rax
    call Std_Memory_memset
    add rsp, 24
    mov rax, 15
    mov dword ptr [rbp-340], eax
main__while_cond49:
    movsxd rax, dword ptr [rbp-340]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setge al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__while_body50
    jmp main__while_end51
main__while_body50:
    movzx eax, word ptr [rbp-322]
    mov r10, rax
    movsxd rax, dword ptr [rbp-340]
    mov rbx, rax
    mov rax, r10
    mov rcx, rbx
    shr rax, cl
    movzx eax, ax
    mov r10, rax
    mov rax, 0x1
    mov rbx, rax
    mov rax, r10
    and rax, rbx
    movzx eax, ax
    mov word ptr [rbp-342], ax
    mov rax, qword ptr [rbp-336]
    mov r10, rax
    mov rax, 15
    mov r11, rax
    movsxd rax, dword ptr [rbp-340]
    mov rbx, rax
    mov rax, r11
    sub rax, rbx
    movsxd rax, eax
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    movzx eax, word ptr [rbp-342]
    movzx eax, ax
    mov eax, eax
    mov r11, rax
    mov rax, 1
    mov rax, r11
    cmp rax, 1
    sete al
    movzx eax, al
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__cond_true53
    jmp main__cond_false54
main__cond_true53:
    mov rax, 49
    mov dword ptr [rbp-348], eax
    jmp main__cond_end55
main__cond_false54:
    mov rax, 48
    mov dword ptr [rbp-348], eax
main__cond_end55:
    movsxd rax, dword ptr [rbp-348]
    movsx eax, al
    mov byte ptr [rbp-349], al
    movzx eax, byte ptr [rbp-349]
    movsx eax, al
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-349]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
main__while_post52:
    movsxd rax, dword ptr [rbp-340]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-340], eax
    movsxd rax, dword ptr [rbp-340]
    jmp main__while_cond49
main__while_end51:
    mov rax, qword ptr [rbp-336]
    mov r10, rax
    lea rax, [rip + main____str54]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str55]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 99
    mov r10, rax
    mov rax, 99
    mov rax, r10
    cmp rax, 99
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str56]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 20
    mov r10, rax
    mov rax, 22
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov r10, rax
    mov rax, 42
    mov rax, r10
    cmp rax, 42
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str57]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 5
    mov r10, rax
    mov rax, 5
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    mov r10, rax
    mov rax, 25
    mov rax, r10
    cmp rax, 25
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str58]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 99
    mov r10, rax
    mov rax, 99
    mov rax, r10
    cmp rax, 99
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str59]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 30
    mov r10, rax
    mov rax, 30
    mov rax, r10
    cmp rax, 30
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str60]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 456
    mov r10, rax
    mov rax, 456
    mov rax, r10
    cmp rax, 456
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str61]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 1
    mov r10, rax
    mov rax, 1
    mov rax, r10
    cmp rax, 1
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str62]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str63]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 6
    mov r10, rax
    mov rax, 6
    mov rax, r10
    cmp rax, 6
    setge al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str64]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 1
    mov r10, rax
    mov rax, 1
    mov rax, r10
    cmp rax, 1
    setge al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str65]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 601
    mov r10, rax
    mov rax, 0
    mov r11, rax
    mov rax, 10000
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    movsxd rax, eax
    mov r11, rax
    mov rax, 6
    push r10
    mov r10, rax
    mov rax, 100
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov rbx, rax
    pop rax
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str66]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str67]
    mov r10, rax
    lea rax, [rip + main____str68]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str69]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 64
    mov r10, rax
    mov rax, 8
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str70]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 8
    mov r10, rax
    mov rax, 8
    mov rax, r10
    cmp rax, 8
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__if_true56
    jmp main__if_false57
main__if_true56:
    mov rax, 1
    mov r10, rax
    mov rax, 1
    mov rax, r10
    cmp rax, 1
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str71]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    jmp main__if_end58
main__if_false57:
    mov rax, 1
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str72]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
main__if_end58:
    mov rax, 1
    mov r10, rax
    mov rax, 1
    mov rax, r10
    cmp rax, 1
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str73]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str74]
    mov qword ptr [rbp-360], rax
    mov rax, qword ptr [rbp-360]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str75]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str76]
    mov qword ptr [rbp-368], rax
    mov rax, qword ptr [rbp-368]
    mov r10, rax
    lea rax, [rip + main____str77]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str78]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 392
    mov dword ptr [rbp-372], eax
    movsxd rax, dword ptr [rbp-372]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setg al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str79]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str80]
    mov qword ptr [rbp-384], rax
    mov rax, qword ptr [rbp-384]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str81]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str82]
    mov qword ptr [rbp-392], rax
    mov rax, qword ptr [rbp-392]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str83]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 0
    mov dword ptr [rbp-396], eax
    mov rax, 1
    mov dword ptr [rbp-400], eax
    movsxd rax, dword ptr [rbp-400]
    mov r10, rax
    movsxd rax, dword ptr [rbp-396]
    mov r11, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str84]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str85]
    mov r10, rax
    lea rax, [rip + main____str86]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str87]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    lea rax, [rip + main____str88]
    mov r10, rax
    lea rax, [rip + main____str89]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str90]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    sub rsp, 8
    call LibTest_Another_get_info
    add rsp, 8
    mov r10, rax
    lea rax, [rip + main____str91]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_strcmp
    add rsp, 24
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str92]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 5
    mov r10, rax
    mov rax, 12345
    mov r11, rax
    mov rax, 10
    push r10
    mov r10, rax
    mov rax, 20
    push r11
    mov r11, rax
    mov rax, 30
    push r10
    mov r10, rax
    mov rax, 40
    push r11
    mov r11, rax
    mov rax, 50
    push r10
    push r11
    push rax
    sub rsp, 16
    movsxd rax, dword ptr [rsp + 64]
    mov rdi, rax
    movsxd rax, dword ptr [rsp + 56]
    mov rsi, rax
    movsxd rax, dword ptr [rsp + 48]
    mov rdx, rax
    movsxd rax, dword ptr [rsp + 40]
    mov rcx, rax
    movsxd rax, dword ptr [rsp + 32]
    mov r8, rax
    movsxd rax, dword ptr [rsp + 24]
    mov r9, rax
    movsxd rax, dword ptr [rsp + 16]
    mov qword ptr [rsp + 0], rax
    call LibTest_Another_test_varargs
    add rsp, 72
    movsxd rax, eax
    mov dword ptr [rbp-404], eax
    movsxd rax, dword ptr [rbp-404]
    mov r10, rax
    mov rax, 150
    mov rax, r10
    cmp rax, 150
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str93]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 77
    mov r10, rax
    mov eax, 0x4048f5c3
    movd xmm0, eax
    cvtss2sd xmm0, xmm0
    movq rax, xmm0
    mov r11, rax
    lea rax, [rip + main____str94]
    push r10
    push r11
    push rax
    movsxd rax, dword ptr [rsp + 16]
    mov rdi, rax
    movsd xmm0, qword ptr [rsp + 8]
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call LibTest_Another_test_diffvarargs
    add rsp, 24
    sub rsp, 16
    mov rax, rsp
    mov qword ptr [rbp-416], rax
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 1
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 2
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 10
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 1
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 20
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 2
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 30
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-416]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-416]
    mov r11, rax
    mov rax, 1
    movsxd rax, eax
    push r10
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov rbx, rax
    pop rax
    add rax, rbx
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-416]
    mov r11, rax
    mov rax, 2
    movsxd rax, eax
    push r10
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov rbx, rax
    pop rax
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-420], eax
    movsxd rax, dword ptr [rbp-420]
    mov r10, rax
    mov rax, 60
    mov rax, r10
    cmp rax, 60
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str95]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 12
    mov r10, rax
    mov rax, 4
    mov r11, rax
    mov rax, 3
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str96]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, 8
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movsxd rax, eax
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_alloc
    add rsp, 8
    mov qword ptr [rbp-432], rax
    mov rax, qword ptr [rbp-432]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 100
    mov qword ptr [rbp-440], rax
    mov rax, qword ptr [rbp-440]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-440]
    mov rax, qword ptr [rbp-432]
    mov r10, rax
    mov rax, 1
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 200
    mov qword ptr [rbp-448], rax
    mov rax, qword ptr [rbp-448]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-448]
    mov rax, qword ptr [rbp-432]
    mov r10, rax
    mov rax, 2
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 300
    mov qword ptr [rbp-456], rax
    mov rax, qword ptr [rbp-456]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-456]
    mov rax, qword ptr [rbp-432]
    mov r10, rax
    mov rax, 3
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 400
    mov qword ptr [rbp-464], rax
    mov rax, qword ptr [rbp-464]
    mov rbx, rax
    mov rcx, r10
    mov qword ptr [rcx], rbx
    mov rax, qword ptr [rbp-464]
    mov rax, qword ptr [rbp-432]
    mov r10, rax
    mov rax, 0
    movsxd rax, eax
    mov r11, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r11
    imul rax, rbx
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov r10, rax
    mov rax, qword ptr [rbp-432]
    mov r11, rax
    mov rax, 1
    movsxd rax, eax
    push r10
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov rbx, rax
    pop rax
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-432]
    mov r11, rax
    mov rax, 2
    movsxd rax, eax
    push r10
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov rbx, rax
    pop rax
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-432]
    mov r11, rax
    mov rax, 3
    movsxd rax, eax
    push r10
    mov r10, rax
    mov rax, 8
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    mov rax, qword ptr [rcx]
    mov rbx, rax
    pop rax
    add rax, rbx
    mov qword ptr [rbp-472], rax
    mov rax, qword ptr [rbp-472]
    mov r10, rax
    mov rax, 1000
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    cmp rax, rbx
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str97]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    mov rax, qword ptr [rbp-432]
    push rax
    mov rax, qword ptr [rsp + 0]
    mov rdi, rax
    call Std_Memory_dealloc
    add rsp, 8
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-480], rax
    mov rax, qword ptr [rbp-480]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-480]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-480]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x9
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-480]
    mov r10, rax
    mov rax, 81
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-480]
    push rax
    mov rax, qword ptr [rsp + 0]
    mov rdi, rax
    call Test_Program_passbystruct
    add rsp, 8
    movsxd rax, eax
    mov dword ptr [rbp-484], eax
    movsxd rax, dword ptr [rbp-484]
    mov r10, rax
    mov rax, 81
    mov rax, r10
    cmp rax, 81
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str98]
    push r10
    push rax
    sub rsp, 8
    movzx eax, byte ptr [rsp + 16]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 8]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 24
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-496], rax
    mov rax, qword ptr [rbp-496]
    mov r10, rax
    mov rax, qword ptr [rbp-480]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call Test_Program_returnbystruct
    add rsp, 16
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov rbx, rax
    pop rcx
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-496]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, qword ptr [rbp-480]
    push r10
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call Test_Program_returnbystruct
    add rsp, 16
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    mov rbx, rax
    pop rcx
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-496]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movzx eax, al
    mov eax, eax
    mov r10, rax
    mov rax, 9
    mov rax, r10
    cmp rax, 9
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_rhs59
    jmp main__land_false60
main__land_rhs59:
    mov rax, qword ptr [rbp-496]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    mov r10, rax
    mov rax, 81
    mov rax, r10
    cmp rax, 81
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__land_true61
    jmp main__land_false60
main__land_true61:
    mov rax, 1
    jmp main__land_end62
main__land_false60:
    mov r10, rax
    mov rax, 0
main__land_end62:
    mov r11, rax
    lea rax, [rip + main____str99]
    push r10
    push r11
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    lea rax, [rip + Test_Program_funptr_test]
    mov qword ptr [rbp-504], rax
    lea rax, [rip + Test_Program_funptr_test]
    mov qword ptr [rbp-512], rax
    mov rax, 15
    mov r10, rax
    mov rax, 27
    mov r11, rax
    mov rax, qword ptr [rbp-504]
    push r10
    push r11
    push rax
    pop rax
    mov r11, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    movsxd rax, dword ptr [rsp + 0]
    mov rsi, rax
    call r11
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-516], eax
    movsxd rax, dword ptr [rbp-516]
    mov r10, rax
    mov rax, 42
    mov rax, r10
    cmp rax, 42
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str100]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, 20
    mov r10, rax
    mov rax, 22
    mov r11, rax
    mov rax, qword ptr [rbp-512]
    push r10
    push r11
    push rax
    pop rax
    mov r11, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    movsxd rax, dword ptr [rsp + 0]
    mov rsi, rax
    call r11
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-520], eax
    movsxd rax, dword ptr [rbp-520]
    mov r10, rax
    mov rax, 42
    mov rax, r10
    cmp rax, 42
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str101]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, qword ptr [rbp-504]
    mov qword ptr [rbp-528], rax
    mov rax, qword ptr [rbp-528]
    mov r10, rax
    xor rax, rax
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str102]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, qword ptr [rbp-528]
    mov qword ptr [rbp-536], rax
    mov rax, 7
    mov r10, rax
    mov rax, 35
    mov r11, rax
    mov rax, qword ptr [rbp-536]
    push r10
    push r11
    push rax
    pop rax
    mov r11, rax
    movsxd rax, dword ptr [rsp + 8]
    mov rdi, rax
    movsxd rax, dword ptr [rsp + 0]
    mov rsi, rax
    call r11
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-540], eax
    movsxd rax, dword ptr [rbp-540]
    mov r10, rax
    mov rax, 42
    mov rax, r10
    cmp rax, 42
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str103]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    lea rax, [rip + Test_Program_passbystruct]
    mov qword ptr [rbp-552], rax
    mov rax, qword ptr [rbp-480]
    mov r10, rax
    mov rax, qword ptr [rbp-552]
    push r10
    push rax
    pop rax
    mov r11, rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    call r11
    add rsp, 16
    movsxd rax, eax
    mov dword ptr [rbp-556], eax
    movsxd rax, dword ptr [rbp-556]
    mov r10, rax
    mov rax, 81
    mov rax, r10
    cmp rax, 81
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str104]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, 12
    mov dword ptr [rbp-560], eax
    movsxd rax, dword ptr [rbp-560]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__cond_true63
    jmp main__cond_false64
main__cond_true63:
    mov rax, 0
    mov dword ptr [rbp-564], eax
    jmp main__cond_end65
main__cond_false64:
    movsxd rax, dword ptr [rbp-560]
    mov r10, rax
    movsxd rax, dword ptr [rbp-560]
    push r10
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Test_Program_inlined_function
    add rsp, 8
    movsxd rax, eax
    mov rbx, rax
    pop rax
    imul rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-564], eax
main__cond_end65:
    movsxd rax, dword ptr [rbp-564]
    mov r10, rax
    mov rax, 156
    mov rax, r10
    cmp rax, 156
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str105]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    mov rax, 0
    mov dword ptr [rbp-568], eax
    movsxd rax, dword ptr [rbp-568]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne main__cond_true66
    jmp main__cond_false67
main__cond_true66:
    mov rax, 0
    mov dword ptr [rbp-572], eax
    jmp main__cond_end68
main__cond_false67:
    movsxd rax, dword ptr [rbp-568]
    mov r10, rax
    movsxd rax, dword ptr [rbp-568]
    push r10
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call Test_Program_inlined_function
    add rsp, 8
    movsxd rax, eax
    mov rbx, rax
    pop rax
    imul rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-572], eax
main__cond_end68:
    movsxd rax, dword ptr [rbp-572]
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    sete al
    movzx eax, al
    mov r10, rax
    lea rax, [rip + main____str106]
    push r10
    push rax
    movzx eax, byte ptr [rsp + 8]
    movsx eax, al
    mov rdi, rax
    mov rax, qword ptr [rsp + 0]
    mov rsi, rax
    call Test_Program_assert
    add rsp, 16
    lea rax, [rip + Test_Program_inlined_function]
    mov qword ptr [rbp-584], rax
    mov rax, qword ptr [rbp-584]
    mov qword ptr [rbp-592], rax
    mov rax, qword ptr [rbp-592]
    mov qword ptr [rbp-600], rax
    lea rax, [rip + main____str107]
    mov r10, rax
    mov rax, 5
    mov r11, rax
    mov rax, qword ptr [rbp-600]
    push r10
    push r11
    push rax
    pop rax
    mov r11, rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call r11
    add rsp, 8
    movsxd rax, eax
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    movsxd rax, dword ptr [rsp + 16]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 32
    movsxd rax, eax
    lea rax, [rip + main____str108]
    mov r10, rax
    lea rax, [rip + main____str109]
    push r10
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 24]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    mov rax, qword ptr [rsp + 16]
    mov rsi, rax
    mov qword ptr [rsp + 8], rsi
    xor eax, eax
    call printf
    add rsp, 32
    movsxd rax, eax
    lea rax, [rip + main____str110]
    push rax
    sub rsp, 8
    mov rax, qword ptr [rsp + 8]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    xor eax, eax
    call printf
    add rsp, 16
    movsxd rax, eax
    movsxd rax, dword ptr [rbp-280]
    leave
    ret

.globl Test_Program_funptr_test
Test_Program_funptr_test:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    mov dword ptr [rbp-16], esi
    movsxd rax, dword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-16]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

.globl Test_Program_passbystruct
Test_Program_passbystruct:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov rax, qword ptr [rbp-8]
    mov rcx, rax
    movsxd rax, dword ptr [rcx]
    leave
    ret

.globl Test_Program_returnbystruct
Test_Program_returnbystruct:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Test_Program_construct_thing
Test_Program_construct_thing:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    sub rsp, 8
    mov rax, rsp
    mov qword ptr [rbp-8], rax
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 0
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x0
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 4
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov r10, rax
    mov rax, 0x5
    mov byte ptr [rbp-9], al
    movzx eax, byte ptr [rbp-9]
    mov rbx, rax
    mov rcx, r10
    mov byte ptr [rcx], bl
    movzx eax, byte ptr [rbp-9]
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    mov rax, 10
    mov dword ptr [rbp-16], eax
    movsxd rax, dword ptr [rbp-16]
    mov rbx, rax
    mov rcx, r10
    mov dword ptr [rcx], ebx
    movsxd rax, dword ptr [rbp-16]
    mov rax, qword ptr [rbp-8]
    leave
    ret

.globl Test_Program_strlen
Test_Program_strlen:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov qword ptr [rbp-8], rdi
    mov rax, 0
    mov dword ptr [rbp-12], eax
Test_Program_strlen__while_cond0:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-12]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_strlen__while_body1
    jmp Test_Program_strlen__while_end2
Test_Program_strlen__while_body1:
    movsxd rax, dword ptr [rbp-12]
    mov dword ptr [rbp-16], eax
    movsxd rax, dword ptr [rbp-12]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-12], eax
    movsxd rax, dword ptr [rbp-16]
    jmp Test_Program_strlen__while_cond0
Test_Program_strlen__while_end2:
    movsxd rax, dword ptr [rbp-12]
    leave
    ret

.globl Test_Program_strcmp
Test_Program_strcmp:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov qword ptr [rbp-8], rdi
    mov qword ptr [rbp-16], rsi
    mov rax, 0
    mov dword ptr [rbp-20], eax
Test_Program_strcmp__while_cond0:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_strcmp__land_rhs3
    jmp Test_Program_strcmp__land_false4
Test_Program_strcmp__land_rhs3:
    mov rax, qword ptr [rbp-16]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_strcmp__land_true5
    jmp Test_Program_strcmp__land_false4
Test_Program_strcmp__land_true5:
    mov rax, 1
    jmp Test_Program_strcmp__land_end6
Test_Program_strcmp__land_false4:
    mov r10, rax
    mov rax, 0
Test_Program_strcmp__land_end6:
    mov r11, rax
    mov rax, 0
    mov rax, r11
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_strcmp__while_body1
    jmp Test_Program_strcmp__while_end2
Test_Program_strcmp__while_body1:
    mov rax, qword ptr [rbp-8]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r11, rax
    mov rax, qword ptr [rbp-16]
    push r10
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    cmp rax, rbx
    setne al
    movzx eax, al
    mov r10, rax
    mov rax, 0
    mov rax, r10
    cmp rax, 0
    setne al
    movzx eax, al
    cmp rax, 0
    jne Test_Program_strcmp__if_true7
    jmp Test_Program_strcmp__if_end8
Test_Program_strcmp__if_true7:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    leave
    ret
Test_Program_strcmp__if_end8:
    movsxd rax, dword ptr [rbp-20]
    mov dword ptr [rbp-24], eax
    movsxd rax, dword ptr [rbp-20]
    mov r10, rax
    mov rax, 1
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    mov dword ptr [rbp-20], eax
    movsxd rax, dword ptr [rbp-24]
    jmp Test_Program_strcmp__while_cond0
Test_Program_strcmp__while_end2:
    mov rax, qword ptr [rbp-8]
    mov r10, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov r10, rax
    mov rax, qword ptr [rbp-16]
    mov r11, rax
    movsxd rax, dword ptr [rbp-20]
    movsxd rax, eax
    mov rbx, rax
    mov rax, r11
    add rax, rbx
    mov rcx, rax
    movzx eax, byte ptr [rcx]
    movsx eax, al
    movsx eax, al
    movsxd rax, eax
    mov rbx, rax
    mov rax, r10
    sub rax, rbx
    movsxd rax, eax
    leave
    ret

.globl Test_Program_abort
Test_Program_abort:
    mov rax, 1
    push rax
    movsxd rax, dword ptr [rsp + 0]
    mov rdi, rax
    call exit
    add rsp, 8
    xor eax, eax
    ret

.globl Test_Program_test_overflow
Test_Program_test_overflow:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, 30000
    mov word ptr [rbp-2], ax
    mov rax, 10000
    mov word ptr [rbp-4], ax
    movsx eax, word ptr [rbp-2]
    mov r10, rax
    movsx eax, word ptr [rbp-4]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsx eax, ax
    leave
    ret

.globl Test_Program_test_mul_overflow
Test_Program_test_mul_overflow:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov rax, 0xc8
    mov byte ptr [rbp-1], al
    mov rax, 0xc8
    mov byte ptr [rbp-2], al
    movzx eax, byte ptr [rbp-1]
    mov r10, rax
    movzx eax, byte ptr [rbp-2]
    mov rbx, rax
    mov rax, r10
    imul rax, rbx
    movzx eax, al
    leave
    ret

.globl differentsig
differentsig:
    lea rax, [rip + differentsig____str111]
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    xor eax, eax
    call printf
    add rsp, 24
    movsxd rax, eax
    xor eax, eax
    ret

.globl Test_Program_rawthing
Test_Program_rawthing:
    push rbp
    mov rbp, rsp
    sub rsp, 16
    mov dword ptr [rbp-8], edi
    mov rax, 456
    mov r10, rax
    movsxd rax, dword ptr [rbp-8]
    mov rbx, rax
    mov rax, r10
    add rax, rbx
    movsxd rax, eax
    leave
    ret

.section .rodata
.balign 1
Test_Program_printwrapper____str0:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0

.balign 1
Test_Program_printwrapper____str1:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0

.balign 1
Test_Program_printwrapper____str2:
    .byte 0x5f, 0x5f, 0x46, 0x55, 0x4e, 0x43, 0x54, 0x49, 0x4f, 0x4e, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0

.balign 1
Test_Program_printwrapper____str3:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x25, 0x73, 0x21, 0x20, 0x4e, 0x75, 0x6d, 0x62, 0x65, 0x72, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
Test_Program_printwrapper____str4:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0

.balign 1
Test_Program_assert____str5:
    .byte 0x54, 0x45, 0x53, 0x54, 0x20, 0x50, 0x41, 0x53, 0x53, 0x45, 0x44, 0x3a, 0x20, 0x25, 0x73, 0x0a, 0

.balign 1
Test_Program_assert____str6:
    .byte 0x54, 0x45, 0x53, 0x54, 0x20, 0x46, 0x41, 0x49, 0x4c, 0x3a, 0x20, 0x25, 0x73, 0x0a, 0

.balign 1
main____str7:
    .byte 0x22, 0x62, 0x75, 0x66, 0x22, 0x20, 0x61, 0x6c, 0x6c, 0x6f, 0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0

.balign 1
main____str8:
    .byte 0x6d, 0x65, 0x6d, 0x73, 0x65, 0x74, 0x20, 0x74, 0x6f, 0x20, 0x7a, 0x65, 0x72, 0x6f, 0

.balign 1
main____str9:
    .byte 0x57, 0x68, 0x61, 0x74, 0x27, 0x73, 0x20, 0x79, 0x6f, 0x75, 0x72, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x3f, 0x20, 0x3e, 0x20, 0

.balign 1
main____str10:
    .byte 0x70, 0x72, 0x69, 0x6e, 0x74, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x20, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x65, 0x64, 0x20, 0x6c, 0x65, 0x6e, 0x67, 0x74, 0x68, 0

.balign 1
main____str11:
    .byte 0x4e, 0x6f, 0x20, 0x69, 0x6e, 0x70, 0x75, 0x74, 0x20, 0x72, 0x65, 0x61, 0x64, 0x2c, 0x20, 0x75, 0x73, 0x69, 0x6e, 0x67, 0x20, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x0a, 0

.balign 1
main____str12:
    .byte 0x73, 0x74, 0x72, 0x6c, 0x65, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x20, 0x72, 0x65, 0x61, 0x64, 0

.balign 1
main____str13:
    .byte 0x43, 0x48, 0x61, 0x6e, 0x63, 0x65, 0

.balign 1
main____str14:
    .byte 0x55, 0x73, 0x65, 0x72, 0x20, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x65, 0x64, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0

.balign 1
main____str15:
    .byte 0x55, 0x73, 0x65, 0x72, 0x20, 0x64, 0x69, 0x64, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x65, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x74, 0x68, 0x65, 0x20, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x20, 0x28, 0x6e, 0x6f, 0x74, 0x20, 0x72, 0x65, 0x71, 0x75, 0x69, 0x72, 0x65, 0x64, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x74, 0x65, 0x73, 0x74, 0x73, 0x29, 0

.balign 1
main____str16:
    .byte 0x46, 0x72, 0x65, 0x65, 0x69, 0x6e, 0x67, 0x20, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x2e, 0x2e, 0x2e, 0x0a, 0

.balign 1
main____str17:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str18:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x2d, 0x32, 0x35, 0x35, 0x33, 0x36, 0

.balign 1
main____str19:
    .byte 0x74, 0x65, 0x73, 0x74, 0x5f, 0x6d, 0x75, 0x6c, 0x5f, 0x6f, 0x76, 0x65, 0x72, 0x66, 0x6c, 0x6f, 0x77, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x36, 0x34, 0

.balign 1
main____str20:
    .byte 0x36, 0x20, 0x2a, 0x20, 0x37, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x32, 0

.balign 1
main____str21:
    .byte 0x32, 0x30, 0x20, 0x2f, 0x20, 0x2d, 0x34, 0x20, 0x3d, 0x3d, 0x20, 0x2d, 0x35, 0

.balign 1
main____str22:
    .byte 0x32, 0x30, 0x20, 0x25, 0x20, 0x36, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0

.balign 1
main____str23:
    .byte 0x30, 0x78, 0x46, 0x46, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0x35, 0x35, 0

.balign 1
main____str24:
    .byte 0x43, 0x61, 0x6c, 0x6c, 0x69, 0x6e, 0x67, 0x20, 0x74, 0x65, 0x73, 0x74, 0x5f, 0x63, 0x68, 0x61, 0x6e, 0x63, 0x65, 0x28, 0x29, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str25:
    .byte 0x43, 0x61, 0x6c, 0x6c, 0x69, 0x6e, 0x67, 0x20, 0x65, 0x78, 0x74, 0x65, 0x6e, 0x64, 0x65, 0x64, 0x20, 0x43, 0x20, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x74, 0x65, 0x73, 0x74, 0x28, 0x29, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str26:
    .byte 0x64, 0x65, 0x72, 0x65, 0x66, 0x2e, 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x34, 0

.balign 1
main____str27:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x64, 0x65, 0x72, 0x65, 0x66, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x32, 0x33, 0

.balign 1
main____str28:
    .byte 0x61, 0x64, 0x64, 0x70, 0x74, 0x72, 0x5b, 0x30, 0x5d, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x35, 0

.balign 1
main____str29:
    .byte 0x61, 0x64, 0x64, 0x70, 0x74, 0x72, 0x5b, 0x31, 0x5d, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x32, 0x33, 0

.balign 1
main____str30:
    .byte 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x76, 0x69, 0x61, 0x20, 0x69, 0x6e, 0x74, 0x70, 0x74, 0x72, 0x3a, 0x20, 0x25, 0x70, 0x0a, 0

.balign 1
main____str31:
    .byte 0x73, 0x65, 0x74, 0x74, 0x65, 0x72, 0x2f, 0x67, 0x65, 0x74, 0x74, 0x65, 0x72, 0x20, 0x76, 0x69, 0x61, 0x20, 0x3d, 0x3e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str32:
    .byte 0x31, 0x20, 0x3c, 0x3c, 0x20, 0x31, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0

.balign 1
main____str33:
    .byte 0x34, 0x20, 0x3e, 0x3e, 0x20, 0x31, 0x20, 0x3d, 0x3d, 0x20, 0x32, 0

.balign 1
main____str34:
    .byte 0x74, 0x79, 0x70, 0x65, 0x6f, 0x66, 0x28, 0x29, 0x20, 0x3d, 0x20, 0x25, 0x73, 0x20, 0x77, 0x69, 0x74, 0x68, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x6f, 0x66, 0x28, 0x29, 0x20, 0x3d, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str35:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x43, 0x68, 0x61, 0x6e, 0x63, 0x65, 0x2e, 0x53, 0x6d, 0x61, 0x6c, 0x6c, 0x54, 0x65, 0x73, 0x74, 0x3a, 0x3a, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0

.balign 1
main____str36:
    .byte 0x72, 0x61, 0x77, 0x74, 0x68, 0x69, 0x6e, 0x67, 0x28, 0x31, 0x30, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0x36, 0x36, 0

.balign 1
main____str37:
    .byte 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str38:
    .byte 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x20, 0x25, 0x64, 0x0a, 0

.balign 1
main____str39:
    .byte 0x61, 0x63, 0x20, 0x3d, 0x3d, 0x20, 0x34, 0

.balign 1
main____str40:
    .byte 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x70, 0x74, 0x72, 0x5b, 0x30, 0x5d, 0x20, 0x3d, 0x3d, 0x20, 0x36, 0x2e, 0x32, 0x38, 0x66, 0

.balign 1
main____str41:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x2e, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0

.balign 1
main____str42:
    .byte 0x45, 0x78, 0x70, 0x65, 0x63, 0x74, 0x20, 0x61, 0x64, 0x64, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x6f, 0x66, 0x20, 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x2b, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x76, 0x61, 0x6c, 0x20, 0x3d, 0x20, 0x25, 0x6c, 0x66, 0x0a, 0

.balign 1
main____str43:
    .byte 0x64, 0x6f, 0x75, 0x62, 0x6c, 0x65, 0x76, 0x61, 0x6c, 0x20, 0x2b, 0x20, 0x66, 0x6c, 0x6f, 0x61, 0x74, 0x76, 0x61, 0x6c, 0x20, 0x7e, 0x3d, 0x20, 0x37, 0x2e, 0x35, 0x31, 0x34, 0x35, 0x36, 0x38, 0

.balign 1
main____str44:
    .byte 0x53, 0x75, 0x6d, 0x20, 0x6f, 0x66, 0x20, 0x76, 0x61, 0x72, 0x69, 0x2e, 0x74, 0x65, 0x73, 0x74, 0x56, 0x61, 0x72, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x61, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x2e, 0x73, 0x6d, 0x61, 0x6c, 0x6c, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x36, 0x38, 0

.balign 1
main____str45:
    .byte 0x66, 0x31, 0x32, 0x38, 0x2c, 0x20, 0x66, 0x36, 0x34, 0x2c, 0x20, 0x66, 0x33, 0x32, 0x20, 0x61, 0x73, 0x73, 0x69, 0x67, 0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0

.balign 1
main____str46:
    .byte 0x66, 0x69, 0x6e, 0x61, 0x6c, 0x20, 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x65, 0x71, 0x75, 0x61, 0x6c, 0x73, 0x20, 0x31, 0x36, 0x38, 0

.balign 1
main____str47:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x41, 0x4e, 0x44, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str48:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x4f, 0x52, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str49:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x58, 0x4f, 0x52, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str50:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x4e, 0x4f, 0x54, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str51:
    .byte 0x62, 0x72, 0x65, 0x61, 0x6b, 0x20, 0x65, 0x78, 0x69, 0x74, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0x20, 0x65, 0x61, 0x72, 0x6c, 0x79, 0

.balign 1
main____str52:
    .byte 0x63, 0x6f, 0x6e, 0x74, 0x69, 0x6e, 0x75, 0x65, 0x20, 0x73, 0x6b, 0x69, 0x70, 0x73, 0x20, 0x69, 0x74, 0x65, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x62, 0x75, 0x74, 0x20, 0x73, 0x74, 0x69, 0x6c, 0x6c, 0x20, 0x61, 0x64, 0x76, 0x61, 0x6e, 0x63, 0x65, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0

.balign 1
main____str53:
    .byte 0x62, 0x72, 0x65, 0x61, 0x6b, 0x20, 0x61, 0x6e, 0x64, 0x20, 0x63, 0x6f, 0x6e, 0x74, 0x69, 0x6e, 0x75, 0x65, 0x20, 0x63, 0x6f, 0x6f, 0x70, 0x65, 0x72, 0x61, 0x74, 0x65, 0x20, 0x69, 0x6e, 0x20, 0x77, 0x68, 0x69, 0x6c, 0x65, 0x2d, 0x6c, 0x6f, 0x6f, 0x70, 0

.balign 1
main____str54:
    .byte 0x30, 0x30, 0x31, 0x31, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x31, 0x31, 0x31, 0x30, 0x30, 0x31, 0

.balign 1
main____str55:
    .byte 0x42, 0x69, 0x74, 0x77, 0x69, 0x73, 0x65, 0x20, 0x72, 0x65, 0x70, 0x72, 0x65, 0x73, 0x65, 0x6e, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x6f, 0x66, 0x20, 0x31, 0x32, 0x33, 0x34, 0x35, 0x20, 0x69, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0

.balign 1
main____str56:
    .byte 0x6f, 0x62, 0x6a, 0x65, 0x63, 0x74, 0x2d, 0x6c, 0x69, 0x6b, 0x65, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x65, 0x78, 0x70, 0x61, 0x6e, 0x64, 0x73, 0x20, 0x74, 0x6f, 0x20, 0x6c, 0x69, 0x74, 0x65, 0x72, 0x61, 0x6c, 0

.balign 1
main____str57:
    .byte 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x2d, 0x73, 0x74, 0x79, 0x6c, 0x65, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x65, 0x78, 0x70, 0x61, 0x6e, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str58:
    .byte 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x61, 0x72, 0x67, 0x75, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x73, 0x74, 0x61, 0x79, 0x20, 0x70, 0x61, 0x72, 0x65, 0x6e, 0x74, 0x68, 0x65, 0x73, 0x69, 0x7a, 0x65, 0x64, 0

.balign 1
main____str59:
    .byte 0x23, 0x69, 0x66, 0x2f, 0x23, 0x65, 0x6c, 0x73, 0x65, 0x20, 0x62, 0x72, 0x61, 0x6e, 0x63, 0x68, 0x20, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str60:
    .byte 0x23, 0x65, 0x6c, 0x69, 0x66, 0x20, 0x62, 0x72, 0x61, 0x6e, 0x63, 0x68, 0x20, 0x73, 0x65, 0x6c, 0x65, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str61:
    .byte 0x23, 0x75, 0x6e, 0x64, 0x65, 0x66, 0x20, 0x63, 0x6c, 0x65, 0x61, 0x72, 0x73, 0x20, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x69, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x20, 0x66, 0x6f, 0x72, 0x20, 0x67, 0x75, 0x61, 0x72, 0x64, 0x73, 0

.balign 1
main____str62:
    .byte 0x5f, 0x5f, 0x43, 0x48, 0x41, 0x4e, 0x43, 0x45, 0x5f, 0x5f, 0x20, 0x62, 0x75, 0x69, 0x6c, 0x74, 0x69, 0x6e, 0x20, 0x64, 0x65, 0x66, 0x69, 0x6e, 0x65, 0x64, 0

.balign 1
main____str63:
    .byte 0x6d, 0x61, 0x6a, 0x6f, 0x72, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x75, 0x6e, 0x64, 0x65, 0x72, 0x20, 0x31, 0

.balign 1
main____str64:
    .byte 0x6d, 0x69, 0x6e, 0x6f, 0x72, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x72, 0x65, 0x66, 0x6c, 0x65, 0x63, 0x74, 0x73, 0x20, 0x70, 0x72, 0x6f, 0x67, 0x72, 0x65, 0x73, 0x73, 0

.balign 1
main____str65:
    .byte 0x70, 0x61, 0x74, 0x63, 0x68, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x73, 0x65, 0x74, 0

.balign 1
main____str66:
    .byte 0x61, 0x67, 0x67, 0x72, 0x65, 0x67, 0x61, 0x74, 0x65, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x6f, 0x6e, 0x65, 0x6e, 0x74, 0x73, 0

.balign 1
main____str67:
    .byte 0x30, 0x2e, 0x36, 0x2e, 0x31, 0

.balign 1
main____str68:
    .byte 0x30, 0x2e, 0x36, 0x2e, 0x31, 0

.balign 1
main____str69:
    .byte 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x30, 0x2e, 0x36, 0x2e, 0x31, 0

.balign 1
main____str70:
    .byte 0x5f, 0x5f, 0x50, 0x4f, 0x49, 0x4e, 0x54, 0x45, 0x52, 0x5f, 0x57, 0x49, 0x44, 0x54, 0x48, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x6f, 0x66, 0x28, 0x76, 0x6f, 0x69, 0x64, 0x2a, 0x29, 0

.balign 1
main____str71:
    .byte 0x5f, 0x5f, 0x49, 0x53, 0x36, 0x34, 0x42, 0x49, 0x54, 0x5f, 0x5f, 0x20, 0x72, 0x65, 0x66, 0x6c, 0x65, 0x63, 0x74, 0x73, 0x20, 0x36, 0x34, 0x2d, 0x62, 0x69, 0x74, 0

.balign 1
main____str72:
    .byte 0x5f, 0x5f, 0x49, 0x53, 0x36, 0x34, 0x42, 0x49, 0x54, 0x5f, 0x5f, 0x20, 0x72, 0x65, 0x66, 0x6c, 0x65, 0x63, 0x74, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x36, 0x34, 0x2d, 0x62, 0x69, 0x74, 0

.balign 1
main____str73:
    .byte 0x5f, 0x5f, 0x4c, 0x49, 0x4e, 0x55, 0x58, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x63, 0x72, 0x6f, 0x20, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0

.balign 1
main____str74:
    .byte 0x2e, 0x2f, 0x74, 0x65, 0x73, 0x74, 0x73, 0x2f, 0x65, 0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x73, 0x2f, 0x63, 0x61, 0x6c, 0x6c, 0x5f, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x66, 0x2e, 0x63, 0x65, 0

.balign 1
main____str75:
    .byte 0x5f, 0x5f, 0x46, 0x49, 0x4c, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0

.balign 1
main____str76:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x50, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0

.balign 1
main____str77:
    .byte 0x54, 0x65, 0x73, 0x74, 0x2e, 0x50, 0x72, 0x6f, 0x67, 0x72, 0x61, 0x6d, 0

.balign 1
main____str78:
    .byte 0x5f, 0x5f, 0x4d, 0x4f, 0x44, 0x55, 0x4c, 0x45, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x6d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0

.balign 1
main____str79:
    .byte 0x5f, 0x5f, 0x4c, 0x49, 0x4e, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x70, 0x6f, 0x73, 0x69, 0x74, 0x69, 0x76, 0x65, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0

.balign 1
main____str80:
    .byte 0x4e, 0x6f, 0x76, 0x20, 0x32, 0x34, 0x20, 0x32, 0x30, 0x32, 0x35, 0

.balign 1
main____str81:
    .byte 0x5f, 0x5f, 0x44, 0x41, 0x54, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0

.balign 1
main____str82:
    .byte 0x30, 0x38, 0x3a, 0x35, 0x38, 0x3a, 0x32, 0x38, 0

.balign 1
main____str83:
    .byte 0x5f, 0x5f, 0x54, 0x49, 0x4d, 0x45, 0x5f, 0x5f, 0x20, 0x79, 0x69, 0x65, 0x6c, 0x64, 0x73, 0x20, 0x6e, 0x6f, 0x6e, 0x2d, 0x65, 0x6d, 0x70, 0x74, 0x79, 0x20, 0x76, 0x61, 0x6c, 0x75, 0x65, 0

.balign 1
main____str84:
    .byte 0x5f, 0x5f, 0x43, 0x4f, 0x55, 0x4e, 0x54, 0x45, 0x52, 0x5f, 0x5f, 0x20, 0x69, 0x6e, 0x63, 0x72, 0x65, 0x6d, 0x65, 0x6e, 0x74, 0x73, 0x20, 0x73, 0x65, 0x71, 0x75, 0x65, 0x6e, 0x74, 0x69, 0x61, 0x6c, 0x6c, 0x79, 0

.balign 1
main____str85:
    .byte 0x6d, 0x61, 0x69, 0x6e, 0

.balign 1
main____str86:
    .byte 0x6d, 0x61, 0x69, 0x6e, 0

.balign 1
main____str87:
    .byte 0x5f, 0x5f, 0x46, 0x55, 0x4e, 0x43, 0x54, 0x49, 0x4f, 0x4e, 0x5f, 0x5f, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x6d, 0x61, 0x69, 0x6e, 0

.balign 1
main____str88:
    .byte 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0

.balign 1
main____str89:
    .byte 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0

.balign 1
main____str90:
    .byte 0x63, 0x6f, 0x6e, 0x73, 0x74, 0x61, 0x6e, 0x74, 0x20, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x20, 0x6d, 0x65, 0x72, 0x67, 0x69, 0x6e, 0x67, 0

.balign 1
main____str91:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0

.balign 1
main____str92:
    .byte 0x4c, 0x69, 0x62, 0x54, 0x65, 0x73, 0x74, 0x2e, 0x41, 0x6e, 0x6f, 0x74, 0x68, 0x65, 0x72, 0x3a, 0x3a, 0x67, 0x65, 0x74, 0x5f, 0x69, 0x6e, 0x66, 0x6f, 0x28, 0x29, 0x20, 0x77, 0x6f, 0x72, 0x6b, 0x73, 0

.balign 1
main____str93:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x31, 0x35, 0x30, 0

.balign 1
main____str94:
    .byte 0x76, 0x61, 0x72, 0x61, 0x72, 0x67, 0x73, 0x20, 0x74, 0x65, 0x73, 0x74, 0

.balign 1
main____str95:
    .byte 0x66, 0x69, 0x78, 0x65, 0x64, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x36, 0x30, 0

.balign 1
main____str96:
    .byte 0x66, 0x69, 0x78, 0x65, 0x64, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x69, 0x7a, 0x65, 0x20, 0x69, 0x73, 0x20, 0x63, 0x6f, 0x72, 0x72, 0x65, 0x63, 0x74, 0

.balign 1
main____str97:
    .byte 0x64, 0x79, 0x6e, 0x61, 0x6d, 0x69, 0x63, 0x20, 0x61, 0x72, 0x72, 0x61, 0x79, 0x20, 0x73, 0x75, 0x6d, 0x20, 0x69, 0x73, 0x20, 0x31, 0x30, 0x30, 0x30, 0

.balign 1
main____str98:
    .byte 0x70, 0x61, 0x73, 0x73, 0x62, 0x79, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x38, 0x31, 0

.balign 1
main____str99:
    .byte 0x72, 0x65, 0x74, 0x75, 0x72, 0x6e, 0x62, 0x79, 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x6d, 0x61, 0x74, 0x63, 0x68, 0x65, 0x73, 0x20, 0x69, 0x6e, 0x70, 0x75, 0x74, 0

.balign 1
main____str100:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0

.balign 1
main____str101:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x76, 0x69, 0x61, 0x20, 0x73, 0x65, 0x63, 0x6f, 0x6e, 0x64, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0

.balign 1
main____str102:
    .byte 0x72, 0x61, 0x77, 0x20, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x6e, 0x6f, 0x74, 0x20, 0x6e, 0x75, 0x6c, 0x6c, 0

.balign 1
main____str103:
    .byte 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x5f, 0x74, 0x65, 0x73, 0x74, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x76, 0x69, 0x61, 0x20, 0x72, 0x61, 0x77, 0x20, 0x70, 0x6f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x20, 0x69, 0x73, 0x20, 0x34, 0x32, 0

.balign 1
main____str104:
    .byte 0x73, 0x74, 0x72, 0x75, 0x63, 0x74, 0x20, 0x66, 0x75, 0x6e, 0x70, 0x74, 0x72, 0x20, 0x72, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x20, 0x69, 0x73, 0x20, 0x38, 0x31, 0

.balign 1
main____str105:
    .byte 0x69, 0x6e, 0x6c, 0x69, 0x6e, 0x61, 0x62, 0x6c, 0x65, 0x5f, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x31, 0x32, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x31, 0x35, 0x36, 0

.balign 1
main____str106:
    .byte 0x69, 0x6e, 0x6c, 0x69, 0x6e, 0x61, 0x62, 0x6c, 0x65, 0x5f, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x28, 0x30, 0x29, 0x20, 0x3d, 0x3d, 0x20, 0x30, 0

.balign 1
main____str107:
    .byte 0x56, 0x61, 0x6c, 0x75, 0x65, 0x20, 0x69, 0x73, 0x20, 0x25, 0x64, 0x21, 0x0a, 0

.balign 1
main____str108:
    .byte 0x41, 0x72, 0x63, 0x68, 0x20, 0x69, 0x73, 0x20, 0x25, 0x73, 0x2e, 0x0a, 0

.balign 1
main____str109:
    .byte 0x78, 0x38, 0x36, 0

.balign 1
main____str110:
    .byte 0x41, 0x6c, 0x6c, 0x20, 0x74, 0x65, 0x73, 0x74, 0x73, 0x20, 0x63, 0x6f, 0x6d, 0x70, 0x6c, 0x65, 0x74, 0x65, 0x64, 0x20, 0x73, 0x75, 0x63, 0x63, 0x65, 0x73, 0x73, 0x66, 0x75, 0x6c, 0x6c, 0x79, 0x21, 0x0a, 0

.balign 1
differentsig____str111:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x20, 0x66, 0x72, 0x6f, 0x6d, 0x20, 0x73, 0x69, 0x67, 0x66, 0x75, 0x6e, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x21, 0x0a, 0


.att_syntax prefix
