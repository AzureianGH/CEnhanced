// ChanceCode GNU assembler x86-64 backend (.intel_syntax) output

.intel_syntax noprefix

.extern Std_IO_printnl_ptr_to_char
.extern Std_String_strcmp_ptr_to_char_ptr_to_char
.extern Std_String_strlen_ptr_to_char
.extern Std_String_strcpy_ptr_to_char_ptr_to_char
.extern Std_String_strcat_ptr_to_char_ptr_to_char
.extern Std_String_strchr_ptr_to_char_char
.extern Std_String_strstr_ptr_to_char_ptr_to_char
.extern Std_String_ends_with_ptr_to_char_ptr_to_char
.extern Std_String_strdup_ptr_to_char
.extern Std_String_strncmp_ptr_to_char_ptr_to_char_i32
.extern Std_Memory_memcpy_ptr_to_void_ptr_to_void_i32
.extern Std_Memory_memset_ptr_to_void_u8_i32
.extern Std_Memory_memcmp_ptr_to_void_ptr_to_void_i32
.extern Std_Memory_memmove_ptr_to_void_ptr_to_void_i32
.extern Std_Memory_memchr_ptr_to_void_u8_i32
.extern Std_Memory_memrchr_ptr_to_void_u8_i32
.extern Std_Memory_bzero_ptr_to_void_i32
.extern Std_Memory_bcopy_ptr_to_void_ptr_to_void_i32
.extern Std_Memory_alloc_i32
.extern Std_Memory_dealloc_ptr_to_void
.extern Std_Memory_realloc_ptr_to_void_i32
.extern Std_Memory_cleanalloc_i32_i32
.extern Std_Memory_swap_ptr_to_void_ptr_to_void_i32
.extern Std_IO_format_ptr_to_char
.extern Std_IO_print_ptr_to_char
.extern Std_IO_sprintf_ptr_to_char_i32_ptr_to_char
.extern Std_IO_readin
.extern Std_File_open_ptr_to_char_ptr_to_char
.extern Std_File_close_ptr_to_void
.extern Std_File_read_ptr_to_void_ptr_to_void_i32
.extern Std_File_write_ptr_to_void_ptr_to_void_i32
.extern Std_File_flush_ptr_to_void
.extern Std_File_seek_ptr_to_void_i64_i32
.extern Std_File_tell_ptr_to_void
.extern Std_File_read_all_ptr_to_char_ptr_to_ptr_to_void_ptr_to_i32
.extern Std_File_write_all_ptr_to_char_ptr_to_void_i32
.extern Std_File_exists_ptr_to_char
.extern Std_File_remove_ptr_to_char
.extern Std_File_getcwd
.extern Std_Net_last_error
.extern Std_Net_init
.extern Std_Net_cleanup
.extern Std_Net_tcp_listen_i32_i32
.extern Std_Net_tcp_accept_i32
.extern Std_Net_tcp_connect_ptr_to_char_i32
.extern Std_Net_tcp_send_all_i32_ptr_to_void_i32
.extern Std_Net_tcp_recv_i32_ptr_to_void_i32
.extern Std_Net_tcp_shutdown_i32_i32
.extern Std_Net_tcp_close_i32
.extern Std_Net_htonl_u32
.extern Std_Net_htons_u16
.extern Std_Net_inetaddr_ptr_to_char
.extern Std_Environment_exit_i32
.extern __cert__malloc
.extern __cert__free
.extern __cert__realloc
.extern __cert__memcpy
.extern __cert__memset
.extern __cert__strlen
.extern __cert__strcmp
.extern __cert__exception_enter_try
.extern __cert__exception_leave_try
.extern __cert__exception_has_pending
.extern __cert__exception_clear
.extern __cert__exception_get_code
.extern __cert__exception_get_category
.extern __cert__exception_get_message
.extern __cert__exception_get_file
.extern __cert__exception_get_line
.extern __cert__exception_get_symbol
.extern __cert__exception_matches_type
.extern __cert__exception_propagate
.extern __cert__runtime_error_ex
.extern __cert__runtime_error
.extern __cert__panic
.extern __cert__GC__prep_exit
.extern __cert__GC__prep_enter
.extern __cert__GC__add_ptr
.extern __cert__GC__remove_ptr
.extern __cert__new
.extern __cert__delete
.extern __cert__box_i64
.extern __cert__box_f64
.extern __cert__box_ptr
.extern __cert__unbox_i64
.extern __cert__unbox_f64
.extern __cert__unbox_ptr
.extern __cert__object_is_type
.extern __cert__null_deref
.extern __cert__null_deref_fallback

.text

__cc_hidden_fn1:
    lea rax, [rip + __cc_hidden_fn1____str0]
    push rax
    sub rsp, 16
    mov rax, qword ptr [rsp + 16]
    mov rdi, rax
    mov qword ptr [rsp + 0], rdi
    xor eax, eax
    call Std_IO_printnl_ptr_to_char
    add rsp, 24
    movsxd rax, eax
    mov rax, 0
    ret

.globl main
main:
    sub rsp, 8
    call __cc_hidden_fn1
    add rsp, 8
    movsxd rax, eax
    ret

.section __TEXT,__const
.balign 1
__cc_hidden_fn1____str0:
    .byte 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x77, 0x6f, 0x72, 0x6c, 0x64, 0x21, 0


.att_syntax prefix
