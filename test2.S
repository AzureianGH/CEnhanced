// ChanceCode macOS ARM64 backend output
.build_version macos, 15, 0
.section __TEXT,__text,regular,pure_instructions

.extern _ArmTest_testfun

.globl _ArmTest_testfun
.p2align 2
_ArmTest_testfun:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
