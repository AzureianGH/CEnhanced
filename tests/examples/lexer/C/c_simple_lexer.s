	.arch armv8.5-a
	.build_version macos,  15, 0
	.text
	.cstring
	.align	3
lC0:
	.ascii "EOF\0"
	.align	3
lC1:
	.ascii "NUMBER\0"
	.align	3
lC2:
	.ascii "IDENT\0"
	.align	3
lC3:
	.ascii "LPAREN\0"
	.align	3
lC4:
	.ascii "RPAREN\0"
	.align	3
lC5:
	.ascii "PLUS\0"
	.align	3
lC6:
	.ascii "MINUS\0"
	.align	3
lC7:
	.ascii "STAR\0"
	.align	3
lC8:
	.ascii "SLASH\0"
	.align	3
lC9:
	.ascii "UNKNOWN\0"
	.data
	.align	3
_kTokenNames:
	.xword	lC0
	.xword	lC1
	.xword	lC2
	.xword	lC3
	.xword	lC4
	.xword	lC5
	.xword	lC6
	.xword	lC7
	.xword	lC8
	.xword	lC9
	.text
	.align	2
_is_alpha:
LFB27:
	sub	sp, sp, #16
LCFI0:
	strb	w0, [sp, 15]
	ldrsb	w0, [sp, 15]
	cmp	w0, 96
	ble	L2
	ldrsb	w0, [sp, 15]
	cmp	w0, 122
	ble	L3
L2:
	ldrsb	w0, [sp, 15]
	cmp	w0, 64
	ble	L4
	ldrsb	w0, [sp, 15]
	cmp	w0, 90
	ble	L3
L4:
	ldrsb	w0, [sp, 15]
	cmp	w0, 95
	bne	L5
L3:
	mov	w0, 1
	b	L7
L5:
	mov	w0, 0
L7:
	add	sp, sp, 16
LCFI1:
	ret
LFE27:
	.align	2
_is_digit:
LFB28:
	sub	sp, sp, #16
LCFI2:
	strb	w0, [sp, 15]
	ldrsb	w0, [sp, 15]
	cmp	w0, 47
	ble	L9
	ldrsb	w0, [sp, 15]
	cmp	w0, 57
	bgt	L9
	mov	w0, 1
	b	L11
L9:
	mov	w0, 0
L11:
	add	sp, sp, 16
LCFI3:
	ret
LFE28:
	.align	2
_skip_whitespace:
LFB29:
	stp	x29, x30, [sp, -32]!
LCFI4:
	mov	x29, sp
LCFI5:
	str	x0, [x29, 24]
	b	L13
L15:
	ldr	x0, [x29, 24]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 24]
	str	x1, [x0, 8]
L13:
	ldr	x0, [x29, 24]
	ldr	x1, [x0]
	ldr	x0, [x29, 24]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrsb	w0, [x0]
	cmp	w0, 0
	beq	L16
	ldr	x0, [x29, 24]
	ldr	x1, [x0]
	ldr	x0, [x29, 24]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrsb	w0, [x0]
	and	w0, w0, 255
	bl	_isspace
	cmp	w0, 0
	bne	L15
L16:
	nop
	ldp	x29, x30, [sp], 32
LCFI6:
	ret
LFE29:
	.align	2
_make_token:
LFB30:
	sub	sp, sp, #64
LCFI7:
	mov	x3, x8
	str	x0, [sp, 24]
	str	w1, [sp, 20]
	str	x2, [sp, 8]
	ldr	w0, [sp, 20]
	str	w0, [sp, 40]
	ldr	x0, [sp, 24]
	ldr	x1, [x0]
	ldr	x0, [sp, 8]
	add	x0, x1, x0
	str	x0, [sp, 48]
	ldr	x0, [sp, 24]
	ldr	x0, [x0, 8]
	mov	w1, w0
	ldr	x0, [sp, 8]
	sub	w0, w1, w0
	str	w0, [sp, 56]
	add	x2, sp, 40
	ldp	x0, x1, [x2]
	ldr	x2, [x2, 16]
	stp	x0, x1, [x3]
	str	x2, [x3, 16]
	add	sp, sp, 64
LCFI8:
	ret
LFE30:
	.align	2
_lex_token:
LFB31:
	stp	x29, x30, [sp, -64]!
LCFI9:
	mov	x29, sp
LCFI10:
	str	x19, [sp, 16]
LCFI11:
	mov	x19, x8
	str	x0, [x29, 40]
	ldr	x0, [x29, 40]
	bl	_skip_whitespace
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	str	x0, [x29, 56]
	ldr	x0, [x29, 40]
	ldr	x1, [x0]
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrb	w0, [x0]
	strb	w0, [x29, 55]
	ldrsb	w0, [x29, 55]
	cmp	w0, 0
	bne	L20
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 0
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L20:
	ldrsb	w0, [x29, 55]
	bl	_is_alpha
	cmp	w0, 0
	beq	L22
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 40]
	str	x1, [x0, 8]
	b	L23
L24:
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 40]
	str	x1, [x0, 8]
L23:
	ldr	x0, [x29, 40]
	ldr	x1, [x0]
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrsb	w0, [x0]
	bl	_is_alpha
	cmp	w0, 0
	bne	L24
	ldr	x0, [x29, 40]
	ldr	x1, [x0]
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrsb	w0, [x0]
	bl	_is_digit
	cmp	w0, 0
	bne	L24
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 2
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L22:
	ldrsb	w0, [x29, 55]
	bl	_is_digit
	cmp	w0, 0
	beq	L25
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 40]
	str	x1, [x0, 8]
	b	L26
L27:
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 40]
	str	x1, [x0, 8]
L26:
	ldr	x0, [x29, 40]
	ldr	x1, [x0]
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x0, x1, x0
	ldrsb	w0, [x0]
	bl	_is_digit
	cmp	w0, 0
	bne	L27
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 1
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L25:
	ldr	x0, [x29, 40]
	ldr	x0, [x0, 8]
	add	x1, x0, 1
	ldr	x0, [x29, 40]
	str	x1, [x0, 8]
	ldrsb	w0, [x29, 55]
	cmp	w0, 47
	beq	L28
	cmp	w0, 47
	bgt	L29
	cmp	w0, 45
	beq	L30
	cmp	w0, 45
	bgt	L29
	cmp	w0, 43
	beq	L31
	cmp	w0, 43
	bgt	L29
	cmp	w0, 42
	beq	L32
	cmp	w0, 42
	bgt	L29
	cmp	w0, 40
	beq	L33
	cmp	w0, 41
	beq	L34
	b	L29
L31:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 5
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L30:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 6
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L32:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 7
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L28:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 8
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L33:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 3
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L34:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 4
	ldr	x0, [x29, 40]
	bl	_make_token
	b	L21
L29:
	mov	x8, x19
	ldr	x2, [x29, 56]
	mov	w1, 9
	ldr	x0, [x29, 40]
	bl	_make_token
L21:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI12:
	ret
LFE31:
	.cstring
	.align	3
lC10:
	.ascii "%-8s '%.*s'\12\0"
	.text
	.align	2
_print_token:
LFB32:
	sub	sp, sp, #64
LCFI13:
	stp	x29, x30, [sp, 32]
LCFI14:
	add	x29, sp, 32
LCFI15:
	str	x19, [sp, 48]
LCFI16:
	mov	x19, x0
	ldr	w1, [x19]
	adrp	x0, _kTokenNames@PAGE
	add	x0, x0, _kTokenNames@PAGEOFF;
	uxtw	x1, w1
	ldr	x0, [x0, x1, lsl 3]
	ldr	w1, [x19, 16]
	ldr	x2, [x19, 8]
	str	x2, [sp, 16]
	str	w1, [sp, 8]
	str	x0, [sp]
	adrp	x0, lC10@PAGE
	add	x0, x0, lC10@PAGEOFF;
	bl	_printf
	nop
	ldp	x29, x30, [sp, 32]
	ldr	x19, [sp, 48]
	add	sp, sp, 64
LCFI17:
	ret
LFE32:
	.cstring
	.align	3
lC11:
	.ascii "sum = a1 + 23*(foo - 5)/bar\0"
	.text
	.align	2
	.globl _main
_main:
LFB33:
	stp	x29, x30, [sp, -112]!
LCFI18:
	mov	x29, sp
LCFI19:
	str	w0, [x29, 60]
	str	x1, [x29, 48]
	ldr	w0, [x29, 60]
	cmp	w0, 1
	ble	L37
	ldr	x0, [x29, 48]
	ldr	x0, [x0, 8]
	str	x0, [x29, 104]
	b	L38
L37:
	adrp	x0, lC11@PAGE
	add	x0, x0, lC11@PAGEOFF;
	str	x0, [x29, 104]
L38:
	ldr	x0, [x29, 104]
	str	x0, [x29, 88]
	str	xzr, [x29, 96]
L41:
	add	x0, x29, 88
	add	x1, x29, 64
	mov	x8, x1
	bl	_lex_token
	add	x0, x29, 16
	add	x1, x29, 64
	ldp	x2, x3, [x1]
	ldr	x1, [x1, 16]
	stp	x2, x3, [x0]
	str	x1, [x0, 16]
	add	x0, x29, 16
	bl	_print_token
	ldr	w0, [x29, 64]
	cmp	w0, 0
	bne	L41
	mov	w0, 0
	ldp	x29, x30, [sp], 112
LCFI20:
	ret
LFE33:
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x1
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB27-.
	.set L$set$2,LFE27-LFB27
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB27
	.long L$set$3
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB28-.
	.set L$set$6,LFE28-LFB28
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI2-LFB28
	.long L$set$7
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB29-.
	.set L$set$10,LFE29-LFB29
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI4-LFB29
	.long L$set$11
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$14,LEFDE7-LASFDE7
	.long L$set$14
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB30-.
	.set L$set$15,LFE30-LFB30
	.quad L$set$15
	.uleb128 0
	.byte	0x4
	.set L$set$16,LCFI7-LFB30
	.long L$set$16
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$17,LCFI8-LCFI7
	.long L$set$17
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$18,LEFDE9-LASFDE9
	.long L$set$18
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB31-.
	.set L$set$19,LFE31-LFB31
	.quad L$set$19
	.uleb128 0
	.byte	0x4
	.set L$set$20,LCFI9-LFB31
	.long L$set$20
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$21,LCFI10-LCFI9
	.long L$set$21
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$22,LCFI11-LCFI10
	.long L$set$22
	.byte	0x93
	.uleb128 0x6
	.byte	0x4
	.set L$set$23,LCFI12-LCFI11
	.long L$set$23
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$24,LEFDE11-LASFDE11
	.long L$set$24
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB32-.
	.set L$set$25,LFE32-LFB32
	.quad L$set$25
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI13-LFB32
	.long L$set$26
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$27,LCFI14-LCFI13
	.long L$set$27
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x4
	.set L$set$29,LCFI16-LCFI15
	.long L$set$29
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$30,LCFI17-LCFI16
	.long L$set$30
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$31,LEFDE13-LASFDE13
	.long L$set$31
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB33-.
	.set L$set$32,LFE33-LFB33
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI18-LFB33
	.long L$set$33
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$34,LCFI19-LCFI18
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$35,LCFI20-LCFI19
	.long L$set$35
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE13:
	.ident	"GCC: (Homebrew GCC 15.2.0) 15.2.0"
	.subsections_via_symbols
