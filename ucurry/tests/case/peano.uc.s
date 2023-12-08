	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 13, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	stp	x22, x21, [sp, #-48]!           ; 16-byte Folded Spill
	stp	x20, x19, [sp, #16]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #32]             ; 16-byte Folded Spill
	add	x29, sp, #32
	sub	sp, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	w0, #24
	bl	_malloc
Lloh0:
	adrp	x21, l_vcon_name@PAGE
	strb	wzr, [x0, #8]
Lloh1:
	add	x21, x21, l_vcon_name@PAGEOFF
	stur	x0, [x29, #-40]
	str	x21, [x0]
	mov	w0, #24
	bl	_malloc
Lloh2:
	adrp	x22, l_vcon_name.4@PAGE
	ldur	x8, [x29, #-40]
Lloh3:
	add	x22, x22, l_vcon_name.4@PAGEOFF
	stur	x0, [x29, #-48]
	str	x8, [x0, #16]
	str	x22, [x0]
	mov	w0, #24
	bl	_malloc
	cmp	x21, x22
	strb	wzr, [x0, #8]
	str	x21, [x0]
	b.eq	LBB0_2
; %bb.1:
	mov	w8, #1
	b	LBB0_3
LBB0_2:                                 ; %then
	mov	w0, #24
	bl	_malloc
	mov	x10, sp
	ldr	x9, [x0, #16]
	sub	x8, x10, #16
	strb	wzr, [x0, #8]
	str	x21, [x0]
	mov	sp, x8
	mov	w8, wzr
	stur	x9, [x10, #-16]
LBB0_3:                                 ; %ifcon
Lloh4:
	adrp	x0, l_fmt.2@PAGE
Lloh5:
	add	x0, x0, l_fmt.2@PAGEOFF
	str	x8, [sp, #-16]!
	bl	_printf
	add	sp, sp, #16
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	cmp	x22, x22
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x19, #16]
	str	x22, [x19]
	b.eq	LBB0_5
; %bb.4:
	mov	w8, #10
	b	LBB0_6
LBB0_5:                                 ; %then29
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	w8, #2
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x19, #16]
	str	x22, [x19]
LBB0_6:                                 ; %ifcon48
Lloh6:
	adrp	x0, l_fmt.2@PAGE
Lloh7:
	add	x0, x0, l_fmt.2@PAGEOFF
	str	x8, [sp, #-16]!
	bl	_printf
	add	sp, sp, #16
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	x20, x0
	mov	w0, #24
	bl	_malloc
	cmp	x22, x21
	str	x22, [x20]
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x20, #16]
	str	x20, [x19, #16]
	str	x22, [x19]
	b.eq	LBB0_11
; %bb.7:                                ; %else67
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	x20, x0
	mov	w0, #24
	bl	_malloc
	cmp	x22, x22
	str	x22, [x20]
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x20, #16]
	str	x20, [x19, #16]
	str	x22, [x19]
	b.ne	LBB0_10
; %bb.8:                                ; %then83
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	x20, x0
	mov	w0, #24
	bl	_malloc
	cmp	x22, x22
	str	x22, [x20]
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x20, #16]
	str	x20, [x19, #16]
	str	x22, [x19]
	b.ne	LBB0_10
; %bb.9:                                ; %then101
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	x20, x0
	mov	w0, #24
	bl	_malloc
	mov	x8, x0
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x20, #16]
	str	x22, [x20]
	ldr	x8, [x8]
	str	x20, [x19, #16]
	str	x22, [x19]
	cmp	x8, x21
	b.eq	LBB0_13
LBB0_10:                                ; %ifcon133.sink.split
	bl	_nomatch
	mov	w8, w0
	b	LBB0_12
LBB0_11:
	mov	w8, #2
LBB0_12:                                ; %ifcon133
Lloh8:
	adrp	x19, l_fmt.2@PAGE
Lloh9:
	add	x19, x19, l_fmt.2@PAGEOFF
	mov	x0, x19
	str	x8, [sp, #-16]!
	bl	_printf
	add	sp, sp, #16
	mov	w8, #4
	mov	x0, x19
	str	x8, [sp, #-16]!
	bl	_printf
	add	sp, sp, #16
	mov	w0, wzr
	sub	sp, x29, #32
	ldp	x29, x30, [sp, #32]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #16]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp], #48             ; 16-byte Folded Reload
	ret
LBB0_13:
	mov	w8, #3
	b	LBB0_12
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh6, Lloh7
	.loh AdrpAdd	Lloh8, Lloh9
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_fmt:                                  ; @fmt
	.asciz	"%d"

l_fmt.1:                                ; @fmt.1
	.asciz	"%s"

l_fmt.2:                                ; @fmt.2
	.asciz	"%d\n"

l_fmt.3:                                ; @fmt.3
	.asciz	"%s\n"

l_vcon_name:                            ; @vcon_name
	.asciz	"Zero"

l_vcon_name.4:                          ; @vcon_name.4
	.asciz	"Addone"

.subsections_via_symbols
