	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 13, 0
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:                                ; %entry
	sub	sp, sp, #64
	stp	x22, x21, [sp, #16]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #32]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #48]             ; 16-byte Folded Spill
	.cfi_def_cfa_offset 64
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	mov	w0, #24
	bl	_malloc
	mov	x19, x0
	mov	w0, #24
	bl	_malloc
	mov	x20, x0
	mov	w0, #24
	bl	_malloc
Lloh0:
	adrp	x21, l_vcon_name@PAGE
Lloh1:
	adrp	x22, l_vcon_name.4@PAGE
Lloh2:
	add	x21, x21, l_vcon_name@PAGEOFF
Lloh3:
	add	x22, x22, l_vcon_name.4@PAGEOFF
	cmp	x22, x21
	str	x20, [x19, #16]
	strb	wzr, [x0, #8]
	str	x21, [x0]
	str	x0, [x20, #16]
	str	x22, [x20]
	str	x22, [x19]
	b.eq	LBB0_5
; %bb.1:                                ; %else
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
	b.ne	LBB0_4
; %bb.2:                                ; %then26
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
	b.ne	LBB0_4
; %bb.3:                                ; %then43
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
	b.eq	LBB0_6
LBB0_4:                                 ; %else69
	bl	_nomatch
LBB0_5:                                 ; %ifcon73
	ldp	x29, x30, [sp, #48]             ; 16-byte Folded Reload
	mov	w0, wzr
	ldp	x20, x19, [sp, #32]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #64
	ret
LBB0_6:                                 ; %then63
Lloh4:
	adrp	x0, l_fmt.2@PAGE
	mov	w8, #3
Lloh5:
	add	x0, x0, l_fmt.2@PAGEOFF
	str	x8, [sp]
	bl	_printf
	b	LBB0_5
	.loh AdrpAdd	Lloh1, Lloh3
	.loh AdrpAdd	Lloh0, Lloh2
	.loh AdrpAdd	Lloh4, Lloh5
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
