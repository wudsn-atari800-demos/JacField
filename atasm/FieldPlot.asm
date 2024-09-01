;	>> FieldPlot <<
;
;	Assmbler routine for test in BASIC, ATASM format.
;	INIT=USR($600)
;	DOIT=USR($603, page x/page y) plots 256 pixels with x/y coordinates and returns
;
;	(c)_28-06-93_by_JAC!_(Abgänger!!)
;

base	equ $4800
llo	equ base
lhi	equ base+$100
btab	equ base+$200

p1	equ $20
x1	equ $22
x2	equ $23

;================================================================
set	macro
	lda #<\2
	sta \1
	lda #>\2
	sta \1+1
	endm

add	macro
	clc
	lda \1
	adc #<\2
	sta \1
	lda \1+1
	adc #>\2
	sta \1+1
	endm

sub	macro
	sec
	lda \1
	sbc #<\2
	sta \1
	lda \1+1
	sbc #>\2
	sta \1+1
	endm

;================================================================
*	equ $600
org1	equ *
	jmp init
	jmp doit

;================================================================

init	pla
	pla
	pla
	sta x1
	lda 88
	sta p1
	lda 89
	sta p1+1
	ldx #0
init1	lda p1
	sta llo,x
	clc
	adc x1
	sta p1
	lda p1+1
	sta lhi,x
	adc #0
	sta p1+1
	txa
	and #7
	tay
	lda pixtab,y
	sta btab,x
	inx
	bne init1
	rts

pixtab	db $80,$40,$20,$10,$08,$04,$02,$01

;================================================================
doit	pla
	pla
	sta doit2+2		;Set_Ybase.
	clc
	adc #1
	sta doit1+2		;Set_Xbase.
	pla

	lda 88
	sta p1
	lda 89
	sta p1+1
	lda #$bf
	sec
	sbc p1+1
	tax
	lda #0
	tay
doit0	sta (p1),y		;Clear_(88)_to_$c000
	iny
	sta (p1),y
	iny
	sta (p1),y
	iny
	sta (p1),y
	iny
	bne doit0
	inc p1+1
	dex
	bne doit0

	ldx #0
doit1	ldy $ff00,x		;Ytab.
doit2	lda $ff00,x		;Xtab.
	sta doit3+1
	lsr
	lsr
	lsr
	clc
	adc llo,y
	sta p1
	lda lhi,y
	adc #0
	sta p1+1
doit3	lda btab
	ldy #0
	ora (p1),y
	sta (p1),y
	inx
	bne doit1
	rts

end1	dw 0,0
	dw org1,[end1-org1]
