;	>> FieldPlot <<
;
;	Assmbler routine for test in BASIC, ATASM format.
;	INIT=USR($600)
;	Plays $20 frames of 256 pixels with x/y coordinates at $6000 to $9fff and loops.
;
;	(c)_28-06-93_by_JAC!_(Abgänger!!)
;

base	equ $4800
llo	equ base
lhi	equ base+$100
btab	equ base+$200
sm	equ $b000

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
*	equ $5000
org1	equ *
	jmp init
;	jmp doit

pixtab	db $80,$40,$20,$10,$08,$04,$02,$01

;================================================================

init	pla
	set p1,[sm-$380]
	ldx #0
init1	lda p1
	sta llo,x
	clc
	adc #16
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

;================================================================
	lda #0
	sta $d40e
	lda #$60
	sta x1

doit	jsr clear
	lda x1
	sta doit2+2
	clc
	adc #1
	sta doit1+2
	ldx #0
doit1	ldy $ff00,x		;Ytab.
doit2	lda $ff00,x		;Xtab.
	sta doit3+1
	lsr
	lsr
	lsr
	ora llo,y
	sta p1
	lda lhi,y
	sta p1+1
doit3	lda btab
	ldy #0
	ora (p1),y
	sta (p1),y
	inx
	bne doit1

	inc x1
	inc x1
	lda x1
	cmp #$a0
	bne doit
	lda #$60
	sta x1
	jmp doit

clear	lda #0
	sta $d01a
	lda #68
	cmp $d40b
	bne *-3
	lda #6
	sta $d01a
	lda #0
	tax
clear1	sta sm,x
	sta sm+$100,x
	sta sm+$200,x
	sta sm+$300,x
	sta sm+$400,x
	sta sm+$500,x
	sta sm+$600,x
	sta sm+$80,x
	sta sm+$180,x
	sta sm+$280,x
	sta sm+$380,x
	sta sm+$480,x
	sta sm+$580,x
	sta sm+$680,x
	inx
	bpl clear1
	lda #4
	sta $d01a
	rts

end1	dw 0,0
	dw org1,[end1-org1]
