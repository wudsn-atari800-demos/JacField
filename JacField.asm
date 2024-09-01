;
;	>>> JacField-1995.xex Preface <<<
;
;	(c) 2010-12-22 JAC!
;
;	This loader allows the old executable to run from standard DOS.

cnt	= $14
blend	= $80

;===============================================================
;	Patched version, which loads the block from $1000-$1cc3 to $2000-$2cc3 instead 

;===============================================================

	opt h+
	org $9000

;===============================================================

	.proc fader	;Fade out all 9 color registers
fade_loop
	ldy #0		;Assume all zero
	ldx #8
color_loop
	lda 704,x
	pha
	and #$f0
	sta fade_color
	pla
	and #15
	beq zero
	sec
	sbc #1
	ora #$00
fade_color = *-1
	tay		;At least one not zero

zero	sta 704,x
	dex
	bpl color_loop
	lda cnt
	clc
	adc #2
wait	cmp cnt
	bne wait
	cpy #0
	bne fade_loop
	rts
	.endp		;End of fade

	ini fader

;===============================================================

	org $8800

chr	ins "atasm/JacField.chr"

	.proc main

	mwa #dl $230
	mva #>chr $2f4

	mva #13 blend
blendup	jsr kernel
	lda cnt
	and #3
	bne blendup
	lda blend
	asl
	asl
	sta $d200
	sta $d202
	lda blend
	ora #$a0
	sta $d201
	sta $d203
	dec blend
	bne blendup
	lda #0
	sta $d201
	sta $d203

wait	jsr kernel

	lda $d20f
	and #12
	cmp #12
	bne continue
	lda $d01f
	and #7
	cmp #7
	bne continue
	jmp wait
continue

blenddown
	jsr kernel
	lda cnt
	and #3
	bne blenddown
	lda blend
	asl
	asl
	sta $d200
	sta $d202
	lda blend
	ora #$a0
	sta $d201
	sta $d203
	inc blend
	lda blend
	cmp #13
	bne blenddown
	lda #0
	sta $d201
	sta $d203

	jmp jacfield

;===============================================================

	.proc kernel
	lda #15
wait	cmp $d40b
	bne wait
	ldx #0
loop	txa
	and #7
	adc blend
	tay
	lda colors,y
	sta $d40a
	sta $d017
	inx
	cpx #192
	bne loop
	rts
	.endp

colors	.byte $04,$06,$08,$0a,$0c,$0e,$0c,$0a,$08,$06,$04,$02
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00
;===============================================================

	.proc jacfield
	ldx #$0d
	ldy #0
copy
	lda $2000,y
source_adr = *-2
	sta $1000,y
target_adr = *-2
	lda #0
	sta $2000,y
clear_adr = *-2
	iny
	bne copy
	inc source_adr+1
	inc target_adr+1
	inc clear_adr+1
	dex
	bne copy
	lda #$ff
	sta $d301
	jmp $1000
	.endp

;===============================================================

	.local dl
	.byte $70,$70,$70
	.byte $42
	.word text
	.byte $70
	.byte $02,$02,$02,$02,$02
	.byte $70
	.byte $02,$02,$02,$02
	.byte $70
	.byte $02,$02,$02,$02,$70,$02,$02,$70,$02,$02
	.byte $41
	.word dl

	.endl

;===============================================================

text	.byte "            JACFIELD - 1995             "
	.byte " THIS IS THE ORIGINAL IMPLEMENTATION OF "
	.byte " THE SILLY THINGS DOT FIELD. IT STARTED "
	.byte "     IN 1993, AND IT WOULD HAVE BEEN    "
	.byte "         RELEASED IN POLAND 1995        "
	.byte "             BUT NEVER WAS...           "
	.byte "  THE ROUTINE DID NOT YET DOUBLE-BUFFER "
	.byte "  CORRECTLY AND THERE WAS ONLY ENOUGH   "
	.byte "   RASTER TIME LEFT FOR A VERY SIMPLE   "
	.byte "    TUNE AND A SINGLE ANIMATED PIXEL.   "
	.byte "IT TOOK 15 YEARS BEFORE THE TUNE TURNED "
	.byte " INTO 505'S TUNE AND BEFORE THE SINGLE  "
	.byte "   ANIMATED PIXEL BECAME 256 ANIMATED   "
	.byte "            PIXELS AT 50 FPS.           "
	.byte "     IF YOU WANT TO CONTACT ME VISIT    "
	.byte "              WWW.WUDSN.COM             "
	.byte "BECAUSE THE INFORMATION IN THE NEXT PART"
	.byte "IS OUTDATED FOR 15 YEARS :-)   JAC! 2010"
	.endp

	opt h-
	ins "JacField-1995.xex"	

	opt h+
	run main
