;	>> FieldPlot <<
;
;	(c)_28-06-93_by_JAC!_(Abgänger!!)
;	(r)_29-12-93_by_JAC!_(Infooooormer!!)

bcol1	= $74
bcol2	= $76
bcol3	= $78

fcol1	= $34
fcol2	= $38
fcol3	= $3e

sound	= $3800		;"SND:Great$3800.snd"
sndinit	= $3c4b
sndplay	= $3c7f
field	= $4400		;"DAT:JacField.dat".
chr	= $8400		;"CHR:JacField.chr".

sm1	= $8800		;Buffer1.
sm1a	= $9000		;Buffer1,after_lms.
sm2	= $a800		;Buffer2.
sm2a	= $b000		;Buffer2,after_lms.

addtab	= $1f00

x1	= $80
x2	= $81
p1	= $82
p2	= $84
tp	= $86
sp	= $88
smup	= $90			;Screen_Up.
fump	= $91			;Frame_Up.
fstep	= $92			;Frame_stepwidth.
scrcnt	= $98
scrpos	= $9a

dliup	= $fe			;DLI_counter.
cnt	= $ff			;Frame_counter.

base	= $2000			;Heap_base.
llo	= base			;LineAdr_low_bytes.
lhi1	= base+$100		;LineAdr_high_bytes,buffer_1.
lhi2	= base+$200		;LineAdr_high_bytes,buffer_2.
oldlo1	= base+$300		;PixelAdr_low_bytes,buffer1.
oldhi1	= base+$400		;PixelAdr_high_bytes,buffer1.
oldlo2	= base+$500		;PixelAdr_low_bytes,buffer2.
oldhi2	= base+$600		;PixelAdr_high_bytes,buffer2.
btab1	= base+$700		;Field_Color_1 (4 pixels repeated).
btab2	= base+$800		;Field_Color_3 (4 pixels repeated).
btab3	= base+$900		;Field_Color_3 (4 pixels repeated).
otab	= base+$a00		;Field_Offset_x_bytes (x div 4).
bhigh	= base+$b00		;Color_table_(btab1/2/3_highword).
berg	= base+$c00		;4_Pages containing the 4 mirror images.

;================================================================

	opt l+

	.macro col
	lda #:1
	sta $d012
	.endm


	.macro set
	lda #<:2
	sta :1
	lda #>:2
	sta :1+1
	.endm

;================================================================

	.macro	Zta		;STA $nn
	.byte $85,<:1
	.endm

	.macro	Ztx		;STX $nn
	.byte $86,<:1
	.endm

	.macro	Zty		;STY $nn
	.byte $84,<:1
	.endm

;================================================================


	org sound
	ins "JacField-Great$3800.snd"
	org chr
	ins "JacField.chr"
	org field
	ins "JacField.dat"

;=================================================================
;
;	>> P-Zero-Code <<_(c)_27-06-93_by_JAC!
;	ATTENTION:_$f0,$f1_are_used_by_"sndplay"
;
;=================================================================
	org $9c
org2	= *

Aclr	ldy #>oldlo1		;Clear_Plots.
	lda smup
	.byte $f0,$02		;beq Aclr1
	ldy #>oldlo2
Aclr1	Zty Aclr3+2		;Which_buffer_?
	iny
	Zty Aclr2+2

	ldx #0
	lda #0
Aclr2	ldy oldhi1,x
	Zty Aclr4+2		;Hi_byte.
Aclr3	ldy oldlo1,x		;Lo_byte.
Aclr4	sta $ff00,y		;Clear.
	inx			;Next.
	.byte $d0, $f2		;bne Aclr2, Last_?
	rts

;=================================================================

Aplot1	ldx #0
Aplotb	ldy berg,x
	lda bhigh,y
	sta Aplot9+2
;	.byte $8d,$e4,$00	;Force same code as 1995
	tya
Aplot2	adc $ff00,x		;Ytab.
	tay
Aplot3	lda lhi1,y		;Y_pos.
	Zta Aplotp+2
Aplot4	sta oldhi1,x
	lda llo,y		;Y_pos.
Aplot6	ldy $ff00,x		;Xtab.
	adc otab,y		;X_pos_(byte).
	Zta Aplotp+1
Aplot8	sta oldlo1,x
Aplot9	lda btab1,y		;X_pos_(pixel).
	ldy #0
Aplotp	ora $ffff		;ORA (P1)
	.byte $91,<[Aplotp+1]	;STA (P1),y
	inx			;Next_Point.
	.byte $d0, $d0		;bne Aplotb
	rts
end2

;=================================================================

	org $1000
org1	jmp start

	.byte 125,'-JACFIELD.COM- Demo',$9b
	.byte '(c) 12/95 by JAC! of Wudsn ursel for TLB.',$9b
	.byte 'If you want to contact me:',$9b
	.byte 'Peter Dell',$9b
	.byte 'In der Klink 32',$9b
	.byte '66265 Heusweiler',$9b
	.byte 'Germany',$9b,$9b
	.byte 'http://fsinfo.cs.uni-sb.de/~jac',$9b
	.byte 'email: jac@cscip.uni-sb.de',$9b,0

dc	= $0e
dl	.byte $70,$70,$c0,$20,$d2
	.word scrsm
	.byte $70,$60
	.byte $40+dc
dl1	.word sm2
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte $c0+dc
dl2	.word sm2+$800
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc,dc
	.byte $41
	.word dl

pixtab1	.byte $40,$10,$04,$01
pixtab2	.byte $80,$20,$08,$02
pixtab3	.byte $c0,$30,$0c,$03

textcol	.byte 31,20,10,0

scrsm	.ds 48

scrcode	.byte ' !", $24, "#$%&`()*+,-./0123456789:;<=>?'
	.byte '@abcdefghijklmnopqrstuvwxyz[\]^_'

scrtxt	.byte '            '
	.byte 'welcome ... i`m jac! of wudsn ursel and back here '
	.byte 'to blow your minds ... watch out for 256 dots! '
	.byte '                                                      '
	.byte 'great? well, let`s spin it!'
	.byte '                                                      '
	.byte 'and now to the limits ... '
	.byte '                                                      '
	.byte '                              '
scrres	.byte ' this demo is a special contribution to the latest '
	.byte 'production from tlb ... and hopefully release in poland '
	.byte 'in the end of december 1995. it`s been a hard way to get '
	.byte 'this finished in time ... because sometimes x-max parties '
	.byte 'can take quite a long time - as i know now. well, i hope '
	.byte 'you like this. there are 256 dots rotated, projected '
	.byte 'and animated in 1 frame - so who needs a pentium ;)   '
	.byte 'you can reach me on the internet "http://fsinfo.cs.uni-sb.de/ jac" '
	.byte 'or by email via "jac@cscip.uni-sb.de". '
	.byte 'or snail mail to: peter dell , in der klink 32, 66265 heusweiler, germany '
	.byte '        '
	.byte 'this demo is entierely coded by myself, except for the sound '
	.byte 'which is taken from an old compyshop mag. '
	.byte 'at last for the greetings: '
	.byte 'tlb  -  zap  -  paul  -  erwin, rudi & pille of wudsn  -  emanuel   ...'
	.byte 'and for the ladies: '
	.byte 'nuria  -  gaetana  -  anja  -  jeanette   -  sandra  - (the other) sandra  ...'
	.byte 'and all those i`ve forgotten. '
	.byte 'well but now i have to hurry to get this one to tlb, see you.        '
	.byte 'jac! of wudsn ursel  22-12-95  11h15 '
	.byte '                                                      '
	.byte $ff

;================================================================
	.macro cop
	lda scrsm+2+[%1*2]+1
	sta scrsm+2+[%1*2]
	lda scrsm+2+[%1*2]+2
	sta scrsm+2+[%1*2]+1
	.endm

scroll	inc scrcnt
	lda scrcnt
	and #3
	eor #3
	sta scrpos
	cmp #3
	beq *+5
	jmp scrx
	cop 0
	cop 1
	cop 2
	cop 3
	cop 4
	cop 5
	cop 6
	cop 7
	cop 8
	cop 9
	cop 10
	cop 11
	cop 12
	cop 13
	cop 14
	cop 15
	cop 16
	cop 17
	cop 18
	cop 19
	cop 20
	cop 21
	ldy #0
	lda (sp),y
	bpl scr3
	set sp,scrres
	lda #0
scr3	sta scrsm+46
	inc sp
	bne *+4
	inc sp+1
scrx	rts

;===============================================================
start	jsr scrinit
	jsr genberg
	jsr gentext
	jsr genadd
	jsr convtxt
;	jsr Ainit

	lda #48
	sta $d004
	lda #$ff
	sta $d011
	lda #$1
	sta $d01b

	lda #0			;Reset_field_start_frame.
	sta fump
	sta fstep
	
	lda #$c0		;Start_the_show.
	sta $d40e

;;	jmp xx
	lda #fcol1
	sta dli3+1
	lda #1
	sta fstep
	jsr moveup
;;xx	jmp xx
yy
	lda #255
	jsr delay
	lda #255
	jsr delay
	lda #90
	jsr delay
	lda #fcol1
	sta dli3+1
	lda #250
	jsr delay
	lda #100
	jsr delay
	lda #1
	sta fstep
	lda #255
	jsr delay
	lda #100
	jsr delay

loop	jsr moveup

	lda #255
	jsr delay
	lda $d20a
	lsr
	bcc noneg
	lda fstep
	eor #$ff
	clc
	adc #1
	sta fstep
noneg	jmp loop

;===============================================================
moveup
moveup1	ldy #0
	lda (tp),y		;Get_texture.
	bpl moveup2		;Last_texture_?
	set tp,texture
	jmp moveup1

moveup2	ldx #0			;Init_counters.
	lda #$f0
	sta x1
	lda #16
	sta x2

moveup3	jsr movepoint
moveup4	lda #1
	jsr delay
	jmp moveup3

moveup5	inx			;Next_point.
	sec
	lda x1
	sbc #$10
	sta x1
	dec x2			;Next_mirror_point.
	bne moveup6
	lda #16
	sta x2
	inc x1
moveup6	cpx #0			;Last_point_of_this_texture.
	bne moveup3
	inc tp+1		;Next_texture.
	rts


movepoint
	txa
	tay
	lda (tp),y		;Get_point_height_from_texture.
	sec
	sbc berg,x		;Calc_distance.
	beq moveup5
	tay
	lda berg,x
	clc
	adc addtab,y		;Add/Sub_steprate.
	sta berg,x		;Store_X.
	pha
	txa
	eor #$ff		;X_Mirror.
	tay
	pla
	sta berg+$200,y		;Store_X´.
	ldy x1
	sta berg+$100,y		;Store_Y.
	pha
	tya
	eor #$ff		;Y_Mirror.
	tay
	pla
	sta berg+$300,y		;Store_Y´.
	rts
;==============================================================
delay	clc
	adc cnt
delay1	cmp cnt
	bne delay1
	rts

;==============================================================
main	jsr swap
	jsr Aclr
	jsr Aplot
	jsr Amove
main1	rts


;==============================================================
convtxt	ldy #0
	set sp,scrtxt
convtx1	lda (sp),y		;Convert_ASCII_to_charset.
	cmp #$ff		;Termination?
	beq convtx5
	bmi convtx4		;Special_char!
	ldx #63
convtx2	cmp scrcode,x
	beq convtx3
	dex
	bpl convtx2
convtx3	txa
convtx4	sta (sp),y
	iny
	bne convtx1
	inc sp+1
	bne convtx1
convtx5
	set sp,scrtxt
	rts

;==============================================================
genadd	ldx #0
genadd1	lda #1
	sta addtab,x
	eor #$ff
	clc
	adc #1
	sta addtab+$80,x	;Height_add_tab.
	inx
	bpl genadd1
	lda #0
	sta addtab
	rts

;==============================================================
gentext	ldy #0
	set tp,texture		;Convert_textures_to_height.
gentex1	lda (tp),y
	bmi gentex2		;End_of_textures.
	and #3
	tax
	lda textcol,x
	sta (tp),y
	iny
	bne *+4
	inc tp+1
	bne gentex1
gentex2	rts

;==============================================================
genberg	ldx #0
genber1	lda #$1f
	sta berg,x

	lda #>btab3		;Color_for_height_[$00..$1f].
	cpx #12
	bcc *+4
	lda #>btab2
	cpx #24
	bcc *+4
	lda #>btab1
	sta bhigh,x
	inx
	bne genber1

	lda #$f0
	sta x1
genber4	ldy x1
	lda #16
	sta x2
genber5	lda berg,x
	sta berg+$100,y
	inx
	sec
	tya
	sbc #$10
	tay
	dec x2
	bne genber5
	inc x1
	cpx #0
	bne genber4

	ldx #0
	ldy #$ff
genber6	lda berg,x
	sta berg+$200,y
	lda berg+$100,x
	sta berg+$300,y
	dey
	inx
	bne genber6
	rts

;==============================================================
scrinit	cld
	sei
	lda #0
	sta $d40e
	sta $d20e
scrini1	sta $d400,x
	sta $d200,x
	sta $d000,x
	inx
	bne scrini1
	lda #3
	sta $d20f

	lda #$fe
	sta $d301
	set $d402,dl

	ldx #$18
	set p1,sm1
	set p2,sm2
	lda #0
	tay
scrini2	sta (p1),y		;Clear_screens.
	sta (p2),y
	iny
	bne scrini2
	inc p1+1
	inc p2+1
	dex
	bne scrini2

	ldx #0
scrini3	txa
	and #3			;Pixel_offset_(X&7)
	tay
	lda pixtab1,y		;Field_Color_1
	sta btab1,x
	lda pixtab2,y		;Field_Color_2
	sta btab2,x
	lda pixtab3,y		;Field_Color_3
	sta btab3,x
	txa
	lsr
	lsr
	sta otab,x		;
	inx
	bne scrini3

	set p1,sm1
	ldx #0
scrini4	lda p1			;Gen_Llo,Lhi1,Lhi2.
	sta llo,x
	lda p1+1
	sta lhi1,x
	adc #>[sm2-sm1]
	sta lhi2,x
	adw p1 #32 
	inx
	bne scrini4

	ldx #0
scrini5	lda #<sm1		;Init_clear_adresses.
	sta oldlo1,x
	sta oldlo2,x
	lda #>sm1
	sta oldhi1,x
	sta oldhi2,x
	inx
	bpl scrini5

	lda #1			;Init_DL/SM_pointer.
	sta smup
	jsr swap


	lda #$60		;Patch_sound_routines.
	sta $3ca3		;"RTS"
	sta $3dff		;"RTS"
	jsr sndinit		;Init_sound.

	lda #0
	sta cnt
	set $fffa,nmi
	lda #>chr		;Set_charbase.
	sta $d409
	lda #0			;Do_not_display_inverse_chars.
	sta $d401
	rts

screxit	sei
	lda #0
	sta $d40e
	sta $d20e
screxi1	sta $d400,x
	sta $d200,x
	sta $d000,x
	inx
	bne screxi1
	lda #$ff
	sta $d301
	jmp ($fffc)

;=================================================================
nmi	pha
	bit $d40f
	bmi dli
	lda #14			;
	sta $d017
	lda #bcol3
	sta $d018
	lda #$23
	sta $d400
	lda #0
	sta dliup
	lda scrpos
	sta $d404

	lda $d20f		;Any_key_?
	and #12
	cmp #12
	bne vbi1
	lda $d01f		;Console_?
	and #7
	cmp #7
	bne vbi1
	lda $d010		;Trigger_?
	bne vbi2
vbi1	jmp screxit		;Go_to_system_and_say_GOODBYE.
vbi2	inc cnt			;Count_frames.
nmix	pla
	rti

dli	inc dliup
	lda dliup
	cmp #1
	beq dli1
	cmp #2
	beq dli2
	txa
	pha
	tya
	pha
	col $0e
	jsr main
	col $34
	jsr sndplay
	col $78
	jsr scroll
	col $00
	pla
	tay
	pla
	tax
	jmp nmix

dli1	lda #bcol1
	sta $d40a
	sta $d01a
	lda #bcol2
	sta $d40a
	sta $d01a
	lda #bcol3
	sta $d40a
	sta $d01a
	jmp nmix

dli2	lda #bcol2
	sta $d40a
	sta $d01a
	lda #bcol1
	sta $d40a
	sta $d01a
	lda #0
	sta $d40a
	sta $d01a
dli3	lda #0			;fcol1
	sta $d016
	lda #fcol2
	sta $d017
	lda #fcol3
	sta $d018
	lda #$21		;Small playfield
	sta $d400
	jmp nmix

;=================================================================
swap	lda smup
	eor #1
	sta smup
	bne swap1

	lda #>[sm1]		;SMUP=0_/_SMHI=SM1
	sta dl1+1
	lda #>[sm1a]
	sta dl2+1
	rts

swap1	lda #>[sm2]		;SMUP=1_/_SMHI=SM2
	sta dl1+1
	lda #>sm2a
	sta dl2+1
	rts

;==============================================================
Aplot	ldx #>lhi1
	ldy #>oldlo1
	lda smup		;Set_ScreenBase.
	beq Aplot0
	ldx #>lhi2
	ldy #>oldlo2

Aplot0	Ztx Aplot3+2		;lhi1/2
	Zty Aplot8+2		;oldlo1/2
	iny
	Zty Aplot4+2		;oldhi1/2

	lda fump		;Lowbits_count_frame.
	and #31			;0..31
	asl
	clc
	adc #>field
	Zta Aplot6+2		;x-positions
	clc
	adc #1
	Zta Aplot2+2		;y-positions

	lda fump		;Highbits_count_berg.
	lsr
	lsr
	lsr
	lsr
	lsr
	and #3			;0..3
	clc
	adc #>berg
	sta Aplotb+2

	clc
	jmp Aplot1

;=================================================================
Amove	clc
	lda fump		;Next_frame.
	adc fstep		;Add/Sub_stepwidth.
	sta fump
	rts

end1
;=================================================================
	org $3000

texture	.byte "                "
	.byte "333   333  33333"
	.byte "3333 33333 33333"
	.byte "  33 33 33 33   "
	.byte "  33 33 33 33   "
	.byte "  33 33333 33   "
	.byte "  33 33333 33   "
	.byte "  33 33 33 33 33"
	.byte "3333 33 33 33333"
	.byte "333  33 33  333 "
	.byte "                "
	.byte "  111  111 1111 "
	.byte " 1  1 1  1 1  1 "
	.byte " 1  1 1  1 1111 "
	.byte " 1  1 1  1 1  1 "
	.byte "  111 1  1 1  1 "

	.byte "                "
	.byte " 333    33 33333"
	.byte "3333    33 33333"
	.byte "3333    33  33  "
	.byte "3333    33  33  "
	.byte " 333    33  33  "
	.byte "3333    33  33  "
	.byte "3333    33  33  "
	.byte "3333 33333  33  "
	.byte " 333 33333  33  "
	.byte "                "
	.byte " 111 111 111 11 "
	.byte "   1 1 1 1 1 1  "
	.byte " 111 111 111 1  "
	.byte " 1   1   1   1  "
	.byte " 111 111 111 1  "

	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "
	.byte "                "

	.byte "       11       "
	.byte "       11       "
	.byte "       22       "
	.byte "     112211     "
	.byte "   1122332211   "
	.byte "   1122332211   "
	.byte "   1122332211   "
	.byte "   1122332211   "
	.byte "   1122332211   "
	.byte "     112211     "
	.byte "     112211     "
	.byte "  1    11    1  "
	.byte " 121   11   121 "
	.byte "12321      12321"
	.byte " 121        121 "
	.byte "  1          1  "

	.byte $ff				;Terminate_textures.

	run org1
