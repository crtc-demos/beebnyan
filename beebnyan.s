	.org $e00

	.alias videoptr $40
	.alias frameno $42
	.alias audioptr $43
	.alias palentry $45
	.alias palidx $47

	.temps $48..$8f
	
	.alias anim_start $1100
	.alias song_start $2000
	.alias song_end song_start + 22150
	
	; 6250Hz
	.alias RHZ 160
	
entry:
	.(
	lda #7
	jsr mos_setmode
	jsr mos_cursoroff

	@load_file_to animation, anim_start
	lda #<anim_start
	sta videoptr
	lda #>anim_start
	sta videoptr+1
	
	@load_file_to songname, song_start

	lda #<[song_start + 1024]
	sta audioptr
	lda #>[song_start + 1024]
	sta audioptr+1

	lda #0
	sta palentry
	sta palentry+1
	lda #8
	sta palidx
	
	jsr initfreq
	jsr inittimer1irq
	
	lda #0
	sta frameno
loop
	jsr render
	; busy-wait...
	.(
	ldx #8
wait2
	ldy #255
wait
	nop
	nop
	nop
	dey
	bne wait
	dex
	bne wait2
	.)

	.(
	lda frameno
	inc a
	sta frameno
	cmp #12
	bne no_wrap
	lda #0
	sta frameno
	lda #<anim_start
	sta videoptr
	lda #>anim_start
	sta videoptr+1
no_wrap
	.)
	
	jmp loop
	rts
	.)
	
animation:
	.asc "nyansqz",13
songname:
	.asc "song",13
	
	.include "lib/mos.s"
	.include "lib/load.s"
	
	.context render
	.var2 vidtmp1
	.var vidtmp2
	.var vidtmp3
	.var vidtmp4
render:
	lda (videoptr)
	cmp #127
	.(
	bne notnew
	; End of the data: this shouldn't ever happen here.
	rts
notnew:	.)
	cmp #255
	.(
	bne notraw
	jmp rawframe
notraw:	.)
	bit #$80
	bne diffframe
		
rleframe
	sta %vidtmp4
	
	;lda #'R'
	;jsr oswrch
	
	; skip over frame type byte.
	inc videoptr
	.(
	bne nohi
	inc videoptr+1
nohi:	.)
	
	.(
do_spans
	lda %vidtmp4
	beq spans_done
	dec %vidtmp4
	
	; a run-length encoded frame.
	lda (videoptr)
	sta %vidtmp1
	ldy #1
	lda (videoptr), y
	sta %vidtmp1+1
	iny
	lda (videoptr), y
	tax
	; now (vidtmp1) is the span start, X is the (uncompressed) length.
	lda videoptr
	clc
	adc #3
	sta videoptr
	.(
	bcc nohi
	inc videoptr+1
nohi:	.)
	
	stz %vidtmp3
rlespans
	ldy %vidtmp3
	; get repeat count
	lda (videoptr), y
	; stow repeat count in vidtmp2
	sta %vidtmp2
	iny
	; get byte to write
	lda (videoptr), y
	iny
	; store Y index in vidtmp3
	sty %vidtmp3

	; now A has byte to write, vidtmp2 has repeat count (where 0 means 256).
	ldy %vidtmp2
	.(
fill
	dey
	sta (%vidtmp1), y
	bne fill
	.)
	
	; add run-length to screen ptr, treating vidtmp2==0 as vidtmp2==256.
	.(
	lda %vidtmp2
	beq skip256
	clc
	adc %vidtmp1
	sta %vidtmp1
	bcc nohi
skip256
	inc %vidtmp1+1
nohi:	.)
	
	; subtract the length of this span from total (X).
	.(
	lda %vidtmp2
	beq xtozero
	txa
	sec
	sbc %vidtmp2
xtozero
	tax
	.)
	
	bne rlespans
	
	; add RLE data length to frame pointer.
	.(
	lda videoptr
	clc
	adc %vidtmp3
	sta videoptr
	bcc nohi
	inc videoptr+1
nohi:	.)

	jmp do_spans

spans_done
	.)

	rts

diffframe
	; number of spans in vidtmp2.
	and #$7f
	sta %vidtmp2
	
	;lda #'D'
	;jsr oswrch
	
	; skip over frame type byte.
	inc videoptr
	.(
	bne nohi
	inc videoptr+1
nohi:	.)
	
	.(
do_span
	lda %vidtmp2
	beq all_done

	dec %vidtmp2
	
	lda (videoptr)
	sta %vidtmp1
	ldy #1
	lda (videoptr), y
	sta %vidtmp1+1
	iny
	lda (videoptr), y
	; keep (uncompressed span) length in vidtmp3.
	sta %vidtmp3
	tay
	; now (vidtmp1) is span start, Y is the length.
	; advance (videoptr) to data.
	lda videoptr
	clc
	adc #3
	sta videoptr
	.(
	bcc nohi
	inc videoptr+1
nohi:	.)
	
fill
	dey
	lda (videoptr), y
	sta (%vidtmp1), y
	cpy #0
	bne fill
	
	; skip (videoptr) over data, treating vidtmp3==0 as vidtmp3=256.
	.(
	lda %vidtmp3
	beq skip256
	clc
	adc videoptr
	sta videoptr
	bcc nohi
skip256
	inc videoptr+1
nohi:	.)
	
	jmp do_span
	
all_done
	rts
	.)

rawframe
	.(
	
	;lda #'W'
	;jsr oswrch
	
	; skip over frame-type byte
	inc videoptr
	.(
	bne nohi
	inc videoptr+1
nohi:	.)

	lda #<$7C00
	ldx #>$7C00
	sta %vidtmp1
	stx %vidtmp1+1
	
	; copy src pointer
	lda videoptr
	ldx videoptr+1
	sta %vidtmp2
	stx %vidtmp3

	ldx #3
	ldy #0
copy
	lda (%vidtmp2), y
	sta (%vidtmp1), y
	dey
	bne copy

	inc %vidtmp3
	inc %vidtmp1+1
	dex
	bne copy
	
	ldy #0
copy2
	lda (%vidtmp2), y
	sta (%vidtmp1), y
	iny
	cpy #232
	bne copy2
	
	; add frame size to frame ptr
	lda videoptr
	clc
	adc #<1000
	sta videoptr
	lda videoptr+1
	adc #>1000
	sta videoptr+1

	rts
	.)

	.ctxend

oldirq1v
	.word 0

inittimer1irq
	.(
	lda $204
	ldx $205
	sta oldirq1v
	stx oldirq1v+1
	
	sei
	
	; Continuous interrupts for timer 1.
	lda USR_ACR
	and #$3f
	ora #$40
	sta USR_ACR
	
	; Point at IRQ handler
	lda #<irqhandler
	ldx #>irqhandler
	sta $204
	stx $205
	
	; Enable Usr timer1 interrupt
	lda #$c0
	sta USR_IER
	
	lda #<RHZ
	ldx #>RHZ
	sta USR_T1C_L
	stx USR_T1C_H
	
	; Disable system VIA interrupts
	;lda #$7f
	;sta SYS_IER
	; Enable system timer 1 and CA1 (vsync)
	;lda #$c1
	;sta SYS_IER
	
	cli
	rts
	.)

soundstrobe
	.(
	sta $fe41
	; strobe is supposed to be at least 8uS. This should do.
	stz $fe40
	nop
	nop
	nop
	nop
	lda #$08
	sta $fe40
	rts
	.)

; initialise sound frequences on 3 channels.
initfreq
	.(
	sei
	
	lda #$ff
	sta $fe43
	
	; channel 3
	lda #$81
	jsr soundstrobe
	lda #$00
	jsr soundstrobe
	
	; channel 2
	lda #$a2
	jsr soundstrobe
	lda #$00
	jsr soundstrobe
	
	; channel 1
	lda #$c3
	jsr soundstrobe
	lda #$00
	jsr soundstrobe
	
	cli
	rts
	.)

exitirq
	.(
	pla
	sta $fc
	jmp (oldirq1v)
	.)

irqhandler
	.(
	lda $fc
	pha
	
	lda #$c0
	bit USR_IFR
	; top bit clear - not interrupt from 6522 (user VIA).
	bpl exitirq
	; bit 6 clear - not our interrupt, process next in chain.
	bvc exitirq
	
	; Clear timer1 interrupt flag
	lda USR_T1C_L
	
	phy
	lda palidx
	cmp #8
	bne active_piece

	lda (audioptr)

	sta palentry
	lda #0
	asl palentry
	rol a
	asl palentry
	rol a
	sta palentry + 1
	
	lda palentry
	clc
	adc #<song_start
	sta palentry
	lda palentry + 1
	adc #>song_start
	sta palentry + 1
	
	lda #0
	sta palidx

	lda audioptr
	clc
	adc #1
	sta audioptr
	.(
	bcc no_hi
	inc audioptr + 1
no_hi:	.)

	lda audioptr + 1
	cmp #>song_end
	bne active_piece
	lda audioptr
	cmp #<song_end
	bne active_piece

	lda #<[song_start + 1024]
	sta audioptr
	lda #>[song_start + 1024]
	sta audioptr + 1

active_piece
	lda palidx
	lsr
	tay
	lda (palentry),y
	bcs hi_part
	and #15
	jmp send_to_chip
hi_part
	lsr a
	lsr a
	lsr a
	lsr a

send_to_chip
	tay
	ora #$90
	jsr soundstrobe
	tya
	ora #$b0
	jsr soundstrobe
	tya
	ora #$d0
	jsr soundstrobe
	
	inc palidx
	
	; we handled our interrupt: don't let anyone else see it, they'll be
	; jealous.
	ply
	pla
	sta $fc
	rti

	.)
