
        ; Baud rate	(4 = 2400)
        BAUD = 4

;-----------------------------------------------------------------------------------
; OpenSerial: Setup the code to handle RS232 I/O
;-----------------------------------------------------------------------------------

OpenSerial:
        sei                           ; Disable interrupts

        ; Disable all PIA interrupt sources
        lda	PIA1_CRB
        and	#$FE			; Disable interrupts (60hz retrace int?)
        sta	PIA1_CRB
        lda	PIA1_CRA
        and	#$FE
        sta	PIA1_CRA		; Disable interrupts

        lda	PIA2_CRB
        and	#$FE			; Disable interrupts (60hz retrace int?)
        sta	PIA2_CRB
        lda	PIA2_CRA
        and	#$FE
        sta	PIA2_CRA		; Disable interrupts

        lda	BAS4_VECT_IRQ
        sta	IRQB4LO			; Save IRQ lo byte for BASIC 2/4
        lda	BAS4_VECT_IRQ+1
        sta	IRQB4HI			; Save IRQ hi byte for BASIC 2/4

        ; Install IRQ
        lda	#<IrqHandler
        ldx	#>IrqHandler
        sta	BAS4_VECT_IRQ	; Let's see if we can get away with modifying
        stx	BAS4_VECT_IRQ+1	; both versions vectors
        
        lda	#$00		; Rx pin in (PA0) (rest input as well)
        sta	VIA_DDRA	; Set directions
        lda	#$40		; Shift register disabled, no latching, T1 free-run, T2 one-shot
        sta	VIA_ACR		

        lda	#$EC		; Tx as output high, uppercase+graphics ($EE for lower)
        			; CA1 trigger on falling edge
        sta	VIA_PCR		
        ; Set VIA interrupts so that our timer is the only interrupt source
        lda	#$7F		; Clear all interrupt flags
        sta	VIA_IER
        lda	#$C2		; Enable Timer 1 interrupt and CA1 interrupt
        sta	VIA_IER

        lda	#0
        sta	VIA_TIM1L
        sta	VIA_TIM1H	; Need to clear high before writing latch
        			; Otherwise it seems to fail half the tile?

        ; Set up the VIA timer based on the baud rate
        ; Initial baud rate	
        ldx	#BAUD
        lda	BAUDTBLL,X
        sta	VIA_TIM1LL
        lda	BAUDTBLH,X
        sta	VIA_TIM1HL

        lda	POLLINT,X
        sta	POLLRES
        sta	POLLTGT

        ; Below 1200 baud there's not enough serial events between
        ; keyboard polls to use the split routines
        cpx	#3		; Use fast keyboard scanning above 600
        bcc	@slow
        lda	#$FF		; Fast/split keyboard scanning
        sta	KFAST		
@slow:
        cli
        rts

;-----------------------------------------------------------------------------------
; GetSerialChar: Will fetch a character from the receive buffer and store it into A@
; If no data is available, SER_ERR_NO_DATA is returned in X/Y@
;-----------------------------------------------------------------------------------

GetSerialChar:   
        ldx	RXBUFR
        cpx	RXBUFW
        beq	@nodata		; No character available
        lda	RXBUF,X		; New character
        inc	RXBUFR		; Acknowledge byte by incrementing 
        rts

@nodata:
        lda #$ff
        ldx #<SER_ERR_NO_DATA
        ldy #>SER_ERR_NO_DATA 
        rts

;-----------------------------------------------------------------------------------
; PutSerialChar: Output character in A@ This blocks if we're still sending another
; char@
;-----------------------------------------------------------------------------------

PutSerialChar:
        ldx	TXNEW
        bne	PutSerialChar		; Loop till we can send a character
        sta	TXBYTE
        lda	#$FF
        sta	TXNEW
        rts


;-----------------------------------------------------------------------------------
; SerialIoctl: Pass 0 in A to disable serial, 2 to enable
;-----------------------------------------------------------------------------------

SerialIoctl:
        cmp #0
        bne @enable
        rts

@enable:
        ; Set-up timers based on BAUD
        ldx	#BAUD
        lda	BAUDTBLL,X	; Set interrupt timer 
        sta	VIA_TIM1LL
        lda	BAUDTBLH,X
        sta	VIA_TIM1HL
        
        lda	POLLINT,X	; Set keyboard polling interval based
        sta	POLLRES		; on current baud/timer rate
        sta	POLLTGT

        lda	#0
        sta	RXBUFW
        sta	RXBUFR
        sta	KFAST

        ; Below 1200 baud there's not enough serial events between
        ; keyboard polls to use the split routines
        cpx	#3		; Use fast keyboard scanning above 600
        bcc	@slow
        lda	#$FF		; Fast/split keyboard scanning
        sta	KFAST		
@slow:
        rts

;-----------------------------------------------------------------------------------
; CloseSerial: Teardown serial comms@
;-----------------------------------------------------------------------------------

CloseSerial:
        sei

        ; Restore IRQ vector init values
        lda	IRQB4LO
        sta	BAS4_VECT_IRQ
        lda	IRQB4HI
        sta	BAS4_VECT_IRQ+1

; Startup Values for VIA
        lda	#$00
        sta	VIA_DDRA	; $E843
        sta	VIA_IFR		; $E84D

        lda	#$1E ; #$5B?
        sta	VIA_TIM1L	; $E844

        lda	#$FF
        sta	VIA_PORTAH	; $E841
        sta	VIA_PORTA	; $E84F
        sta	VIA_TIM1HL	; $E847

        lda	#$0C ; #$0E?
        sta	VIA_PCR		; $E84C

; Kernal Initialization Values in Sequence
        lda	#$7F
        sta	VIA_IER		; $E84E
        ldx	#$FF
        lda	#$0F
        sta	PIA1_PA		; $E810
        asl
        sta	VIA_PORTB	; $E840
        sta	VIA_DDRB	; $E842
        stx	PIA2_PB		; $E822
        stx	VIA_TIM1H	; $E845

        lda	#$3D
        sta	PIA1_CRB	; $E813
        bit	PIA1_PB		; $E812

        lda	#$3C
        sta	PIA2_CRA	; $E821
        sta	PIA2_CRB	; $E823
        sta	PIA1_CRA	; $E811
        stx	PIA2_PB		; $E822

        lda	#$0E
        sta	VIA_IER		; $E84E

        lda	#$00
        sta	VIA_ACR		; $E84B

        lda	#$0F
        sta	VIA_SR		; $E84A

        ldx	#$07
        lda	$E74D,X		; Timer 2 LO Values DATA
        sta	VIA_TIM2L	; $E848

        ; Enable interrupts
        cli

        rts

;-----------------------------------------------------------------------------------

IrqHandler: ; 36 cycles till we hit here from IRQ firing
        ; 3 possible interrupt sources: (order of priority)
        ;  TIM2 - RX timer (after start bit)
        ;  CA1 falling - Start bit of data to recieve
        ;  TIM1 - TX timer/kbd poll

        lda	#$20		;2; TIMER2 flag
        bit	VIA_IFR		;4;
        bne	@tim2		;3; CA1 triggered $02
        bvs	@tim1		; Timer 1       $40
        jmp	@ca1

        ; Timer 2  $20
@tim2:
        lda	VIA_TIM2L	;4; Acknowledge
        lda	VIA_PORTAH	;4; Clear any pending CA1 interrupts
        ; Read in bit from serial port, build up byte
        ; If 8 recieved, indicate byte, and disable our
        ; interrupt
        lda	VIA_PORTA	;4;
        and	#$01		;2; Only read the Rx pin
        ror			;2; Move into carry
        ror	RXCUR		;5

        dec	RXBIT		;5
        bne	@tim2retrig	;3

        ; We've receieved a byte, signal to program
        ; disable our interrupt
        
        ldx	RXBUFW
        lda	RXCUR
        sta	RXBUF,X
        
        inc	RXBUFW

        
        lda	#$22		; Disable timer 2 interrupt and CA1
        sta	VIA_IER
        lda	#$82		; Enable CA1 interrupt
        sta	VIA_IER
        ; Clear any CA1 interrupt soruce
        lda	VIA_PORTAH
        jmp	@exit

@tim2retrig:
        ldx	BAUD		;3
        lda	TIM2BAUDL,X	;4
        sta	VIA_TIM2L	;4
        lda	TIM2BAUDH,X	;4
        sta	VIA_TIM2H	;4<--From start of IRQ to here is 93 ($5D) cycles!, need to subtract from BAUDTBL
        jmp	@exit		; to give us TIM2BAUD

@tim1:
        lda	VIA_TIM1L
        ; Transmit next bit if sending
        jsr	SERTX		; Use old routine for now


        lda	KFAST		; Which keyboard scan routine
        bne	@fastkbd

        ;"Slow" keyboard polling (all rows at once)
        dec	POLLTGT		; Check if we're due to poll
        bne	@exit
        lda	POLLRES		; Reset keyboard poll count
        sta	POLLTGT

        jsr	KBDPOLL		; Do keyboard polling
        jmp	@keyend
        
@fastkbd:
        lda	POLLTGT
        beq	@final		; 0 
        cmp	#$11
        beq	@first		; 12
        ; One of the 10 scanning rows ;1-11
        jsr	KBDROWPOLL
        dec	POLLTGT
        jmp	@exit
@first:	
        jsr	KBDROWSETUP
        dec	POLLTGT
        jmp	@exit
@final:
        lda	POLLRES
        sta	POLLTGT		; Reset polling counter
        jsr	KBDROWCONV
@keyend:
        cmp	KBDBYTE		; Check if same byte as before
        sta	KBDBYTE
        beq	@exit		; Don't signal the key for a repeat
        lda	KBDBYTE		
        beq	@exit		; Don't signal for no key pressed
        lda	#$FF
        sta	KBDNEW		; Signal a pressed key
        bne	@exit		; Always
        ;--------------------------------	
@ca1:
        lda	VIA_PORTAH	; Acknowledge int
        ; We hit a start bit, set up TIM2
        ; We want the first event to be in 1@5 periods
        ; And enable tim2 interrupt
        ldx	BAUD
        lda	BAUDTBLL,X
        sta	VIA_TIM2L
        lda	BAUDTBLH,X
        sta	VIA_TIM2H	; Timer 2 is off


        lda	#$02		; Disable CA1 interrupt
        sta	VIA_IER
        lda	#$A0		; Enable Timer 2 interrupt
        sta	VIA_IER

        lda	#8
        sta	RXBIT
@exit:
        ; Restore registers saved on stack by KERNAL
        pla			; Pop Y
        tay
        pla			; Pop X
        tax
        pla			; Pop A
        rti			; Return from interrupt

;-----------------------------------------------------------------------
; Do a Tx sample event
SERTX:
        LDA	TXSTATE
        CMP	#STRDY
        BEQ	@ready
        CMP	#STSTART
        BEQ	@start
        CMP	#STBIT
        BEQ	@datab
        CMP	#STSTOP
        BEQ	@stop
        CMP	#STIDLE
        BEQ	@idle
        ; Invalid state
        LDA	#STRDY
        STA	TXSTATE
        JMP	@ready		; Treat as ready state
@idle	; Force idle for 1 baud period
        LDA	#1
        JSR	SETTX		; Idle
        LDA	#STRDY
        STA	TXSTATE
        RTS
@stop	; Send stop bit
        LDA	#1
        JSR	SETTX		; Send stop bit
        LDA	#STRDY
        STA	TXSTATE
        RTS
@datab	; Send data bit
        LDA	#0
        ROR	TXCUR		; Rotate current bit into carry
        ROL			; Place into A
        JSR	SETTX
        INC	TXBIT
        LDA	TXBIT
        CMP	#BITCNT
        BNE	@done		; If more bits to go
        LDA	#STSTOP
        STA	TXSTATE
        RTS
        
@start	; Send start bit
        LDA	#0	
        STA	TXBIT		; Reset bit count
        JSR	SETTX		; Send Start bit
        LDA	#STBIT
        STA	TXSTATE
        RTS
        
@ready
        LDA	#1
        JSR	SETTX		; Idle state
        
        LDA	TXNEW		; Check if we have a byte waiting to send
        BPL	@done		; If not check again next baud		
        LDA	TXBYTE
        STA	TXCUR		; Copy byte to read
        LDA	#0
        STA	TXNEW		; Reset new flag
        LDA	#STSTART	
        STA	TXSTATE
@done
        RTS

;-----------------------------------------------------------------------
; Set Tx pin to value in A
; Tx pin is on userport M (CB2)
; If serial inversion needed, change BEQ to BNE
SETTX:
        CMP	#0
        BEQ	@low		; BEQ for normal, BNE for 'inverted'
        LDA	VIA_PCR
        ORA	#$20		; Make bit 5 high
        STA	VIA_PCR
        RTS
@low
        LDA	VIA_PCR
        AND	#$DF		; Make bit 5 low
        STA	VIA_PCR
        RTS



; Poll a single row (58/63 cycles) 
; Assumes KROW has been setup, and SHIFT and CTRL are cleared before the first call
KBDROWPOLL:	;6;
        LDY	KROW		;3;
        STY	PIA1_PA		;4; Set scan row
        LDA	PIA1_PB		;4; Read in row
        EOR	#$FF		;2; Invert
        TAX			;2; Save
        AND	SHIFTMASK,Y	;4; Is a modifier pressed?
        ORA	SHIFT		;3; OR into shift if so
        STA	SHIFT		;3;
        TXA			;2;
        AND	CTRLMASK,Y	;4; Is a modifier pressed?
        ORA	CTRL		;3; OR into ctrl if so
        STA	CTRL		;3;
        TXA			;2;
        AND	KEYMASK,Y	;4; Mask out modifier keys
        BEQ	@nokey		;2/3 Do we have a keypress in this row?
        STY	KROWFND		;3; Found keypress in this row
        STA	KBITFND		;3; Saved bitmask
@nokey
        INC	KROW
        RTS			;6;
        ; KBITFND and KROWFND will be set to the last key press found
        ; if one is found, with CTRL and SHIFT non-zero if modifer pressed

; Setup for start of a keyboard polling by rows (29 cycles)
KBDROWSETUP:	;6;
        LDA	#0		;2;
        STA	KROW		;3;
        STA	SHIFT		;3;
        STA	CTRL		;3;
        STA	KROWFND		;3;
        STA KBITFND		;3; If KBITFND clear at end of polling then no key pressed
        RTS			;6

; If a key way pressed in the polling convert to a scancode
; Returns pressed key or 0
; 90 cycles worst case
KBDROWCONV:	;6;
        LDA	KBITFND		;3;
        TAX			;2
        BEQ	@nokey		;2/3
        BIT	KBITFND		;4;Test bits 6 and 7 of mask
        BMI	@k7		;2/3
        BVS	@k6		;2/3
        LDA	LOG2_TBL,X	;4; Get the highest bit pressed (6 and 7 are clear)
@found:	; We've got the column of our bitpress in A
        STA	KBITFND		;3; Overwrite bitmask with column to save
        LDA	KROWFND		;3; Each row is 8 long, so we need to multiply by 8
        ASL			;2; *2
        ASL			;2; *4
        ASL			;2; *8
        			; CLC Not needed, KEYOFF's top 3 bits are 0
        ADC	KBITFND		;3; A now contains our offset into the tables
        TAX			;2; Save into X
        
        LDA	SHIFT		;3;
        BEQ	@notshift	;2/3
        ; Shift pressed, read upper table
        LDA	KBDMATRIX_SHIFT,X	;4;
        RTS			;6 (57/59 cycles to here)
@notshift
        LDA	KBDMATRIX,X	;4;
        BMI	@special	;2/3; Don't have control modify special keys
        LDA	CTRL		;3;
        BEQ	@notctrl	;2/3
        ; Ctrl pressed, read lower table and bitmask to CTRL keys
        LDA	KBDMATRIX,X	;4;
        AND	#$9F		;2;
@special			; 
        RTS			;6; (71/73 if we didn't take @special)
        			;   (61/63 if we took @special)
@notctrl	
        LDA	MODE1		;3; Check mode
        AND	#MODE1_CASE	;2; Check if we need to do case fixing (all upper)
        BEQ	@casefix	;2/3;
        ; Normal key
        LDA	KBDMATRIX,X	;4;
        RTS			;6; (77/79 to here)
@casefix
        LDA	KBDMATRIX,X	;4;
        CMP	#$61		;2; a
        BCC	@nocasefix	;2/3; <'a' don't change
        CMP	#$7B		;2; z+1
        BCS	@nocasefix	;2/3; >='z'+1 don't change
        ORA	#$20		;2; Convert lowercase to uppercase
@nocasefix
        RTS			;6; (88/90 max to here)

@k7	LDA	#0		;2; Table is backwards
        BEQ	@found		;3;
@k6	LDA	#1		;2;
        BNE	@found		;3;
@nokey
        RTS			;6; (20 cycles to here)




;----------------------------------------------------------------------
; Poll the keyboard
;~ 500 cycles
KBDPOLL:
        LDA	#$FF
        STA	KEY		; Indicate if we haven't found a key
        LDA	#0
        STA	CTRL
        STA	SHIFT
        LDY	#9 		; Keyboard matrix is 10x8 (9->0)

@loop	STY	PIA1_PA		; Set scan row	
        LDA	PIA1_PB		; Read in row
        EOR 	#$FF		; Invert bits so that 1 means pressed
        TAX			; Save scanned value
        AND	SHIFTMASK,Y	; Check if shift pressed for this row
        BEQ	@noshift
        STA	SHIFT		; Non-zero value indicates shift was pressed
@noshift
        TXA			; Restore scancode
        AND	CTRLMASK,Y	; Check if ctrl pressed for this row
        BEQ	@noctrl
        STA	CTRL		; Non-zero value indicates ctrl was pressed
@noctrl TXA			; Restore scancode
        AND	KEYMASK,Y	; Mask out modifiers
        BEQ	@nextrow	; No key was pressend in this row
        ; Found a keypress, convert to an index in the table
        STA	KBDTMP
        BIT	KBDTMP		; Test high bits
        BMI	@b7
        BVS	@b6
        TAX	
        LDA	LOG2_TBL,X	; Read in highest set bit
        BPL	@store		; Branch always (Value is between 2 and 7)
@b7:	LDA	#0		;   Table is backwards so 7->0
        BEQ	@store		; Branch always
@b6:	LDA	#1		;   Table is backwards so 6->1
@store:	STA	KEY		; Column in table
        STY	KEYOFF		; Row in table
@nextrow:
        DEY			; Next row
        BPL	@loop
; Okay we have our key, if any, and our modifiers
        LDA	KEY
        BPL	@haskey		; Check if we have a key (KEY got changed from initial)
        LDA	#0
        RTS			; No key
@haskey:
        ; Convert row+col into an index
        LDA	KEYOFF		; Each row is 8 long, so we need to multiply by 8
        ASL			; x2
        ASL			; x4
        ASL			; x8
        			; CLC Not needed, KEYOFF's top 3 bits are 0
        ADC	KEY		; A now contains our offset into the tables
        TAX			; Save into X
        
        LDA	SHIFT
        BEQ	@notshift
        ; Shift pressed, read upper table
        LDA	KBDMATRIX_SHIFT,X
        RTS
@notshift:
        LDA	KBDMATRIX,X
        BMI	@special	; Don't have control modify special keys
        LDA	CTRL
        BEQ	@notctrl
        ; Ctrl pressed, read lower table and bitmask to CTRL keys
        LDA	KBDMATRIX,X
        AND	#$9F		
@special:
        RTS
@notctrl:	
        LDA	MODE1		; Check mode
        AND	#MODE1_CASE	; Check if we need to do case fixing (all upper)
        BEQ	@casefix
        ; Normal key
        LDA	KBDMATRIX,X
        RTS
@casefix:
        LDA	KBDMATRIX,X
        CMP	#$61		; a
        BCC	@nocasefix	; <'a' don't change
        CMP	#$7B		; z+1
        BCS	@nocasefix	; >='z'+1 don't change
        ORA	#$20		; Convert lowercase to uppercase
@nocasefix:
        RTS

;-----------------------------------------------------------------------
; Static data

; Baud rate timer values, 1x baud rate
;		 110  300  600 1200 2400 4800 9600
BAUDTBLL:   .byte $83, $05, $83, $41, $A1, $D0, $68
BAUDTBLH:   .byte $23, $0D, $06, $03, $01, $00, $00

; Poll interval mask for ~60Hz keyboard polling based on the baud timer
        ; 	110  300  600 1200 2400 4800  9600 (Baud)
POLLINT:    .byte 2,   5,  10,  20,  40,  80, 160
;Poll freq Hz  	 55   60   60   60   60   60   60
;If POLLINT value is below 12 we need to use the all at once keyboard scan

; Timer 2 isn't freerunning so we have to subtract the cycles till we reset
; it from the rate (- $5D)
TIM2BAUDL:  .byte $26, $A8, $26, $e4, $44, $73, $0B
TIM2BAUDH:  .byte $23, $0C, $06, $02, $01, $00, $00

; A log2 table allows quick decoding by giving the index of the highest bit set
; Remembering that modifers need to be checked seperatly
; This table is inverted due to our key tables being backwards
; 7 - LOG_2(x)
LOG2_TBL .byte -1,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4
         .byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
         .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
         .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2

;----------------------------------------------------------------------
; $00 = shift
; $EF = non-existant
; $FF = REV  (ctrl key)
; $F0 = HOME (MENU key)
;
; $F1 = UP
; $F2 = DOWN
; $F3 = RIGHT
; $F4 = LEFT
;
KBDMATRIX 
    IFCONST BUISKBD
; Matrix for Business Keyboards
        .byte	 '@', $0E, $F3, '8', '-', '8', '5', '2'  ;$8E = BothShift+2, $1D = CursRight
        .byte	 '9', $EF, '^', '7', '0', '7', '4', '1'
        .byte	 '5', '\', 'k', ';', 'h', 'f', 's', $1B ; $9B = ESC
        .byte	 '6', '[', 'l', $0D, 'j', 'g', 'd', 'a'
        .byte	 $08, 'p', 'i', '@', 'y', 'r', 'w', $09 ;$C0 = nonshiftable @, $FF= nonshift DEL
        .byte	 '4', ']', 'o', $F2, 'u', 't', 'e', 'q'	 ; $91 = CursUP
        .byte	 '3', $00, $19, '@', '@', 'b', 'c', $00 ; $AE-> KP@
        .byte	 '2', $04, $0F, '0', $2C, 'n', 'v', 'z'  ; Repeat->^D, $0F = Z+A+L??
        .byte	 '1', '/', $15, $F0, 'm', ' ', 'x', $FF ; $15 - RVS + A + L??, B1 = KP1
        .byte	 $16, $EF, ':', $03, '9', '6', '3', $08 ; $88 Left Arrow to BS?, ^V=TAB+<-+DEL

; Keymasks to remove modifers from the scan results
; There are backward of the table above! Above goes from 9->0, these are 0->9
KEYMASK .byte	$FF,$BF,$FF,$FF,$FF,$FF,$BE,$FF,$FE,$BF
; Which bits indicate shift keys
SHIFTMASK .byte  $00,$00,$00,$00,$00,$00,$41,$00,$00,$00
; Which bits indicate ctrl keys
CTRLMASK .byte   $00,$00,$00,$00,$00,$00,$00,$00,$01,$00

; Keyboard matrix with shift pressed, needed for consistent shifts	
; Matrix for Business Keyboards
KBDMATRIX_SHIFT
        .byte	 '> ,$0E,$F4,'8 ,'= ,'( ,'% ,'"  ;";$8E = BothShift+2, $9D = CursRight
        .byte	 '9 ,$EF,'^ ,'7 ,'0 ,$27,'$ ,'!
        .byte	 '5 ,'| ,'K ,'+ ,'H ,'F ,'S ,$1B ; $1B = ESC
        .byte	 '6 ,'{ ,'L ,$0D,'J ,'G ,'D ,'A
        .byte	 $08,'P ,'I ,'@ ,'Y ,'R ,'W ,$09 ;$C0 = nonshiftable @, $FF= nonshift DEL
        .byte	 '4 ,'} ,'O ,$F1,'U ,'T ,'E ,'Q	 ; $91 = CursUP
        .byte	 '3 ,$00,$19,'@ ,'> ,'B ,'C ,$00 ; $AE-> KP@
        .byte	 '2 ,$04,$0F,'0 ,'< ,'N ,'V ,'Z  ; Repeat->^D, $0F = Z+A+L??
        .byte	 '1 ,'? ,$15,$F0,'M ,'  ,'X ,$FF ; $15 - RVS + A + L??, B1 = KP1
        .byte	 $16,$EF,'* ,$83,') ,'& ,'# ,$08 ; $88 Left Arrow to BS?, ^V=TAB+<-+DEL




        ELSE
; Matrix for Graphics keyboards 
        .byte	$F3,$F0,$5F, '(, '&, '%, '#, '!
        .byte	$08,$F2,$EF, '), '\, '', '$, '"		;" ; (Appease the syntax highlighter)
        .byte	 '9, '7, '^, 'o, 'u, 't, 'e, 'q
        .byte	 '/, '8,$EF, 'p, 'i, 'y, 'r, 'w
        .byte	 '6, '4,$EF, 'l, 'j, 'g, 'd, 'a
        .byte	 '*, '5,$EF, ':, 'k, 'h, 'f, 's
        .byte	 '3, '1,$0D, ';, 'm, 'b, 'c, 'z
        .byte	 '+, '2,$EF, '?, ',, 'n, 'v, 'x
        .byte	 '-, '0,$00, '>,$FF, '], '@,$00
        .byte	 '=, '@,$EF,$03, '<, ' , '[,$FF 	

; $88 (08) is on DEL, (Should be $94/$14)
; $5f (_) is on the <- key?

; Keymasks to remove modifers from the scan results
KEYMASK .byte	$FF,$DF,$FF,$DF,$DF,$DF,$FF,$DF,$D6,$DE
; Which bits indicate shift keys:
SHIFTMASK .byte  $00,$00,$00,$00,$00,$00,$00,$00,$21,$00
; Which bits indicate ctrl keys
CTRLMASK .byte   $00,$00,$00,$00,$00,$00,$00,$00,$08,$01

; Keyboard matrix with shift pressed, needed for consistent shifts	
; Matrix for Graphics keyboards 
KBDMATRIX_SHIFT
        .byte	$F4,$F0,$5F, '(, '&, '%, '#, '!
        .byte	$08,$F1,$EF, '), '\, '`, '$, '"   ;";
        .byte	 '9, '7, '|, 'O, 'U, 'T, 'E, 'Q
        .byte	 '/, '8,$EF, 'P, 'I, 'Y, 'R, 'W
        .byte	 '6, '4,$EF, 'L, 'J, 'G, 'D, 'A
        .byte	 '*, '5,$EF, ':, 'K, 'H, 'F, 'S
        .byte	 '3, '1,$0D, ';, 'M, 'B, 'C, 'Z
        .byte	 '+, '2,$EF, '?, ',, 'N, 'V, 'X
        .byte	 '-, '0,$00, '>,$FF, '}, '~,$00
        .byte	 '=, '@,$EF,$03, '<, ' , '{,$FF
    ENDIF

;-----------------------------------------------------------------------
