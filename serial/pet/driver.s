BAUD            = 4                   ; Baud rate, index into BaudTblLo/Hi (4 = 2400)

ST_STARTBIT     = 0                   ; Waiting/Sending for start bit
ST_READY        = 1                   ; Ready to start sending
ST_DATABIT      = 2                   ; Sending/receiving data
ST_STOPBIT      = 3                   ; Sending/receiving stop bit
ST_IDLE         = 4

BITCOUNT        = 8                   ; 8-bit bytes to recieve

; 6520 PIAs - Not in CC65 pet.inc
PIA1_PA         = $E810
PIA1_PB         = $E812
PIA1_CRA        = $E811
PIA1_CRB        = $E813

PIA2_PB         = $E822
PIA2_CRA        = $E821
PIA2_CRB        = $E823

BASIC_VECT_IRQ  = $0090               ; 90/91 - Hardware interrupt vector

;-----------------------------------------------------------------------------------
; OpenSerial: Setup the code to handle RS232 I/O
;-----------------------------------------------------------------------------------

OpenSerial:
                sei                   ; Disable interrupts

                ; Disable all PIA interrupt sources
                lda PIA1_CRB
                and #$FE              ; Disable interrupts (60hz retrace interval?)
                sta PIA1_CRB
                lda PIA1_CRA
                and #$FE
                sta PIA1_CRA          ; Disable interrupts

                lda PIA2_CRB
                and #$FE              ; Disable interrupts (60hz retrace interval?)
                sta PIA2_CRB
                lda PIA2_CRA
                and #$FE
                sta PIA2_CRA          ; Disable interrupts

                lda BASIC_VECT_IRQ
                sta IrqBasicLo        ; Save IRQ lo byte for BASIC
                lda BASIC_VECT_IRQ+1
                sta IrqBasicHi        ; Save IRQ hi byte for BASIC

                ; Install IRQ
                lda #<IrqHandler
                ldx #>IrqHandler
                sta BASIC_VECT_IRQ
                stx BASIC_VECT_IRQ+1

                lda #$00              ; Rx pin in (PA0) (rest input as well)
                sta VIA_DDRA          ; Set directions
                lda #$40              ; Shift register disabled, no latching, T1 free-run, T2 one-shot
                sta VIA_CR

                lda #$EC              ; Tx as output high, uppercase+graphics ($EE for lower)
                                      ; CA1 trigger on falling edge
                sta VIA_PCR
                ; Set VIA interrupts so that our timer is the only interrupt source
                lda #$7F              ; Clear all interrupt flags
                sta VIA_IER
                lda #$C2              ; Enable Timer 1 interrupt and CA1 interrupt
                sta VIA_IER

                lda #0
                sta VIA_T1CL
                sta VIA_T1CH          ; Need to clear high before writing latch
                                      ; Otherwise it seems to fail half the time?

                ldx #BAUD             ; Set up timers based on BAUD
                lda BaudTblLo,X       ; Set interrupt timer
                sta VIA_T1LL
                lda BaudTblHi,X
                sta VIA_T1LH

                lda KbdPollIntTbl,X   ; Set keyboard polling interval based
                sta KbdPollIntrvl     ; on current baud/timer rate
                sta KbdPollCnt

                cli
                rts

;-----------------------------------------------------------------------------------
; GetSerialChar: Will fetch a character from the receive buffer and store it into A.
; If no data is available, SER_ERR_NO_DATA is returned in X/Y.
;-----------------------------------------------------------------------------------

GetSerialChar:
                ldx RxBufReadPtr
                cpx RxBufWritePtr
                beq @nodata           ; No character available
                lda RX_BUF,X          ; New character
                inc RxBufReadPtr      ; Acknowledge byte by incrementing
                rts

@nodata:
                lda #$ff
                ldx #<SER_ERR_NO_DATA
                ldy #>SER_ERR_NO_DATA
                rts

;-----------------------------------------------------------------------------------
; PutSerialChar: Output character in A. This blocks if we're still sending another
; char.
;-----------------------------------------------------------------------------------

PutSerialChar:
                ldx TxNewFlag
                bne PutSerialChar     ; Loop till we can send a character
                sta TxNextByte
                lda #$FF
                sta TxNewFlag
                rts


;-----------------------------------------------------------------------------------
; StartSerial: Start serial communication. OpenSerial must have been called already.
;-----------------------------------------------------------------------------------

StartSerial:
                lda #0
                sta RxBufWritePtr
                sta RxBufReadPtr

                rts

;-----------------------------------------------------------------------------------
; CloseSerial: Teardown serial comms. We try to restore everything to its original
; state.
;-----------------------------------------------------------------------------------

CloseSerial:    
                sei                   ; Disable interrupts

                ; Restore IRQ vector init values
                lda IrqBasicLo
                sta BASIC_VECT_IRQ
                lda IrqBasicHi
                sta BASIC_VECT_IRQ+1

                ; Startup Values for VIA
                lda #$00
                sta VIA_DDRA
                sta VIA_IFR

                lda #$1E
                sta VIA_T1CL

                lda #$FF
                sta VIA_PA1
                sta VIA_PA2
                sta VIA_T1LH

                lda #$0C
                sta VIA_PCR

                ; Kernal Initialization Values in Sequence
                lda #$7F
                sta VIA_IER
                ldx #$FF
                lda #$0F
                sta PIA1_PA
                asl
                sta VIA_PB
                sta VIA_DDRB
                stx PIA2_PB
                stx VIA_T1CH

                lda #$3D
                sta PIA1_CRB
                bit PIA1_PB

                lda #$3C
                sta PIA2_CRA
                sta PIA2_CRB
                sta PIA1_CRA
                stx PIA2_PB

                lda #$0E
                sta VIA_IER

                lda #$00
                sta VIA_CR

                lda #$0F
                sta VIA_SR

                ldx #$07
                lda $E74D,X
                sta VIA_T2CL

                cli                   ; Enable interrupts
                rts

;-----------------------------------------------------------------------------------
; GetKeyboardChar: Get a character from the keyboard, if available
;-----------------------------------------------------------------------------------

GetKeyboardChar:
                lda KbdNewFlag        ; Check the flag set by the polling routine
                bne @keypressed
                rts                   ; Return 0 to indicate no key was pressed

@keypressed:    lda KbdByte           ; Load character that was pressed
                ldx #0
                stx KbdNewFlag        ; Acknowledge key press
                rts

;-----------------------------------------------------------------------
; Process a Tx sample event
;-----------------------------------------------------------------------

SendBit:
                lda TxState
                cmp #ST_READY
                beq @ready
                cmp #ST_STARTBIT
                beq @startbit
                cmp #ST_DATABIT
                beq @databit
                cmp #ST_STOPBIT
                beq @stopbit
                cmp #ST_IDLE
                beq @idle
                ; Invalid state
                lda #ST_READY
                sta TxState
                jmp @ready            ; Treat as ready state
                ; Force idle for 1 baud period
@idle:          lda #1
                jsr SetTxPin          ; Idle
                lda #ST_READY
                sta TxState
                rts
                ; Send stop bit
@stopbit:       lda #1
                jsr SetTxPin          ; Send stop bit
                lda #ST_READY
                sta TxState
                rts
@databit:  ; Send data bit
                lda #0
                ror TxCurByte         ; Rotate current bit into carry
                rol                   ; Place into A
                jsr SetTxPin
                inc TxBitNum
                lda TxBitNum
                cmp #BITCOUNT
                bne @done             ; If more bits to go
                lda #ST_STOPBIT
                sta TxState
                rts
                ; Send start bit
@startbit:      lda #0
                sta TxBitNum          ; Reset bit count
                jsr SetTxPin          ; Send Start bit
                lda #ST_DATABIT
                sta TxState
                rts

@ready:         lda #1
                jsr SetTxPin          ; Idle state
                lda TxNewFlag         ; Check if we have a byte waiting to send
                bpl @done             ; If not check again next baud
                lda TxNextByte
                sta TxCurByte         ; Copy byte to read
                lda #0
                sta TxNewFlag         ; Reset new flag
                lda #ST_STARTBIT
                sta TxState
@done:          rts

;-----------------------------------------------------------------------
; Set Tx pin to value in A
; Tx pin is on userport M (CB2)
; If serial inversion needed, change BEQ to BNE
;-----------------------------------------------------------------------

SetTxPin:
                cmp #0
                beq @low              ; BEQ for normal, BNE for 'inverted'
                lda VIA_PCR
                ora #$20              ; Make bit 5 high
                sta VIA_PCR
                rts

@low:           lda VIA_PCR
                and #$DF              ; Make bit 5 low
                sta VIA_PCR
                rts

.if BAUD < 3    ; Use "slow" (full-keyboard) polling when < 1200 Bd

;----------------------------------------------------------------------
; Poll the keyboard
; ~ 500 cycles
;-----------------------------------------------------------------------

PollKeyboard:
                lda #$FF
                sta KeyCol            ; Indicate if we haven't found a key
                lda #0
                sta CtrlFlag
                sta ShiftFlag
                ldy #9                ; Keyboard matrix is 10x8 (9->0)

@loop:          sty PIA1_PA           ; Set scan row
                lda PIA1_PB           ; Read in row
                eor #$FF              ; Invert bits so that 1 means pressed
                tax                   ; Save scanned value
                and ShiftMask,Y       ; Check if shift pressed for this row
                beq @noshift
                sta ShiftFlag         ; Non-zero value indicates shift was pressed
@noshift:       txa                   ; Restore scancode
                and CtrlMask,Y        ; Check if ctrl pressed for this row
                beq @noctrl
                sta CtrlFlag          ; Non-zero value indicates ctrl was pressed
@noctrl:        txa                   ; Restore scancode
                and KeyMask,Y         ; Mask out modifiers
                beq @nextrow          ; No key was pressend in this row
                ; Found a keypress, convert to an index in the table
                sta KbdTemp
                bit KbdTemp           ; Test high bits
                bmi @b7
                bvs @b6
                tax
                lda Log2Tbl,X         ; Read in highest set bit
                bpl @store            ; Branch always (Value is between 2 and 7)
@b7:            lda #0                ; Table is backwards so 7->0
                beq @store            ; Branch always
@b6:            lda #1                ;  Table is backwards so 6->1
@store:         sta KeyCol            ; Column in table
                sty KeyOffset         ; Row in table
@nextrow:       dey                   ; Next row
                bpl @loop
                ; Okay we have our key, if any, and our modifiers
                lda KeyCol
                bpl @haskey           ; Check if we have a key (KeyCol got changed from initial)
                lda #0
                rts                   ; No key
                ; Convert row+col into an index
@haskey:        lda KeyOffset         ; Each row is 8 long, so we need to multiply by 8
                asl                   ; x2
                asl                   ; x4
                asl                   ; x8
                                      ; CLC not needed, KeyOffset's top 3 bits are 0
                adc KeyCol            ; A now contains our offset into the tables
                tax                   ; Save into X

                lda ShiftFlag
                beq @notshift
                ; Shift pressed, read upper table
                lda KbdMatrixShift,X
                rts

@notshift:      lda KbdMatrix,X
                bmi @special          ; Don't have control modify special keys
                lda CtrlFlag
                beq @notctrl
                ; Ctrl pressed, read lower table and bitmask to CtrlFlag keys
                lda KbdMatrix,X
                and #$9F
@special:       rts
                ; Normal key
@notctrl:       lda KbdMatrix,X
                rts

.else           ; We're at 1200 Bd or higher, so use "fast" (single-row) polling

;-----------------------------------------------------------------------
; Setup for start of a keyboard polling by rows (29 cycles)
;-----------------------------------------------------------------------

SetupKbdRow:                          ;6;
                lda #0                ;2;
                sta KeyRow            ;3;
                sta ShiftFlag         ;3;
                sta CtrlFlag          ;3;
                sta KeyRowFound       ;3;
                sta KeyBitsFound      ;3; If KeyBitsFound clear at end of polling then no key pressed
                rts                   ;6;

;-----------------------------------------------------------------------
; Poll a single row (58/63 cycles)
; Assumes KeyRow has been setup, and ShiftFlag and CtrlFlag are cleared before the first call
;-----------------------------------------------------------------------

PollKbdRow:                           ;6;
                ldy KeyRow            ;3;
                sty PIA1_PA           ;4; Set scan row
                lda PIA1_PB           ;4; Read in row
                eor #$FF              ;2; Invert
                tax                   ;2; Save
                and ShiftMask,Y       ;4; Is a modifier pressed?
                ora ShiftFlag         ;3; OR into shift if so
                sta ShiftFlag         ;3;
                txa                   ;2;
                and CtrlMask,Y        ;4; Is a modifier pressed?
                ora CtrlFlag          ;3; OR into ctrl if so
                sta CtrlFlag          ;3;
                txa                   ;2;
                and KeyMask,Y         ;4; Mask out modifier keys
                beq @nokey            ;2/3; Do we have a keypress in this row?
                sty KeyRowFound       ;3; Found keypress in this row
                sta KeyBitsFound      ;3; Saved bitmask

@nokey:         inc KeyRow
                rts                   ;6;
                ; KeyBitsFound and KeyRowFound will be set to the last key press found
                ; if one is found, with CtrlFlag and ShiftFlag non-zero if modifer pressed

;-----------------------------------------------------------------------
; If a key way pressed in the polling convert to a scancode
; Returns pressed key or 0
; 90 cycles worst case
;-----------------------------------------------------------------------

ConvertKbdRow:                        ;6;
                lda KeyBitsFound      ;3;
                beq @nokey            ;2/3;
                tax                   ;2;
                bit KeyBitsFound      ;4;Test bits 6 and 7 of mask
                bmi @k7               ;2/3
                bvs @k6               ;2/3
                lda Log2Tbl,X         ;4; Get the highest bit pressed (6 and 7 are clear)
                ; We've got the column of our bitpress in A
@found:         sta KeyBitsFound      ;3; Overwrite bitmask with column to save
                lda KeyRowFound       ;3; Each row is 8 long, so we need to multiply by 8
                asl                   ;2; *2
                asl                   ;2; *4
                asl                   ;2; *8
                ; CLC Not needed, KeyOffset's top 3 bits are 0
                adc KeyBitsFound      ;3; A now contains our offset into the tables
                tax                   ;2; Save into X

                lda ShiftFlag         ;3;
                beq @notshift         ;2/3;
                ; Shift pressed, read upper table
                lda KbdMatrixShift,X  ;4;
                rts                   ;6; (57/59 cycles to here)

@notshift:      lda KbdMatrix,X       ;4;
                bmi @special          ;2/3; Don't have control modify special keys
                lda CtrlFlag          ;3;
                beq @notctrl          ;2/3
                ; Ctrl pressed, read lower table and bitmask to CtrlFlag keys
                lda KbdMatrix,X       ;4;
                and #$9F              ;2;
@special:       rts                   ;6; (71/73 if we didn't take @special)
                                      ;   (61/63 if we took @special)
                ; Normal key
@notctrl:       lda KbdMatrix,X       ;4;
                rts                   ;6; (71/73 to here)

@k7:            lda #0                ;2; Table is backwards
                beq @found            ;3;
@k6:            lda #1                ;2;
                bne @found            ;3;
@nokey:         rts                   ;6; (20 cycles to here)

.endif          ; Baud-based keyboard polling routines

;-----------------------------------------------------------------------------------
; IRQ handler that is installed when OpenSerial is called
;-----------------------------------------------------------------------------------

IrqHandler:     ; 36 cycles till we hit here from IRQ firing
                ; 3 possible interrupt sources: (order of priority)
                ;  TIM2 - RX timer (after start bit)
                ;  CA1 falling - Start bit of data to recieve
                ;  TIM1 - TX timer/kbd poll

                lda #$20              ;2; TIMER2 flag
                bit VIA_IFR           ;4;
                bne @tim2             ;3; CA1 triggered $02
                bvs @tim1             ; Timer 1       $40
                jmp @ca1

                ; Timer 2  $20
@tim2:          lda VIA_T2CL          ;4; Acknowledge
                lda VIA_PA1           ;4; Clear any pending CA1 interrupts
                ; Read in bit from serial port, build up byte
                ; If 8 recieved, indicate byte, and disable our
                ; interrupt
                lda VIA_PA2           ;4;
                and #$01              ;2; Only read the Rx pin
                ror                   ;2; Move into carry
                ror RxCurByte         ;5;

                dec RxBitNum          ;5;
                bne @tim2retrig       ;3;

                ; We've receieved a byte, signal to program
                ; disable our interrupt

                ldx RxBufWritePtr
                lda RxCurByte
                sta RX_BUF,X

                inc RxBufWritePtr

                lda #$22              ; Disable timer 2 interrupt and CA1
                sta VIA_IER
                lda #$82              ; Enable CA1 interrupt
                sta VIA_IER
                                      ; Clear any CA1 interrupt soruce
                lda VIA_PA1
                jmp @exit

@tim2retrig:    ldx #BAUD             ;3;
                lda Tim2BaudLo,X      ;4;
                sta VIA_T2CL          ;4;
                lda Tim2BaudHi,X      ;4;
                sta VIA_T2CH          ;4; <--From start of IRQ to here is 93 ($5D) cycles!, need to subtract from BAUDTBL
                jmp @exit             ;   to give us TIM2BAUD

@tim1:          lda VIA_T1CL
                ; Transmit next bit if sending
                jsr SendBit

.if BAUD < 3    ; Less than 1200 Bd => poll full keyboard
                ;"Slow" keyboard polling (all rows at once)
                dec KbdPollCnt        ; Check if we're due to poll
                bne @exit
                lda KbdPollIntrvl     ; Reset keyboard poll count
                sta KbdPollCnt

                jsr PollKeyboard      ; Do keyboard polling
                jmp @keyend

.else           ; 1200 Bd or higher => poll individual rows 

                ; The following code counts down from a baud-based value (higher baudrate
                ;   means higher value), until it reaches 11. That triggers the setting up
                ;   and then execution of per-row keyboard polling.
                dec KbdPollCnt
                beq @finish           ; 0, so finish polling
                lda KbdPollCnt
                cmp #$11
                beq @setup            ; 11, so setup polling
                bcs @exit             ; > 11, so we're still counting down
                ; One of the 10 scanning rows ;1-10
                jsr PollKbdRow
                jmp @exit

@setup:         jsr SetupKbdRow
                jmp @exit

@finish:        lda KbdPollIntrvl
                sta KbdPollCnt        ; Reset polling counter
                jsr ConvertKbdRow

.endif          ; Baud-based keyboard polling routine

@keyend:        cmp KbdByte           ; Check if same byte as before
                sta KbdByte
                beq @exit             ; Don't signal the key for a repeat
                lda KbdByte
                beq @exit             ; Don't signal for no key pressed
                lda #$FF
                sta KbdNewFlag        ; Signal a pressed key
                bne @exit             ; Always

@ca1:           lda VIA_PA1           ; Acknowledge interrupt
                ; We hit a start bit, set up TIM2
                ; We want the first event to be in 1@5 periods
                ; And enable tim2 interrupt
                ldx #BAUD
                lda BaudTblLo,X
                sta VIA_T2CL
                lda BaudTblHi,X
                sta VIA_T2CH          ; Timer 2 is off


                lda #$02              ; Disable CA1 interrupt
                sta VIA_IER
                lda #$A0              ; Enable Timer 2 interrupt
                sta VIA_IER

                lda #8
                sta RxBitNum

@exit:          ; Restore registers saved on stack by KERNAL
                pla                   ; Pop Y
                tay
                pla                   ; Pop X
                tax
                pla                   ; Pop A
                rti                   ; Return from interrupt

;-----------------------------------------------------------------------
; Static data

                ; Baud rate timer values, 1x baud rate  
                ;     110  300  600 1200 2400 4800 9600
BaudTblLo:      .byte $83, $05, $83, $41, $A1, $D0, $68
BaudTblHi:      .byte $23, $0D, $06, $03, $01, $00, $00

                ; Poll interval mask for ~60Hz keyboard polling based on the baud timer
                ;      110  300  600 1200 2400 4800 9600 (Baud)
KbdPollIntTbl:     .byte 2,   5,  10,  20,  40,  80, 160
      ; Poll freq Hz    55   60   60   60   60   60   60
                ; If KbdPollIntrvl value is below 12 we need to use the all at once keyboard scan

                ; Timer 2 isn't freerunning so we have to subtract the cycles till we reset
                ; it from the rate (- $5D)
Tim2BaudLo:     .byte $26, $A8, $26, $e4, $44, $73, $0B
Tim2BaudHi:     .byte $23, $0C, $06, $02, $01, $00, $00

                ; A log2 table allows quick decoding by giving the index of the highest bit set
                ; Remembering that modifers need to be checked seperatly
                ; This table is inverted due to our key tables being backwards
                ; 7 - LOG_2(x)
Log2Tbl:        .byte 255,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4
                .byte   3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
                .byte   2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
                .byte   2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2

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
.if BSNSS_KBD

                ; Matrix for Business Keyboards
KbdMatrix:      .byte '@', $0E, $F3, '8', '-', '8', '5', '2'    ; $8E = BothShift+2, $1D = CursRight
                .byte '9', $EF, '^', '7', '0', '7', '4', '1'
                .byte '5', '\', 'k', ';', 'h', 'f', 's', $1B    ; $9B = ESC
                .byte '6', '[', 'l', $0D, 'j', 'g', 'd', 'a'
                .byte $08, 'p', 'i', '@', 'y', 'r', 'w', $09    ; $C0 = nonshiftable ., $FF= nonshift DEL
                .byte '4', ']', 'o', $F2, 'u', 't', 'e', 'q'  ; $91 = CursUP.
                .byte '3', $00, $19, '.', '.', 'b', 'c', $00    ; $AE-> KP.
                .byte '2', $04, $0F, '0', $2C, 'n', 'v', 'z'    ; Repeat->^D, $0F = Z+A+L??
                .byte '1', '/', $15, $F0, 'm', ' ', 'x', $FF    ; $15 - RVS + A + L??, B1 = KP1
                .byte $16, $EF, ':', $03, '9', '6', '3', $08    ; $88 Left Arrow to BS?, ^V=TAB+<-+DEL

                ; Keymasks to remove modifers from the scan results
                ; There are backward of the table above! Above goes from 9->0, these are 0->9
KeyMask:        .byte $FF, $BF, $FF, $FF, $FF, $FF, $BE, $FF, $FE, $BF
                ; Which bits indicate shift keys
ShiftMask:      .byte $00, $00, $00, $00, $00, $00, $41, $00, $00, $00
                ; Which bits indicate ctrl keys
CtrlMask:       .byte $00, $00, $00, $00, $00, $00, $00, $00, $01, $00

                ; Keyboard matrix with shift pressed, needed for consistent shifts
                ; Matrix for Business Keyboards
KbdMatrixShift: .byte '>', $0E, $F4, '8', '=', '(', '%', '"'    ;" ;$8E = BothShift+2, $9D = CursRight
                .byte '9', $EF, '^', '7', '0', $27, '$', '!'
                .byte '5', '|', 'K', '+', 'H', 'F', 'S', $1B    ; $1B = ESC
                .byte '6', '{', 'L', $0D, 'J', 'G', 'D', 'A'
                .byte $08, 'P', 'I', '@', 'Y', 'R', 'W', $09    ; $C0 = nonshiftable ., $FF= nonshift DEL
                .byte '4', '}', 'O', $F1, 'U', 'T', 'E', 'Q'    ; $91 = CursUP
                .byte '3', $00, $19, '.', '>', 'B', 'C', $00    ; $AE-> KP.
                .byte '2', $04, $0F, '0', '<', 'N', 'V', 'Z'    ; Repeat->^D, $0F = Z+A+L??
                .byte '1', '?', $15, $F0, 'M', ' ', 'X', $FF    ; $15 - RVS + A + L??, B1 = KP1
                .byte $16, $EF, '*', $83, ')', '&', '#', $08    ; $88 Left Arrow to BS?, ^V=TAB+<-+DEL

.else

                ; Matrix for Graphics keyboards
KbdMatrix:      .byte $F3, $F0, $5F, '(', '&', '%', '#', '!'
                .byte $08, $F2, $EF, ')', '\', "'", '$', '"'    ;" ; (Appease the syntax highlighter)
                .byte '9', '7', '^', 'O', 'U', 'T', 'E', 'Q'
                .byte '/', '8', $EF, 'P', 'I', 'Y', 'R', 'W'
                .byte '6', '4', $EF, 'L', 'J', 'G', 'D', 'A'
                .byte '*', '5', $EF, ':', 'K', 'H', 'F', 'S'
                .byte '3', '1', $0D, ';', 'M', 'B', 'C', 'Z'
                .byte '+', '2', $EF, '?', ',', 'N', 'V', 'X'
                .byte '-', '0', $00, '>', $FF, ']', '@', $00
                .byte '=', '.', $EF, $03, '<', ' ', '[', $FF

                ; $88 (08) is on DEL, (Should be $94/$14)
                ; $5f (_) is on the <- key?

                ; Keymasks to remove modifers from the scan results
KeyMask:        .byte $FF, $DF, $FF, $DF, $DF, $DF, $FF, $DF, $D6, $DE
                ; Which bits indicate shift keys:
ShiftMask:      .byte $00, $00, $00, $00, $00, $00, $00, $00, $21, $00
                ; Which bits indicate ctrl keys
CtrlMask:       .byte $00, $00, $00, $00, $00, $00, $00, $00, $08, $01

                ; Keyboard matrix with shift pressed, needed for consistent shifts
                ; Matrix for Graphics keyboards
KbdMatrixShift: .byte $F4, $F0, $5F, '(', '&', '%', '#', '!'
                .byte $08, $F1, $EF, ')', '\', '`', '$', '"'   ;";
                .byte '9', '7', '|', $CF, $D5, $D4, $C5, $D1
                .byte '/', '8', $EF, $D0, $C9, $D9, $D2, $D7
                .byte '6', '4', $EF, $CC, $CA, $C7, $C4, $C1
                .byte '*', '5', $EF, ':', $CB, $C8, $C6, $D3
                .byte '3', '1', $0D, ';', $CD, $C2, $C3, $DA
                .byte '+', '2', $EF, '?', ',', $CE, $D6, $D8
                .byte '-', '0', $00, '>', $FF, '}', '~', $00
                .byte '=', '.', $EF, $03, '<', ' ', '{', $FF

.endif
