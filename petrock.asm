;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) Dave Plummer, 02/11/2022 Initial commit
;-----------------------------------------------------------------------------------

.SETCPU "65C02"

; Definitions -----------------------------------------------------------------------

DEBUG           = 1                 ; Enable code that only is included for debug builds
EPROM           = 0                 ; When TRUE, no BASIC stub, no load address in file
COLUMNS         = 40                ; Screen width, either 40 or 80

SERIAL_BUF_LEN  = 40                ; How many input characters we can accept

; Defines below this line should generally not require changing

DEVICE_NUM      = 2
MINUTE_JIFFIES  = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES  = 60                ; Number of jiffies in a second

NUM_BANDS       = 16                ; 16 bands in the spectrum data

; System Locations ------------------------------------------------------------------

.INCLUDE "common.inc"

.if C64
    .INCLUDE "c64.inc"
    .INCLUDE "kernal.inc"
    .if EPROM
        BASE    = $8000     ; Open C64 ROM space (not re)
    .else
        BASE    = $0801     ; C64 Start of BASIC
    .endif
.endif

.if PET
    .INCLUDE "pet.inc"
    .INCLUDE "petbasic4.inc"
    .if EPROM
        BASE    = $B000     ; Open PET ROM space
    .else
        BASE    = $0401     ; PET Start of BASIC
    .endif
.endif

JIFFY_TIMER = $008F

; Our Definitions -------------------------------------------------------------------

zptmp  = $BD
zptmpB = $00
zptmpC = $1F

.org SCRATCH_START
.bss

; These are scratch variables - they are here in the cassette buffer so that we can
; be burned into ROM (if we just used .byte we couldn't write values back)

ScratchStart:    
    temp:            .res  1                ; General scratch variable
    MultiplyTemp:    .res  1                ; Scratch variable for multiply code
    resultLo:        .res  1                ; Results from multiply operations
    resultHi:        .res  1                
    serialBuffer:    .res SERIAL_BUF_LEN    ; Our input buffer
    Peaks:           .res NUM_BANDS         ; Data for band peaks
ScratchEnd:


.assert * <= SCRATCH_END, error         ; Make sure we haven't run off the end of the buffer

; Start of Binary -------------------------------------------------------------------

.code

; BASIC program to load and execute ourselves.  Simple lines of tokenized BASIC that
; have a banner comment and then a SYS command to start the machine language code.

.if !EPROM
                .org 0000
                .word BASE
                .org  BASE
Line10:         .word Line15                        ; Next line number
                .word 10                            ; Line Number 10    
                .byte TK_REM                        ; REM token
                .literal " - SPECTRUM ANALYZER DISPLAY", 00
Line15:         .word Line16
                .word 15
                .byte TK_REM
                .literal " - GITHUB/PLUMMERSSOFTWARELLC/", 00
Line16:         .word Line20
                .word 16
                .byte TK_REM
                .literal " - PETROCK - COPYRIGHT 2022", 00                
Line20:         .word endOfBasic                    ; PTR to next line, which is 0000
                .word 20                            ; Line Number 20
                .byte TK_SYS                        ;   SYS token
                .literal " "
                .literal .string(*+7)               ; Entry is 7 bytes from here, which is
                                                    ;  not how I'd like to do it but you cannot
                                                    ;  use a forward reference in STR$()

                .byte 00                            ; Do not modify without understanding 
endOfBasic:     .word 00                            ;   the +7 expression above, as this is
.else                                               ;   exactly 7 bytes and must match it
    .org BASE
.endif                                              

;-----------------------------------------------------------------------------------
; Start of Code
;-----------------------------------------------------------------------------------

start:          cld
                jsr InitVariables       ; Since we can be in ROM, zero stuff out
                jsr ClearScreen         

                ldy #>startstr          ; Output exiting text and exit
                lda #<startstr
                jsr WriteLine

                ; Do stuff here

                ldy #>exitstr           ; Output exiting text and exit
                lda #<exitstr
                jsr WriteLine

                rts


;-----------------------------------------------------------------------------------
; InitVariables 
;-----------------------------------------------------------------------------------
; We use a bunch of storage in the system (on the PET, it's Cassette Buffer #2) and
; it starts out in an unknown state, so we have code to zero it or set it to defaults
;-----------------------------------------------------------------------------------

InitVariables:  ldx #ScratchEnd-ScratchStart
                lda #$00                    ; Init variables to #0
:               sta ScratchStart, x
                dex
                cpx #$ff
                bne :-

                rts


;-----------------------------------------------------------------------------------
; GetCursorAddr - Returns address of X/Y position on screen
;-----------------------------------------------------------------------------------
;       IN  X:  X pos
;       IN  Y:  Y pos
;       OUT X:  lsb of address
;       OUT Y:  msb of address
;-----------------------------------------------------------------------------------

GetCursorAddr:  stx temp
.if COLUMNS=80
                asl temp                ; We have 80 columns, so double X
.endif
                ldx #COLUMNS
                jsr Multiply            ; Result of Y*COLUMNS in AY
                sta resultLo
                sty resultHi
                lda resultLo
                clc
                adc #<SCREEN_MEM
                bcc nocarry
                inc resultHi
                clc
nocarry:        adc temp
                sta resultLo
                lda resultHi
                adc #>SCREEN_MEM
                sta resultHi
                ldx resultLo
                ldy resultHi
                rts

;-----------------------------------------------------------------------------------
; Multiply      Multiplies X * Y == ResultLo/ResultHi
;-----------------------------------------------------------------------------------
;               X       8 bit value in
;               Y       8 bit value in
;-----------------------------------------------------------------------------------

Multiply:
                stx resultLo
                sty MultiplyTemp
                lda #$00
                tay
                sty resultHi
                beq enterLoop
doAdd:          clc
                adc resultLo
                tax
                tya
                adc resultHi
                tay
                txa
loop:           asl resultLo
                rol resultHi
enterLoop:      lsr MultiplyTemp
                bcs doAdd
                bne loop
                rts

;-----------------------------------------------------------------------------------
; ClearScreen
;-----------------------------------------------------------------------------------

ClearScreen:    lda #$93
                jsr CHROUT    
                rts

                lda #$20
                ldx #$00
 :              sta SCREEN_MEM, x
                sta SCREEN_MEM + $100, x
                sta SCREEN_MEM + $200, x
                sta SCREEN_MEM + $300, x
                dex
                bne :-
                rts

;-----------------------------------------------------------------------------------
; WriteLine - Writes a line of text to the screen using CHROUT ($FFD2)
;-----------------------------------------------------------------------------------
;           Y:  MSB of address of null-terminated string
;           A:  LSB
;-----------------------------------------------------------------------------------

WriteLine:      sta zptmp
                sty zptmp+1
                ldy #0
@loop:          lda (zptmp),y
                beq done
                jsr CHROUT
                iny
                bne @loop
done:           rts

;-----------------------------------------------------------------------------------
; RepeatChar - Writes a character A to the output X times
;-----------------------------------------------------------------------------------
;           A:  Character to write
;           X:  Number of times to repeat it
;-----------------------------------------------------------------------------------
            
RepeatChar:     jsr CHROUT
                dex
                bne RepeatChar
                rts

startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0
