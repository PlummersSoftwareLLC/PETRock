;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) Dave Plummer, 02/11/2022 Initial commit
;-----------------------------------------------------------------------------------

; Definitions -----------------------------------------------------------------------

DEBUG               = 1                 ; Enable code that only is included for debug builds
EPROM               = 0                 ; When TRUE, no BASIC stub, no load address in file

SERIAL_BUF_LEN      = 40                ; How many input characters we can accept

; Defines below this line should generally not require changing

DEVICE_NUM          = 2
MINUTE_JIFFIES      = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES      = 60                ; Number of jiffies in a second

NUM_BANDS           = 16                ; 16 bands in the spectrum data

; Symbol definitions for square drawing

TOPLEFTSYMBOL		= 112               ; PETSCI for graphics that make a square 
BOTTOMRIGHTSYMBOL	= 125
TOPRIGHTSYMBOL		= 110
BOTTOMLEFTSYMBOL	= 109

HLINESYMBOL			= 64
VLINESYMBOL			= 93

; System Locations ------------------------------------------------------------------

.ifndef C64
    C64         = 0
.endif

.ifndef PET
    PET         = 0
.endif

.if (.not (PET .xor C64))
    .fatal "Define exactly one of PET or C64 to equal 1."
.endif

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
    temp2:           .res  1                ; Second  scratch variable
    temp3:           .res  1                ; Third scratch variable
    temp4:           .res  1                ; Fourth scratch variable
    MultiplyTemp:    .res  1                ; Scratch variable for multiply code
    resultLo:        .res  1                ; Results from multiply operations
    resultHi:        .res  1                
    serialBuffer:    .res SERIAL_BUF_LEN    ; Our input buffer
    Peaks:           .res NUM_BANDS         ; Data for band peaks
    SquareX:		 .res  1                ; Args for DrawSquare
    SquareY:		 .res  1
    Width:			 .res  1
    Height:			 .res  1


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

                lda #0
                sta SquareX
                lda #2
                sta SquareY
                lda #COLUMNS-1
                sta Width
                lda #20
                sta Height
:				jsr DrawSquare
                inc SquareX
                inc SquareY
                dec Width
                dec Width
                dec Height
                dec Height
                bne :-


: 				jsr GETIN				; Keyboard Handling
                cmp #0
                beq :-

                cmp #$03
                bne :-

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
;		IN  X:	X pos
;       IN  Y:  Y pos
;       OUT X:  lsb of address
;       OUT Y:  msb of address
;-----------------------------------------------------------------------------------

ScreenLineAddresses:

.word			SCREEN_MEM +  0 * COLUMNS, SCREEN_MEM +  1 * COLUMNS, SCREEN_MEM +  2 * COLUMNS, SCREEN_MEM +  3 * COLUMNS 
.word           SCREEN_MEM +  4 * COLUMNS, SCREEN_MEM +  5 * COLUMNS, SCREEN_MEM +  6 * COLUMNS, SCREEN_MEM +  7 * COLUMNS
.word			SCREEN_MEM +  8 * COLUMNS, SCREEN_MEM +  9 * COLUMNS, SCREEN_MEM + 10 * COLUMNS, SCREEN_MEM + 11 * COLUMNS 
.word           SCREEN_MEM + 12 * COLUMNS, SCREEN_MEM + 13 * COLUMNS, SCREEN_MEM + 14 * COLUMNS, SCREEN_MEM + 15 * COLUMNS
.word 			SCREEN_MEM + 16 * COLUMNS, SCREEN_MEM + 17 * COLUMNS, SCREEN_MEM + 18 * COLUMNS, SCREEN_MEM + 19 * COLUMNS 
.word           SCREEN_MEM + 20 * COLUMNS, SCREEN_MEM + 21 * COLUMNS, SCREEN_MEM + 22 * COLUMNS, SCREEN_MEM + 23 * COLUMNS
.word			SCREEN_MEM + 24 * COLUMNS

GetCursorAddr:  stx temp
                 tya
                asl
                tay
                clc
                lda ScreenLineAddresses,y
                adc temp
                tax
                lda ScreenLineAddresses+1,y
                adc #0
                tay
                rts
;-----------------------------------------------------------------------------------
; Multiply      Multiplies X * Y == ResultLo/ResultHi
;-----------------------------------------------------------------------------------
;               X       8 bit value in
;               Y       8 bit value in
;-----------------------------------------------------------------------------------

Multiply:       stx resultLo
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

;-----------------------------------------------------------------------------------
; SymbolTable	- All 16 combinations of lines and what PETSCII char they map to
;-----------------------------------------------------------------------------------

SymbolTable:	.byte 32
                .byte 0
                .byte 0
                .byte 93		; VLINESYMBOL_ID
                .byte 0
                .byte 110		; BOTTOMLEFTSYMBOL_ID
                .byte 125		; TOPLEFTSYMBOL_ID
                .byte 115
                .byte 0
                .byte 112		; BOTTOMRIGHTSYMBOL_ID
                .byte 109		; TOPRIGHTSYMBOL_ID
                .byte 107
                .byte 64		; HLINESYMBOL_ID
                .byte 114
                .byte 113
                .byte 91

                ; The above table must represent a full 4 bits, and hence is 16 bytes

                .assert * = SymbolTable + 16, error

;-----------------------------------------------------------------------------------
; DrawSquare
;-----------------------------------------------------------------------------------
; Draw a square on the offscreen buffer.
;
; TopLeftX
; TopLeftY
; Width
; Height
;-----------------------------------------------------------------------------------

DrawSquare:		ldx		SquareX
                ldy		SquareY
                lda		#TOPLEFTSYMBOL
                jsr		OutputSymbolXY

                lda		Width
                jsr		DrawHLine

                lda		Height
                jsr		DrawVLine

                lda		SquareX
                clc
                adc		Width
                tax
                ldy		SquareY
                lda		#TOPRIGHTSYMBOL
                jsr		OutputSymbolXY
                lda		Height
                jsr		DrawVLine

                ldx		SquareX
                lda		SquareY
                clc
                adc		Height
                tay
                lda		#BOTTOMLEFTSYMBOL
                jsr		OutputSymbolXY
                lda		Width
                jsr		DrawHLine

                lda     SquareX
                clc
                adc		Width
                tax	
                lda		SquareY
                clc
                adc		Height
                tay		
                lda		#BOTTOMRIGHTSYMBOL
                jsr		OutputSymbolXY
                
                rts

;-----------------------------------------------------------------------------------
; OutputSymbolXY	Draws the given symbol A into the screen at pos X, Y
;-----------------------------------------------------------------------------------
;				X		X Coord	[PRESERVED]
;				Y		Y Coord [PRESERVED]
;				A		Symbol
;-----------------------------------------------------------------------------------
; Unlike my original impl, this doesn't merge, so lines can't intersect, but this
; way no intermediate buffer is required and it draws right to the screen directly.
;-----------------------------------------------------------------------------------
        
OutputSymbolXY:	sta		temp4
                txa
                pha
                tya
                pha
                
                jsr		GetCursorAddr
                stx     zptmp
                sty     zptmp+1
                ldy     #0
                lda     temp4
                sta     (zptmp),y

                pla
                tay
                pla
                tax
                rts

;-----------------------------------------------------------------------------------
; DrawHLine		Draws a horizontal line in the offscreen buffer
;-----------------------------------------------------------------------------------
;				X		X Coord of Start
;				Y		Y Coord of Start
;				A		Length of line
;-----------------------------------------------------------------------------------
            
DrawHLine:		; from X+1, Y to X-1, Y
                sec
                sbc #1						; Two less than full length due to corners
                sta temp3
                txa
                pha
;				beq hdone
:				inx
                lda #HLINESYMBOL
                jsr	OutputSymbolXY
                dec temp3
                bne :-
hdone:			pla
                tax
                rts

DrawVLine:		; from X, Y+1 to X, Y-1
                sec
                sbc #1
                sta temp3
                tya
                pha
                beq vdone
:				iny
                lda #VLINESYMBOL
                jsr OutputSymbolXY
                dec temp3
                bne :-
vdone:  		pla
                tay
                rts


startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0

OffscreenBuffer: 
                .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
                .res  1024,$EE
