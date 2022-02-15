;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) PlummersSoftwareLLC, 02/11/2022 Initial commit
;         David Plummer
;         Rutger Bergen
;-----------------------------------------------------------------------------------

.SETCPU "6502"

; Definitions -----------------------------------------------------------------------

DEBUG               = 1                         ; Enable code that only is included for debug builds
EPROM               = 0                         ; When TRUE, no BASIC stub, no load address in file
TIMING              = 0                         ; Change border color to show drawing progress
SERIAL_BUF_LEN      = 16                        ; How many input characters we can accept

; Defines below this line should generally not require changing

DEVICE_NUM          = 2
NUM_BANDS           = 16                        ; 16 bands in the spectrum data

; Symbol definitions for square drawing - PETSCI for graphics that make a square 

TOPLEFTSYMBOL		  = 79                
BOTTOMRIGHTSYMBOL	= 122               
TOPRIGHTSYMBOL		= 80
BOTTOMLEFTSYMBOL	= 76
VLINE1SYMBOL		  = 101                         ; Vertical line left side
VLINE2SYMBOL      = 103                         ; Vertical line right side
HLINE1SYMBOL		  = 99                          ; Horizontal line top side
HLINE2SYMBOL		  = 100                         ; Horizontal line bottom side

; System Locations ------------------------------------------------------------------

.INCLUDE "common.inc"

.if COLUMNS = 40
    BAND_WIDTH      = 2
    LEFT_MARGIN     = 4
.endif
.if COLUMNS = 80
    BAND_WIDTH      = 4
    LEFT_MARGIN     = 8
.endif

TOP_MARGIN          = 3
BOTTOM_MARGIN       = 2
BAND_HEIGHT         = 20

.assert (TOP_MARGIN + BOTTOM_MARGIN + BAND_HEIGHT <= 25), error

; Our Definitions -------------------------------------------------------------------

.org SCRATCH_START
.bss

; These are loca BSS variables - they are here in the cassette buffer so that we can
; be burned into ROM (if we just used .byte we couldn't write values back)

ScratchStart:    
    tempFillSquare:	 .res  1                    ; Temp used by FillSquare
    tempDrawLine:    .res  1                    ; Temp used by DrawLine
    tempOutput:      .res  1                    ; Temp used by OutputSymbol
    lineChar:	       .res  1                    ; Line draw char
    charColor:		   .res  1                    ; Current output color
    SquareX:		     .res  1                    ; Args for DrawSquare
    SquareY:		     .res  1
    Width:			     .res  1
    Height:			     .res  1				            ; Height of area to draw
    ClearHeight:	   .res  1				            ; Height of area to clear
ScratchEnd:

.assert * <= SCRATCH_END, error                 ; Make sure we haven't run off the end of the buffer

; Start of Binary -------------------------------------------------------------------

.code

; BASIC program to load and execute ourselves.  Simple lines of tokenized BASIC that
; have a banner comment and then a SYS command to start the machine language code.

.if !EPROM
                .org 0000
                .word BASE
                .org  BASE
Line10:         .word Line15                    ; Next line number
                .word 10                        ; Line Number 10    
                .byte TK_REM                    ; REM token
                .literal " - SPECTRUM ANALYZER DISPLAY", 00
Line15:         .word Line16
                .word 15
                .byte TK_REM
                .literal " - GITHUB/PLUMMERSSOFTWARELLC/", 00
Line16:         .word Line20
                .word 16
                .byte TK_REM
                .literal " - PETROCK - COPYRIGHT 2022", 00                
Line20:         .word endOfBasic                ; PTR to next line, which is 0000
                .word 20                        ; Line Number 20
                .byte TK_SYS                    ;   SYS token
                .literal " "
                .literal .string(*+7)           ; Entry is 7 bytes from here, which
                                                ;  not how I'd like to do it but you cannot
                                                ;  use a forward reference in STR$()

                .byte 00                        ; Do not modify without understanding 
endOfBasic:     .word 00                        ;   the +7 expression above, as this is
.else                                           ;   exactly 7 bytes and must match it
    .org BASE
.endif                                              

;-----------------------------------------------------------------------------------
; Start of Code
;-----------------------------------------------------------------------------------

start:          cld
                
.if PET	
                lda #12                         ; Switch to uppercas/PETSCII
                sta 59468
.endif
                jsr InitVariables               ; Since we can be in ROM, zero stuff out
                jsr ClearScreen         

              .if C64
                lda #BLACK
                sta SCREEN_COLOR
                sta BORDER_COLOR
                lda #LIGHT_GREEN
                sta charColor

                ldy #>clrwhite                  ; Set cursor to white and clear screen
                lda #<clrwhite
                jsr WriteLine
              .endif

                lda #0
                sta SquareX
                sta SquareY
                lda #COLUMNS
                sta Width
                lda #ROWS
                sta Height
                jsr DrawSquare

;                ldy #>startstr                 ; Output exiting text and exit
;                lda #<startstr
;                jsr WriteLine

drawLoop:	  
              .if C64                           ; Wait for V-Blank
:       				bit $D011
                bpl :-
              .endif				
                
              .if TIMING
                lda #DARK_GREY
                sta BORDER_COLOR
              .endif

                lda Peaks+NUM_BANDS-1           ; Wrap data around from end to start
                pha	

                lda #NUM_BANDS - 1              ; Scroll the others in place
                tax
:	        			lda Peaks, x
                sta Peaks+1, x
                clc
                jsr DrawBand
                dex
                bpl :-

                pla
                sta Peaks


              .if TIMING
                lda #BLACK
                sta BORDER_COLOR			
:			        	bit $D011
                bmi :-
              .endif

                jsr GETIN				                ; Keyboard Handling
                cmp #$03
                bne drawLoop

                ldy #>exitstr                   ; Output exiting text and exit
                lda #<exitstr
                jsr WriteLine

                rts

; Dummy data until replaced by live spectrum data

Peaks:			.byte 20, 20, 19, 16, 11, 6, 3, 2, 2, 4, 7, 12, 15, 17, 19, 19

;-----------------------------------------------------------------------------------
; InitVariables 
;-----------------------------------------------------------------------------------
; We use a bunch of storage in the system (on the PET, it's Cassette Buffer #2) and
; it starts out in an unknown state, so we have code to zero it or set it to defaults
;-----------------------------------------------------------------------------------

InitVariables:  ldx #ScratchEnd-ScratchStart
                lda #$00                        ; Init variables to #0
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

                .word SCREEN_MEM +  0 * COLUMNS, SCREEN_MEM +  1 * COLUMNS 
                .word SCREEN_MEM +  2 * COLUMNS, SCREEN_MEM +  3 * COLUMNS
                .word SCREEN_MEM +  4 * COLUMNS, SCREEN_MEM +  5 * COLUMNS
                .word SCREEN_MEM +  6 * COLUMNS, SCREEN_MEM +  7 * COLUMNS
                .word SCREEN_MEM +  8 * COLUMNS, SCREEN_MEM +  9 * COLUMNS
                .word SCREEN_MEM + 10 * COLUMNS, SCREEN_MEM + 11 * COLUMNS 
                .word SCREEN_MEM + 12 * COLUMNS, SCREEN_MEM + 13 * COLUMNS
                .word SCREEN_MEM + 14 * COLUMNS, SCREEN_MEM + 15 * COLUMNS
                .word SCREEN_MEM + 16 * COLUMNS, SCREEN_MEM + 17 * COLUMNS
                .word SCREEN_MEM + 18 * COLUMNS, SCREEN_MEM + 19 * COLUMNS 
                .word SCREEN_MEM + 20 * COLUMNS, SCREEN_MEM + 21 * COLUMNS
                .word SCREEN_MEM + 22 * COLUMNS, SCREEN_MEM + 23 * COLUMNS
                .word SCREEN_MEM + 24 * COLUMNS

GetCursorAddr:  tya
                asl
                tay
                txa
                clc
                adc ScreenLineAddresses,y
                tax
                lda ScreenLineAddresses+1,y
                adc #0
                tay
                rts

;-----------------------------------------------------------------------------------
; ClearScreen
;-----------------------------------------------------------------------------------

ClearScreen:    lda #CLRHOME                    ; PETSCII for clear screen
                jsr CHROUT    
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
; FillSquare
;-----------------------------------------------------------------------------------
; Fills a square on the screen buffer with lineChar, such as space to clear it
;
; SquareX
; SquareY
; Width
; Height
; lineChar
;-----------------------------------------------------------------------------------

FillSquare:		  tya						                  ; Save Y
                pha
                txa						                  ; Save X
                pha
                    
                lda ClearHeight				          ; tempHeight = Height
                sta tempFillSquare
                ldx SquareX
                ldy SquareY
                jsr GetCursorAddr		            ; Start of line Y
                stx zptmpB				              ; Save cursor addr in zptmp
                sty zptmpB+1

addrloop:		    lda lineChar
                ldy Width				                ; Starting at zptmp we write Width bytes
                dey
:				        sta (zptmpB),y			            ; I'd prefer X but that zp mode isn't supported!
                dey
                bpl :-

                lda zptmpB				              ; Advance down a line by adding 40 or 80 to the
                clc						                  ;  zero page screen pointer.  We added it to lsb
                adc #COLUMNS				            ;  and inc the msb if we saw the carry get get
                sta zptmpB				              ;  by the addition.
                bcc :+
                inc zptmpB+1
:				        dec tempFillSquare
                bpl addrloop

                pla							                ; Restore X
                tax
                pla						            	    ; Restore Y
                tay

                rts

;-----------------------------------------------------------------------------------
; DrawSquare
;-----------------------------------------------------------------------------------
; Draw a square on the screen buffer using PETSCII graphics characters.  Each corner
; get's a special PETSCII corner character and the top and bottom and left/right
; sides are specified as separate characters also.
;
; Does not draw the color chars on the 64, expects those to be filled in by someone
; or somethig else, as it slows things down if not strictly needed.
; 
; SquareX      - Arg: X pos of square
; SquareY        Arg: Y pos of square
; Width          Arg: Square width      Must be 2+
; Height         Arg: Square Height     Must be 2+
;-----------------------------------------------------------------------------------

DrawSquare:		  ldx	SquareX
                ldy	SquareY

                lda	Height				              ; Early out - do nothing for less than 2 height
                cmp #2
                bpl :+
                rts
:
                lda	Width				                ; Early out - do nothing for less than 2 width
                cmp #2
                bpl :+
                rts
:
                lda	#TOPLEFTSYMBOL              ; Top Left Corner
                jsr	OutputSymbolXY
                lda #HLINE1SYMBOL               ; Top Line
                sta	lineChar
                lda	Width
                sec
                sbc #2					                ; 2 less due to start and end chars
                cmp #1
                bmi :+
                inx							                ; start one over after start char
                jsr	DrawHLine
                dex						                  ; put x back where it was
:
                lda #VLINE1SYMBOL		            ; Otherwise draw middle vertical lines
                sta	lineChar
                lda	Height
                sec
                sbc #2
                cmp #1
                bmi :+
                iny
                jsr	DrawVLine
                dey
:                
                lda	SquareX
                clc
                adc	Width
                sec
                sbc #1
                tax
                ldy SquareY
                lda	#TOPRIGHTSYMBOL
                jsr	OutputSymbolXY

                lda #VLINE2SYMBOL
                sta	lineChar
                lda	Height
                sec	
                sbc #2
                iny	
                jsr	DrawVLine
bottomline:
                ldx	SquareX
                lda	SquareY
                clc	
                adc	Height
                sec	
                sbc	#1
                tay	
                lda	#BOTTOMLEFTSYMBOL
                jsr	OutputSymbolXY
                lda #HLINE2SYMBOL
                sta	lineChar
                
                lda	Width
                sec	
                sbc #2				   		            ; Account for first and las chars 
                inx							                ; Start one over past stat char
                jsr	DrawHLine
                dex						            	    ; Put X back where it was

                lda SquareX
                clc
                adc	Width
                sec	
                sbc #1
                tax
                lda	SquareY
                clc
                adc	Height
                sec
                sbc #1
                tay	
                lda	#BOTTOMRIGHTSYMBOL
                jsr	OutputSymbolXY			
donesquare:		  rts

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
        
OutputSymbolXY:	sta	tempOutput
                txa
                pha
                tya
                pha

                jsr	GetCursorAddr				        ; Store the screen code in
                stx zptmp						            ; screen RAM
                sty zptmp+1
                ldy #0
                lda tempOutput
                sta (zptmp),y

              .if C64
                pha									            ; Add difference to get us up
                lda zptmp		   				          ; to color RAM and then store
                clc									            ; the current color at the same
                adc	#<(COLOR_MEM - SCREEN_MEM)	; offset into color RAM as we
                sta zptmp						            ; did the char code into screen
                lda zptmp+1						          ; memory
                adc	#>(COLOR_MEM - SCREEN_MEM)
                sta zptmp+1
                lda	charColor
                sta	(zptmp),y
                pla
              .endif

                pla
                tay
                pla
                tax
                rts

;-----------------------------------------------------------------------------------
; DrawHLine		Draws a horizontal line in the offscreen buffer
;-----------------------------------------------------------------------------------
;				X		X Coord of Start [PRESERVED]
;				Y		Y Coord of Start [PRESERVED]
;				A		Length of line
;-----------------------------------------------------------------------------------

DrawHLine:		  sta tempDrawLine					        ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                tya
                pha
                txa
                pha
                
                jsr GetCursorAddr
                stx zptmp
                sty zptmp+1
                lda lineChar				            ; Store the line char in screen mem
                ldy tempDrawLine
:				        sta (zptmp), y
                dey
                bpl :-

                pla
                tax
                pla
                tay
                rts

DrawVLine:		  sta tempDrawLine			            ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                jsr GetCursorAddr
                stx zptmp
                sty zptmp+1
vloop:			    lda lineChar				                ; Store the line char in screen mem
                ldy #0
                sta (zptmp), y
                lda zptmp					              ; Now add 40/80 to the lsb of ptr
                clc
                adc #COLUMNS
                sta zptmp
                bcc :+						
                inc zptmp+1				            	; On overflow in the msb as well
:				        dec tempDrawLine			                  ; One less line to go
                bpl vloop					
                rts
;-----------------------------------------------------------------------------------
; DrawBand		Draws a single band of the spectrum analyzer
;-----------------------------------------------------------------------------------
;				X		Band Number		[PRESERVED]
;				A		Height of bar
;-----------------------------------------------------------------------------------

DrawBand:		    sta Height
                txa					                    ; X pos will be column number times BAND_WIDTH plus margin
                pha
                asl						                  ; Multiplty column number by 2 or 4
              .if COLUMNS = 80
                asl
              .endif
                clc	
                adc #LEFT_MARGIN		            ; Add that to the left margin, and it's the left edge
                sta SquareX

                lda #BAND_WIDTH			            ; All bands are this wide.  Looks cool to overlap by up to 1.
                sta Width

                lda #TOP_MARGIN			            ; Everything starts at the top margin
                sta SquareY

                lda #BAND_HEIGHT - 1	
              .if COLUMNS = 40                  ; This optimization of erasing just down to the top
                sec                             ; doesn't work on bands > 2 wide, where they have stuff
                sbc Height                      ; in the middle that might need to be erased
              .endif
                sta ClearHeight			            ; We clear the whole bar area
                lda #' '				                ; Clear the top, empty portion of the bar
                sta lineChar			
                jsr FillSquare
drawbar:
                lda #(BAND_HEIGHT + TOP_MARGIN)
                sec                             ; doesn't work on bands > 2 wide, where they have stuff
                sbc Height                      ; in the middle that might need to be erased
                sta SquareY
                jsr DrawSquare
                pla
                tax
                rts

; String literals at the end of file, as was the style at the time!

startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0
clrwhite:		    .literal $99, $93, 0

