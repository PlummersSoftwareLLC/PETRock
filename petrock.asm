;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) PlummersSoftwareLLC, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------

.SETCPU "6502"

; Include the system headers and application defintions ----------------------------

__C64__ = 1

.include "petrock.inc"

; Our BSS Data  --------------------------------------------------------------------

.org SCRATCH_START
.bss

; These are loca BSS variables - they are here in the cassette buffer so that we can
; be burned into ROM (if we just used .byte we couldn't write values back)

ScratchStart:    
    tempFillSquare:	 .res  1                    ; Temp used by FillSquare
    tempDrawLine:    .res  1                    ; Temp used by DrawLine
    tempOutput:      .res  1                    ; Temp used by OutputSymbol
    tempByteCopy:    .res  1
    tempX:           .res  1                    ; Preserve X Pos
    tempY:           .res  1                    ; Preserve Y Pos
    lineChar:	       .res  1                    ; Line draw char
    SquareX:		     .res  1                    ; Args for DrawSquare
    SquareY:		     .res  1
    Width:			     .res  1
    Height:			     .res  1				            ; Height of area to draw
    ClearHeight:	   .res  1				            ; Height of area to clear
    DataIndex:       .res  1                    ; Index into fakedata for demo
    MultiplyTemp:	   .res  1                    ; Scratch variable for multiply code
    resultLo:		     .res  1			              ; Results from multiply operations
    resultHi:		     .res  1
    VU:              .res  1                    ; VU Audio Data
    Peaks:           .res  NUM_BANDS            ; Peak Data for current frame    
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
; Start of Assembly Code
;-----------------------------------------------------------------------------------

start:          cld
                jsr InitVariables               ; Since we can be in ROM, zero stuff out

                lda #BLACK
                sta SCREEN_COLOR
                sta BORDER_COLOR

                jsr EmptyBorder
drawLoop:	  
:       				bit SCREEN_CONTROL              ; Wait for a known point in the raster
                bpl :-
                
              .if TIMING
                lda #DARK_GREY
                sta BORDER_COLOR
              .endif

                jsr FillPeaks

                lda #LIGHT_BLUE
                sta TEXT_COLOR

                ldx #24                          ; Print "Current Frame" banner
                ldy #09
                clc
                jsr PlotEx
                ldy #>framestr                   
                lda #<framestr
                jsr WriteLine

                ldx DataIndex
                lda #0
                jsr BASIC_INTOUT
                lda #' '
                jsr CHROUT
                jsr CHROUT
              
                lda #LIGHT_GREEN
                sta TEXT_COLOR

                jsr DrawVU                      ; Draw the VU bar at the top of the screen

                ldx #NUM_BANDS - 1              ; Draw each of the bands in reverse order
:
           			lda Peaks, x                    ; Scroll the others in place
                jsr DrawBand
                dex
                bpl :-

              .if TIMING
                lda #BLACK
                sta BORDER_COLOR			
              .endif

:			        	bit SCREEN_CONTROL
                bmi :-

                jsr GETIN				                ; Keyboard Handling
                cmp #$03
                bne drawLoop

                ldy #>exitstr                   ; Output exiting text and exit
                lda #<exitstr
                jsr WriteLine

                rts

EmptyBorder:    ldy #>clrwhite                  ; Set cursor to white and clear screen
                lda #<clrwhite
                jsr WriteLine
                lda #0
                sta SquareX
                sta SquareY
                lda #XSIZE
                sta Width
                lda #YSIZE
                sta Height
                jsr DrawSquare
                jsr InitVU                      ; Let the VU meter paint its color mem, etc
                rts

ScrollBands:    lda Peaks+NUM_BANDS-1           ; Wrap data around from end to start
                pha
                lda #NUM_BANDS - 1              ; Draw each of the bands in reverse order
                tax
 :         			lda Peaks, x                    ; Scroll the others in place
                sta Peaks+1, x
                dex
                bpl :-
                pla
                sta Peaks

;-----------------------------------------------------------------------------------
; FillPeaks 
;-----------------------------------------------------------------------------------
; Copy data from the current index of the fake data table to the current peak data 
; and vu value
;-----------------------------------------------------------------------------------

FillPeaks:    tya
              pha
              txa
              pha

              ldx DataIndex         ; Multiply the row number by 16 to get the offset
              ldy #16               ; into the data table
              jsr Multiply

              lda resultLo          ; Now add the offset and the table base together
              clc                   ;  and store the resultant ptr in zptmpC
              adc #<AudioData
              sta zptmpB
              lda resultHi
              adc #>AudioData
              sta zptmpB+1

              ldy #15               ; Copy the 16 bytes at the ptr address to the
:             lda (zptmpB), y       ;   PeakData table
              sta Peaks, y
              dey
              bpl :-

              lda #<PeakData        ; Copy the single VU byte from the PeakData
              sta zptmpB            ;  table into the VU variable
              lda #>PeakData
              sta zptmpB+1
              ldy DataIndex
              lda (zptmpB), y
              sta VU

              inc DataIndex           ; Inc DataIndex - Assumes wrap, so if you
                                      ;   have exacly 256 bytes, you'd need to
                                      ;   check and fix that here

              pla
              tax
              pla
              tay
              rts

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

                .word SCREEN_MEM +  0 * XSIZE, SCREEN_MEM +  1 * XSIZE 
                .word SCREEN_MEM +  2 * XSIZE, SCREEN_MEM +  3 * XSIZE
                .word SCREEN_MEM +  4 * XSIZE, SCREEN_MEM +  5 * XSIZE
                .word SCREEN_MEM +  6 * XSIZE, SCREEN_MEM +  7 * XSIZE
                .word SCREEN_MEM +  8 * XSIZE, SCREEN_MEM +  9 * XSIZE
                .word SCREEN_MEM + 10 * XSIZE, SCREEN_MEM + 11 * XSIZE 
                .word SCREEN_MEM + 12 * XSIZE, SCREEN_MEM + 13 * XSIZE
                .word SCREEN_MEM + 14 * XSIZE, SCREEN_MEM + 15 * XSIZE
                .word SCREEN_MEM + 16 * XSIZE, SCREEN_MEM + 17 * XSIZE
                .word SCREEN_MEM + 18 * XSIZE, SCREEN_MEM + 19 * XSIZE 
                .word SCREEN_MEM + 20 * XSIZE, SCREEN_MEM + 21 * XSIZE
                .word SCREEN_MEM + 22 * XSIZE, SCREEN_MEM + 23 * XSIZE
                .word SCREEN_MEM + 24 * XSIZE
                .assert( (* - ScreenLineAddresses) = YSIZE * 2), error

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
; DrawVU        Draw the current VU meter at the top of the screen
;-----------------------------------------------------------------------------------
                
                ; Color memory bytes that will back the VU meter, and only need to be set once
VUColorTable:   .byte RED, RED, RED, YELLOW, YELLOW, YELLOW, YELLOW, YELLOW
                .byte GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN
                .byte BLACK, BLACK
                .byte GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN, GREEN
                .byte YELLOW, YELLOW, YELLOW, YELLOW, YELLOW, RED, RED, RED
                VUColorTableLen = * - VUColorTable
                .assert(VUColorTableLen >= MAX_VU * 2 + 2), error   ; VU plus two spaces in the middle

                ; Copy the color memory table for the VU meter to the right place in color RAM

InitVU:         ldy #VUColorTableLen-1
:               lda VUColorTable, y
                sta VUCOLORPOS, y
                dey 
                bpl :-

                ; Draw the VU meter on right, then draw its mirror on the left

DrawVU:         lda #<VUPOS1
                sta zptmp
                lda #>VUPOS1
                sta zptmp+1
                lda #<VUPOS2
                sta zptmpB
                lda #>VUPOS2
                sta zptmpB+1                

                ldy #0
                ldx #MAX_VU-1
vuloop:         lda #VUSYMBOL
                cpy VU              ; If we're at or below the VU value we use the
                bcc :+              ;   VUSYMBOL to draw the current char else we use
                lda #MEDIUMSHADE    ;   the partial shade symbol
:               sta (zptmp),y       ; Store the char in screen memory
                sta tempOutput

                tya                 ; 
                pha                 ; Save Y
                txa           
                tay                 ; Move X into Y

                lda tempOutput                
                sta (zptmpB), y

                pla
                tay
                iny
                dex
                cpy #MAX_VU
                bcc vuloop

                rts

;-----------------------------------------------------------------------------------
; Multiply		Multiplies X * Y == ResultLo/ResultHi
;-----------------------------------------------------------------------------------
;				X		8 bit value in
;				Y		8 bit value in
;
; Apparent credit to Leif Stensson for this approach!
;-----------------------------------------------------------------------------------

Multiply:
                stx resultLo      
                sty resultHi
                lda  #0
                ldx  #8
                lsr  resultLo
mloop:          bcc  no_add
                clc
                adc  resultHi
no_add:         ror
                ror  resultLo
                dex
                bne  mloop
                sta  resultHi
                rts

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
                adc #XSIZE				            ;  and inc the msb if we saw the carry get get
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
               ; dey                            ; Normally post-dec Y to fix it up, but not needed here
:                                               ;   because Y is loaded explicitly below anyway
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
              ; dex						             	    ; Put X back where it was if you need to preserve X

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
                stx tempX
                sty tempY

                jsr	GetCursorAddr				        ; Store the screen code in
                stx zptmp						            ; screen RAM
                sty zptmp+1

                ldy #0
                lda tempOutput
                sta (zptmp),y

                lda zptmp
                clc
                adc #<(COLOR_MEM - SCREEN_MEM)
                sta zptmp
                lda zptmp+1
                adc #>(COLOR_MEM - SCREEN_MEM)
                sta zptmp+1
                lda TEXT_COLOR
                sta (zptmp),y
                ldx tempX
                ldy tempY

                rts

;-----------------------------------------------------------------------------------
; DrawHLine		Draws a horizontal line in the offscreen buffer
;-----------------------------------------------------------------------------------
;				X		X Coord of Start [PRESERVED]
;				Y		Y Coord of Start [PRESERVED]
;				A		Length of line
;-----------------------------------------------------------------------------------
; BUGBUG (Optimization, Davepl) - This code draws color RAM edvery time a line is
; drawn, which would allow for different colors every time, which might be cool, but
; if that ability is never used then the color memory only really need to be 
; written once (or each time the screen is cleared, etc).
;-----------------------------------------------------------------------------------

DrawHLine:		  sta tempDrawLine					          ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                tya                                 ; Save X, Y
                pha
                txa
                pha
                
                jsr GetCursorAddr
                stx zptmp
                sty zptmp+1
                
                txa                              ; Add the distance to color memory to
                clc                              ; the zptmp pointer and store it in
                adc #<(COLOR_MEM - SCREEN_MEM)   ; zptmpB so that it in turn points at
                sta zptmpB                       ; the right place in color ram
                tya
                clc
                adc #>(COLOR_MEM - SCREEN_MEM)
                sta zptmpB+1

                ldy tempDrawLine                 ; Draw the line
                dey
:				        lda lineChar                     ; Store the line character in screen ram
                sta (zptmp), y
                lda TEXT_COLOR                   ; Store current color in color ram
                sta (zptmpB), y
                dey                              ; Rinse and repeat
                bpl :-

                pla                              ; Restore X, Y
                tax
                pla
                tay
                rts

DrawVLine:		  sta tempDrawLine			            ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                jsr GetCursorAddr                 ; Get the screen memory addr of the
                stx zptmp                         ;   line's X/Y start position
                sty zptmp+1

                txa                               ; Take the screen memory pointer and add
                clc                               ;   the distance to color memory to it, 
                adc #<(COLOR_MEM - SCREEN_MEM)    ;   so zptmpB points at the right area of
                sta zptmpB                        ;   color memory.
                tya
                clc
                adc #>(COLOR_MEM - SCREEN_MEM)
                sta zptmpB+1

vloop:			    lda lineChar				              ; Store the line char in screen mem

                ldy #0
                sta (zptmp), y

                lda zptmp                         ; Add the offset to color memory
                clc                               ;  to the character pointer and
                adc #<(COLOR_MEM - SCREEN_MEM)    ;  then store the current color
                sta zptmpB                        ;  in color memory where it goes
                lda zptmp+1
                adc #>(COLOR_MEM - SCREEN_MEM)
                sta zptmpB+1
                lda TEXT_COLOR
                sta (zptmpB), y

                lda zptmp					                ; Now add 40/80 to the lsb of ptr
                clc
                adc #XSIZE
                sta zptmp
                bcc :+						
                inc zptmp+1				            	  ; On overflow in the msb as well
:

				        dec tempDrawLine			            ; One less line to go
                bpl vloop					
                rts

;-----------------------------------------------------------------------------------
; DrawBand		Draws a single band of the spectrum analyzer
;-----------------------------------------------------------------------------------
;				X		Band Number		[PRESERVED]
;				A		Height of bar
;-----------------------------------------------------------------------------------

BandColors:     .byte RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, PURPLE,  RED
                .byte ORANGE, YELLOW, GREEN, CYAN, BLUE, PURPLE, RED, ORANGE

DrawBand:		    sta Height

                lda BandColors, x               ; Draw this band in the color specified for this bar
                sta TEXT_COLOR

                txa					                    ; X pos will be column number times BAND_WIDTH plus margin
                pha
                asl						                  ; Multiplty column number by 2 or 4
                clc	
                adc #LEFT_MARGIN		            ; Add that to the left margin, and it's the left edge
                sta SquareX

                lda #BAND_WIDTH			            ; All bands are this wide.  Looks cool to overlap by up to 1.
                sta Width

                lda #TOP_MARGIN			            ; Everything starts at the top margin
                sta SquareY

                lda #BAND_HEIGHT - 1	
                sec                             
                sbc Height                      
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

;-----------------------------------------------------------------------------------
; PlotEx		Replacement for KERNAL plot that fixes color ram update bug
;-----------------------------------------------------------------------------------
;				X		Cursor Y Pos
;				Y   Cursor X Pos
;       (NOTE Reversed)
;-----------------------------------------------------------------------------------
PlotEx:
        bcs     :+
        jsr     PLOT            ; Set cursor position using original ROM PLOT
        jmp     UPDCRAMPTR      ; Set pointer to color RAM to match new cursor position
:       jmp     PLOT            ; Get cursor position

; String literals at the end of file, as was the style at the time!

startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0
framestr:       .literal "  CURRENT FRAME: ", 0
clrwhite:		    .literal $99, $93, 0
.include "fakedata.inc"

