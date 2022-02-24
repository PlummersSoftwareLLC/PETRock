;-----------------------------------------------------------------------------------
; PETROCK: Spectrum Analyzer Display for C64
;-----------------------------------------------------------------------------------
; (c) Plummer's Software Ltd, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------

.SETCPU "6502"

; Include the system headers and application defintions ----------------------------

__C64__ = 1                             ; Only the C64 is supported
.include "petrock.inc"                  ; Project includes and defintions

; Our BSS Data  --------------------------------------------------------------------

.org SCRATCH_START                      ; Program counter to casssette buffer so that
.bss                                    ;  we can definte our BSS storage variables

; These are local BSS variables.  We're using the cassette buffer for storage.  All
; will be initlialzed to 0 bytes at application startup.

ScratchStart:
    tempFillSquare:  .res  1            ; Temp used by FillSquare
    tempDrawLine:    .res  1            ; Temp used by DrawLine
    tempOutput:      .res  1            ; Temp used by OutputSymbol
    tempByteCopy:    .res  1
    tempHeight:      .res  1            ; Used by DrawBand
    tempX:           .res  1            ; Preserve X Pos
    tempY:           .res  1            ; Preserve Y Pos
    lineChar:        .res  1            ; Line draw char
    SquareX:         .res  1            ; Args for DrawSquare
    SquareY:         .res  1
    Width:           .res  1
    Height:          .res  1            ; Height of area to draw
    ClearHeight:     .res  1            ; Height of area to clear
    DataIndex:       .res  1            ; Index into fakedata for demo
    MultiplyTemp:    .res  1            ; Scratch variable for multiply code
    resultLo:        .res  1            ; Results from multiply operations
    resultHi:        .res  1
    shiftCountdown:  .res  1            ; We scroll color every N frames
    VU:              .res  1            ; VU Audio Data
    Peaks:           .res  NUM_BANDS    ; Peak Data for current frame
    NextStyle:       .res  1            ; The next style we will pick
    CharDefs:        .res  8            ; Storage for the visualDef currently in use
    SerialBufLen = 12                   ; Matches the packet size from ESP32
    SerialBufPos:    .res  1            ; Current index into serial buffer
    SerialBuf:       .res  SerialBufLen ; Serial buffer for: "DP" + 1 byte vu + 8 PeakBytes
    DemoMode:        .res  1            ; Demo mode enabled
    CurSchemeIndex:  .res  1            ; Current band color scheme index
.include "serdrv.var"                   ; Include serial driver variables
ScratchEnd:

.assert * <= SCRATCH_END, error         ; Make sure we haven't run off the end of the buffer

; Start of Binary -------------------------------------------------------------------

.code

; BASIC program to load and execute ourselves.  Lines of tokenized BASIC that
; have a banner comment and then a SYS command to start the machine language code.

                .org 0000             ; File begins with program start address so we
                .word BASE            ;  emit that as the first two bytes
                .org  BASE

Line10:         .word Line1           ; Next line number
                .word 0               ; Line Number 10
                .byte TK_REM          ; REM token
                .literal " - SPECTRUM ANALYZER DISPLAY", 00
Line1:          .word Line2
                .word 1
                .byte TK_REM
                .literal " - GITHUB/PLUMMERSSOFTWARELLC/", 00
Line2:         .word Line3
                .word 2
                .byte TK_REM
                .literal " - PETROCK - COPYRIGHT 2022", 00
Line3:         .word endOfBasic       ; PTR to next line, which is 0000
                .word 3               ; Line Number 20
                .byte TK_SYS          ;   SYS token
                .literal " "
                .literal .string(*+7) ; Entry is 7 bytes from here, which
                                      ;  not how I'd like to do it but you cannot
                                      ;  use a forward reference in STR$()

                .byte 00              ; Do not modify without understanding
endOfBasic:     .word 00              ;   the +7 expression above, as this is

;-----------------------------------------------------------------------------------
; Start of Assembly Code
;-----------------------------------------------------------------------------------

start:          jmp realStart

.include "serdrv.s"                   ; Include serial driver routines here, so
                                      ;   they're available from this point
                                      ;   onwards

realStart:      cld                   ; Turn off decimal mode
                jsr InitVariables     ; Zero (init) all of our BSS storage variables

                lda #BLACK            ; Screen and border to black
                sta SCREEN_COLOR
                sta VIC_BORDERCOLOR

                jsr EmptyBorder       ; Draw the screen frame and decorations

                jsr OpenSerial        ; Open the serial port for data from the ESP32   
                          
                lda #2                ; Could just be non-zero, but mightbe device ID!
                ldy #0                ;   either way, zero is disable, we want the opposite
                ldx #0
                jsr SerialIoctl       ; Enable Serial!  Behold the power!
drawLoop:       
                jsr GetSerialChar
                cmp #$ff              ; If byte is $ff, check if "no data" was flagged
                bne @havebyte
                cpx #<SER_ERR_NO_DATA
                bne @havebyte
                cpy #>SER_ERR_NO_DATA 
                beq @donedata

@havebyte:      jsr GotSerial
                jmp drawLoop

               .if TIMING             ; If 'TIMING' is defined we turn the border bit RASTHI
                jsr InitTimer         ; Prep the timer for this frame
                lda #$11              ; Start the timer
                sta CRA
@waitforraster: bit RASTHI
                bmi @waitforraster
                lda #DARK_GREY        ;  color to different colors at particular
                sta VIC_BORDERCOLOR   ;  places in the draw code to help see how
              .endif    

@donedata:      lda DemoMode          ; Load demo data if demo mode is on
                beq @dovu
                jsr FillPeaks
@dovu:          jsr DrawVU            ; Draw the VU bar at the top of the screen

              .if TIMING              ; If 'TIMING' is defined we turn the border
                lda #LIGHT_GREY       ;  color to different colors at particular
                sta VIC_BORDERCOLOR   ;  places in the draw code to help see how
              .endif                  ;  long various parts of it are taking.

drawAllBands:   ldx #NUM_BANDS - 1    ; Draw each of the bands in reverse order
:
                lda Peaks, x          ; X = band numner, A = value
                jsr DrawBand
                dex
                bpl :-
                ; Check to see its time to scroll the color memory

              .if TIMING
                lda #BLACK
                sta VIC_BORDERCOLOR
:               bit RASTHI
                bpl :-
                lda #0                ; Stop the clock
                sta CRA
                lda #LIGHT_BLUE
                sta TEXT_COLOR
                ldx #24               ; Print "Current Frame" banner
                ldy #09
                clc
                jsr PlotEx
                ldy #>framestr
                lda #<framestr
                jsr WriteLine
                lda CTLO              ; Display the number of ms the frame took. I realized
                eor #$FF              ; that 65536 - time is the same as flipping the bits,
                tax                   ; so that's why I xor instead of subtracting
                lda CTHI
                eor #$ff
                jsr BASIC_INTOUT
                lda #' '
                jsr CHROUT
                lda #'M'
                jsr CHROUT
                lda #'S'
                jsr CHROUT
                lda #' '
                jsr CHROUT
                jsr CHROUT
              .endif

                jsr GETIN             ; Keyboard Handling - check for RUN

                cmp #$53              ; Letter "S"
                bne @notStyle
                jsr SetNextStyle
                bne drawLoop

@notStyle:      cmp #$43              ; Letter "C"
                bne @notColor
                jsr SetNextScheme
                jmp drawLoop

@notColor:      cmp #$C3              ; Shift "C"
                bne @notShiftC
                jsr SetPrevScheme
                jmp drawLoop

@notShiftC:     cmp #$44              ; Letter "D"
                bne @notDemo
                jsr SwitchDemoMode
                jmp drawLoop

@notDemo:       cmp #$03
                bne drawLoop

                ldy #>exitstr         ; Output exiting text and exit
                lda #<exitstr
                jsr WriteLine

                rts

EmptyBorder:    ldy #>clrGREEN        ; Set cursor to white and clear screen
                lda #<clrGREEN
                jsr WriteLine
                lda #0
                sta SquareX
                sta SquareY
                lda #XSIZE
                sta Width
                lda #YSIZE
                sta Height
                jsr DrawSquare
                jsr InitVU            ; Let the VU meter paint its color mem, etc

                lda #LIGHT_BLUE
                sta TEXT_COLOR

                ldy #XSIZE/2-titlelen/2+1         ; Print title banner
                ldx #YSIZE-1
                clc
                jsr PlotEx
                ldy #>titlestr
                lda #<titlestr
                jsr WriteLine

                jsr FillBandColors

                jsr SetNextStyle      ; Select the first visual style

                rts


;-----------------------------------------------------------------------------------
; GotSerial - Process incoming serial bytes from the ESP32 
;-----------------------------------------------------------------------------------
; Store character in serial buffer. Processes packet if character completes it.
;-----------------------------------------------------------------------------------

GotSerial:      ldy SerialBufPos
                cpy #SerialBufLen      
                bne @nooverflow
                ldy #0
                sta SerialBufPos
@nooverflow:                    
                sta SerialBuf, y
                iny
                sty SerialBufPos
                
                cmp #00                   ; Look for carriage return meaning end
                beq :+
                rts                       ; No CR, back to caller

:               cpy SerialBufPos          ; Are we in the right char pos for it?
                beq :+                    ;  Yep - Process packet
                ldy #0                    ;  Nope - Restart filling buffer
                sta SerialBufPos
                beq @done

:               jsr GotSerialPacket

@done:          rts

BogusData:
                ldy #0
                sty SerialBufPos
                rts

; GotSerialPacket - Recieved a string followed by a carriage return so inspect it
;                   to see if it could be a data packet, as indicated by 'DP' as
;                   the first two bytes.  Data Packet? Dave Plummer?  You decide!

GotSerialPacket: 
                ldy SerialBufPos          ; Get received packet length
                lda SerialBuf             ; Look for 'D'
                cmp #MAGIC_BYTE_0
                bne BogusData

                lda SerialBuf+1           ; Look for 'P'
                cmp #MAGIC_BYTE_1
                bne BogusData

                lda SerialBuf + 2
                sta VU
                PeakDataNibbles = SerialBuf + 3
        
                ldy #0
                ldx #0
                
:               lda PeakDataNibbles, y    ; Get the next byte from the buffer
                and #%11110000            ; Get the top nibble
                lsr
                lsr
                lsr
                lsr
                clc
                adc #1                    ; Value of 1 is useless in the graph so add one to values
                
                sta Peaks+1, x            ; Store it in the peaks table
                lda PeakDataNibbles, y    ; Get that SAME byte from the buffer
                and #%00001111            ; Now we want the low nibble
                clc
                adc #1
                sta Peaks, x              ; Store it in the peaks table

                inx                       ; advance to the next peak
                inx
                iny                       ; Advance to the next byte of serial data

                cpy #8                    ; Have we done bytes 0-3 yet?
                bne :-                    ; Repeat until we have
                rts
                
;-----------------------------------------------------------------------------------
; FillPeaks
;-----------------------------------------------------------------------------------
; Copy data from the current index of the fake data table to the current peak data
; and vu value
;-----------------------------------------------------------------------------------

FillPeaks:      tya
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
:               lda (zptmpB), y       ;   PeakData table
                and #$0f              ; Normalize value between 1 and 16
                clc
                adc #1
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

                inc DataIndex         ; Inc DataIndex - Assumes wrap, so if you
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
                lda #$00              ; Init variables to #0
:               sta ScratchStart, x
                dex
                cpx #$ff
                bne :-

                rts

;-----------------------------------------------------------------------------------
; SwitchDemoMode
;-----------------------------------------------------------------------------------
; Toggle demo mode. If we switch it off, clear peaks and VU data.
;-----------------------------------------------------------------------------------

SwitchDemoMode: lda DemoMode
                eor #$01
                sta DemoMode
                bne @done

                lda #0
                ldy #15
:               sta Peaks, y
                dey
                bpl :-

                sta VU

@done:          rts

;-----------------------------------------------------------------------------------
; GetCursorAddr - Returns address of X/Y position on screen
;-----------------------------------------------------------------------------------
;           IN  X:  X pos
;           IN  Y:  Y pos
;           OUT X:  lsb of address
;           OUT Y:  msb of address
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

ClearScreen:    lda #CLRHOME          ; PETSCII for clear screen
                jsr CHROUT
                rts

;-----------------------------------------------------------------------------------
; WriteLine -   Writes a line of text to the screen using CHROUT ($FFD2)
;-----------------------------------------------------------------------------------
;               Y:  MSB of address of null-terminated string
;               A:  LSB
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
                cpy VU                ; If we're at or below the VU value we use the
                bcc :+                ;   VUSYMBOL to draw the current char else we use
                lda #MEDIUMSHADE      ;   the partial shade symbol
:               sta (zptmp),y         ; Store the char in screen memory
                sta tempOutput

                tya
                pha                   ; Save Y
                txa
                tay                   ; Move X into Y

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
; Multiply      Multiplies X * Y == ResultLo/ResultHi
;-----------------------------------------------------------------------------------
;               X   8 bit value in
;               Y   8 bit value in
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

FillSquare:     tya                   ; Save Y
                pha
                txa                   ; Save X
                pha

                lda ClearHeight       ; tempHeight = Height
                sta tempFillSquare
                ldx SquareX
                ldy SquareY
                jsr GetCursorAddr     ; Start of line Y
                stx zptmpB            ; Save cursor addr in zptmp
                sty zptmpB+1

addrloop:       lda lineChar
                ldy Width             ; Starting at zptmp we write Width bytes
                dey
:               sta (zptmpB),y        ; I'd prefer X but that zp mode isn't supported!
                dey
                bpl :-

                lda zptmpB            ; Advance down a line by adding 40 or 80 to the
                clc                   ;  zero page screen pointer.  We added it to lsb
                adc #XSIZE            ;  and inc the msb if we saw the carry get get
                sta zptmpB            ;  by the addition.
                bcc :+
                inc zptmpB+1
:               dec tempFillSquare
                bpl addrloop

                pla                   ; Restore X
                tax
                pla                   ; Restore Y
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

DrawSquare:     ldx SquareX
                ldy SquareY

                lda Height            ; Early out - do nothing for less than 2 height
                cmp #2
                bpl :+
                rts
:
                lda Width             ; Early out - do nothing for less than 2 width
                cmp #2
                bpl :+
                rts
:
                lda #TOPLEFTSYMBOL    ; Top Left Corner
                jsr OutputSymbolXY
                lda #HLINE1SYMBOL     ; Top Line
                sta lineChar
                lda Width
                sec
                sbc #2                ; 2 less due to start and end chars
                cmp #1
                bmi :+
                inx                   ; start one over after start char
                jsr DrawHLine
                dex                   ; put x back where it was
:
                lda #VLINE1SYMBOL     ; Otherwise draw middle vertical lines
                sta lineChar
                lda Height
                sec
                sbc #2
                cmp #1
                bmi :+
                iny
                jsr DrawVLine
               ; dey                  ; Normally post-dec Y to fix it up, but not needed here
:                                     ;   because Y is loaded explicitly below anyway
                lda SquareX
                clc
                adc Width
                sec
                sbc #1
                tax
                ldy SquareY
                lda #TOPRIGHTSYMBOL
                jsr OutputSymbolXY

                lda #VLINE2SYMBOL
                sta lineChar
                lda Height
                sec
                sbc #2
                iny
                jsr DrawVLine
bottomline:
                ldx SquareX
                lda SquareY
                clc
                adc Height
                sec
                sbc #1
                tay
                lda #BOTTOMLEFTSYMBOL
                jsr OutputSymbolXY
                lda #HLINE2SYMBOL
                sta lineChar

                lda Width
                sec
                sbc #2                ; Account for first and las chars
                inx                   ; Start one over past stat char
                jsr DrawHLine
              ; dex                   ; Put X back where it was if you need to preserve X

                lda SquareX
                clc
                adc Width
                sec
                sbc #1
                tax
                lda SquareY
                clc
                adc Height
                sec
                sbc #1
                tay
                lda #BOTTOMRIGHTSYMBOL
                jsr OutputSymbolXY
donesquare:     rts

;-----------------------------------------------------------------------------------
; OutputSymbolXY    Draws the given symbol A into the screen at pos X, Y
;-----------------------------------------------------------------------------------
;               X       X Coord [PRESERVED]
;               Y       Y Coord [PRESERVED]
;               A       Symbol
;-----------------------------------------------------------------------------------
; Unlike my original impl, this doesn't merge, so lines can't intersect, but this
; way no intermediate buffer is required and it draws right to the screen directly.
;-----------------------------------------------------------------------------------

OutputSymbolXY: sta tempOutput
                stx tempX
                sty tempY

                jsr GetCursorAddr     ; Store the screen code in
                stx zptmp             ; screen RAM
                sty zptmp+1

                ldy #0
                lda tempOutput
                sta (zptmp),y

                ldx tempX
                ldy tempY
                rts

;-----------------------------------------------------------------------------------
; DrawHLine     Draws a horizontal line in the offscreen buffer
;-----------------------------------------------------------------------------------
;               X       X Coord of Start [PRESERVED]
;               Y       Y Coord of Start [PRESERVED]
;               A       Length of line
;-----------------------------------------------------------------------------------
; BUGBUG (Optimization, Davepl) - This code draws color RAM edvery time a line is
; drawn, which would allow for different colors every time, which might be cool, but
; if that ability is never used then the color memory only really need to be
; written once (or each time the screen is cleared, etc).
;-----------------------------------------------------------------------------------

DrawHLine:      sta tempDrawLine      ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                tya                   ; Save X, Y
                pha
                txa
                pha

                jsr GetCursorAddr
                stx zptmp
                sty zptmp+1

                ldy tempDrawLine      ; Draw the line
                dey
                lda lineChar          ; Store the line character in screen ram
:               sta (zptmp), y
                dey                   ; Rinse and repeat
                bpl :-

                pla                   ; Restore X, Y
                tax
                pla
                tay
                rts

DrawVLine:      sta tempDrawLine      ; Start at the X/Y pos in screen mem
                cmp #1
                bpl :+
                rts
:
                jsr GetCursorAddr     ; Get the screen memory addr of the
                stx zptmp             ;   line's X/Y start position
                sty zptmp+1

vloop:          lda lineChar          ; Store the line char in screen mem

                ldy #0
                sta (zptmp), y

                lda zptmp             ; Now add 40/80 to the lsb of ptr
                clc
                adc #XSIZE
                sta zptmp
                bcc :+
                inc zptmp+1           ; On overflow in the msb as well

:

                dec tempDrawLine      ; One less line to go
                bne vloop
                rts


;-----------------------------------------------------------------------------------
; PutText - Put a string of characters at the center of a message line
;-----------------------------------------------------------------------------------
;           A - Message line number
;           X - Low byte of message address
;           Y - High byte of message address
;-----------------------------------------------------------------------------------

PutText:
                MESSAGE_BLOCK_LOC = SCREEN_MEM + XSIZE * (TOP_MARGIN + BAND_HEIGHT) + LEFT_MARGIN
                TEXT_WIDTH = XSIZE - LEFT_MARGIN - RIGHT_MARGIN
                
                stx zptmpB
                sty zptmpB+1

                tax
                ldy #XSIZE
                jsr Multiply

                clc
                lda resultLo
                adc #<MESSAGE_BLOCK_LOC
                sta zptmp
                lda resultHi
                adc #>MESSAGE_BLOCK_LOC
                sta zptmp+1

                ldy #ff
:               iny
                lda (zptmpB),y
                bne :-
                dey



                lda #XSIZE
                sec
                sbc 



;-----------------------------------------------------------------------------------
; GetTextLength - Determine length of a text message
;-----------------------------------------------------------------------------------
;       OUT A - Message length
;           X - Low byte of message address
;           Y - High byte of message address
;-----------------------------------------------------------------------------------

GetTextLength:



;-----------------------------------------------------------------------------------
; SetPrevScheme - Switch to previous color scheme
;-----------------------------------------------------------------------------------

SetPrevScheme:
                dec CurSchemeIndex
                bpl FillBandColors    ; If index >= 0, we're done

                lda #<BandSchemeTable ; Base address for color scheme table
                sta zptmpB
                lda #>BandSchemeTable
                sta zptmpB+1
                
                ldy #1                ; Prep for first scheme table entry

@loop:          iny                   ; Move on to next table entry
                
                lda (zptmpB),y        ; Check if we hit the null pointer
                iny
                ora (zptmpB),y

                bne @loop             ; No? Continue looking

                dey                   ; Back up one table entry
                dey
                tya
                clc
                lsr                   ; Divide index by two and store
                sta CurSchemeIndex

                bcs FcColorMem        ; Branch always (lsr shifted bit into carry)


;-----------------------------------------------------------------------------------
; SetNextScheme - Switch to next color scheme
;-----------------------------------------------------------------------------------

SetNextScheme:
                lda #<BandSchemeTable ; Base address for color scheme table
                sta zptmpB
                lda #>BandSchemeTable
                sta zptmpB+1

                inc CurSchemeIndex    ; Bump up color scheme index
                lda CurSchemeIndex
                asl
                tay

                lda (zptmpB),y
                iny
                ora (zptmpB),y
                bne FcColorMem
                sta CurSchemeIndex    ; Zero pointer = end of table, so start over
                beq FcColorMem


;-----------------------------------------------------------------------------------
; FillBandColors - Color bands using the current band color scheme
;
; This routine spends quite a few instructions juggling bytes around registers, the
; stack and zptmpC. The reason basically is that indirect indexed addressing can
; only be done when loading and saving A, using Y as the index register. We have two
; pointers to apply indirect indexed adressing to (color RAM and band color scheme).
;-----------------------------------------------------------------------------------

FillBandColors:
                lda #<BandSchemeTable ; Base address for color scheme table
                sta zptmpB
                lda #>BandSchemeTable
                sta zptmpB+1

FcColorMem:     lda #YSIZE-TOP_MARGIN-BOTTOM_MARGIN   ; Count of rows to paint color for
                sta tempY

                BAND_COLOR_LOC = COLOR_MEM + XSIZE * TOP_MARGIN + LEFT_MARGIN

                lda #<BAND_COLOR_LOC                  ; Base address for bar color RAM
                sta zptmp
                lda #>BAND_COLOR_LOC
                sta zptmp+1

                ; The following is the assembly version of:
                ;   colorCount = (byte**)BandSchemeTable[CurSchemeIndex][0]

                lda CurSchemeIndex    ; Load color scheme address from table...
                asl
                tay
                lda (zptmpB),y
                tax
                iny 
                lda (zptmpB),y

                stx zptmpB            ; ...and make that the new base address in zptmpB
                sta zptmpB+1

@fcrow:         ldy #0                ; Load scheme color count
                lda (zptmpB),y        ;   and save it as the scheme color index
                sta zptmpC

                lda #NUM_BANDS        ; Color in from right-hand char of right-most bar
                asl                   ; First char index = (NUM_BANDS * 2) - 1
                tay
                dey

@fcloop:        tya                   ; Push character index on stack
                pha

                ldy zptmpC            ; Load color from scheme and hold it in X
                lda (zptmpB),y
                tax
                dey                   ; Back up one color in the scheme
                bne @notzero          ; Color index zero? We've used our scheme colors
                lda (zptmpB),y        ;   so it's time to reload scheme color count
                tay
@notzero:       sty zptmpC            ; Store scheme color index

                pla                   ; Pop character index
                tay

                txa                   ; Write color to bar chars
                sta (zptmp),y
                dey
                sta (zptmp),y
                dey
                bpl @fcloop

                lda zptmp             ; Move on to next row
                clc
                adc #XSIZE
                sta zptmp
                bcc :+
                inc zptmp+1

:               dec tempY
                bne @fcrow

                rts

;-----------------------------------------------------------------------------------
; DrawBand      Draws a single band of the spectrum analyzer
;-----------------------------------------------------------------------------------
;               X       Band Number     [PRESERVED]
;               A       Height of bar
;-----------------------------------------------------------------------------------


; DrawBand - Static version that makes assumptions:
;               No dynamic color memory
;               Band Width of 2
;
; Walks down the screen and depending on whether the current
; pos is above, equal, or below the bar itself, draws blanks,
;
; the bar top, the bar middle, or bar bottom

DrawBand:       cmp #1                ; Can't draw height 1, so draw 0
                bne :+
                lda #0
:               sta Height            ; Height is height of bar itself
                txa
                asl
                sta SquareX           ; Bar xPos on screen

                ; Square Y will be the screen line number of the top of the bar

                lda #YSIZE - BOTTOM_MARGIN
                sec
                sbc Height
                sta SquareY

                ; tempY is the current screen line

                lda #TOP_MARGIN
                sta tempY             ; We start on the first screen line of the analyzer

                SCREEN_LOC = (SCREEN_MEM + XSIZE * TOP_MARGIN + LEFT_MARGIN)

                lda #<SCREEN_LOC      ; zptmp points to top left of first bar
                sta zptmp             ;  in screen memory
                lda #>SCREEN_LOC
                sta zptmp+1

lineSwitch:     ldy SquareX           ; Y will be the X-pos (zp addr mode not supported on X register)
                lda tempY             ; Current screen line
                cmp #YSIZE - BOTTOM_MARGIN - 1
                bne @notlastline
                lda Height            ; If 0 height, write blanks instead of band base
                bne drawLastLine
                beq drawLastBlanks
@notlastline:   cmp SquareY           ; Compare to screen line of top of bar
                bcc drawBlanks
                beq drawFirstLine
                bcs drawMiddleLine
drawBlanks:
                lda #' '
                sta (zptmp),y
                iny
                sta (zptmp),y
                inc tempY
                bne lineLoop
drawFirstLine:
                lda CharDefs + visualDef::TOPLEFTSYMBOL
                sta (zptmp),y
                iny
                lda CharDefs + visualDef::TOPRIGHTSYMBOL
                sta (zptmp),y
                inc tempY
                bne lineLoop
drawMiddleLine:
                lda CharDefs + visualDef::VLINE1SYMBOL
                sta (zptmp),y
                iny
                lda CharDefs + visualDef::VLINE2SYMBOL
                sta (zptmp),y
                inc tempY
                cpy #YSIZE-BOTTOM_MARGIN-1
                bne lineLoop
drawLastLine:
                ldy SquareX
                lda CharDefs + visualDef::BOTTOMLEFTSYMBOL   ; using the previous lines zptmp but add one
                sta (zptmp),y         ; line to the Y register.
                iny
                lda CharDefs + visualDef::BOTTOMRIGHTSYMBOL
                sta (zptmp),y
                rts
drawLastBlanks:
                lda #' '
                sta (zptmp),y
                iny
                sta (zptmp),y
                rts

lineLoop:       lda zptmp             ; Advance zptmp by one screen line down
                clc
                adc #XSIZE
                sta zptmp
                lda zptmp+1
                adc #0
                sta zptmp+1
                jmp lineSwitch

;-----------------------------------------------------------------------------------
; PlotEx        Replacement for KERNAL plot that fixes color ram update bug
;-----------------------------------------------------------------------------------
;               X       Cursor Y Pos
;               Y       Cursor X Pos
;               (NOTE Reversed)
;-----------------------------------------------------------------------------------
PlotEx:
                bcs     :+
                jsr     PLOT          ; Set cursor position using original ROM PLOT
                jmp     UPDCRAMPTR    ; Set pointer to color RAM to match new cursor position
:               jmp     PLOT          ; Get cursor position

;-----------------------------------------------------------------------------------
; InitTimer     Initlalize a VIA timer to run at 1ms so we can do timings
;-----------------------------------------------------------------------------------

InitTimer:

                lda   #$7F            ; Mask to turn off the CIA IRQ
                ldx   #<TIMERSCALE    ; Timer low value
                ldy   #>TIMERSCALE    ; Timer High value
                sta   ICR
                stx   TALO            ; Set to 1msec (1022 cycles per IRQ)
                sty   TAHI
                lda   #$FF            ; Set counter to FFFF
                sta   CTLO
                sta   CTHI
                ldy   #$51
                sty   CRB             ; Enable and go
                rts

;-----------------------------------------------------------------------------------
; SetNextStyle - Select a visual style for the spectrum analyzer by copying a small
;             character table of PETSCII screen codes into our 'styletable' that
;             we use to draw the spectrum analyzer bars.  It defines the PETSCII
;             chars that we use to draw the corners and lines.
;-----------------------------------------------------------------------------------
; Copy the next style into the style table and increment the style table pointer
; with wraparound so that we can pick the next style next time in
;-----------------------------------------------------------------------------------

SetNextStyle:   lda NextStyle         ; Take the style index and multiply by 2
                tax                   ;   to get the Y index into the lookup table
                asl                   ;   so we can fetch the actual address of the
                tay                   ;   char table.  Because it is a mult of 8 in
                inx                   ;   size we could do without a lookup, but why assume...
                txa                   ; Increment the NextStyle index and do a MOD 4 on it
                and #3                ;   and then put it back so that the index cycles 0-3
                sta NextStyle

                lda StyleTable, y     ; Get the entry in the styletable, which is stored as
                sta zptmp             ;   a list of word addresses, and put that address
                iny                   ;   into zptmp as the 'source' of our memcpy
                lda StyleTable, y
                sta zptmp+1
                ldy #.sizeof(visualDef) - 1  ; Y is the size we're going to copy (the size of the struct)
:               lda (zptmp),y         ; Copy from source to dest
                sta CharDefs, y
                dey
                bpl :-
                rts

; Visual style definitions.  See the 'visualDef' structure defn in petrock.inc
; Each of these small tables includes the characters needed to draw the corners,
; and horizontal and vertical lines needed to form a box.

SkinnyRoundStyle:                     ; PETSCII screen codes for round tube bar style
  .byte 85, 73, 74, 75, 66, 66, 74, 75

DrawSquareStyle:                      ; PETSCII screen codes for square linedraw style
  .byte 79, 80, 76, 122, 101, 103, 76, 122

BreakoutStyle:                        ; PETSCII screen codes for style that looks like breakout
  .byte 239, 250, 239, 250, 239, 250, 239, 250

CheckerboardStyle:                    ; PETSCII screen codes for checkerboard style
  .byte 102, 92, 102, 92, 102, 92,102, 92

; Lookup table - each of the above mini tables is listed in this lookup table so that
;                we can easily find items 0-3
;
; The code currently assumes that there are four entries such that is can easily
; modulus the values.  These are the four entries.

StyleTable:
  .word SkinnyRoundStyle, BreakoutStyle, CheckerboardStyle, DrawSquareStyle

;-----------------------------------------------------------------------------------
; Band color schemes
;
; Collection of band color schemes the user can cycle through:
; - The pointer table is zero-pointer terminated.
; - Each scheme is a list of colors that the background color fill routine cycles
;   through. The number of colors in a scheme are specified just before the first
;   actual color value. Note that the color schemes are applied to the bars right
;   to left.
;-----------------------------------------------------------------------------------

BandSchemeTable: 
                .word RainbowScheme
                .word WhiteScheme
                .word GreenScheme
                .word RedScheme
                .word RWBScheme
                .word 0

RainbowScheme:  .byte 16
                .byte RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, PURPLE, RED
                .byte ORANGE, YELLOW, GREEN, CYAN, BLUE, PURPLE, RED, YELLOW

WhiteScheme:    .byte 1
                .byte WHITE

GreenScheme:    .byte 1
                .byte GREEN

RedScheme:      .byte 1
                .byte RED

RWBScheme:      .byte 3
                .byte RED, WHITE, BLUE


; String literals at the end of file, as was the style at the time!

.include "fakedata.inc"

startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0
framestr:       .literal "  RENDER TIME: ", 0
titlestr:       .literal 12, "C64PETROCK.COM", 0
titlelen = * - titlestr
clrGREEN:       .literal $99, $93, 0
