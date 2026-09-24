;-----------------------------------------------------------------------------------
; PETROCK: Spectrum Analyzer Display for C64 and PET
;-----------------------------------------------------------------------------------
; (c) Plummer's Software Ltd, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------
;
; General Idea for Newcomers:
;
; Draws 16 vertical bands of the spectrum analyzer which can be up to 16 high.  The
; program first clears the screen, draws the border and text, fills in color, and the
; main draw loop calls DrawBand for each one in turn.  Each frame draws a new set of
; peaks from the PeakData table, which has 16 entries, one per band.  That data is
; replaced either by a new frame of demo data or an incoming serial packet and the
; process is repeated.  At 2400 baud, the serial link carries about 21 packets per
; second.
;
; Color RAM can be filled with different patterns by stepping through the visual styles
; with the C key, but it is not drawn each and every frame.
;
; A bar is drawn as blanks above the bar, the top of the bar, then the middle pieces,
; then the bottom.  To save time, only the rows that changed since the band was last
; drawn are redrawn.  A visual style definition is set that includes all of the
; PETSCII chars you need to draw a band, like the corners and sides, etc.  It can be
; changed with the S key.
;
; Every frame the serial port is checked for incoming data which is then stored in the
; SerialBuf.  Packets have a fixed size.  If a packet starts with the magic byte and
; ends with a nul, it is used as new peakdata and stored in the PeakData table.  If
; not, bytes are skipped until the next nul, after which a new packet is expected.  The
; code on the ESP32 sends it over as 16 nibbles packed into 8 bytes plus a VU value.
;
; The built-in serial code on the C64 is poor, and serial/c64/driver.s contains a new
; impl that works well for receiving data up to 4800 baud.
; On the PET, built-in serial code is effectively absent. For the PET,
; serial/c64/driver.s contains an implementation that is confirmed to receive data
; up to 2400 baud.
;
;-----------------------------------------------------------------------------------


.SETCPU "6502"

; Include the system headers and application defintions ----------------------------

.include "settings.inc"
.include "petrock.inc"                    ; Project includes and defintions

; Our BSS Data  --------------------------------------------------------------------

.org SCRATCH_START                        ; Program counter to casssette buffer so that
.bss                                      ;  we can define our BSS storage variables

; These are local BSS variables.  We're using the cassette buffer for storage.  All
; will be initlialzed to 0 bytes at application startup.

ScratchStart:
    tempDrawLine:    .res  1              ; Temp used by DrawLine
    tempOutput:      .res  1              ; Temp used by OutputSymbol
    tempX:           .res  1              ; Preserve X Pos
    tempY:           .res  1              ; Preserve Y Pos
    lineChar:        .res  1              ; Line draw char
    SquareX:         .res  1              ; Args for DrawSquare
    SquareY:         .res  1
    Width:           .res  1
    Height:          .res  1              ; Height of area to draw
    ClearHeight:     .res  1              ; Height of area to clear
    DataIndex:       .res  1              ; Index into fakedata for demo
    resultLo:        .res  1              ; Results from multiply operations
    resultHi:        .res  1
    VU:              .res  1              ; VU Audio Data
    Peaks:           .res  NUM_BANDS      ; Peak Data for current frame
    PrevPeaks:       .res  NUM_BANDS      ; Band heights on screen ($FF = must redraw)
    BandIndex:       .res  1              ; Band number being drawn by DrawBand
    BandCol:         .res  1              ; Screen column offset of that band
    OldHeight:       .res  1              ; Height of that band before drawing
    NewTop:          .res  1              ; Screen row of that band's new top
    RunVector:       .res  2              ; Entry point into BlankRun or MidRun
    NextStyle:       .res  1              ; The next style we will pick
    CharDefs:        .res  VISUALDEF_SIZE ; Storage for the visualDef currently in use
    RedrawFlag:      .res  1              ; Flag to redraw screen
    DemoMode:        .res  1              ; Demo mode enabled
.if C64         ; Color's only relevant on the C64
    CurSchemeIndex:  .res  1              ; Current band color scheme index
    BorderColor:     .res  1              ; Border color at startup
    BkgndColor:      .res  1              ; Background color at startup
    TextColor:       .res  1              ; Text color at startup
.endif
    TextTimeout:     .res  1              ; Text timeout second count (0 = disabled)
.if PET         ; The PET counts jiffies. The C64 uses a CIA timer
    TextTimerStart:  .res  1              ; Jiffy count at start of current second
.endif
.if .not (PET && SERIAL)
    DemoToggle:      .res  1              ; Update toggle to delay demo mode updates
.endif
.if SERIAL                                ; Include serial driver variables
    SerialBufPos:    .res  1              ; Current index into serial buffer
    SerialBuf:       .res  PACKET_LENGTH  ; Serial buffer for: "DP" + 1 byte vu + 8 PeakBytes
    SerialBufLen = *-SerialBuf            ; Length of Serial Buffer
  .if C64
.include "serial/c64/vars.s"
  .elseif PET
.include "serial/pet/vars.s"
  .endif
.endif

ScratchEnd:

.assert * <= SCRATCH_END, error           ; Make sure we haven't run off the end of the buffer
.assert <RunVector <> $FF, error          ; JMP (RunVector) fails if it straddles a page

.if SERIAL
.assert SerialBufLen = PACKET_LENGTH, error
.endif

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
                .literal " - C64PETROCK.COM", 00
Line2:          .word Line3
                .word 2
                .byte TK_REM
                .literal " - PETROCK - COPYRIGHT 2022", 00
Line3:          .word endOfBasic       ; PTR to next line, which is 0000
                .word 3               ; Line Number 20
                .byte TK_SYS          ;   SYS token
                .literal .sprintf(" %d", PROGRAM)

                .byte 00
endOfBasic:     .word 00


.res            PROGRAM - *

;-----------------------------------------------------------------------------------
; Start of Assembly Code
;-----------------------------------------------------------------------------------

.if PET
                lda PET_DETECT        ; Check if we're dealing with original ROMs
                cmp #PET_2000
                bne @goodpet

                ldy #>notonoldrom     ; Disappoint user
                lda #<notonoldrom
                jsr WriteLine

                rts
@goodpet:
.endif

.if SERIAL

                jmp start

  .if C64
.include "serial/c64/driver.s"
  .elseif PET
.include "serial/pet/driver.s"
  .endif

.endif

start:
                cld                   ; Turn off decimal mode

                jsr InitVariables     ; Zero (init) all of our BSS storage variables

.if C64         ; TOD and color only available on C64
                jsr InitTODClocks

                lda VIC_BORDERCOLOR   ; Save current colors for later
                sta BorderColor
                lda VIC_BG_COLOR0
                sta BkgndColor
                lda TEXT_COLOR
                sta TextColor

                lda #BLACK            ; Screen and border to black
                sta VIC_BG_COLOR0
                sta VIC_BORDERCOLOR
.endif
                ldy #>clrGREEN        ; Set cursor to green and clear screen, setting text
                lda #<clrGREEN        ;   color to light green on the C64
                jsr WriteLine

                jsr EmptyBorder       ; Draw the screen frame and decorations
                jsr SetNextStyle      ; Select the first visual style

.if C64         ; Color only supported on C64
                jsr FillBandColors    ; Do initial fill of band color RAM
.endif

.if SERIAL
                jsr OpenSerial        ; Open the serial port for data from the ESP32
                jsr StartSerial       ; Enable Serial!  Behold the power!
.endif

drawLoop:

.if SERIAL
                jsr GetSerialChar
                bcs @donedata         ; Carry set means there was no data

                jsr GotSerial
                jmp drawLoop
.endif

.if TIMING && C64                     ; If 'TIMING' is defined on the C64 we turn the border bit RASTHI
                jsr InitTimer         ; Prep the timer for this frame
                lda #$11              ; Start the timer
                sta CIA2_CRA
@waitforraster: bit RASTHI
                bmi @waitforraster
                lda #DARK_GREY        ;  Color to different colors at particular
                sta VIC_BORDERCOLOR   ;    places in the draw code to help see how
.endif

@donedata:      lda DemoMode          ; Load demo data if demo mode is on
                beq @redraw
                jsr FillPeaks
                ldx #$10
                ldy #$ff
@delay:         dey
                bne @delay
                dex
                bne @delay

@redraw:        lda RedrawFlag
                beq @afterdraw        ; We didn't get a complete packet yet, so no point in drawing anything
                lda #0
                sta RedrawFlag        ; Acknowledge packet

                jsr DrawVU            ; Draw the VU bar at the top of the screen

.if TIMING && C64                     ; If 'TIMING' is defined we turn the border
                lda #LIGHT_GREY       ;   color to different colors at particular
                sta VIC_BORDERCOLOR   ;   places in the draw code to help see how
.endif                                ;   long various parts of it are taking.

                ldx #NUM_BANDS - 1    ; Draw each of the bands in reverse order
:
                lda Peaks, x          ; X = band numner, A = value
                jsr DrawBand
                dex
                bpl :-

.if SERIAL && (C64 || (PET && SENDSTAR))
                lda #'*'              ; Send a * back to the host
                jsr PutSerialChar
.endif

.if TIMING && C64
                ; Check to see its time to scroll the color memory

                lda #BLACK
                sta VIC_BORDERCOLOR
:               bit RASTHI
                bpl :-
                lda #0                ; Stop the clock
                sta CIA2_CRA
                lda #LIGHT_BLUE
                sta TEXT_COLOR
                ldx #24               ; Print "Current Frame" banner
                ldy #09
                clc
                jsr PlotEx
                ldy #>framestr
                lda #<framestr
                jsr WriteLine
                lda CIA2_TB           ; Display the number of ms the frame took. I realized
                eor #$FF              ;   that 65536 - time is the same as flipping the bits,
                tax                   ;   so that's why I XOR instead of subtracting
                lda CIA2_TB+1
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
.endif          ; TIMING && C64

@afterdraw:     jsr CheckTextTimer

.if SERIAL
                jsr GetKeyboardChar   ; Get a character from the serial driver's keyboard handler
.else
                jsr GETIN             ; No serial, use regular GETIN routine
.endif

                cmp #0
                bne @notEmpty

                jmp drawLoop

@notEmpty:

                cmp #KEY_S
                bne @notStyle
                jsr SetNextStyle
                jmp drawLoop

@notStyle:
.if C64         ; Color only available on C64
                cmp #KEY_C
                bne @notColor
                jsr SetNextScheme
                jmp drawLoop

@notColor:      cmp #KEY_C_SHIFT
                bne @notShiftC
                jsr SetPrevScheme
                jmp drawLoop

@notShiftC:
.endif
                cmp #KEY_D
                bne @notDemo
                jsr SwitchDemoMode
                jmp drawLoop

@notDemo:       cmp #KEY_B
                bne @notborder
                jsr ToggleBorder
                jmp drawLoop

@notborder:     cmp #KEY_RUNSTOP
                beq @exit

                jsr ShowHelp
                jmp drawLoop

@exit:
.if SERIAL
                jsr CloseSerial
.endif

.if C64         ; Color only available on C64
                lda BorderColor       ; Restore colors to how we found them
                sta VIC_BORDERCOLOR
                lda BkgndColor
                sta VIC_BG_COLOR0
                lda TextColor
                sta TEXT_COLOR
.endif
                jsr ClearScreen

                ldy #>exitstr         ; Output exiting text and exit
                lda #<exitstr
                jsr WriteLine

                rts

;-----------------------------------------------------------------------------------
; ToggleBorder - Toggle border around spectrum analyzer area
;-----------------------------------------------------------------------------------

ToggleBorder:   lda #<SCREEN_MEM
                sta zptmp
                lda #>SCREEN_MEM
                sta zptmp+1

                ldy #0
                lda (zptmp),y
                cmp #' '

                bne ClrBorderMem

; Note: this routine flows into the next one

;-----------------------------------------------------------------------------------
; EmptyBorder - Draw border around spectrum analyzer area
;-----------------------------------------------------------------------------------

EmptyBorder:    lda #0
                sta SquareX
                sta SquareY
                lda #XSIZE
                sta Width
                lda #YSIZE
                sta Height
                jsr DrawSquare

.if C64         ; Color only available on C64
                jsr InitVU            ; Let the VU meter paint its color mem, etc

                lda #LIGHT_BLUE
                sta TEXT_COLOR
.endif

                ldy #XSIZE/2-titlelen/2+1         ; Print title banner
                ldx #YSIZE-1
                clc
                jsr PlotEx
                ldy #>titlestr
                lda #<titlestr
                jsr WriteLine

                rts

;-----------------------------------------------------------------------------------
; ClearBorder   Remove border and decorations
;-----------------------------------------------------------------------------------

ClearBorder:
                lda #<SCREEN_MEM
                sta zptmp
                lda #>SCREEN_MEM
                sta zptmp+1

ClrBorderMem:   ldy #XSIZE-1          ; Top line
                lda #' '
:               sta (zptmp),y
                dey
                bpl :-

                ldx #YSIZE-2

@rowloop:       lda zptmp             ; Left and right lines
                clc
                adc #XSIZE
                sta zptmp
                lda zptmp+1
                adc #0
                sta zptmp+1

                lda #' '
                ldy #0
                sta (zptmp),y

                ldy #XSIZE-1
                sta (zptmp),y

                dex
                bne @rowloop

                lda zptmp             ; Bottom line
                clc
                adc #XSIZE
                sta zptmp
                lda zptmp+1
                adc #0
                sta zptmp+1

                ldy #XSIZE-1
                lda #' '
:               sta (zptmp),y
                dey
                bpl :-

                rts

.if SERIAL

;-----------------------------------------------------------------------------------
; GotSerial     Process incoming serial bytes from the ESP32
;-----------------------------------------------------------------------------------
; Store character in serial buffer. Processes packet if character completes it.
;
; Packets have a fixed length, and the data in them can contain NUL bytes. So we
; only accept a packet if its first byte is the magic byte and its last byte is the
; NUL terminator. If a packet fails that check, we've lost track of where packets
; start. We then skip bytes until the next NUL, and start a new packet after it.
;-----------------------------------------------------------------------------------

GotSerial:      ldy SerialBufPos
                bmi @skipping             ; SerialBufPos is $FF while we skip to a NUL
                bne @store                ; Not the first byte of a packet

                cmp #MAGIC_BYTE_0         ; A packet must start with the magic byte
                beq @store
                cmp #00                   ; If this is a NUL, a packet may follow it
                beq @done
                dey                       ; Otherwise, skip to the next NUL
                sty SerialBufPos
                rts

@skipping:      cmp #00                   ; Found the NUL we were looking for?
                bne @done                 ;  Nope - Keep skipping
                iny                       ;  Yep - Next byte should start a packet
                sty SerialBufPos
                rts

@store:         sta SerialBuf, y
                iny
                cpy #SerialBufLen         ; Do we have a complete packet?
                beq @complete
                sty SerialBufPos          ;  Nope - Wait for more
@done:          rts

@complete:      ldy #0                    ; Next packet fills the buffer from the start
                sty SerialBufPos

                cmp #00                   ; Last byte must be the NUL terminator
                beq GotSerialPacket

                dey                       ; Not a valid packet, so skip to the next NUL
                sty SerialBufPos
                rts

;-----------------------------------------------------------------------------------
; GotSerialPacket - Unpack a complete data packet, as indicated by the 'DP' in the
;                   nibbles of the first byte.  Data Packet? Dave Plummer?  You decide!
;-----------------------------------------------------------------------------------

GotSerialPacket:
                lda SerialBuf+MAGIC_LEN
                .if COL80
                asl
                .endif
                sta VU

                PeakDataNibbles = SerialBuf + MAGIC_LEN + VU_LEN

                ldy #0
                ldx #0

:               lda PeakDataNibbles, y    ; Get the next byte from the buffer
                and #%11110000            ; Get the top nibble
                lsr
                lsr
                lsr
                lsr
                clc
                adc #1                    ; Add one to values

                sta Peaks+1, x            ; Store it in the peaks table
                lda PeakDataNibbles, y    ; Get that SAME byte from the buffer
                and #%00001111            ; Now we want the low nibble
                clc
                adc #1
                sta Peaks, x              ; Store it in the peaks table

                inx                       ; Advance to the next peak
                inx
                iny                       ; Advance to the next byte of serial data

                cpy #8                    ; Have we done bytes 0-3 yet?
                bne :-                    ; Repeat until we have

                lda #1
                sta RedrawFlag            ; Time to redraw!
                rts

.endif          ; SERIAL

;-----------------------------------------------------------------------------------
; FillPeaks
;-----------------------------------------------------------------------------------
; Copy data from the current index of the fake data table to the current peak data
; and vu value
;-----------------------------------------------------------------------------------

FillPeaks:
.if .not (PET && SERIAL)
                lda DemoToggle
                eor #$01
                sta DemoToggle
                beq @proceed

                rts

@proceed:
.endif
                tya
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
                sta zptmpB            ;   table into the VU variable
                lda #>PeakData
                sta zptmpB+1
                ldy DataIndex
                lda (zptmpB), y
                .if COL80
                asl
                .endif
                sta VU

                lda #1
                sta RedrawFlag        ; Time to redraw!

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
; We use a bunch of storage in the system (on the C64 it's the datasette buffer) and
; it starts out in an unknown state, so we have code to zero it or set it to defaults
;-----------------------------------------------------------------------------------

InitVariables:  ldx #ScratchEnd-ScratchStart
                lda #$00              ; Init variables to #0
:               sta ScratchStart, x
                dex
                cpx #$ff
                bne :-

                lda #1
                sta RedrawFlag

                rts

;-----------------------------------------------------------------------------------
; SwitchDemoMode
;-----------------------------------------------------------------------------------
; Toggle demo mode. If we switch it off, clear peaks and VU data.
;-----------------------------------------------------------------------------------

SwitchDemoMode: lda DemoMode          ; Toggle demo mode bit
                eor #$01
                sta DemoMode
                bne @enabled          ; If we enabled demo mode, we're done

                lda #0                ; Zero out peaks and VU
                ldy #15
:               sta Peaks, y
                dey
                bpl :-

                sta VU

                lda #1
                sta RedrawFlag        ; Force redraw

                ldx #<DemoOffText     ; Tell user what just happened
                ldy #>DemoOffText

                bne @done

@enabled:       ldx #<DemoOnText
                ldy #>DemoOnText

@done:          jmp ShowTextLine

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
; ClearScreen   Guess.
;-----------------------------------------------------------------------------------

ClearScreen:    jmp CLRSCR

;-----------------------------------------------------------------------------------
; WriteLine     Writes a line of text to the screen using CHROUT ($FFD2)
;-----------------------------------------------------------------------------------
;               Y:  MSB of address of null-terminated string
;               A:  LSB
;-----------------------------------------------------------------------------------

WriteLine:      sta zptmp
                sty zptmp+1
WLRaw:          ldy #0
@loop:          lda (zptmp),y
                beq @done
                jsr CHROUT
                iny
                bne @loop
@done:          rts

;-----------------------------------------------------------------------------------
; ShowHelp      Show help text
;-----------------------------------------------------------------------------------

ShowHelp:
                lda #0
                ldx #<EmptyText
                ldy #>EmptyText
                jsr PutText

                lda #1
                ldx #<HelpText1
                ldy #>HelpText1
                jsr PutText

                lda #2
                ldx #<HelpText2
                ldy #>HelpText2
                jsr PutText

                lda #3
                sta TextTimeout

                jmp StartTextTimer

;-----------------------------------------------------------------------------------
; ShowTextLine - Puts a line of text in the middle of the text block
;-----------------------------------------------------------------------------------
;               X:  LSB of address of null-terminated string
;               Y:  MSB
;-----------------------------------------------------------------------------------

ShowTextLine:
                txa
                pha
                tya
                pha

                lda #0
                ldx #<EmptyText
                ldy #>EmptyText
                jsr PutText

                lda #2
                ldx #<EmptyText
                ldy #>EmptyText
                jsr PutText

                lda #1
                sta TextTimeout

                jsr StartTextTimer

                pla
                tay
                pla
                tax

                lda #1

; Note: this routine flows into the next one

;-----------------------------------------------------------------------------------
; PutText       Put a string of characters at the center of a message line
;-----------------------------------------------------------------------------------
;               A:  Message line number within the text block
;               X:  LSB of address of null-terminated string
;               Y:  MSB
;-----------------------------------------------------------------------------------

PutText:
                stx zptmp
                sty zptmp+1

                clc                   ; Set cursor to start of desired line
                adc #TOP_MARGIN+BAND_HEIGHT
                tax
                ldy #LEFT_MARGIN
.if C64         ; Color only available on C64
                lda #WHITE
                sta TEXT_COLOR
.endif
                jsr PlotEx

                ldy #$ff              ; Determine length of string by counting until NUL
:               iny
                lda (zptmp),y
                bne :-
                dey

                tya                   ; Calculate number of spaces to center text
                clc
                sbc #TEXT_WIDTH       ; Subtract screen width from text length and invert
                eor #$ff              ;   negative result to get total whitespaces around
                lsr                   ;   text. Divide that by 2.

                tax
                tay

                lda #' '              ; Write leading whitespace
:               jsr CHROUT
                dey
                bne :-

                jsr WLRaw             ; Write text

                txa
                tay

                lda #' '              ; Write trailing whitespace
:               jsr CHROUT
                dey
                bne :-

                rts

;-----------------------------------------------------------------------------------
; DrawVU        Draw the current VU meter at the top of the screen
;-----------------------------------------------------------------------------------

.if C64         ; Color only available on the C64
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

.endif

                ; Draw the VU meter on right, then draw its mirror on the left

DrawVU:         ldy #0                ; Y walks the right half from left to right,
                ldx #MAX_VU-1         ;   X walks its mirror from right to left
vuloop:         lda #VUSYMBOL
                cpy VU                ; If we're at or below the VU value we use the
                bcc :+                ;   VUSYMBOL to draw the current char else we use
                lda #MEDIUMSHADE      ;   the partial shade symbol
:               sta VUPOS1, y         ; Store the char in screen memory
                sta VUPOS2, x
                iny
                dex
                bpl vuloop

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
; DrawHLine     Draws a horizontal line in screen memory
;-----------------------------------------------------------------------------------
;               X       X Coord of Start [PRESERVED]
;               Y       Y Coord of Start [PRESERVED]
;               A       Length of line
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

;-----------------------------------------------------------------------------------
; DrawVLine     Draws a vertical line in screen memory
;-----------------------------------------------------------------------------------
;               X       X Coord of Start [PRESERVED]
;               Y       Y Coord of Start [PRESERVED]
;               A       Length of line
;-----------------------------------------------------------------------------------

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

.if C64         ; Color only available on the C64

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

.endif          ; C64

;-----------------------------------------------------------------------------------
; DrawBand      Draws a single band of the spectrum analyzer
;-----------------------------------------------------------------------------------
;               X       Band Number     [PRESERVED]
;               A       Height of bar
;-----------------------------------------------------------------------------------
; A band's screen rows hold blanks above the bar, the top symbols on the bar's top
; row and middle symbols below that. Its bottom row holds blanks, "single-height"
; symbols or the bottom symbols, for a height of 0, 1 or more, respectively.
;
; To save time, we only redraw what changed since the band was last drawn. The
; height it was drawn with is kept in PrevPeaks:
; - If the band grew, we draw the top symbols on the new top row, and fill the rows
;   below it with middle symbols, down to the row above the bottom row.
; - If the band shrank, we fill the rows above the new top row with blanks, up to
;   BAND_TOP_ROW, and draw the top symbols on the new top row.
; - We only redraw the bottom row if what it holds changes.
; A PrevPeaks value of $FF forces a complete redraw of the band.
;
; The runs of blanks and middles are drawn by jumping into unrolled sequences of
; stores (BlankRun and MidRun) that always continue to their last row. This can
; rewrite rows with the symbols they already hold, which is quicker than stopping
; the run early.
;-----------------------------------------------------------------------------------

                BAND_SCREEN_LOC = SCREEN_MEM + LEFT_MARGIN
                BAND_BOTTOM_LOC = BAND_SCREEN_LOC + BAND_BOTTOM_ROW * XSIZE

DrawBand:       cmp #MAX_PEAK+1       ; Limit height to what fits on the screen
                bcc :+
                lda #MAX_PEAK
:               cmp PrevPeaks, x      ; If the band is on screen with this height
                bne :+                ;   already, we're done
                rts

:               sta Height            ; Save new height, and swap it in for the
                lda PrevPeaks, x      ;   old one in PrevPeaks
                sta OldHeight
                lda Height
                sta PrevPeaks, x

                stx BandIndex         ; Save band number and calculate the band's
                txa                   ;   column offset on screen
                asl
                .if COL80
                asl
                .endif
                sta BandCol

                lda Height            ; Determine screen row of new top of band
                jsr BandTopRow
                sta NewTop

                lda OldHeight         ; If the old height isn't valid, we draw the
                cmp #$FF              ;   complete band
                beq @full

                jsr BandTopRow        ; Compare old top row with new top row
                cmp NewTop
                beq @bottom           ; Same row, so only the bottom row can change
                bcc @shrunk           ; Old top is above new top, so the band shrank

                jsr DrawTopRow        ; Band grew
                jsr DrawMidRun
                jmp @bottom

@shrunk:        jsr DrawBlankRun
                jsr DrawTopRow

@bottom:        lda OldHeight         ; The bottom row stays the same if both the old
                cmp #2                ;   and new height are 2 or more
                bcc :+
                lda Height
                cmp #2
                bcs @done

:               lda OldHeight         ; Otherwise, only redraw it if its type changed
                jsr BottomRowType
                sta OldHeight
                lda Height
                jsr BottomRowType
                cmp OldHeight
                bne @drawbottom
@done:          ldx BandIndex
                rts

@full:          jsr DrawBlankRun
                jsr DrawTopRow
                jsr DrawMidRun
                lda Height
                jsr BottomRowType

@drawbottom:    ldx BandCol
                cmp #1
                beq @oneline
                bcs @barbottom

                lda #' '              ; Height 0: blanks
                sta BAND_BOTTOM_LOC, x
                sta BAND_BOTTOM_LOC+1, x
                .if COL80
                sta BAND_BOTTOM_LOC+2, x
                sta BAND_BOTTOM_LOC+3, x
                .endif
                ldx BandIndex
                rts

@oneline:       lda CharDefs + visualDef::ONELINE1SYMBOL    ; Height 1: single-height symbols
                sta BAND_BOTTOM_LOC, x
                .if COL80
                sta BAND_BOTTOM_LOC+1, x
                sta BAND_BOTTOM_LOC+2, x
                .endif
                lda CharDefs + visualDef::ONELINE2SYMBOL
                sta BAND_BOTTOM_LOC+BAND_WIDTH-1, x
                ldx BandIndex
                rts

@barbottom:     lda CharDefs + visualDef::BOTTOMLEFTSYMBOL  ; Height 2+: bottom symbols
                sta BAND_BOTTOM_LOC, x
                .if COL80
                lda CharDefs + visualDef::BOTTOMMIDDLESYMBOL               ; Draw center pieces on 80 column screens only
                sta BAND_BOTTOM_LOC+1, x
                sta BAND_BOTTOM_LOC+2, x
                .endif
                lda CharDefs + visualDef::BOTTOMRIGHTSYMBOL
                sta BAND_BOTTOM_LOC+BAND_WIDTH-1, x
                ldx BandIndex
                rts

;-----------------------------------------------------------------------------------
; BandTopRow    Returns in A the screen row of the top of a band with height A. For
;               heights 0 and 1 that's BAND_BOTTOM_ROW, as all rows above the bottom
;               row are then blank.
;-----------------------------------------------------------------------------------

BandTopRow:     cmp #1
                bcs :+
                lda #1
:               eor #$FF              ; A = BAND_BOTTOM_ROW + 1 - A
                sec
                adc #BAND_BOTTOM_ROW + 1
                rts

;-----------------------------------------------------------------------------------
; BottomRowType Returns in A what the bottom row of a band with height A holds:
;               0 = blanks, 1 = single-height symbols, 2 = bottom symbols
;-----------------------------------------------------------------------------------

BottomRowType:  cmp #2
                bcc :+
                lda #2
:               rts

;-----------------------------------------------------------------------------------
; DrawTopRow    Draws the top symbols of the band at BandCol on row NewTop, unless
;               that's the bottom row
;-----------------------------------------------------------------------------------

DrawTopRow:     ldy NewTop
                cpy #BAND_BOTTOM_ROW
                bcs @done

                lda BandRowLo - BAND_TOP_ROW, y     ; Point zptmp to row NewTop
                sta zptmp
                lda BandRowHi - BAND_TOP_ROW, y
                sta zptmp+1

                ldy BandCol
                lda CharDefs + visualDef::TOPLEFTSYMBOL
                sta (zptmp), y
                iny
                .if COL80
                lda CharDefs + visualDef::TOPMIDDLESYMBOL               ; Draw center pieces on 80 column screens only
                sta (zptmp), y
                iny
                sta (zptmp), y
                iny
                .endif
                lda CharDefs + visualDef::TOPRIGHTSYMBOL
                sta (zptmp), y
@done:          rts

;-----------------------------------------------------------------------------------
; DrawBlankRun  Fills the band at BandCol with blanks, from the row above NewTop up
;               to BAND_TOP_ROW
;-----------------------------------------------------------------------------------

DrawBlankRun:   ldy NewTop
                cpy #BAND_TOP_ROW+1   ; Nothing to do if the band has maximum height
                bcc @done

                lda BlankRunLo - BAND_TOP_ROW - 1, y    ; Run entry for row NewTop-1
                sta RunVector
                lda BlankRunHi - BAND_TOP_ROW - 1, y
                sta RunVector+1

                ldx BandCol
                lda #' '
                jmp (RunVector)       ; The run returns to our caller
@done:          rts

;-----------------------------------------------------------------------------------
; DrawMidRun    Fills the band at BandCol with middle symbols, from the row below
;               NewTop down to the row above BAND_BOTTOM_ROW, one column at a time
;-----------------------------------------------------------------------------------

DrawMidRun:     ldy NewTop
                iny
                cpy #BAND_BOTTOM_ROW  ; Nothing to do if there are no rows between the
                bcc :+                ;   top and bottom rows
                rts

:               lda MidRunLo - BAND_TOP_ROW - 1, y      ; Run entry for row NewTop+1
                sta RunVector
                lda MidRunHi - BAND_TOP_ROW - 1, y
                sta RunVector+1

                ldx BandCol
                lda CharDefs + visualDef::VLINE1SYMBOL
                jsr JumpRun
                inx
                .if COL80
                lda CharDefs + visualDef::HLINE1MIDDLESYMBOL            ; Draw center pieces on 80 column screens only
                jsr JumpRun
                inx
                jsr JumpRun
                inx
                .endif
                lda CharDefs + visualDef::VLINE2SYMBOL
JumpRun:        jmp (RunVector)       ; The run returns to our caller

;-----------------------------------------------------------------------------------
; Unrolled runs of stores used by DrawBand, with X holding the band's column offset
;-----------------------------------------------------------------------------------

STA_ABSX_LEN    = 3                                   ; Length of STA abs,X instruction
BLANK_RUN_ROWS  = BAND_BOTTOM_ROW - BAND_TOP_ROW      ; Rows above the bottom row
MID_RUN_ROWS    = BAND_BOTTOM_ROW - BAND_TOP_ROW - 1  ; Rows between top and bottom rows

BlankRun:                             ; All columns of a band, from bottom to top
.repeat BLANK_RUN_ROWS, row
  .repeat BAND_WIDTH, col
                sta BAND_SCREEN_LOC + (BAND_BOTTOM_ROW - 1 - row) * XSIZE + col, x
  .endrepeat
.endrepeat
                rts
.assert (* - BlankRun) = BLANK_RUN_ROWS * BAND_WIDTH * STA_ABSX_LEN + 1, error

MidRun:                               ; One column of a band, from top to bottom
.repeat MID_RUN_ROWS, row
                sta BAND_SCREEN_LOC + (BAND_TOP_ROW + 1 + row) * XSIZE, x
.endrepeat
                rts
.assert (* - MidRun) = MID_RUN_ROWS * STA_ABSX_LEN + 1, error

; Entry points into the runs, by the screen row they start with, from top to bottom

BlankRunLo:
.repeat BLANK_RUN_ROWS, row
                .byte <(BlankRun + (BLANK_RUN_ROWS - 1 - row) * BAND_WIDTH * STA_ABSX_LEN)
.endrepeat
BlankRunHi:
.repeat BLANK_RUN_ROWS, row
                .byte >(BlankRun + (BLANK_RUN_ROWS - 1 - row) * BAND_WIDTH * STA_ABSX_LEN)
.endrepeat

MidRunLo:
.repeat MID_RUN_ROWS, row
                .byte <(MidRun + row * STA_ABSX_LEN)
.endrepeat
MidRunHi:
.repeat MID_RUN_ROWS, row
                .byte >(MidRun + row * STA_ABSX_LEN)
.endrepeat

; Screen addresses of the rows that can hold the top of a band, from top to bottom

BandRowLo:
.repeat BAND_BOTTOM_ROW - BAND_TOP_ROW, row
                .byte <(BAND_SCREEN_LOC + (BAND_TOP_ROW + row) * XSIZE)
.endrepeat
BandRowHi:
.repeat BAND_BOTTOM_ROW - BAND_TOP_ROW, row
                .byte >(BAND_SCREEN_LOC + (BAND_TOP_ROW + row) * XSIZE)
.endrepeat

;-----------------------------------------------------------------------------------
; PlotEx        Replacement for KERNAL plot that fixes color ram update bug
;-----------------------------------------------------------------------------------
;               X       Cursor Y Pos
;               Y       Cursor X Pos
;               (NOTE Reversed) (No really, pay attention, they're BACKWARDS!)
;-----------------------------------------------------------------------------------

PlotEx:
.if C64         ; On the C64 we use, but fix, the PLOT kernal routine
                bcs     :+
                jsr     PLOT          ; Set cursor position using original ROM PLOT
                jmp     UPDCRAMPTR    ; Set pointer to color RAM to match new cursor position
:               jmp     PLOT          ; Get cursor position
.endif

.if PET         ; PET has no PLOT in kernal.
                bcs     @fetch         ; Fetch values if carry set
                sty     CURS_X
                stx     CURS_Y
                txa
                asl
                tay
                lda     ScreenLineAddresses,y
                sta     SCREEN_PTR
                lda     ScreenLineAddresses+1,y
                sta     SCREEN_PTR+1
                rts

@fetch:         ldy     CURS_X
                ldx     CURS_Y
                rts
.endif          ; PET

.if C64         ; CIAs only available on C64

;----------------------------------------------------------------------------
; InitTODClocks - Initialize CIA clockS to correct external frequency (50/60Hz)
;
; This routine figures out whether the C64 is connected to a 50Hz or 60Hz
; external frequency source - that traditionally being the power grid the
; AC adapter is connected to. It needs to know this to make the CIA time of
; day clock run at the right speed; getting it wrong makes the clock 20% off.
; This routine was effectively sourced from the following web page:
; https://codebase64.org/doku.php?id=base:efficient_tod_initialisation
; Credits for it go to Silver Dream.
;----------------------------------------------------------------------------

InitTODClocks:
                lda CIA2_CRB
                and #$7f                ; Set CIA2 TOD clock, not alarm
                sta CIA2_CRB

                sei
                lda #$00
                sta CIA2_TOD10          ; Start CIA2 TOD clock
@tickloop:      cmp CIA2_TOD10          ; Wait until tenths value changes
                beq @tickloop

                lda #$ff                ; Count down from $ffff (65535)
                sta CIA2_TA             ; Use timer A
                sta CIA2_TA+1

                lda #%00010001          ; Set TOD to 60Hz mode and start the
                sta CIA2_CRA            ;   timer.

                lda CIA2_TOD10
@countloop:     cmp CIA2_TOD10          ; Wait until tenths value changes
                beq @countloop

                ldx CIA2_TA+1
                cli

                lda CIA1_CRA

                cpx #$51                ; If timer HI > 51, we're at 60Hz
                bcs @pick60hz1

                ora #$80                ; Configure CIA1 TOD to run at 50Hz
                bne @setfreq1

@pick60hz1:     and #$7f                ; Configure CIA1 TOD to run at 60Hz

@setfreq1:      sta CIA1_CRA

                lda CIA2_CRA

                cpx #$51                ; If timer HI > 51, we're at 60Hz
                bcs @pick60hz2

                ora #$80                ; Configure CIA2 TOD to run at 50Hz
                bne @setfreq2

@pick60hz2:     and #$7f                ; Configure CIA2 TOD to run at 60Hz

@setfreq2:      sta CIA2_CRA

                rts

.endif          ; C64

;-----------------------------------------------------------------------------------
; StartTextTimer - Start the text TOD timer
;-----------------------------------------------------------------------------------

StartTextTimer:
.if C64         ; CIAs only available on the C64
                lda CIA1_CRB            ; Clear CRB7 to set the TOD, not an alarm
                and #$7f
                sta CIA1_CRB

                lda #$00

                sta CIA1_TODHR
                sta CIA1_TODMIN
                sta CIA1_TODSEC
                sta CIA1_TOD10          ; This write starts the clock
.endif

.if PET         ; On the PET we count jiffies
                lda JIFFY_CLOCK
                sta TextTimerStart
.endif
                rts

;-----------------------------------------------------------------------------------
; CheckTextTimer - Clear text if TOD timer is at "TextTimeout" seconds
;-----------------------------------------------------------------------------------

CheckTextTimer:
                lda TextTimeout
                beq @done

.if C64         ; Use the CIA timer on the C64
                cmp CIA1_TODSEC
                bcs @done

                lda #0
                sta TextTimeout
                jmp ClearTextBlock
.endif

.if PET         ; Check if a second's worth of jiffies has passed
                lda JIFFY_CLOCK
                sec
                sbc TextTimerStart
                cmp #SECOND_JIFFIES
                bcc @done

                lda TextTimerStart    ; Start counting the next second
                clc
                adc #SECOND_JIFFIES
                sta TextTimerStart

                dec TextTimeout       ; Decrease timeout second count
                beq ClearTextBlock    ; If we've reached 0, clear the text block
.endif

@done:          rts

; Note: ClearTextBlock must be within branch reach of CheckTextTimer!

;-----------------------------------------------------------------------------------
; ClearTextBlock - Clear text block lines
;-----------------------------------------------------------------------------------

ClearTextBlock:
                clc                   ; Prepare for start of writing
.if C64         ; Color only available on the C64
                lda #WHITE
                sta TEXT_COLOR
.endif

                ldx #TOP_MARGIN+BAND_HEIGHT
                stx tempY

@rowloop:       ldy #LEFT_MARGIN      ; Set cursor at end of left margin
                jsr PlotEx

                ldy #TEXT_WIDTH       ; Write spaces
                lda #' '
:               jsr CHROUT
                dey
                bne :-

                inc tempY             ; Move on to next line
                ldx tempY
                cpx #YSIZE-1
                bne @rowloop

                rts

.if TIMING && C64

;-----------------------------------------------------------------------------------
; InitTimer     Initlalize a CIA timer to run at 1ms so we can do timings
;-----------------------------------------------------------------------------------

InitTimer:

                lda   #$7F            ; Mask to turn off the CIA IRQ
                ldx   #<TIMERSCALE    ; Timer low value
                ldy   #>TIMERSCALE    ; Timer High value
                sta   CIA2_ICR
                stx   CIA2_TA         ; Set to 1msec (1022 cycles per IRQ)
                sty   CIA2_TA+1
                lda   #$FF            ; Set counter to FFFF
                sta   CIA2_TB
                sta   CIA2_TB+1
                ldy   #$51
                sty   CIA2_CRB        ; Enable and go
                rts

.endif          ; TIMING && C64

;-----------------------------------------------------------------------------------
; SetNextStyle - Select a visual style for the spectrum analyzer by copying a small
;               character table of PETSCII screen codes into our 'styletable' that
;               we use to draw the spectrum analyzer bars.  It defines the PETSCII
;               chars that we use to draw the corners and lines.
;-----------------------------------------------------------------------------------
; Copy the next style into the style table and increment the style table pointer
; with wraparound so that we can pick the next style next time in.
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

; Note: this routine flows into the next one

;-----------------------------------------------------------------------------------
; InvalidateBands - Make sure all bands are completely redrawn in the next frame,
;               and that the next frame is drawn right away
;-----------------------------------------------------------------------------------

InvalidateBands:
                lda #$FF              ; No band height matches $FF
                ldx #NUM_BANDS - 1
:               sta PrevPeaks, x
                dex
                bpl :-

                lda #1
                sta RedrawFlag
                rts

; Visual style definitions.  See the 'visualDef' structure defn in petrock.inc
; Each of these small tables includes the characters needed to draw the corners
; and vertical lines needed to form a box. Finally, the characters to use for bands
; of height 1 are also specified.


SkinnyRoundStyle:                     ; PETSCII screen codes for round tube bar style
.if C64
  ;     TL  TR  BL  BR  V1  V2  H1  H2  1L  1R  TM  BM  H1
  .byte 85, 73, 74, 75, 66, 66, 74, 75, 32, 32,  0,  0,  0
.endif
.if PET
  ;     TL  TR  BL  BR  V1  V2  H1  H2  1L  1R  TM  BM  H1
  .byte 85, 73, 74, 75, 93, 93, 74, 75, 32, 32, 67, 70, 32
.endif

DrawSquareStyle:                      ; PETSCII screen codes for square linedraw style
      ; TL             TR              BL                BR                 V1            V2
  .byte TOPLEFTSYMBOL, TOPRIGHTSYMBOL, BOTTOMLEFTSYMBOL, BOTTOMRIGHTSYMBOL, VLINE1SYMBOL, VLINE2SYMBOL
      ; H1                H2                 1L            1R            TM            BM            H1
  .byte BOTTOMLEFTSYMBOL, BOTTOMRIGHTSYMBOL, HLINE1SYMBOL, HLINE2SYMBOL, HLINE1SYMBOL, HLINE2SYMBOL, 32

BreakoutStyle:                        ; PETSCII screen codes for style that looks like breakout
.if C64
   ;     TL   TR   BL   BR   V1   V2   H1   H2   1L   1R  TM  BM  H1
  .byte 239, 250, 239, 250, 239, 250, 239, 250, 239, 250, 0,   0,  0
.endif
.if PET
   ;     TL   TR   BL   BR   V1   V2   H1   H2   1L   1R  TM    BM   H1
  .byte 228, 250, 228, 250, 228, 250, 228, 250, 228, 250, 228, 228, 228
.endif

CheckerboardStyle:                    ; PETSCII screen codes for checkerboard style
   ;    TL   TR  BL   BR  V1   V2  H1   H2  1L   1R  TM   BM   H1
  .byte 102, 92, 102, 92, 102, 92, 102, 92, 102, 92, 102, 102, 102

; Lookup table - each of the above mini tables is listed in this lookup table so that
;                we can easily find items 0-3
;
; The code currently assumes that there are four entries such that is can easily
; modulus the values.  These are the four entries.

StyleTable:
  .word SkinnyRoundStyle, BreakoutStyle, CheckerboardStyle, DrawSquareStyle

.if C64         ; Color only available on the C64

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


.endif          ; C64

; String literals at the end of file, as was the style at the time!

.include "fakedata.inc"

.if PET
notonoldrom:    .literal "SORRY, NO PETROCKING ON ORIGINAL ROMS.", 13, 0
.endif

startstr:       .literal "STARTING...", 13, 0
exitstr:        .literal "EXITING...", 13, 0
framestr:       .literal "  RENDER TIME: ", 0
titlestr:       .literal 12, "C64PETROCK.COM", 0
titlelen = * - titlestr

.if C64         ; Set text color to green on C64
clrGREEN:       .literal $99, $93, 0
.endif
.if PET         ; Color's not a thing on the PET
clrGREEN:       .literal $93, 0
.endif

DemoOnText:     .literal "DEMO MODE ON", 0
DemoOffText:    .literal "DEMO MODE OFF", 0

EmptyText:      .byte    ' ', 0

.if C64         ; Include help on color schemes on C64
HelpText1:      .literal "C: COLOR - S: STYLE - D: DEMO", 0
.endif
.if PET         ; Don't mention color on the PET
HelpText1:      .literal "S: STYLE - D: DEMO", 0
.endif
HelpText2:      .literal "B: BORDER - RUN/STOP: EXIT", 0
