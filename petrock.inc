;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64
;-----------------------------------------------------------------------------------
; (c) PlummersSoftwareLLC, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------

; System selection -----------------------------------------------------------------

.ifndef C64
    C64         = 0
.endif

.ifndef PET
    PET         = 0
.endif

.if (.not (PET .xor C64))
    .fatal "Define exactly one of PET or C64 to equal 1."
.endif

; Definitions ----------------------------------------------------------------------

.if C64
    __C64__             = 1

    .include            "basic_c64.inc"
    .include            "c64.inc"

    BASE                = $0801     ; C64 Start of BASIC
    PROGRAM             = $0900     ; Start of actual program

    zptmp               = $FB
    zptmpB              = $FD
    zptmpC              = $22

    TEXT_COLOR          = $0286   ; Current output color
    SCRATCH_START       = $033C   ; Datasette buffer
    SCRATCH_END         = $03FB   ; End of datasette buffer
    SCREEN_MEM          = $0400   ; Screen buffer of 1000 bytes

    RASTHI              = $D011  ; Hiword of screen raster
    RASTLO              = $D012  ; Loword of screen raster

    COLOR_MEM           = $d800   ; Screen color memory of 1000 bytes
    TIMERSCALE          = 1022    ; Clock runs at 1.022MHz

    BLACK               = 0
    WHITE               = 1
    RED                 = 2
    CYAN                = 3
    PURPLE              = 4
    GREEN               = 5
    BLUE                = 6
    YELLOW              = 7
    ORANGE              = 8
    BROWN               = 9
    LIGHT_RED           = 10
    DARK_GREY           = 11
    MED_GREY            = 12
    LIGHT_GREEN         = 13
    LIGHT_BLUE          = 14
    LIGHT_GREY          = 15

  .if SERIAL
    BAUD300             = 6
    BAUD1200            = 4
    BAUD2400            = 2
    BAUD4800            = 0
  .endif

.endif

.if PET
    .include "pet.inc"

    BASE                = $0401     ; PET Start of BASIC
  .if SERIAL
    RX_BUF              = $0500     ; Circular buffer for serial input (256 bytes)
    PROGRAM             = $0600     ; Start of actual program
  .else
    PROGRAM             = $0500
  .endif

    zptmp               = $BD
    zptmpB              = $00
    zptmpC              = $1F

    SCRATCH_START       = $033A   ; Second cassette buffer on PET
    SCRATCH_END         = $03F9   ; End of cassette buffer
    SCREEN_MEM          = $8000

    CLRSCR              = $E015
    YSIZE               = 25
  .if COL80
    XSIZE               = 80
  .else
    XSIZE               = 40
  .endif
.endif

; Includes --------------------------------------------------------------------------

    .include        "cbm_kernal.inc"

.if SERIAL
  .if C64           ; Currently only pull in serial kernal includes for C64
    .include        "ser-kernel.inc"
  .endif
                    ; Error code includes also used by PET driver
    .include        "ser-error.inc"

    ; Serial data packet

    MAGIC_LEN           = 1       ; Number of magic bytes if any
    MAGIC_BYTE_0        = (3 << 4 + 15); Index of "D" plus index of "P"
    DATA_LEN            = 8       ; Size of Data portion
    VU_LEN              = 1       ; Size of VU
    PACKET_LENGTH       = MAGIC_LEN + DATA_LEN + VU_LEN + 1 ; NUL terminated so +1
.endif

; System Definition -----------------------------------------------------------------

SECOND_JIFFIES      = 60                ; Number of jiffies in a second
MINUTE_JIFFIES      = 60 * SECOND_JIFFIES ; Number of jiffies in a minute

CLRHOME             = $93               ; Clear screen

; Defines below this line should generally not require changing

NUM_BANDS           = 16                ; 16 bands in the spectrum data

; Center the VU meter on the second screen line

VUPOS1              = SCREEN_MEM + XSIZE + XSIZE / 2 + 1
.if COL80
VUPOS2              = SCREEN_MEM + XSIZE + 6
.else
VUPOS2              = SCREEN_MEM + XSIZE + 2
.endif

.if C64
VUCOLORPOS          = COLOR_MEM  + XSIZE + 2
.endif

; Symbol definitions for square drawing - PETSCI for graphics that make a square

TOPLEFTSYMBOL       = 79
TOPMIDDLESYMBOL     = 247
TOPRIGHTSYMBOL      = 80
BOTTOMLEFTSYMBOL    = 76
BOTTOMRIGHTSYMBOL   = 122
VLINE1SYMBOL        = 101               ; Vertical line left side
VLINE2SYMBOL        = 103               ; Vertical line right side
.if C64
HLINE1SYMBOL        = 119               ; Horizontal line top side for C64
HLINE2SYMBOL        = 111               ; Horizontal line bottom side for C64
.endif
.if PET
HLINE1SYMBOL        = 99                ; Horizontal line top side for PET
HLINE2SYMBOL        = 100               ; Horizontal line bottom side for PET
.endif
MEDIUMSHADE         = 45                ; Symbol for an empty VU square
VUSYMBOL            = 244               ; Symbol for a filled VU square

; Classic Linedraw style

; TL   TR   BL   BR   V1   V2   H1   H2   1L   1R   TM   BM   H1
.struct visualDef
  TOPLEFTSYMBOL       .byte
  TOPRIGHTSYMBOL      .byte
  BOTTOMLEFTSYMBOL    .byte
  BOTTOMRIGHTSYMBOL   .byte
  VLINE1SYMBOL        .byte
  VLINE2SYMBOL        .byte
  HLINE1SYMBOL        .byte
  HLINE2SYMBOL        .byte
  ONELINE1SYMBOL      .byte
  ONELINE2SYMBOL      .byte
  TOPMIDDLESYMBOL     .byte
  BOTTOMMIDDLESYMBOL  .byte
  HLINE1MIDDLESYMBOL  .byte
.endstruct
VISUALDEF_SIZE = .sizeof(visualDef)

; Our keyboard definitions ------------------------------------------------

KEY_S           = $53
KEY_C           = $43
KEY_C_SHIFT     = $C3
KEY_D           = $44
KEY_B           = $42
KEY_RUNSTOP     = $03

; Our UI Definitions ------------------------------------------------------

.if COL80
LEFT_MARGIN         = 8
RIGHT_MARGIN        = LEFT_MARGIN
BAND_WIDTH          = 4
TOP_MARGIN          = 3
BOTTOM_MARGIN       = 4
.else
LEFT_MARGIN         = 4
RIGHT_MARGIN        = LEFT_MARGIN
BAND_WIDTH          = 2
TOP_MARGIN          = 3
BOTTOM_MARGIN       = 4
.endif
BAND_HEIGHT         = YSIZE-TOP_MARGIN-BOTTOM_MARGIN
TEXT_WIDTH          = XSIZE-LEFT_MARGIN-RIGHT_MARGIN
MAX_VU              = (((XSIZE-LEFT_MARGIN-RIGHT_MARGIN)/2)-1)

;----------------------------------------------------------------------------
; Common definitions for all Commodore BASICs
;----------------------------------------------------------------------------
    CR              = $0D ; Carriage return
    QUOT            = $22 ; Quote characarter
    CURDN           = $11 ; Cursor down
;---------- tokens ----------------------------------------------------
    TK_MUL          = $ac ; *
    TK_POW          = $ae ; ^
    TK_GT           = $b1 ; >
    TK_EQU          = $b2 ; =
    TK_LT           = $b3 ; <
    TK_IF           = $8b ; IF
    TK_REM          = $8f ; REM
    TK_PRINT        = $99 ; PRINT
    TK_SYS          = $9e ; SYS
    TK_NEW          = $a2 ; NEW
    TK_THEN         = $a7 ; THEN
    TK_PEEK         = $c2 ; PEEK

.assert (TOP_MARGIN + BOTTOM_MARGIN + BAND_HEIGHT <= YSIZE), error
