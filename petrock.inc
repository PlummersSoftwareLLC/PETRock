;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64
;-----------------------------------------------------------------------------------
; (c) PlummersSoftwareLLC, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------

; Definitions -----------------------------------------------------------------------

DEBUG               = 1       ; Enable code that only is included for debug builds
TIMING              = 0       ; Change border color to show drawing progress

.include "basic_c64.inc"

BASE                = $0801     ; C64 Start of BASIC
            
zptmp               = $FB
zptmpB              = $FD
zptmpC              = $22
                
TEXT_COLOR          = $0286   ; Current output color
SCRATCH_START       = $033C   ; Second cassette buffer on PET
SCRATCH_END         = $03FB   ; End of cassette buffer  
SCREEN_MEM          = $0400   ; Screen buffer of 1000 bytes
        
RASTHI              = $D011  ; Hiword of screen raster   
RASTLO              = $D012  ; Loword of screen raster
    
COLOR_MEM           = $d800   ; Screen color memory of 1000 bytes

; Serial magic bytes

MAGIC_LEN = 8                   ; Size of full packet
MAGIC_BYTE_0        = 68        ; ASCII 'D'       
MAGIC_BYTE_1        = 80        ; ASCII 'P'

; Clock related items

TALO                = $DD04
TAHI                = $DD05
CTLO                = $DD06
CTHI                = $DD07
ICR                 = $DD0D
CRA                 = $DD0E
CRB                 = $DD0F
VICIRQ              = $D019
LATEN               = $FA
KEYVAL              = $FC
    
TIMERSCALE          = 1022    ; Clock runs at 1.022MHz
    
; Screen color codes

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

BAUD300             = 4
BAUD1200            = 2
BAUD2400            = 0
; Includes --------------------------------------------------------------------------

    .include        "cbm_kernal.inc"
    .include        "ser-kernel.inc"
    .include        "ser-error.inc"
    .include        "c64.inc"

; System Definition -----------------------------------------------------------------

MINUTE_JIFFIES      = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES      = 60                ; Number of jiffies in a second

CLRHOME             = $93               ; Clear screen

; Defines below this line should generally not require changing

NUM_BANDS           = 16                ; 16 bands in the spectrum data
MAX_VU              = 17                ; Maximum size per side of the VU meter

; Center the VU meter on the second screen line

VUPOS1              = SCREEN_MEM + XSIZE + XSIZE / 2 + 1
VUPOS2              = SCREEN_MEM + XSIZE + 2
VUCOLORPOS          = COLOR_MEM  + XSIZE + 2

; Symbol definitions for square drawing - PETSCI for graphics that make a square 

TOPLEFTSYMBOL       = 79                
TOPRIGHTSYMBOL      = 80
BOTTOMLEFTSYMBOL    = 76
BOTTOMRIGHTSYMBOL   = 122               
VLINE1SYMBOL        = 101               ; Vertical line left side
VLINE2SYMBOL        = 103               ; Vertical line right side
HLINE1SYMBOL        = 119               ; Horizontal line top side
HLINE2SYMBOL        = 111               ; Horizontal line bottom side

MEDIUMSHADE         = 45                ; Symbol for an empty VU square
VUSYMBOL            = 244               ; Symbol for a filled VU square

; Classic Linedraw style


.struct visualDef
  TOPLEFTSYMBOL     .byte
  TOPRIGHTSYMBOL    .byte
  BOTTOMLEFTSYMBOL  .byte
  BOTTOMRIGHTSYMBOL .byte
  VLINE1SYMBOL      .byte
  VLINE2SYMBOL      .byte
  HLINE1SYMBOL      .byte
  HLINE2SYMBOL      .byte
.endstruct

; Our UI Definitions ------------------------------------------------------

LEFT_MARGIN         = 4
RIGHT_MARGIN        = LEFT_MARGIN
BAND_WIDTH          = 2
TOP_MARGIN          = 3
BOTTOM_MARGIN       = 4
BAND_HEIGHT         = YSIZE-TOP_MARGIN-BOTTOM_MARGIN


;----------------------------------------------------------------------------
; Common definitions for all Commodore BASICs
;----------------------------------------------------------------------------
    CR              = $0D ; CARRIAGE RETURN
    QUOT            = $22 ; Quote characarter
    CURDN           = $11 ; cursor down
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
