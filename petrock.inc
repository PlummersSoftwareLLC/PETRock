;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) PlummersSoftwareLLC, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------

.ifndef C64
    C64         = 0
.endif
.ifndef PET
    PET         = 0
.endif

.if (.not (PET .xor C64))
    .fatal "Please define exactly one of PET or C64 to equal 1."
.endif

; Definitions -----------------------------------------------------------------------

DEBUG               = 1     ; Enable code that only is included for debug builds
EPROM               = 0     ; When TRUE, no BASIC stub, no load address in file
TIMING              = 0     ; Change border color to show drawing progress

.if C64
    __C64__      = 1

    .include "basic_c64.inc"

    .if EPROM
        BASE    = $8000     ; Open C64 ROM space (not re)
    .else
        BASE    = $0801     ; C64 Start of BASIC
    .endif

    zptmp         = $FB
    zptmpB        = $FD
    zptmpC        = $22


    SCRATCH_START = $033C   ; Second cassette buffer on PET
    SCRATCH_END   = $03FB   ; End of cassette buffer  
  	SCREEN_MEM    = $0400   ; Screen buffer of 1000 bytes

    SCREEN_COLOR   = VIC_BG_COLOR0
    BORDER_COLOR   = VIC_BORDERCOLOR 
    SCREEN_CONTROL = VIC_CTRL1

    COLOR_MEM     = $d800   ; Screen color memory of 1000 bytes


    ; Screen color codes

    BLACK         = 0
    WHITE         = 1
    RED           = 2
    CYAN          = 3
    PURPLE        = 4
    GREEN         = 5
    BLUE          = 6
    YELLOW        = 7
    ORANGE        = 8
    BROWN         = 9
    LIGHT_RED     = 10
    DARK_GREY     = 11
    MED_GREY      = 12
    LIGHT_GREEN   = 13
    LIGHT_BLUE    = 14
    LIGHT_GREY    = 15
.endif

.if PET

    __CBM510__   = 1        ; If building for the PET we'll assume 80 column machine
    .if EPROM
        BASE    = $B000     ; Open PET ROM space
    .else
        BASE    = $0401     ; PET Start of BASIC
    .endif

    zptmp  		  = $BD
    zptmpB 		  = $00
    zptmpC 		  = $1F
    
    SCRATCH_START = $033A   ; Second cassette buffer on PET
    SCRATCH_END   = $03F9   ; End of cassette buffer  
	  SCREEN_MEM    = $8000

.endif

; Includes --------------------------------------------------------------------------

        .include        "cbm_kernal.inc"
        .include        "ser-kernel.inc"
        .include        "ser-error.inc"
        .include        "c64.inc"

; System Definition -----------------------------------------------------------------

MINUTE_JIFFIES      = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES      = 60                ; Number of jiffies in a second

CLRHOME		          = $93               ; Clear screen


; Defines below this line should generally not require changing

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

; System Locations ----------------------------------------------------------

.if XSIZE = 40
    BAND_WIDTH      = 2
    LEFT_MARGIN     = 4
.endif
.if XSIZE = 80
    BAND_WIDTH      = 4
    LEFT_MARGIN     = 8
.endif

TOP_MARGIN          = 3
BOTTOM_MARGIN       = 2
BAND_HEIGHT         = 20

;----------------------------------------------------------------------------
; Common definitions for all Commodore BASICs
;----------------------------------------------------------------------------
	CR			          = $0D ; CARRIAGE RETURN
	QUOT		          = $22 ; Quote characarter
	CURDN		          = $11 ; cursor down
;---------- tokens ----------------------------------------------------------
	TK_MUL		        = $ac ; *
	TK_POW		        = $ae ; ^
	TK_GT		          = $b1 ; >
	TK_EQU		        = $b2 ; =
	TK_LT		          = $b3 ; <
	TK_IF		          = $8b ; IF
	TK_REM		        = $8f ; REM
	TK_PRINT	        = $99 ; PRINT
	TK_SYS		        = $9e ; SYS
	TK_NEW		        = $a2 ; NEW
	TK_THEN		        = $a7 ; THEN
	TK_PEEK		        = $c2 ; PEEK

.assert (TOP_MARGIN + BOTTOM_MARGIN + BAND_HEIGHT <= YSIZE), error
