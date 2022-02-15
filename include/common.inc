;----------------------------------------------------------------------------
; System definitions
;----------------------------------------------------------------------------

.ifndef C64
    C64         = 0
.endif

.ifndef PET
    PET         = 0
.endif

.if (.not (PET .xor C64))
    .fatal "Please define exactly one of PET or C64 to equal 1."
.endif

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

MINUTE_JIFFIES      = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES      = 60                ; Number of jiffies in a second

;----------------------------------------------------------------------------
; Common definitions for all Commodore BASICs
;----------------------------------------------------------------------------
	CR			= $0D ; CARRIAGE RETURN
	QUOT		= $22 ; Quote characarter
	CLRHOME		= $93 ; clear screen
	CURDN		= $11 ; cursor down

	ROWS		= 25  ; screen rows
;---------- tokens ----------------------------------------------------------
	TK_MUL		= $ac ; *
	TK_POW		= $ae ; ^
	TK_GT		= $b1 ; >
	TK_EQU		= $b2 ; =
	TK_LT		= $b3 ; <

	TK_IF		= $8b ; IF
	TK_REM		= $8f ; REM
	TK_PRINT	= $99 ; PRINT
	TK_SYS		= $9e ; SYS
	TK_NEW		= $a2 ; NEW
	TK_THEN		= $a7 ; THEN
	TK_PEEK		= $c2 ; PEEK
