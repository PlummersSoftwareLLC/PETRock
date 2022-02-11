;----------------------------------------------------------------------------
; Common definitions for BASIC 2 and BASIC 4
;----------------------------------------------------------------------------
	CR			= $0D ; CARRIAGE RETURN
	QUOT		= $22 ; Quote characarter
	CLRHOME		= $93 ; clear screen
	CURDN		= $11 ; cursor down

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

; --------- zero page -------------------------------------------------------

	LINNUM		= $11	; $11=lsb, $12=msb result from RDINT
	STRADR		= $1f	; string address $1f,$20

	MEMSIZ		= $34	; addr. of last RAM byte +1

	MOVTEND		= $55	; block move target end addr+1
	MOVSEND		= $57	; block move source end addr+1
	MOVSRC		= $5c	; block move source start addr

	DN			= $d4	; device number
	FNADR		= $da	; ptr $da,$db to filename
	FNLEN		= $d1	; filename's length
	LF			= $d2	; logical file number
	LVFLAG		= $9d	; LOAD/VERIFY flag
	SA			= $d3	; secondary address
	STATUS		= $96	; status ST
	EAL			= $c9	; $c9/$ca end of program
	VARTAB		= $2a	; $2a/$2b start of basic variables

	CHRGET		= $70	; subroutine: get next byte of BASIC text
	CHRGOT		= $76	; entry to get same byte of text again
	TXTPTR		= $77	; $77/$78 pointer to next CHRGET byte

	MEMUSS		= $fd	; tape load temps

; --------- KERNAL subroutines ----------------------------------------------

	BASIN		= $ffcf ; read char from input channel -> A
	BSOUT		= $ffd2	; Write A to stdout
	CHROUT		= $ffd2	; Write A to stdout (more common name)
	GETIN		= $ffe4	; read char from keyboard buffer -> A

;----------------------------------------------------------------------------
