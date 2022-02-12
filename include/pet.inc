;----------------------------------------------------------------------------
; Definitions for the COMMODORE PET
;----------------------------------------------------------------------------

    COLUMNS       = 80      ; Screen width, either 40 or 80

    SCRATCH_START = $033A   ; Second cassette buffer on PET
    SCRATCH_END   = $03F9   ; End of cassette buffer  
	SCREEN_MEM    = $8000

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

;----------------------------------------------------------------------------

	CHROUT	= $FFD2         ; Output a character
	GETIN   = $FFE4			; Read a character