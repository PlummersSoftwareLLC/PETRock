;----------------------------------------------------------------------------
; Common definitions for all Commodore BASICs
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
