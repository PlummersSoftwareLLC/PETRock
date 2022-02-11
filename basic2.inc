; TODO: convert commented addresses to values for BASIC 4


; --------- BASIC 2 ROM ROUTINES -----------------------------------------

BASIC	.equ 2		; remember BASIC 2 selection

SCROUT	.equ $e3d8	; output A to screen
;	RDOA		.equ $d754	; read hex address -> $FB,$FC
;	RDOB		.equ $d763	; read hex byte -> A
;	CLRSCR		.equ $e015	; clear screen
CRLF	.equ $fdd0	; move cursor to next line
;	FPOUT		.equ $cf8d	; write floating point
INTOUT	.equ $dcd9	; write integer 0-65535 X.equLSB,A.equMSB
;	SCROLD		.equ $e021	; scroll down
;	SCROLU		.equ $e024	; scroll up
;	SPAC2		.equ $d52e	; write 2 spaces
;	SPACE		.equ $d531	; write space
;	STROUT		.equ $bb24	; X.equlen, STRADR.equptr
;	STROUTZ		.equ $bb1d	; A.equLSB, Y.equMSB
;	WROA		.equ $d717	; write hex address
;	HEXOUT      	.equ $d722 ; write A as hex
;	WROB		.equ $d731	; write hex byte
;	WRTWO		.equ $d731	; write XA to screen

;	ADRFP		.equ $cd7f	; uint LSB.equ$5f, MSB.equ$60 -> FAC1
;	ASCII		.equ $d73a	; hex digit -> ascii hex digit
;	FPINT		.equ $cdd1	; FAC1 -> uint $61.equMSB, $62.equLSB
;	FPSTR		.equ $cf93	; fp -> str $0100..NUL
;	HEXIT		.equ $d78d	; hex ascii digit A -> hex digit A
;	INTFP		.equ $c4bc	; int Y.equLSB, A.equMSB -> FAC1
;	STRFP		.equ $c8eb	; string -> fp
RDINT	.equ $c873	; read basic text -> LINNUM

;	ABS		.equ $cd8e	; FAC1 .equ ABS(FAC1)
;	ADD		.equ $c9a0	; FAC1 .equ FAC1 + FAC2
;	ATN		.equ $d32C	; FAC1 .equ ATN(FAC1)
;	COS		.equ $d282	; FAC1 .equ COS(FAC1)
;	DIV		.equ $cc48	; FAC1 .equ FAC2 / FAC1
;	EXP		.equ $d184	; FAC1 .equ EXP(FAC1)
;	FAC12		.equ $cd45	; FAC2 :.equ FAC1
; TODO: add more FP routines!

ACPTR	.equ $f18c	; input IEEE -> A
;	CHKIN		.equ $ffc6	; stdin from logical file # in X
CIOUT	.equ $f16f	; output A -> IEEE
;	CKOUT		.equ $ffc9	; stdout to logical file # in X
;	CLOSE		.equ $f2e0	; close file
CLSEI	.equ $f6f0	; close and unlisten
;	CLRCH		.equ $ffcc	; r.equ stdio to defaults
LISTN	.equ $f0ba	; send LISTEN
;	LOAD		.equ $f408	; LOAD
LOADOP	.equ $f322	; LOAD without pointer change
FILENOTFOUND .equ $f3e6	; FILE NOT FOUND, return to BASIC
;	OPEN		.equ $f563	; open file
OPENI	.equ $f466	; open file on IEEE device
;	SCNT		.equ $f2c1	; prepare.equT for logical file A
SECND	.equ $f128	; send secondary address A
;.equT		.equ $f2cd	;.equ LF,DN,SA from table
TALK	.equ $f0b6	; send TALK
;	TWAIT		.equ $f92b	;
UNLSN	.equ $f183	; send UNLISTEN
UNTLK	.equ $f17f	; send UNTALK

READY	.equ $c389	; basic warm start
;	R.equ		.equ $fd16	; basic cold start
NEWSTT	.equ $c6c4	;.equ up next statement for execution
STOPEQ	.equ $f301	;.equ Z flag if stop key pressed
;	STOPR		.equ $ffe1	; return to basic if stop key pressed
ERRO	.equ $c36c	; BASIC error message
SYNERR	.equ $ce03	; SYNTAX ERROR
IQERR	.equ $d123	; ILLEGAL QUANTITY ERROR

;	MOVBLK		.equ $b357	; move block

;	GRAPHIC		.equ $e01b	;.equ graphic mode
;	TEXT		.equ $e018	;.equ text mode
;	BEEP		.equ $e02a	; beep

LINKPRG	.equ $c442	; rechain lines
RSTXCLR	.equ $c572 	; r.equ TXTPTR and perform CLR
STXTPT	.equ $c5a7	; r.equ TXTPTR
;	CLR		.equ $b5f0	; perform CLR
;	SCRTCH		.equ $b5d4	; perform NEW

MAINS	.equ $c39b	; MAIN's return addr on stack for CHRGET routine

