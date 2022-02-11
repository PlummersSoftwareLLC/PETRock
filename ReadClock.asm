;-----------------------------------------------------------------------------------
; Large Text Clock for CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) Dave Plummer, 12/16/2016
;-----------------------------------------------------------------------------------

.cpu 6502
.OBJFILE <readclock.obj>
.LISTFILE <readclock.lst>
.LISTOFF SOURCE
.LISTOFF INCLUDES

; Definitions -----------------------------------------------------------------------

PET             = 1
EPROM           = 0					; When TRUE, no BASIC stub, no load address in file
BASIC2          = 0
BASIC4          = 1

.INCLUDE "pet.inc"
.INCLUDE "basic4.inc"

.if EPROM
	BASE		.equ $B000		; Open PET ROM space
.else
	BASE		.equ $0401		; PET Start of BASIC
.endif

; Our Definitions -------------------------------------------------------------------

zpPTR = $BD
zpPTR2 = $00
zpPTR3 = $1F

DEVICE_NUM      .equ $08

; System Memory --------------------------------------------------------- ------------

.org 826
    DeviceResponse  .ds 64
.assert $ <= 1000


; Start of Binary -------------------------------------------------------------------

.org  $0000							
.if !EPROM
.word BASE		; Binary loads at 0x0401 on the PET
.endif
.org  BASE

; BASIC program to load and execute ourselves.  Simple lines of tokenized BASIC that
; have a banner comment and then a SYS command to start the machine language code.

.if !EPROM
Line10			.word Line20						; Next line number
				.word 10							; Line Number 10		
				.byte TK_REM						; REM token
				.string " PROGRAM TO READ PETSD CLOCK", 00

Line20			.word endOfBasic					; PTR to next line, which is 0000
				.word 20							; Line Number 20
				.byte TK_SYS                        ;   SYS token
				.string " "
				.string str$(*+7)					; Entry is 7 bytes from here, which is
													;  not how I'd like to do it but you cannot
													;  use a forward reference in STR$()

				.byte 00							; Do not modify without understanding 
endOfBasic		.word 00							;   the +7 expression above, as this is
.endif												;   exactly 7 bytes and must match it

;-----------------------------------------------------------------------------------
; Start of Code
;-----------------------------------------------------------------------------------

start			cld

                ldx #<CommandText
                ldy #>CommandText
                jsr SendCommand
                jsr GetDeviceStatus

                ldy #>DeviceResponse    ; Output load text for easier dev cycke
				lda #<DeviceResponse    
                jsr WriteLine

                jsr CRLF
                jsr CRLF

                ldy #>hello				; Output load text for easier dev cycke
				lda #<hello
				jsr WriteLine


ExitApp			rts

;-----------------------------------------------------------------------------------
; WriteLine - Writes a line of text to the screen using CHROUT ($FFD2)
;-----------------------------------------------------------------------------------
;			Y:	MSB of address of null-terminated string
;           A:  LSB
;-----------------------------------------------------------------------------------

WriteLine		sta zpPTR2
				sty zpPTR2+1
				ldy #0

@loop			lda (zpPTR2),y
				beq done
				jsr CHROUT
				iny
				bne @loop
done 			rts


; During development we output the LOAD statement after running to make the 
; code-test-debug cycle go a little easier - less typing

hello			.string "LOAD ", 34,"READCLOCK.OBJ",34,", 8",13,0

GetDeviceStatus    

	            lda #DEVICE_NUM
	            sta DN
	            jsr TALK    			; TALK
	            lda #$6f			    ; DATA SA 15
	            sta SA
	            jsr SECND               ; send secondary address
                ldy #$00
-
                phy
	            jsr ACPTR       	    ; read byte from IEEE bus
                ply
	            cmp #CR				    ; last byte = CR?
	            beq @done
	            sta DeviceResponse, y
                iny
	            jmp -		            ; branch always
@done:          lda #$00
                sta DeviceResponse, y   ; null terminate the buffer instead of CR
	            jsr UNTLK   		    ; UNTALK
	            rts

;----------------------------------------------------------------------------
; SEND COMMAND
;----------------------------------------------------------------------------
CommandText     .string "I0",0
;CommandText     .string "t-ri",0

SendCommand     stx zpPTR
                sty zpPTR+1

	            lda #DEVICE_NUM
	            sta DN
	            lda #$6f			    ; DATA SA 15
	            sta SA
	            jsr LISTN   		    ; LISTEN
	            lda SA
	            jsr SECND       		; send secondary address
                ldy #0
-               lda (zpPTR), y
                beq @done
	            jsr CIOUT           	; send char to IEEE
                iny
                bne -

@done:
            	jsr UNLSN               ; Unlisten
                rts

