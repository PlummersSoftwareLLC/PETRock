;-----------------------------------------------------------------------------------
; Large Text Clock for CBM/PET 6502
;-----------------------------------------------------------------------------------
; (c) Dave Plummer, 12/26/2016. If you can read it, you can use it! No warranties!
;                   12/26/2021. Ported to the cc65 assembler package (davepl)
;                   01/19/2022. Run clock based on jiffy/RTC time (rbergen)
;-----------------------------------------------------------------------------------
; Environment: xpet -fs9 d:\OneDrive\PET\source\ -device9 1
;            : PET 2001
;-----------------------------------------------------------------------------------
; On PETs that don't have a petSD+, the clock will be initialized from the internal
; system (jiffy) clock in the PET. The system clock can be initialized in BASIC 
; before running this clock, by issuing the following command:
;
; TI$="HHMMSS"
;
; The hours can be specified in 24-hour format; they will be converted to 12-hour
; format, as is the time read from the petSD+.


.SETCPU "65C02"

; Definitions -----------------------------------------------------------------------
PET             = 1
DEBUG           = 1					; Enable code that only is included for debug builds
EPROM           = 0					; When TRUE, no BASIC stub, no load address in file
PETSDPLUS       = 0                 ; When TRUE, read RTC from petSD+
SHOWAMDEFAULT   = 1                 ; Use a dot separator for AM and colon for PM
COLUMNS         = 40                ; Screen width, either 40 or 80

; Defines below this line should generally not require changing

DEVICE_NUM      = 9
MINUTE_JIFFIES  = 3600              ; Number of jiffies in a minute
SECOND_JIFFIES  = 60                ; Number of jiffies in a second

.INCLUDE "pet.inc"
.INCLUDE "basic4.inc"

.if EPROM
    BASE		= $B000		; Open PET ROM space
.else
    BASE		= $0401		; PET Start of BASIC
.endif

; System Locations ------------------------------------------------------------------

SCREEN_MEM  = $8000
JIFFY_TIMER = $008F

; Our Definitions -------------------------------------------------------------------

zptmp  = $BD
zptmpB = $00
zptmpC = $1F

.org 826							; Second cassette buffer on PET
.bss


; These are scratch variables - they are here in the cassette buffer so that we can
; be burned into ROM (if we just used .byte we couldn't write values back)

ScratchStart:    
   ClockCount:       .res  2            ; 16 bit countup timer to move clock to avoid screen burn
   ClockXPos:        .res  1            ; Current cursor X pos of clock
   ClockYPos:	     .res  1            ; Current cursor Y pos of clock
   ClockX:		     .res  1            ; Temp X for clock draw code
   ClockY:		     .res  1            ; Temp Y for clock draw code
   temp:			 .res  1            ; General scratch variable
   bitmask:		     .res  1            ; Bitmask to use to walk through character bitmap row
   bitcount:		 .res  1            ; Count of which row we're on in bitmap character
   tempchar:		 .res  1	        ; Scratch variable
   MultiplyTemp:	 .res  1            ; Scratch variable for multiply code
   resultLo:		 .res  1			; Results from multiply operations
   resultHi:		 .res  1
   remainder:        .res  3            ; Remainder for jiffy load division
   PmFlag:           .res  1            ; Keep track of AM/PM. AM=0, PM=1
   ShowAM:           .res  1            ; Indicate AM with dot instead of colon
ScratchEnd:		 

; This is where we store the time

ClockStart:		 
   HourTens:		 .res  1            ; Various parts of the clock, in text like ASCII '0'
   HourDigits:	     .res  1
   MinTens:		     .res  1
   MinDigits:	     .res  1	
   SecTens:		     .res  1
   SecDigits:	     .res  1
   Tenths:		     .res  1
ClockEnd:

; This is the structure we read the petSD+ clock into for parsing

DeviceBufferStart:
    DevResponse = *                     ; 2017-01-09t12:34:56 mon
    ClkYear:         .res  4
    ClkDash1:        .res  1
    ClkMonth:        .res  2
    ClkDash2:        .res  1
    ClkDay:          .res  2
    ClkLetterT:      .res  1
    ClkHourTens:     .res  1
    ClkHourDigits:   .res  1
    ClkColon1:       .res  1
    ClkMinTens:      .res  1
    ClkMinDigits:    .res  1
    ClkColon2:       .res  1
    ClkSecTens:      .res  1
    ClkSecDigits:    .res  1
    ClkSpace1:       .res  1
    ClkDayOfWeek:    .res  3
DeviceBufferEnd:

.assert (DeviceBufferEnd - DeviceBufferStart) = 23, error   ; Verify length of struct matches fixed RTC format
.assert * <= 1000, error					                ; Make sure we haven't run off the end of the buffer

; Start of Binary -------------------------------------------------------------------

.code

; BASIC program to load and execute ourselves.  Simple lines of tokenized BASIC that
; have a banner comment and then a SYS command to start the machine language code.

.if !EPROM
                .org 0000
                .word BASE
                .org  BASE
Line10:			.word Line20						; Next line number
                .word 10							; Line Number 10	
                .byte TK_REM						; REM token
                .literal "ARKABLE CLOCK BY DAVEPL (C) 2017", 00

Line20:			.word endOfBasic					; PTR to next line, which is 0000
                .word 20							; Line Number 20
                .byte TK_SYS						;   SYS token
                .literal " "
                .literal .string(*+7)	    		; Entry is 7 bytes from here, which is
                                                    ;  not how I'd like to do it but you cannot
                                                    ;  use a forward reference in STR$()

                .byte 00							; Do not modify without understanding 
endOfBasic:		.word 00							;   the +7 expression above, as this is
.else                                               ;   exactly 7 bytes and must match it
    .org BASE
.endif												

;-----------------------------------------------------------------------------------
; Start of Code
;-----------------------------------------------------------------------------------

start:			cld
                jsr InitVariables       ; Since we can be in ROM, zero stuff out
                jsr LoadClock
MainLoop:		
                jsr UpdateClock         ; Update clock values if it's time to do so
                ldy ClockYPos			
                ldx ClockXPos
                jsr DrawClockXY

InnerLoop:		jsr UpdateClockPos      ; Carry will be clear when its time to check
                bcs MainLoop            ;   keyboard and move clock

                jsr GETIN				; Keyboard Handling - check for input
                cmp #0
                beq InnerLoop

                cmp #$03
                bne notEscape
                beq ExitApp				; Escape pressed, go to exit

notEscape:		

.if PETSDPLUS
                cmp #$4C
                bne @notLoad
                jsr LoadClock           ; L pressed, load time off RTC
                jmp @MainLoop

@notLoad:
.endif
                cmp #$5A				
                bne @notZero
                jsr ZeroSeconds		    ; Z pressed, set seconds to 0
                jmp InnerLoop

@notZero:       cmp #$48				
                bne @notHour
                jsr IncrementHour		; H pressed, increment hour
                jmp MainLoop

@notHour:		cmp #$C8
                bne @notHourDn
                jsr DecrementHour		; SHIFT-H pressed, decrement hour
                jmp MainLoop

@notHourDn:		cmp #$4D
                bne @notMin
                jsr IncrementMinute		; M pressed, increment minute
                jsr ZeroSeconds
                jmp MainLoop

@notMin:		cmp #$CD
                bne @notMinDn
                jsr DecrementMinute		; SHIFT-M pressed, decrement minute
                jsr ZeroSeconds
                jmp MainLoop

@notMinDn:		cmp #$53
                bne @notShowAM
                jsr ToggleShowAM		; S pressed, toggle show AM flag
                bcs MainLoop
                jmp InnerLoop

@notShowAM:     cmp #$55
                bne @notUpdate
                jmp MainLoop            ; U pressed, update clock now

@notUpdate:     jsr ShowInstructions	; Any other key shows the help text
                jmp InnerLoop			;  which gets erased after a few seconds

ExitApp:		
.if !PETSDPLUS
                jsr UpdateJiffyClock    ; Set jiffy clock to our time
.endif

                jsr ClearScreen

.if DEBUG
                ldy #>loadstr		    ; Output load text and exit
                lda #<loadstr
                jsr WriteLine
.endif
                rts

;-----------------------------------------------------------------------------------
; InitVariables	
;-----------------------------------------------------------------------------------
; We use a bunch of storage in the system (on the PET, it's Cassette Buffer #2) and
; it starts out in an unknown state, so we have code to zero it or set it to defaults
;-----------------------------------------------------------------------------------

InitVariables:	ldx #ScratchEnd-ScratchStart
                lda #$00				    ; Init variables to #0
:				sta ScratchStart, x
                dex
                cpx #$ff
                bne :-

                ldx #ClockEnd-ClockStart	; Init all clock digits to '0'
                lda #'0'
:				sta ClockStart, x
                dex
                cpx #$ff
                bne :-

                lda #SHOWAMDEFAULT          ; Set ShowAM setting to default
                sta ShowAM

                rts
                
;-----------------------------------------------------------------------------------
; UpdateClockPos - Moves the clock around on the screen so that it doesn't burn 
;                  into the phosophor quite so much.  Doesn't use the bottom 3
;                  rows so that we always have somewhere for instructions.
;
; Carry flag if set indicated on return that the clock has moved
;-----------------------------------------------------------------------------------

UpdateClockPos:
                inc ClockCount			; Increment the low byte of counter
                bne @nomove
                inc ClockCount+1		; Increment the high byte 
                lda ClockCount+1
                cmp #200				; If high byte hasn't reached 200, a suitable delay, nothing to do
                bne @nomove
                 
                lda #$0					; Reset wait timer to zero
                sta ClockCount 
                sta ClockCount+1

                lda JIFFY_TIMER
                and #3
                sta ClockXPos
    
                inc ClockYPos
                lda ClockYPos
                cmp #15					; Don't go lower than this to leave room for instructions
                bne @donemove
                lda #0
                sta ClockYPos
@donemove:
                sec
                rts

@nomove:		clc
                rts

;-----------------------------------------------------------------------------------
; ConvertPetSCII - Convert .literal ASCI to PET screen code		
;                  Apprently not a complete conversion but it works for my purposes
;-----------------------------------------------------------------------------------

ConvertPetSCII:	sta temp
                lda #%00100000
                bit temp
                bvc :+
                beq :+
                lda temp
                and #%10011111
                rts
:				lda temp
                rts


;-----------------------------------------------------------------------------------
; ToggleShowAM - Toggle the showing of dot instead of colon in AM
;
; Sets the carry flag if the clock should be redrawn after it returns (AM)
;-----------------------------------------------------------------------------------
 
ToggleShowAM:
                lda ShowAM              ; load show AM flag, flip bit, save
                eor #1
                sta ShowAM

                ldx #0                  ; Only show banner message if it's PM...
                cpx PmFlag
                bne @prepmsg

                sec                     ; ...otherwise just update clock
                rts

@prepmsg:       cmp #0                  ; Decide what message to print
                bne @showamon
                ldx #<AMOffMessage
                ldy #>AMOffMessage
                jmp @showmsg

@showamon:      ldx #<AMOnMessage
                ldy #>AMOnMessage
@showmsg:       jsr ShowBanner

                clc                     ; Tell caller we output message
                rts


;-----------------------------------------------------------------------------------
; ShowInstructions - Print the help banner on the bottom three rows
;-----------------------------------------------------------------------------------
 
ShowInstructions:
                ldx #<Instructions
                ldy #>Instructions

                jmp ShowBanner


;-----------------------------------------------------------------------------------
; ShowBanner - Print zero-terminated text to the banner at the bottom of the screen
;-----------------------------------------------------------------------------------
;          X:  lsb of start of banner message
;          Y:  msb of start of banner message
;-----------------------------------------------------------------------------------

ShowBanner:
                stx zptmp
                sty zptmp+1
                
                lda #$00						; Reset timer counter so banner will stay up a few seconds
                sta ClockCount
                sta ClockCount+1

                lda #<(SCREEN_MEM + 22 * COLUMNS)	; Place instructions at line 22-25 of the screen
                sta zptmpB
                lda #>(SCREEN_MEM + 22 * COLUMNS)
                sta zptmpB+1
                ldy #0
@loop:			lda (zptmp),y
                beq @done
                jsr ConvertPetSCII				; Our text is in ASCII, convert to PET screen codes
@output:		sta (zptmpB),y
                iny
                bne @loop
@done:			rts


;-----------------------------------------------------------------------------------
; ZeroSeconds - (Re)sets the second zero point to now
;-----------------------------------------------------------------------------------

ZeroSeconds:
                lda #0
                ldx #0
                jmp writeJiffy

;-----------------------------------------------------------------------------------
; SetSeconds - Set jiffy timer to a specific number of seconds 
;-----------------------------------------------------------------------------------
;          X:  Number of seconds ( < 60 ) to set the jiffy timer to 
;-----------------------------------------------------------------------------------

SetSeconds:
                cpx #0                  ; Calculate jiffies if we have seconds to add
                bne @calcjiffies

                lda #0                  ; Zero out A and proceed to write 
                beq writeJiffy

@calcjiffies:   ldy #SECOND_JIFFIES     ; Multiply seconds by jiffies per second
                jsr Multiply            ;   and load results in registers
                lda resultLo
                ldx resultHi

writeJiffy:     ldy #0                  ; Highest byte is always 0
                sei                     ; Write jiffy timer with interrupts disabled.
                sta JIFFY_TIMER       
                stx JIFFY_TIMER-1
                sty JIFFY_TIMER-2
                cli
                rts

;-----------------------------------------------------------------------------------
; LoadClock - Sets the current time of day from hardware or the jiffy clock
;-----------------------------------------------------------------------------------

LoadClock:
.if PETSDPLUS  
                ldx #<CommandText
                ldy #>CommandText
                jsr SendCommand         ; Fetch time from Real Time Clock on petSD+
                jsr GetDeviceStatus
.else
                jsr LoadJiffyClock
.endif
                lda ClkHourTens
                sta HourTens
                lda ClkHourDigits
                sta HourDigits
                lda ClkMinTens
                sta MinTens
                lda ClkMinDigits
                sta MinDigits
                lda ClkSecTens
                sta SecTens
                lda ClkSecDigits
                sta SecDigits
                lda #0
                sta PmFlag              ; It's AM unless we find otherwise

                ; We don't want 24-hour time, so fix it up if needed
                
                lda HourTens            
                ldx HourDigits

                cmp #'0'                ; Check if TENS digit is 0 
                bne @tensnotzero

                cpx #'0'                ; Check if ONES digit is 0. If so, it's
                bne LoadSeconds         ; actually 12.

                lda #'1'
                sta HourTens
                lda #'2'
                sta HourDigits

                jmp LoadSeconds

@tensnotzero:   cmp #'2'                ; Check if TENS digits is 2
                beq TwoInTens           

                cpx #'2'                ; If the ONES digit is < 2, we're done
                bcc LoadSeconds

                lda #1                  ; Hours >= 12, so it's PM
                sta PmFlag

                cpx #'3'                ; If the ONES digit is >= 3, deduct 12
                bcc LoadSeconds

                dec HourTens            ; But otherwise we back up 12 hours
                dec HourDigits
                dec HourDigits

LoadSeconds:    ldx SecTens
                ldy SecDigits
                jsr GetCharsValue       ; Convert second digits to value and set
                jsr SetSeconds          ;   them.

                rts
    
TwoInTens:      dec HourTens            ; If it's 2X:XX we go back 12 hours
                dec HourTens            ; Tens digit goes to zero, we give those
                lda HourDigits          ;   20 hours to the hours digit.  So by
                clc                     ;   adding 20 and then going back 12 from
                adc #8                  ;   there, it's the same as adding 8 to
                                        ;   hours digit while clearing the tens.

                cmp #'9'+1              ; If we have a digits value <= 9, then
                bcc @donedigits         ;   we're done here. Otherwise,
                sbc #10                 ;   subtract 10 from the digits
                inc HourTens            ;   and increase the tens value to 1
 
@donedigits:    sta HourDigits          ; We're good to store our hour digits
                lda #1
                sta PmFlag              ; It's definitely PM
                jmp LoadSeconds


;-----------------------------------------------------------------------------------
; LoadJiffyClock - Sets the clock structure fields to time in jiffy clock
;-----------------------------------------------------------------------------------

LoadJiffyClock:
                sei                     ; Load jiffy clock with interrupts disabled.
                lda JIFFY_TIMER         ;   We put the low byte in the result variable
                ldx JIFFY_TIMER-1       ;   zptmp, and the two high bytes in the two 
                ldy JIFFY_TIMER-2       ;   lowest bytes of the remainder. Together with
                cli                     ;   the initial rotate left below, this sets us 
                sta zptmp               ;   up for the most efficient division to get the
                stx remainder           ;   hour value out of the jiffy clock.
                sty remainder+1

                lda #0                  ; Clear remainder high byte
                sta remainder+2
                
                ; Extract hour from jiffy clock. We need 3 remainder bytes and one result
                ;   byte because the divisor is 3 bytes, and the result < 256
                ldx #3

@hhrol:         rol zptmp               ; We rotate the result and remainder left 3 bits. 
                rol remainder           ;   We do this knowing that the result can be a
                rol remainder+1         ;   maximum of 5 bits long (max hour value is 23
                rol remainder+2         ;   or 10111 in binary).
                
                dex
                bne @hhrol
                
                ldx #5                  ; We will perform a 5 step long division
	
@hhdiv:         rol zptmp               ; Rotate result and remainder left
                rol remainder
                rol remainder+1
                rol remainder+2
                
                sec                     ; Subtract the number of jiffies in an hour from the
                lda remainder           ;   current value in the remainder. That number is 
                sbc #$c0                ;   216,000 or 34bc0 in hex. 
                tay 
                lda remainder+1
                sbc #$4b
                sta zptmpB
                lda remainder+2
                sbc #$03
                
                bcc @hhignore           ; If carry was cleared, subtract wasn't possible
                
                sta remainder+2         ; Subtract was possible, so save the remaining value 
                lda zptmpB              ;   of the remainder in memory.
                sta remainder+1
                tya
                sta remainder
	
@hhignore:      dex                     ; Continue if we have more division steps to take
                bne @hhdiv
                
                rol zptmp               ; Don't forget to shift the last bit into the result
                
                ldx zptmp
                jsr GetDigitChars       ; Split the digits of the calculated hour value and
                stx ClkHourTens         ;   store them in the appropriate fields.
                sty ClkHourDigits

                lda remainder           ; Bump the low byte of the remainder in the result
                sta zptmp               ;   variable.

                ; Extract minutes from jiffy clock. We need 2 remainder bytes and one result
                ;   byte because the divisor is 2 bytes, and the result < 256
                rol zptmp               ; Rotate left by two bits to set things up for 
                rol remainder+1         ;   the calculation of the minutes. This time, the
                rol remainder+2         ;   result can be a maximum of 6 bits long (max 
                rol zptmp               ;   minute value is 59, or 111011 in binary).
                rol remainder+1
                rol remainder+2
                
                ldx #6                  ; Perform a 6 step long division
	
@mmdiv:         rol zptmp               ; Rotate result and remainder left
                rol remainder+1
                rol remainder+2
                
                sec                     ; Subtract the number of jiffies in a minute from
                lda remainder+1         ;   the current value in the remainder.
                sbc #<MINUTE_JIFFIES
                tay
                lda remainder+2
                sbc #>MINUTE_JIFFIES
                
                bcc @mmignore           ; If carry was cleared, subtract wasn't possible
                
                sta remainder+2         ; Subtract was possible, so save the remaining
                tya                     ;   value of the remainder in memory.
                sta remainder+1
	
@mmignore:	    dex                     ; Continue if we have more division steps to take
                bne @mmdiv
                
                rol zptmp               ; Don't forget to shift the last bit into the result
	
                ldx zptmp
                jsr GetDigitChars       ; Split and store the digits of the calculated
                stx ClkMinTens          ;   minute value.
                sty ClkMinDigits

                lda remainder+1         ; Put the low byte of the remainder in the result 
                sta zptmp               ;   variable.

                ; Extract seconds from jiffy clock. We need one remainder byte and one result
                ;   byte because divisor and result are both < 256
                rol zptmp               ; Like before, rotate left by two bits. Like with 
                rol remainder+2         ;   minutes, the maximum value of seconds is 59.
                rol zptmp
                rol remainder+2

                ldx #6                  ; 6 step long division, like before.
	
@secdiv:        rol zptmp               ; The below is a pretty straightforward long  
                rol remainder+2         ;   division of a two-byte value by 60.
                
                sec
                lda remainder+2
                sbc #SECOND_JIFFIES
                
                bcc @secignore
                
                sta remainder+2
	
@secignore:	    dex
                bne @secdiv
                
                rol zptmp
	
                ldx zptmp
                jsr GetDigitChars       ; Split and store the digits of the calculated  
                stx ClkSecTens          ;   second value.
                sty ClkSecDigits

                rts


;-----------------------------------------------------------------------------------
; GetDigitChars - Calculate the tens and digits characters of the value in X
;-----------------------------------------------------------------------------------
;       IN  X:  value to split and convert 
;       OUT X:  tens character
;       OUT Y:  digit character
;-----------------------------------------------------------------------------------

GetDigitChars:
                txa
                ldx #0
                
                sec
	
@tensloop:      sbc #10                 ; Subtract 10 until we dive below zero. Every
                bcc @belowzero          ;   time we stay above zero, we increase X.
                
                inx
                bcs @tensloop
	
@belowzero:     adc #'0'+10             ; Calculate digit character and put it in Y
                tay
                
                txa                     ; Pull tens out of X and calculate tens character
                clc
                adc #'0'
                tax
                
                rts

;-----------------------------------------------------------------------------------
; UpdateJiffyClock - Sets the jiffy clock to the time we say it is
;-----------------------------------------------------------------------------------

UpdateJiffyClock:
                lda #0                  ; Clear out the remainder buffer. We'll 
                sta remainder           ;   reuse that buffer for this.
                sta remainder+1
                sta remainder+2

                ldx HourTens
                ldy HourDigits
                jsr GetCharsValue       ; Convert hours characters to value

                lda #0                  ; Check the PM flag
                cmp PmFlag
                bne @pm

                cpx #12                 ; If it's 12 AM, skip adding hours 
                beq @addminutes
                bne @addhours

@pm:            cpx #12                 ; Add 12 hours unless it's 12 PM
                beq @addhours           
                
                txa
                clc
                adc #12
                tax

@addhours:      clc
@hhloop:        lda #$c0                ; Perform a three-byte add. We're adding the number
                adc remainder           ;   of jiffies per hour, which is 216,000 or 34bc0
                sta remainder           ;   in hex.
                lda #$4b
                adc remainder+1
                sta remainder+1
                lda #$03
                adc remainder+2
                sta remainder+2

                dex                     ; Decrease hour count and continue if necessary
                bne @hhloop

@addminutes:    ldx MinTens
                ldy MinDigits
                jsr GetCharsValue       ; Convert minute characters to value

                cpx #0                  ; No minutes? Skip adding.
                beq @addjiffy

                clc
@mmloop:        lda #<MINUTE_JIFFIES    ; Perform a two-byte add and extend for carry
                adc remainder
                sta remainder
                lda #>MINUTE_JIFFIES
                adc remainder+1
                sta remainder+1
                lda #0
                adc remainder+2
                sta remainder+2

                dex                     ; Decrease minute count and continue if necessary
                bne @mmloop

@addjiffy:      lda remainder           ; Check if we have anything to add to current
                ora remainder+1         ;   jiffy. This is true unless it is midnight.
                ora remainder+2

                beq @done

                lda remainder           ; Load hour and minute jiffies in registers, so
                ldx remainder+1         ;   we can keep the interrupts disabled for the
                ldy remainder+2         ;   shortest duration possible.

                clc

                sei                     ; Add the seconds and change in the jiffy timer
                adc JIFFY_TIMER         ;   to what we calculated and store the result
                sta JIFFY_TIMER         ;   as the new value of the jiffy timer.
                txa
                adc JIFFY_TIMER-1
                sta JIFFY_TIMER-1
                tya
                adc JIFFY_TIMER-2
                sta JIFFY_TIMER-2
                cli

@done:          rts

;-----------------------------------------------------------------------------------
;  GetCharsValue - Convert tens and digits character to combined value
;-----------------------------------------------------------------------------------
;       IN  X:  tens digit 
;       IN  Y:  digit character
;       OUT X:  value that characters represent
;-----------------------------------------------------------------------------------

GetCharsValue:
                sec

                txa                     ; Take '0' off X
                sbc #'0'
                tax

                tya                     ; Take '0' off Y
                sbc #'0'

                cpx #0                  ; If tens is 0, we're done
                beq @done

                clc
@loop:          adc #10                 ; Add tens to value
                dex
                bne @loop

@done:          tax                     ; We return in X
                rts


;-----------------------------------------------------------------------------------
; UpdateClock - Updates the clock (minutes), if a minute has passed 
;-----------------------------------------------------------------------------------
UpdateClock:
                ; The numbers between brackets are the machine cycles per instruction
                sei                     ; Load jiffy timer values with interrupts   (2)
                ldy JIFFY_TIMER-2       ;   disabled. Weirdly, the three bytes of   (3)
                ldx JIFFY_TIMER-1       ;   that value are stored big-endian.       (3)
                lda JIFFY_TIMER         ;                                           (3)
                cli                     ;                                           (2)

                sec                     ; Subtract the number of jiffies per minute (2)
                sbc #<MINUTE_JIFFIES    ;   from the jiffy timer value, low byte    (2) 
                sta zptmp               ;   first. Store A in temp, as we need A to (3)
                txa                     ;   subtract the high byte of the number of (2)
                sbc #>MINUTE_JIFFIES    ;   jiffies per minute from the middle      (2)
                tax                     ;   jiffy byte. We put that back in X, put  (2)
                tya                     ;   the highest jiffy value byte into A     (2)
                sbc #0                  ;   and complete the 3-byte subtract.       (2)

                bcc @noupdate           ; If carry is clear, we're done.            (2)

                tay                     ; Put the highest jiffy value byte after    (2)
                lda zptmp               ;   the subtract back in Y, and load the    (3)
                                        ;   temp value we saved earlier into A.

                sei                     ; Save updated jiffy timer values with      (2)
                sty JIFFY_TIMER-2       ;   interrupts disabled.                    (3)
                stx JIFFY_TIMER-1       ;                                           (3)
                sta JIFFY_TIMER         ;                                           (3)
                cli                     ;                                           (2)
                ;                                                                 +----
                ; Estimated jiffy timer drift in μs per applied clock update:       50

                jmp IncrementMinute     ; Increment the minute count

@noupdate:      rts


;----------------------------------------------------------------------------
; SendCommand
;----------------------------------------------------------------------------
; Sends a command to an IEEE device
;----------------------------------------------------------------------------

CommandText:     .literal "T-RI",0      ; Command to read RTC in the petSD+

SendCommand:    stx zptmpC
                sty zptmpC+1

	            lda #DEVICE_NUM         ; Device 8 or 9, etc
	            sta DN
	            lda #$6f			    ; DATA SA 15 (Must have $#60 or'd in)
	            sta SA
	            jsr LISTN   		    ; LISTEN
	            lda SA
	            jsr SECND       		; send secondary address
                ldy #0
:               lda (zptmpC), y
                beq @done
	            jsr CIOUT           	; send char to IEEE
                iny
                bne :-

@done:
            	jsr UNLSN               ; Unlisten
                rts

;----------------------------------------------------------------------------
; GetDeviceStatus
;----------------------------------------------------------------------------
; Reads the response back from the device.   In our case the device is the
; petSD+ and we've sent it a "t-ti" command to read the clock.  The clock 
; comes back in the following fixed format:
;
; 2017-01-09t18:20:54 mon
; 0123456789012345678
;----------------------------------------------------------------------------

GetDeviceStatus:
	            lda #DEVICE_NUM
	            sta DN
	            jsr TALK    			; TALK
	            lda #$6f			    ; DATA SA 15
	            sta SA
	            jsr SECND               ; send secondary address
                ldy #$00
:                
                phy
	            jsr ACPTR       	    ; read byte from IEEE bus
                ply
	            cmp #CR				    ; last byte = CR?
	            beq @done
	            sta DevResponse, y
                iny
	            jmp :-		            ; branch always
@done:          lda #$00
                sta DevResponse, y      ; null terminate the buffer instead of CR
	            jsr UNTLK   		    ; UNTALK
	            rts

;-----------------------------------------------------------------------------------
; Increment/Decrement Hour/Minute
;
; Allows the user to step the hours or minutes up and down and to have the 00 roll
; under to 59, allows 59 to roll over to 00, and so on.  Handles the 12:59 -> 1:00
; overflow and underflow properly.
;-----------------------------------------------------------------------------------

IncrementMinute:
                inc MinDigits
                lda #'9'+1
                cmp MinDigits
                bne doneHour
                lda #'0'
                sta MinDigits
                inc MinTens
                lda #'5'+1
                cmp MinTens
                bne doneHour
                lda #'0'
                sta MinTens
                ; fall through to increment hour

IncrementHour:
                ldx #'1'                ; We put 1 in X because we need it for quite a few
                                        ;   things down here
                cpx HourDigits          ; Check if it's the 11th hour. If so, we're switching 
                bne @noteleven          ;   from AM to PM or the other way around.
                cpx HourTens
                bne @noteleven

                lda PmFlag              ; Load PM flag, flip bit, save
                eor #1
                sta PmFlag              

@noteleven:     inc HourDigits			; If the hour hit 9, must be 09, so set hour to 10
                lda #'9'+1
                cmp HourDigits
                bne notNineHour
            
                lda #'0'
                sta HourDigits
                stx HourTens            ; X still holds 1
                rts

notNineHour:	lda #'2'+1				; If it's past not 2 (ie: possible 12 hour) then skip
                cmp HourDigits				
                bne doneHour			; Last digit was 2 but first digit not 1 so go to 3
                
                cpx HourTens            ; X still holds 1
                bne doneHour

                lda #'0'				; Roll from 12 to 01
                sta HourTens
                stx HourDigits          ; X still holds 1
doneHour:		rts

DecrementMinute:
                dec MinDigits
                lda #'0'-1
                cmp MinDigits
                bne doneDec
                lda #'9'
                sta MinDigits
                dec MinTens
                lda #'0'-1
                cmp MinTens
                bne doneDec
                lda #'5'
                sta MinTens
                ; fall through to decrement Hour

DecrementHour:
                dec HourDigits
                lda #'0'-1			    ; If we've gone under zero, must have been 10 so go to 09
                cmp HourDigits
                bne notsubzero
                lda #'9'
                sta HourDigits
                lda #'0'
                sta HourTens

                rts

notsubzero:		lda #'1'-1			    ; If we're going under 1 
                cmp HourDigits
                bne @chkeleven
                lda #'0'
                cmp HourTens
                bne @chkeleven
                lda #'1'
                sta HourTens
                lda #'2'
                sta HourDigits

                rts

@chkeleven:     lda #'1'                ; If it's now the 11th hour, we switched from PM to AM or
                cmp HourDigits          ;   the other way around.
                bne doneDec
                cmp HourTens
                bne doneDec

                lda PmFlag              ; Load PM flag, flip bit, save
                eor #1
                sta PmFlag              

doneDec:		rts
        
;-----------------------------------------------------------------------------------
; DrawClockXY	- Draws the current clock at the specified X/Y location on screen
;-----------------------------------------------------------------------------------
;			X:	Clock X position on screen
;			Y:	Clock Y position on screen
;-----------------------------------------------------------------------------------

DrawClockXY:	stx ClockX
                sty ClockY
                jsr ClearScreen

                ; If there is no tens digit to the current hour, move the clock 
                ; left by half a digit onscreen to center it, and skip rendering
                ; the hour tens digit at all.  This could put the clock left edge at
                ; -4 but it still works just fine because that's added to the digit
                ; pos, which is never less than 8 anyway, so you're at 4 minimumn.

                lda HourTens
                cmp #'0'
                bne @notZeroHour
                lda ClockX			; Go left 4 columns (half a big char) to center single-digit hour
                sec
                sbc #4
                sta ClockX
                lda HourTens
                    
@notZeroHour:	clc					; Colon	or dot - We draw it first so other characters can overlap it
                lda #15
                adc ClockX
                tax
                lda #0
                clc
                adc ClockY
                tay

                lda #0
                cmp ShowAM          ; Show colon if "show AM" setting is off, or it's PM
                beq @showcolon
                cmp PmFlag
                bne @showcolon
                lda #'.'            ; "Show AM" is on in the AM, so a dot is what we draw
                jmp @drawchar
@showcolon:     lda #':'
@drawchar:      jsr DrawBigChar

                clc					; First digit of minutes
                lda #21
                adc ClockX
                tax
                lda #0
                clc
                adc ClockY
                tay
                lda MinTens
                jsr DrawBigChar

                clc					; Second Hour Digit
                lda #8
                adc ClockX
                tax
                lda #0
                clc
                adc ClockY
                tay
                lda HourDigits
                jsr DrawBigChar

                clc					; First Hour Digit
                lda #0
                adc ClockX
                tax
                lda #0
                clc
                adc ClockY
                tay
                lda HourTens
                cmp #'0'
                beq @skipHourTens
                jsr DrawBigChar
@skipHourTens:
                clc					; 2nd digit of minutes
                lda #29
                adc ClockX
                tax
                lda #0
                clc
                adc ClockY
                tay
                lda MinDigits
                jsr DrawBigChar

                rts

;-----------------------------------------------------------------------------------
; DrawBigChar - Draws a given big character at the given X/Y positon
;-----------------------------------------------------------------------------------
;			A:	Character to print
;			X:  X pos on screen
;			Y:  Y pos on screen
;-----------------------------------------------------------------------------------

DrawBigChar:	pha						; Save the A for later, it's the character 
                jsr GetCursorAddr		; Get the screen location based on the X/Y coord
                stx zptmp
                sty zptmp+1
                pla

                jsr GetCharTbl			; Find out the character block memory address
                stx zptmpB				;   for the character in A
                sty zptmpB+1

                lda #7					; Now do all 7 rows of the character definition
                sta bitcount

                ldx #0
@byteloop:		ldy #0

                lda #%10000000			; Bitmask that walks right (10000000, 01000000, etc)
                sta bitmask

@bitloop:		lda (zptmpB,x)			; x must be zero, we want indirect through zptmpB
                and bitmask				; If this bit is set in the character data, draw a block
                beq @prtblank			; If not set, skip and draw nothing
                lda #160
                sta (zptmp), y
.if COLUMNS=80
                iny                     ; We have 80 columns, so draw 2 blocks instead of one
                sta (zptmp), y          
.endif
                bne @doneprt
            
@prtblank:		lda #' '
                sta (zptmp), y
.if COLUMNS=80
                iny                     ; We have 80 columns, so draw 2 blanks instead of one
                sta (zptmp), y
.endif

@doneprt:		lsr bitmask
                iny
                cpy #COLUMNS/5			; 8 bits to do, amounting to 8 or 16 characters
                bne @bitloop
                
                inc zptmpB
                bne @nocarry
                inc zptmpB+1
@nocarry:		clc						; Advance the draw point to the next screen line
                lda zptmp
                adc #COLUMNS
                sta zptmp
                lda zptmp+1
                adc #0
                sta zptmp+1

                dec bitcount			; Next character definition row, until all done
                bne @byteloop
                rts

;-----------------------------------------------------------------------------------
; GetCursorAddr - Returns address of X/Y position on screen
;-----------------------------------------------------------------------------------
;		IN  X:	X pos
;       IN  Y:  Y pos
;       OUT X:  lsb of address
;       OUT Y:  msb of address
;-----------------------------------------------------------------------------------

GetCursorAddr:  stx temp
.if COLUMNS=80
                asl temp                ; We have 80 columns, so double X
.endif
                ldx #COLUMNS
                jsr Multiply			; Result of Y*COLUMNS in AY
                sta resultLo
                sty resultHi
                lda resultLo
                clc
                adc #<SCREEN_MEM
                bcc nocarry
                inc resultHi
                clc
nocarry:		adc temp
                sta resultLo
                lda resultHi
                adc #>SCREEN_MEM
                sta resultHi
                ldx resultLo
                ldy resultHi
                rts

;-----------------------------------------------------------------------------------
; Multiply		Multiplies X * Y == ResultLo/ResultHi
;-----------------------------------------------------------------------------------
;				X		8 bit value in
;				Y		8 bit value in
;-----------------------------------------------------------------------------------

Multiply:
                stx resultLo
                sty MultiplyTemp
                lda #$00
                tay
                sty resultHi
                beq enterLoop
doAdd:			clc
                adc resultLo
                tax
                tya
                adc resultHi
                tay
                txa
loop:			asl resultLo
                rol	resultHi
enterLoop:		lsr MultiplyTemp
                bcs doAdd
                bne loop
                rts

;-----------------------------------------------------------------------------------
; ClearScreen
;-----------------------------------------------------------------------------------
;			X:	lsb of address of null-terminated string
;           A:  msb of address
;-----------------------------------------------------------------------------------

ClearScreen:	jmp CLRSCR

;-----------------------------------------------------------------------------------
; WriteLine - Writes a line of text to the screen using CHROUT ($FFD2)
;-----------------------------------------------------------------------------------
;			Y:	MSB of address of null-terminated string
;           A:  LSB
;-----------------------------------------------------------------------------------

WriteLine:		sta zptmp
                sty zptmp+1
                ldy #0
@loop:			lda (zptmp),y
                beq done
                jsr CHROUT
                iny
                bne @loop
done: 			rts

;-----------------------------------------------------------------------------------
; RepeatChar - Writes a character A to the output X times
;-----------------------------------------------------------------------------------
;			A:	Character to write
;           X:  Number of times to repeat it
;-----------------------------------------------------------------------------------
            
RepeatChar:		jsr CHROUT
                dex
                bne RepeatChar
                rts
.if DEBUG
; During development we output the LOAD statement after running to make the 
; code-test-debug cycle go a little easier - less typing

loadstr:		.literal "LOAD ", 34,"PETCLOCK.PRG",34,", 9",13,0
hello:			.literal "STARTING PETCLOCK...", 0

.endif

;-----------------------------------------------------------------------------------
; GetCharTbl - Returns the address of the character block table for whatever petscii
;              character is specified in the accumualtor
;-----------------------------------------------------------------------------------
;			A:	Character to look up
;       OUT X:  lsb of character map entry
;       OUT Y:  msb of character map entry
;-----------------------------------------------------------------------------------


GetCharTbl:		sta tempchar
                ldx #<CharTable
                stx zptmpC
                ldx #>CharTable
                stx zptmpC+1
                ldy #0
@scanloop:		lda (zptmpC),y
                beq FoundChar				; Hit the null terminator, return the zeros after it
                cmp tempchar
                beq FoundChar				; Hit the matching char, return the block address
                iny
                iny
                iny
                bne @scanloop				; Nothing found, keep scanning

FoundChar:		iny
                lda (zptmpC),y				; Low byte of character entry in X
                tax
                iny
                lda (zptmpC),y				; High byte of character entry in Y
                tay
                rts
CharTable:
                .literal ":"
                .word  CharColon
                .literal "."
                .word  CharLowDot
                .literal "0"
                .word  Char0
                .literal "1"
                .word  Char1
                .literal "2"
                .word  Char2
                .literal "3"
                .word  Char3
                .literal "4"
                .word  Char4
                .literal "5"
                .word  Char5
                .literal "6"
                .word  Char6
                .literal "7"
                .word  Char7
                .literal "8"
                .word  Char8
                .literal "9"
                .word  Char9
                
                .literal 0
                .word   0

Char0:
                .byte   %01111111
                .byte   %01100011
                .byte   %01100011
                .byte   %01100011
                .byte   %01100011
                .byte   %01100011
                .byte   %01111111
Char1:			
                .byte	%00001100
                .byte	%00011100
                .byte	%00001100
                .byte	%00001100
                .byte	%00001100
                .byte	%00001100
                .byte	%00111111
Char2:
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%01111111
                .byte	%01100000
                .byte	%01100000
                .byte	%01111111
Char3:
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%00011111
                .byte	%00000011
                .byte	%00000011
                .byte	%01111111
Char4:
                .byte	%01100011
                .byte	%01100011
                .byte   %01100011
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
Char5:
                .byte	%01111111
                .byte	%01100000
                .byte	%01100000
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%01111111
Char6:
                .byte	%01111111
                .byte	%01100000
                .byte	%01100000
                .byte	%01111111
                .byte	%01100011
                .byte	%01100011
                .byte	%01111111
Char7:
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
Char8:
                .byte	%01111111
                .byte	%01100011
                .byte	%01100011
                .byte	%01111111
                .byte	%01100011
                .byte	%01100011
                .byte	%01111111
Char9:				
                .byte	%01111111
                .byte	%01100011
                .byte	%01100011
                .byte	%01111111
                .byte	%00000011
                .byte	%00000011
                .byte	%00000011
CharColon:
                .byte	%00000000
                .byte	%00011000
                .byte	%00011000
                .byte	%00000000
                .byte	%00011000
                .byte	%00011000
                .byte	%00000000
CharLowDot:
                .byte	%00000000
                .byte	%00000000
                .byte	%00000000
                .byte	%00000000
                .byte	%00011000
                .byte	%00011000
                .byte	%00000000

.if COLUMNS=80
Instructions:
                .literal "                                                                                "
                .literal "                                                                                "
                .literal "                             Press RUN/STOP to exit                            ", $00

AMOnMessage:
                .literal "                                                                                "
                .literal "                                                                                "
                .literal "                                  AM: show dot                                 ", $00

AMOffMessage:
                .literal "                                                                                "
                .literal "                                                                                "
                .literal "                                  AM: show colon                               ", $00
.else
Instructions:
                .literal "                                        "
                .literal "                                        "
                .literal "        press run/stop to exit         ", $00

AMOnMessage:
                .literal "                                        "
                .literal "                                        "
                .literal "            am: show dot               ", $00

AMOffMessage:
                .literal "                                        "
                .literal "                                        "
                .literal "            am: show colon             ", $00
.endif

dirname:        .literal "$",0
