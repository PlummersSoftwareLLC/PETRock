;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64
;-----------------------------------------------------------------------------------
; (c) Plummer's Software Ltd, 02/11/2022 Initial commit
;         David Plummer
;         Rutger van Bergen
;-----------------------------------------------------------------------------------
; Serial driver for the C64 using Userport.
;
; Johan Van den Brande, (c) 2014
; 
; Based on George Hug's 'Towards 2400' article in the Transactor Magazine Vol 9 #3.
; (https://archive.org/details/transactor-magazines-v9-i03) 
;
; Modified by Rutger van Bergen for use with the Spectrum Analyzer Display for C64
;-----------------------------------------------------------------------------------

;-----------------------------------------------------------------------------------
; Constants
;-----------------------------------------------------------------------------------

RS232_DEV = 2

;-----------------------------------------------------------------------------------
; Zeropage addresses not defined by C64.inc
;-----------------------------------------------------------------------------------

XSAV            = $97
DFLTN           = $99
DFLTO           = $9a
PTR1            = $9e
PTR2            = $9f
BITCI           = $a8
RIDATA          = $aa
BITTS           = $b4
NXTBIT          = $b5
RODATA          = $b6
RIBUF           = $f7
ROBUF           = $f9

;-----------------------------------------------------------------------------------
; I/O space addresses
;-----------------------------------------------------------------------------------

BAUDOF          = $0299
RIDBE           = $029b
RIDBS           = $029c
RODBS           = $029d
RODBE           = $029e
ENABL           = $02a1

;-----------------------------------------------------------------------------------
; Kernal ROM addresses
;-----------------------------------------------------------------------------------

RSTKEY          = $fe56 
RETURN          = $febc 
OLDOUT          = $f1ca
OLDCHK          = $f21b
FINDFN          = $f30f
SETDEV          = $f31f
NOFILE          = $f701

;-----------------------------------------------------------------------------------
; Read-only words used by code/kernal API routines
;-----------------------------------------------------------------------------------

strtbit:
strt48:         .word 225       ; 225     Made up by Dave after reading Wikipedia
strt24:         .word $01cb     ; 459     From the Transactor article
strt12:         .word $0442     ; 1090    
strt03:         .word $1333     ; 4915    

fullbit:
full48:         .word 208       ; 225     Made up by Dave after reading Wikipedia
full24:         .word $01a5     ; 421     From the Transactor article
full12:         .word $034d     ; 845     not referenced directly, but through Y
full03:         .word $0d52     ; 3410    register indexing

; Control Registers
; 
; 300  - $06  3284 0xCD4   
; 1200 - $08  822  0x336   
; 2400 - $0A  410  0x19a
; 4800 - We lack the technology to predict the next element in this pattern
        
baudrate        = 10           ; chr$(10) == $0A == 2400 baud
databits        = 0             
stopbit         = 0                  

wire            = 0
duplex          = 0
parity          = 0

serial_config:
  .byte baudrate + databits + stopbit
  .byte wire + duplex + parity


;-----------------------------------------------------------------------------------
; OpenSerial: Setup the code to handle RS232 I/O
;-----------------------------------------------------------------------------------

OpenSerial:
        lda #RS232_DEV
        ldx #<serial_config
        ldy #>serial_config
        jsr SETNAM

        lda #RS232_DEV
        tax
        tay
        jsr SETLFS
        jsr OPEN
        
        jsr ser_setup

        rts

;-----------------------------------------------------------------------------------
; GetSerialChar: Will fetch a character from the receive buffer and store it into A.
; If no data is available, SER_ERR_NO_DATA is returned in X/Y.
;-----------------------------------------------------------------------------------


GetSerialChar:   
        ldx #RS232_DEV
        jsr CHKIN
        jsr ser_rshavedata
        beq @nodata
        jsr GetBufferChar
        pha
        jsr CLRCH 
        pla
        ldx #<SER_ERR_OK
        ldy #>SER_ERR_OK 
        rts

@nodata:
        jsr CLRCH 
        lda #$ff
        ldx #<SER_ERR_NO_DATA
        ldy #>SER_ERR_NO_DATA 
        rts

;-----------------------------------------------------------------------------------
; GetBufferChar: This is a minimised call to get the character from the buffer.
; The Kernal code does not allow zero bytes (0x00)... this does.
;-----------------------------------------------------------------------------------
 
GetBufferChar:
        jsr $F14E
        bcc @exit
        jmp $F1B4
@exit:
        clc
        rts

;-----------------------------------------------------------------------------------
; PutSerialChar: Output character in A.
;-----------------------------------------------------------------------------------

PutSerialChar:
        pha
        ldx #RS232_DEV
        jsr CHKOUT
        pla
        jsr BSOUT
        jsr CLRCH
        rts


;-----------------------------------------------------------------------------------
; SerialIoctl: Pass 0 in A to disable serial, 2 to enable
;-----------------------------------------------------------------------------------

SerialIoctl:
      cmp #0
      beq @disable
@enable:
      jsr ser_enable
      sec
      bcs @exit
@disable:
      jsr ser_disable 
@exit:
      rts

;-----------------------------------------------------------------------------------

ser_setup:
        ; set things up for 2400 bps
        
;        lda strt48
;        sta ser_strtlo
;        lda strt48+1
;        sta ser_strthi
;        lda full48
;        sta ser_fulllo
;        lda full48+1
;        sta ser_fullhi

        lda #<ser_nmi64
        ldy #>ser_nmi64
        sta $0318
        sty $0319
        lda #<ser_nchkin
        ldy #>ser_nchkin
        sta $031e
        sty $031f
        lda #<ser_nbsout
        ldy #>ser_nbsout
        sta $0326
        sty $0327
        rts
        
;-----------------------------------------------------------------------------------

ser_nmi64:
        pha             ; new nmi handler
        txa
        pha
        tya
        pha
        cld
        ldx CIA2_TB+1   ; sample timer b hi byte
        lda #$7f        ; disable cia nmi's
        sta CIA2_ICR
        lda CIA2_ICR    ; read/clear flags
        bpl @notcia     ; (restore key)
        cpx CIA2_TB+1   ; tb timeout since timer b sampled?
        ldy CIA2_PRB    ; (sample pin c)
        bcs @mask       ; no
        ora #$02        ; yes, set flag in acc.
        ora CIA2_ICR    ; read/clear flags again
@mask:
        and ENABL       ; mask out non-enabled
        tax             ; these must be serviced
        lsr             ; timer a? (bit 0)
        bcc @ckflag     ; no
        lda CIA2_PRA    ; yes, put but on pin m
        and #$fb
        ora NXTBIT
        sta CIA2_PRA
@ckflag:
        txa
        and #$10        ; *flag nmi (bit 4)
        beq @nmion      ; no

        lda ser_strtlo  ; yes, start-bit to tb                  ; STARTBIT
        sta CIA2_TB
        lda ser_strthi
        sta CIA2_TB+1
        lda #$11        ; start tb counting
        sta CIA2_CRB
        lda #$12        ; *flag nmi off, tb on
        eor ENABL       ; update mask
        sta ENABL
        sta CIA2_ICR    ; enable new config
        lda ser_fulllo  ; change reload latch
        sta CIA2_TB     ;   to full-bit time
        lda ser_fullhi
        sta CIA2_TB+1
        lda #$08        ; # of bits to receive
        sta BITCI
        bne @chktxd     ; branch always
@notcia:
        ldy #$00
        jmp RSTKEY
@nmion:
        lda ENABL       ; re-enable nmi's
        sta CIA2_ICR
        txa
        and #$02        ; timer b? (bit 1)
        beq @chktxd     ; no
        tya             ; yes, get sample of pin c
        lsr
        ror RIDATA      ; rs232 is lsb first
        dec BITCI       ; byte finished?
        bne @txd        ; no
        ldy RIDBE       ; yes, byte to buffer
        lda RIDATA
        sta (RIBUF),y   ; (no overrun test)
        inc RIDBE
        lda #$00        ; stop timer b
        sta CIA2_CRB
        lda #$12        ; tb nmi off, *flag on
@switch:
        ldy #$7f        ; disable nmi's
        sty CIA2_ICR    ; twice
        sty CIA2_ICR
        eor ENABL       ; update mask
        sta ENABL
        sta CIA2_ICR    ; enable new config
@txd:
        txa
        lsr             ; timer a?
@chktxd:
        bcc @exit       ; no
        dec BITTS       ; yes, byte finished?
        bmi @char       ; yes
        lda #$04        ; no, prep next bit
        ror RODATA      ; (fill with stop bits)
        bcs @store
@low:
        lda #$00
@store:
        sta NXTBIT
@exit:
        jmp RETURN      ; restore regs, rti
@char:
        ldy RODBS
        cpy RODBE       ; buffer empty?
        beq @txoff      ; yes
;getbuf
        lda (ROBUF),y   ; no, prep next byte
        inc RODBS
        sta RODATA
        lda #$09        ; # bits to send
        sta BITTS
        bne @low        ; always - do start bit
@txoff:
        ldx #$00        ; stop timer a
        stx CIA2_CRA
        lda #$01        ; disable ta nmi
        bne @switch     ; always
;--------------------------------------
ser_disable:
        pha             ; turns off modem port
@test:
        lda ENABL
        and #$03        ; any current activity?
        bne @test       ; yes, test again
        lda #$10        ; no, disable *flag nmi
        sta CIA2_ICR
        lda #$02
        and ENABL       ; currently receiving?
        bne @test       ; yes, start over
        sta ENABL       ; all off, update mask
        pla
        rts
;--------------------------------------
ser_nbsout:
        pha             ; new bsout
        lda DFLTO
        cmp #RS232_DEV
        bne ser_notmod
        pla
;rsout:
        sta PTR1        ; output to modem
        sty XSAV
ser_point:
        ldy RODBE
        sta (ROBUF),y   ; not official till pointer bumped
        iny
        cpy RODBS       ; buffer full?
        beq ser_fulbuf  ; yes
        sty RODBE       ; no, bump pointer
ser_strtup:
        lda ENABL
        and #$01        ; transmitting now?
        bne ser_ret3    ; yes
        sta NXTBIT      ; no, prep start bit,
        lda #$09
        sta BITTS       ;   # bits to send,
        ldy RODBS
        lda (ROBUF),y
        sta RODATA      ;   and next byte
        inc RODBS
        
        lda BAUDOF      ; full tx bit time to ta
        sta CIA2_TA
        lda BAUDOF+1
        sta CIA2_TA+1
        
        lda #$11        ; start timer a
        sta CIA2_CRA
        lda #$81        ; enable ta nmi
ser_change:
        sta CIA2_ICR    ; nmi clears flag if set
        php             ; save irq status
        sei             ; disable irq's
        ldy #$7f        ; disable nmi's
        sty CIA2_ICR    ; twice
        sty CIA2_ICR
        ora ENABL       ; update mask
        sta ENABL
        sta CIA2_ICR    ; enable new config
        plp             ; restore irq status
ser_ret3:
        clc
        ldy XSAV
        lda PTR1
        rts
ser_fulbuf:
        jsr ser_strtup
        jmp ser_point
ser_notmod:
        pla             ; back to old bsout
        jmp OLDOUT
;--------------------------------------
ser_nchkin:
        jsr FINDFN      ; new chkin
        bne ser_nosuch
        jsr SETDEV
        lda DEVNUM
        cmp #RS232_DEV
        bne ser_back
        sta DFLTN
ser_enable:
        sta PTR1         ; enable rs232 input
        sty XSAV
;baud:
        ; BAUD          BAUDOF+1   (BAUDOF+1) & #6
        ; 2400 == $960     9          9 & 6
        ; 1200 == $4B0     4          4 & 6
        ; 300  == $12C     1          1 & 6 
        
        ; BUGBUG if easy and possible let this work as it used to, even though
        ; I have NFI what it's supposed to be doing.  Let bigger heads prevail.
        ;
        ; lda BAUDOF+1    ; set receive to same
        ; and #$06        ;   baud rate as xmit
        ; tay     
        ; lda strt24,y
        
        ldy #BAUD4800   ; We could allow selection by Y reg here but we only ever need 2400
        lda strtbit,y    ;   so I've coded it to 2400 exclusively for this project
        sta ser_strtlo  ; overwrite values in nmi handler
        lda strtbit+1,y
        sta ser_strthi
        lda fullbit,y
        sta ser_fulllo
        lda fullbit+1,y
        sta ser_fullhi
        
        lda ENABL
        and #$12        ; *flag or tb on?
        bne ser_ret1    ; yes
        sta CIA2_CRB    ; no, stop tb
        lda #$90        ; turn on flag nmi
        jmp ser_change
ser_nosuch:
        jmp NOFILE
ser_back:
        lda DEVNUM
        jmp OLDCHK
;--------------------------------------
; rsget:
        sta PTR1        ; input from modem
        sty PTR2
        ldy RIDBS
        cpy RIDBE       ; buffer empty?
        beq ser_ret2    ; yes
        lda (RIBUF),y   ; no, fetch character
        sta PTR1
        inc RIDBS
ser_ret1:
        clc             ; cc = char in acc.
ser_ret2:
        ldy PTR2
        lda PTR1
;last:
        rts             ; cs = buffer was empty

;----------------------------------------
; A = 0 when no data
; A = 1 when data
ser_rshavedata:
        lda #0
        ldy RIDBS
        cpy RIDBE       ; buffer empty?
        beq @rsempty    ; no
        lda #1
@rsempty:
        rts 
