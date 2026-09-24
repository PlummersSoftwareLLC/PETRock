;-----------------------------------------------------------------------------------
; Spectrum Analyzer Display for C64 and PET
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
; Serial driver config
;-----------------------------------------------------------------------------------

SER_BAUD        = BAUD2400      ; BAUD4800, BAUD2400, BAUD1200 or BAUD300

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

RIDBE           = $029b
RIDBS           = $029c
RODBS           = $029d
RODBE           = $029e
ENABL           = $02a1

;-----------------------------------------------------------------------------------
; Routine vectors
;-----------------------------------------------------------------------------------

NMISR           = $0318
CKISR           = $031e
BSOSR           = $0326

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
; Bit timing, in CPU cycles, by baud rate and machine clock
;
; strtbit is the timer B delay from the start bit NMI to the first data bit sample,
; and fullbit the timer latch for one bit (the bit time minus one).
;
; The NTSC values for 2400 baud and lower are from the Transactor article. The others
; are calculated for the clock of the machine, PAL (985248 Hz) or NTSC (1022727 Hz):
; - fullbit is the bit time minus one, so samples don't drift across a byte.
; - strtbit makes each sample fall about 45 cycles before the middle of its bit.
;   It's about 1.5 bit times minus 154 cycles of NMI latency (before the timer
;   starts, and from underflow to reading the pin) minus those 45 cycles. Sampling
;   early leaves room for the ways samples get delayed, like VIC bad lines (up to
;   ~40 cycles each, for the start bit NMI and for the sample itself).
;-----------------------------------------------------------------------------------

strtbit:        ;  4800  2400  1200   300 baud
                .word   121,  459, 1090, 4915   ; NTSC
                .word   109,  418, 1033, 4727   ; PAL

fullbit:        ;  4800  2400  1200   300 baud
                .word   212,  421,  845, 3410   ; NTSC
                .word   204,  410,  820, 3283   ; PAL

PAL_TIMING      = 8             ; Offset of the PAL values in the tables

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
; Carry is clear if a character was fetched, and set if no data is available.
;
; We read the buffer directly, instead of through CHKIN, GETIN and CLRCH. That's a
; lot quicker, and the NMI handler keeps reception enabled after every byte anyway.
;-----------------------------------------------------------------------------------

GetSerialChar:
        ldy RIDBS
        cpy RIDBE       ; buffer empty?
        beq @nodata     ; yes
        lda (RIBUF),y   ; no, fetch character
        inc RIDBS
        clc
        rts

@nodata:
        sec
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
; StartSerial: Start serial communication. OpenSerial must have been called already.
;-----------------------------------------------------------------------------------

StartSerial     = ser_enable

;-----------------------------------------------------------------------------------
; CloseSerial: Teardown serial comms. We wait for transmission to finish and turn
; off the serial NMIs. Then we restore the KERNAL vectors that ser_setup changed, as
; they point into our code. Finally, we close the RS-232 file, which also returns the
; buffer memory that OPEN took from the top of memory.
;-----------------------------------------------------------------------------------

CloseSerial:
        jsr ser_disable

        lda ser_oldnmi
        sta NMISR
        lda ser_oldnmi+1
        sta NMISR+1
        lda ser_oldchkin
        sta CKISR
        lda ser_oldchkin+1
        sta CKISR+1
        lda ser_oldbsout
        sta BSOSR
        lda ser_oldbsout+1
        sta BSOSR+1

        lda #RS232_DEV
        jmp CLOSE

;-----------------------------------------------------------------------------------
; GetKeyboardChar: Get a character from the keyboard. In this case, just use GETIN
;-----------------------------------------------------------------------------------

GetKeyboardChar = GETIN

;-----------------------------------------------------------------------------------

ser_setup:
        ldy #SER_BAUD   ; set up bit timing for our baud rate
        lda PALFLAG     ;   and the machine's clock
        beq :+
        ldy #SER_BAUD + PAL_TIMING
:       lda strtbit,y   ; values used by the nmi handler
        sta ser_strtlo
        lda strtbit+1,y
        sta ser_strthi
        lda fullbit,y
        sta ser_fulllo
        lda fullbit+1,y
        sta ser_fullhi

        lda NMISR       ; save the vectors we're about to change
        sta ser_oldnmi
        lda NMISR+1
        sta ser_oldnmi+1
        lda CKISR
        sta ser_oldchkin
        lda CKISR+1
        sta ser_oldchkin+1
        lda BSOSR
        sta ser_oldbsout
        lda BSOSR+1
        sta ser_oldbsout+1

        lda #<ser_nmi64
        ldy #>ser_nmi64
        sta NMISR
        sty NMISR+1
        lda #<ser_nchkin
        ldy #>ser_nchkin
        sta CKISR
        sty CKISR+1
        lda #<ser_nbsout
        ldy #>ser_nbsout
        sta BSOSR
        sty BSOSR+1
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
        lda CIA2_PRA    ; yes, put bit on pin m
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
        
        lda ser_fulllo  ; full tx bit time
        sta CIA2_TA
        lda ser_fullhi
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
        ; The original code derived the bit timing from the KERNAL's transmit
        ; bit time in BAUDOF here. We set it up once, in ser_setup.
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
ser_ret1:
        clc
        ldy XSAV        ; restore registers saved by ser_enable
        lda PTR1
        rts
