RxCurByte:      .res 1      ; Current byte being received
RxBitNum:       .res 1      ; Rx data bit #
RxBufWritePtr:  .res 1      ; Write pointer
RxBufReadPtr:   .res 1      ; Read pointer

TxCurByte:      .res 1      ; Current byte being transmitted
TxBitNum:       .res 1      ; Tx data bit #
TxState:        .res 1      ; Next Transmit state
TxNextByte:     .res 1      ; Next byte to transmit
TxNewFlag:      .res 1      ; Indicates to start sending a byte

KbdPollIntrvl:  .res 1      ; KBD Polling interval for baud
KbdPollCnt:     .res 1      ; Polling interval counter
KbdByte:        .res 1      ; (Last) byte read from keyboard
KeyCol:         .res 1      ; Key scan column
KeyRow:         .res 1      ; Key row that is being scanned
KeyRowFound:    .res 1      ; Key row in which pressed key was found
KeyBitsFound:   .res 1      ; Bits of key that was found depressed
KeyOffset:      .res 1      ; Keyboard matrix offset for shift
KbdTemp:        .res 1      ; Keyboard scanning temp, to allow BIT instruction
KbdNewFlag:	    .res 1      ; Indicate if new key was pressed
ShiftFlag:      .res 1      ; Indicator if Shift was pressed
CtrlFlag:       .res 1      ; Indicator if Ctrl was pressed

IrqBasicLo:     .res 1      ; Hardware interrupt lo byte for BASIC
IrqBasicHi:     .res 1      ; Hardware interrupt hi byte for BASIC
