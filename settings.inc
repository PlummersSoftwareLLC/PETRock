.ifdef COL80
    .if COL80
BSNSS_KBD       = 1       ; If 80 columns is flagged via command-line, assume we have a business keyboard
    .endif
.else
COL80           = 0       ; Set to 1 if you use a PET with an 80 column display. Default is 0 (40 columns)
.endif

.ifndef BSNSS_KBD
BSNSS_KBD       = 0       ; Set to 1 if you use a PET with a business keyboard. Default is 0 (graphical)
.endif

DEBUG           = 0       ; Enable code that only is included for debug builds
TIMING          = 0       ; Change border color to show drawing progress. Only supported on the C64
SERIAL          = 1       ; Enable reading serial input from user port
SENDSTAR        = 0       ; Send a * after each screen redraw on the PETs. Will create clicking noises with piezo.

; --- Uncomment one of the following two lines to select the build target.
;     Alternatively, either symbol can be defined using the ca65/cl65 command line.

;C64             = 1      ; Set to 1 to build for C64. Either this or PET must be 1
;PET             = 1      ; Set to 1 to build for PET. Either this or C64 must be 1