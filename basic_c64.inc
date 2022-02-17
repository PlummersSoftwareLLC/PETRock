; Commodore 64 BASIC Entry Points

        BASIC_A000       = $A000      ; Basic cold start vector ($E394)
        BASIC_A002       = $A002      ; Basic warm start vector ($E37B)
        BASIC_A004       = $A004      ; Text "cbmbasic"
        BASIC_A00C       = $A00C      ; Addresses of the BASIC-commands -1 (END, FOR, NEXT, ... 35 addresses of 2 byte each)
        BASIC_A052       = $A052      ; Addresses of the BASIC functions (SGN, INT, ABS, ... 23 addresses of 2 byte each)
        BASIC_A080       = $A080      ; Hierarchy-codes and addresses of the operators -1 (10-times 1+2 Bytes)
        BASIC_A09E       = $A09E      ; BASIC key words as string in PETSCII; Bit 7 of the last character is set
        BASIC_A129       = $A129      ; Keywords which have no action addresses - TAB(, TO, SPC(, ...; Bit 7 of the last character is set
        BASIC_A140       = $A140      ; Keywords of the operands + - * etc.; also AND, OR as strings. Bit 7 of the last character is set
        BASIC_A14D       = $A14D      ; Keywords of the functions (SGN, INT, ABS, etc.) where bit 7 of the last character is set
        BASIC_A19E       = $A19E      ; Error messages (TOO MANY FILES, FILE OPEN, ... ); 29 messages where bit 7 of the last character is set
        BASIC_A328       = $A328      ; Pointer-table to the error messages
        BASIC_A364       = $A364      ; Messages of the interpreter (OK, ERROR IN, READY, BREAK)
        BASIC_A38A       = $A38A      ; Routine to search stack for FOR-NEXT and GOSUB
        BASIC_A3B8       = $A3B8      ; Called at BASIC line insertion. Checks, if enough space available. After completion, $A3BF is executed
        BASIC_A3BF       = $A3BF      ; Move bytes routine
        BASIC_A3FB       = $A3FB      ; Check for space on stack
        BASIC_A408       = $A408      ; Array area overflow check
        BASIC_A435       = $A435      ; Output of error message ?OUT OF MEMORY
        BASIC_A437       = $A437      ; Output of an error message, error number in X-register; uses vector in ($0300) to jump to $E38B
        BASIC_A480       = $A480      ; Input waiting loop; uses vector in ($0302) to jump to basic warm start at $A483
        BASIC_A49C       = $A49C      ; Delete or Insert program lines and tokenize them
        BASIC_RELINK     = $A533      ; Re-link BASIC program
        BASIC_INPLINE    = $A560      ; Input of a line via keyboard
        BASIC_TKCRNCH    = $A579      ; Token crunch -> text line to interpreter code; uses vector in ($0304) to get to $A57C
        BASIC_CALCLINE   = $A613      ; Calculate start address of a program line
        BASIC_NEW        = $A642      ; BASIC command NEW
        BASIC_CLR        = $A65E      ; BASIC command CLR
        BASIC_A68E       = $A68E      ; Set program pointer to BASIC-start (loads $7A, $7B with $2B-1, $2C-1)
        BASIC_LIST       = $A69C      ; BASIC command LIST
        BASIC_A717       = $A717      ; Convert BASIC code to clear text; uses vector (0306) to jump to $A71A
        BASIC_FOR        = $A742      ; BASIC-command FOR: Move 18 bytes to the stack 1) Pointer to the next instruction, 2) actual line number, 3) upper loop value, 4) step with (default value = 1), 5) name of the lop variable and 6) FOR-token.
        BASIC_INTLOOP    = $A7AE      ; Interpreter loop, set up next statement for execution
        BASIC_CHKEND     = $A7C4      ; Check for program end
        BASIC_EXECUTE    = $A7E1      ; execute BASIC command; uses vector ($0308) to point to $A7E4
        BASIC_KEYWORD    = $A7ED      ; Executes BASIC keyword
        BASIC_RESTORE    = $A81D      ; BASIC command RESTORE: set data pointer at $41, $42 to the beginning of the actual basic text
        BASIC_ENDSTOP    = $A82C      ; BASIC command STOP (also END and program interruption)
        BASIC_STOP       = $A82F      ; BASIC command STOP
        BASIC_END        = $A831      ; BASIC command END
        BASIC_CONT       = $A857      ; BASIC command CONT
        BASIC_RUN        = $A871      ; BASIC command RUN
        BASIC_GOSUB      = $A883      ; BASIC command GOSUB: Move 5 bytes to the stack. 1) the pointer within CHRGET, 2) the actual line number, 3) token of GOSUB; after that, GOTO ($a8a0) will be called
        BASIC_GOTO       = $A8A0      ; BASIC command GOTO
        BASIC_RETURN     = $A8D2      ; BASIC command RETURN
        BASIC_DATA       = $A8F8      ; BASIC command DATA
        BASIC_A906       = $A906      ; Find offset of the next separator
        BASIC_IF         = $A928      ; BASIC command IF
        BASIC_REM        = $A93B      ; BASIC command REM
        BASIC_ON         = $A94B      ; BASIC command ON
        BASIC_A96B       = $A96B      ; Get decimal number (0...63999, usually a line number) from basic text into $14/$15
        BASIC_LET        = $A9A5      ; BASIC command LET
        BASIC_A9C4       = $A9C4      ; Value assignment of integer
        BASIC_A9D6       = $A9D6      ; Value assignment of float
        BASIC_A9D9       = $A9D9      ; Value assignment of string
        BASIC_A9E3       = $A9E3      ; Assigns system variable TI$
        BASIC_AA1D       = $AA1D      ; Check for digit in string, if so, continue with $AA27
        BASIC_AA27       = $AA27      ; Add PETSCII digit in string to float accumulator; Assumes C=0
        BASIC_AA2C       = $AA2C      ; Value assignment to string variable (LET for strings)
        BASIC_PRINTNO    = $AA80      ; BASIC command PRINT#
        BASIC_CMD        = $AA86      ; BASIC command CMD
        BASIC_AA9A       = $AA9A      ; Part of the PRINT routine: Output string and continue with the handling of PRINT
        BASIC_PRINT      = $AAA0      ; BASIC command PRINT
        BASIC_AAB8       = $AAB8      ; Outputs variable; Numbers will be converted into string first
        BASIC_AACA       = $AACA      ; Append $00 as end indicator of string
        BASIC_AAD7       = $AAD7      ; Outputs a CR/soft hyphenation (#$0D), followed by a line feed/newline (#$0A), if the channel number is 128
        BASIC_AAF8       = $AAF8      ; TAB( (C=1) and SPC( (C=0)
        BASIC_AB1E       = $AB1E      ; Output string: Output string, which is indicated by accu/Y reg, until 0 byte or quote is found
        BASIC_AB3B       = $AB3B      ; Output of cursor right (or space if output not on screen)
        BASIC_AB3F       = $AB3F      ; Output of a space character
        BASIC_AB42       = $AB42      ; Output of cursor right
        BASIC_AB45       = $AB45      ; Output of question mark (before error message)
        BASIC_AB47       = $AB47      ; Output of a PETSCII character, accu must contain PETSCII value
        BASIC_AB4D       = $AB4D      ; Output error messages for read commands (INPUT / GET / READ)
        BASIC_GET        = $AB7B      ; BASIC command GET
        BASIC_INPUTNO    = $ABA5      ; BASIC command INPUT#
        BASIC_INPUT      = $ABBF      ; BASIC command INPUT
        BASIC_ABEA       = $ABEA      ; Get line into buffer
        BASIC_ABF9       = $ABF9      ; Display input prompt
        BASIC_AC06       = $AC06      ; BASIC commands READ, GET and INPUT share this routine and will be distinguished by a flag in $11
        BASIC_AC35       = $AC35      ; Input routine for GET
        BASIC_ACFC       = $ACFC      ; Messages ?EXTRA IGNORED and ?REDO FROM START, both followed by $0D (CR) and end of string $00.
        BASIC_NEXT       = $AD1D      ; BASIC command NEXT
        BASIC_AD61       = $AD61      ; Check for valid loop
        BASIC_AD8A       = $AD8A      ; FRMNUM: Get expression (FRMEVL) and check, if numeric
        BASIC_AD9E       = $AD9E      ; FRMEVL: Analyzes any Basic formula expression and shows syntax errors. Set type flag $0D (Number: $00, string $FF). Sety integer flag $0E (float: $00, integer: $80) puts values in FAC 
        BASIC_AE83       = $AE83      ; EVAL: Get next element of an expression; uses vector ($030A) to jump to $AE86
        BASIC_AEA8       = $AEA8      ; Value for constant PI in 5 bytes float format
        BASIC_AEF1       = $AEF1      ; Evaluates expression within brackets
        BASIC_AEF7       = $AEF7      ; Check for closed bracket ")"
        BASIC_AEFA       = $AEFA      ; Check for open bracket "("
        BASIC_AEFD       = $AEFD      ; Check for comma
        BASIC_AF08       = $AF08      ; Outputs error message ?SYNTAX ERROR and returns to READY state
        BASIC_AF0D       = $AF0D      ; Calculates NOT
        BASIC_AF14       = $AF14      ; Check for reserved variables. Set carry flag, if FAC points to ROM. This indicates the use of one of the reserved variables TI$, TI, ST.
        BASIC_AF28       = $AF28      ; Get variable: Searches the variable list for one of the variables named in $45, $46
        BASIC_AF48       = $AF48      ; Reads clock counter and generate string, which contains TI$
        BASIC_AFA7       = $AFA7      ; Calculate function: Determine type of function and evaluates it
        BASIC_AFB1       = $AFB1      ; String function: check for open bracket, get expression (FRMEVL), checks for commas, get string.
        BASIC_AFD1       = $AFD1      ; Analyze numeric function
        BASIC_AFE6       = $AFE6      ; BASIC commands OR and AND, distinguished by flag $0B (= $FF at OR, $00 at AND).
        BASIC_B016       = $B016      ; Comparison (<, =, > )
        BASIC_B01B       = $B01B      ; Numeric comparison
        BASIC_B02E       = $B02E      ; String comparison
        BASIC_B081       = $B081      ; BASIC command DIM
        BASIC_B08B       = $B08B      ; Check if variable name is valid
        BASIC_B0E7       = $B0E7      ; Searches variable in list, set variable pointer, create new variable, if name not found
        BASIC_B113       = $B113      ; Check for character
        BASIC_B11D       = $B11D      ; Create variable
        BASIC_B194       = $B194      ; Calculate pointer to first element of array
        BASIC_B1A5       = $B1A5      ; Constant -32768 as float (5 bytes)
        BASIC_B1AA       = $B1AA      ; Convert FAC to integer and save it to accu/Y reg
        BASIC_B1B2       = $B1B2      ; Get positive integer from BASIC text
        BASIC_FACINT     = $B1BF      ; Convert FAC to integer
        BASIC_B1D1       = $B1D1      ; Get array variable from BASIC text
        BASIC_B218       = $B218      ; Search for array name in pointer ($45, $46)
        BASIC_B245       = $B245      ; Output error message ?BAD SUBSCRIPT
        BASIC_B248       = $B248      ; Output error message ?ILLEGAL QUANTITY
        BASIC_B24D       = $B24D      ; Output error message ?REDIM\'D ARRAY
        BASIC_B261       = $B261      ; Create array variable
        BASIC_B30E       = $B30E      ; Calculate address of a array element, set pointer ($47)
        BASIC_B34C       = $B34C      ; Calculate distance of given array element to the one which ($47) points to and write the result to X reg/Y reg
        BASIC_B37D       = $B37D      ; BASIC function FRE
        BASIC_INTFLT     = $B391      ; Convert 16-bit integer in accu/Y reg to float
        BASIC_POS        = $B39E      ; BASIC function POS
        BASIC_B3A2       = $B3A2      ; Convert the byte in Y reg to float and return it to FAC
        BASIC_B3A6       = $B3A6      ; Check for direct mode: value $FF in flag $3A indicates direct mode
        BASIC_B3AE       = $B3AE      ; Output error message ?UNDEF\'D FUNCTION
        BASIC_B3B3       = $B3B3      ; BASIC command DEF FN
        BASIC_B3E1       = $B3E1      ; Check syntax of FN
        BASIC_B3F4       = $B3F4      ; BASIC function FN
        BASIC_B465       = $B465      ; BASIC function STR$
        BASIC_B475       = $B475      ; Make space for inserting into string
        BASIC_B487       = $B487      ; Get string, pointer in accu/Y reg
        BASIC_B4CA       = $B4CA      ; Store string pointer in descriptor stack
        BASIC_B4F4       = $B4F4      ; Reserve space for string, length in accu
        BASIC_B526       = $B526      ; Garbage Collection
        BASIC_B606       = $B606      ; Searches in simple variables and arrays for a string which has to be saved by the next Garbage Collection interation.
        BASIC_B63D       = $B63D      ; Concatenates two strings
        BASIC_B67A       = $B67A      ; Move String to reserved area
        BASIC_B6A3       = $B6A3      ; String management FRESTR
        BASIC_B6DB       = $B6DB      ; Remove string pointer from descriptor stack
        BASIC_CHR        = $B6EC      ; BASIC function CHR$
        BASIC_LEFT       = $B700      ; BASIC function LEFT$
        BASIC_RIGHT      = $B72C      ; BASIC function RIGHT$
        BASIC_MID        = $B737      ; BASIC function MID$
        BASIC_B761       = $B761      ; String parameter from stack: Get pointer for string descriptor and write it to $50, $51 and the length to accu (also X-reg)
        BASIC_LEN        = $B77C      ; BASIC function LEN
        BASIC_B782       = $B782      ; Get string parameter (length in Y-reg), switch to numeric
        BASIC_ASC        = $B78B      ; BASIC function ASC
        BASIC_B79B       = $B79B      ; Read and evaluate expression from BASIC text; the 1 byte value is then stored in X-reg and in FAC+4
        BASIC_VAL        = $B7AD      ; BASIC function VAL
        BASIC_B7EB       = $B7EB      ; GETADR and GETBYT: Get 16-bit integer (to $14, $15) and an 8 bit value (to X-reg) - e.g. parameter for WAIT and POKE.
        BASIC_B7F7       = $B7F7      ; Converts FAC in 2-byte integer (scope 0 ... 65535) to $14, $15 and Y-Reg/accu
        BASIC_PEEK       = $B80D      ; BASIC function PEEK
        BASIC_POKE       = $B824      ; BASIC command POKE
        BASIC_WAIT       = $B82D      ; BASIC command WAIT
        BASIC_FACHALF    = $B849      ; FAC = FAC + 0,5; for rounding
        BASIC_SUBFAC     = $B850      ; FAC = CONSTANT - FAC , accu and Y-register are pointing to CONSTANT (low and high byte)
        BASIC_SUBARG     = $B853      ; FAC = ARG - FAC
        BASIC_B862       = $B862      ; Align exponent of FAC and ARG for addition
        BASIC_B867       = $B867      ; FAC = CONSTANT (accu/Y reg) + FAC
        BASIC_ADDARG     = $B86A      ; FAC = ARG + FAC
        BASIC_INVMANT    = $B947      ; Invert mantissa of FAC
        BASIC_B97E       = $B97E      ; Output error message OVERFLOW
        BASIC_B983       = $B983      ; Multiplies with one byte
        BASIC_B9BC       = $B9BC      ; Constant 1.00 (table of constants in extended floating point format for LOG)
        BASIC_B9C1       = $B9C1      ; Constant 03 (grade of polynome, then 4th coefficient)
        BASIC_B9C2       = $B9C2      ; Constant 0.434255942 (1st coefficient)
        BASIC_B9C7       = $B9C7      ; Constant 0.576584541 (2nd coefficient)
        BASIC_B9CC       = $B9CC      ; Constant 0.961800759 (3rd coefficient)
        BASIC_B9D1       = $B9D1      ; Constant 2.885390073 (4th coefficient)
        BASIC_B9D6       = $B9D6      ; Constant 0.707106781 = 1/SQR(2)
        BASIC_B9DB       = $B9DB      ; Constant 1.41421356 = SQR(2)
        BASIC_B9E0       = $B9E0      ; Constant -0.5
        BASIC_B9E5       = $B9E5      ; Constant 0.693147181 = LOG(2)
        BASIC_LOG        = $B9EA      ; BASIC-function LOG
        BASIC_BA28       = $BA28      ; FAC = constant (accu/Y reg) * FAC
        BASIC_MULTARG    = $BA30      ; FAC = ARG * FAC
        BASIC_BA59       = $BA59      ; Multiplies FAC with one byte and stores result to $26 .. $2A
        BASIC_BA8C       = $BA8C      ; ARG = constant (accu/Y reg)
        BASIC_BAB7       = $BAB7      ; Checks FAC and ARG
        BASIC_FACBY1     = $BAE2      ; FAC = FAC * 10
        BASIC_10         = $BAF9      ; Constant 10 as extended floating point format
        BASIC_OVER10     = $BAFE      ; FAC = FAC / 10
        BASIC_BB0F       = $BB0F      ; FAC = constant (accu/Y reg) / FAC
        BASIC_DIVARG     = $BB14      ; FAC = ARG / FAC
        BASIC_DIVZERO    = $BB8A      ; Output error message ?DIVISION BY ZERO
        BASIC_BBA2       = $BBA2      ; Transfer constant (accu/Y reg) to FAC
        BASIC_BBC7       = $BBC7      ; FAC to accu #4 ($5C to $60)
        BASIC_BBCA       = $BBCA      ; FAC to accu #3 ($57 to $5B)
        BASIC_BBD0       = $BBD0      ; FAC to variable (the address, where $49 points to)
        BASIC_BBFC       = $BBFC      ; ARG to FAC
        BASIC_BC0C       = $BC0C      ; FAC (rounded) to ARG
        BASIC_BC1B       = $BC1B      ; Rounds FAC
        BASIC_BC2B       = $BC2B      ; Get sign of FAC: A=0 if FAC=0, A=1 if FAC positive, A=$FF if FAC negative
        BASIC_BC39       = $BC39      ; BASIC function SGN
        BASIC_BC58       = $BC58      ; BASIC function ABS
        BASIC_BC5B       = $BC5B      ; Compare constant (accu/Y reg) with FAC: A=0 if equal, A=1 if FAC greater, A=$FF if FAC smaller
        BASIC_BC9B       = $BC9B      ; FAC to integer: converts FAC to 4-byte integer
        BASIC_BCCC       = $BCCC      ; BASIC function INT
        BASIC_BCF3       = $BCF3      ; Conversion PETSCII string to floating-point format
        BASIC_BDB3       = $BDB3      ; Constant 9999999.9 (3 constants for float PETSCII conversion)
        BASIC_BDB8       = $BDB8      ; Constant 99999999
        BASIC_BDBD       = $BDBD      ; Constant 1000000000
        BASIC_BDC2       = $BDC2      ; Output of "IN" and line number (from CURLIN $39, $3A)
        BASIC_INTOUT     = $BDCD      ; Output positive integer number in accu/X reg (X lo, A hi)
        BASIC_BDDD       = $BDDD      ; Convert FAC to PETSCII string which starts with $0100 and ends with $00. Start address in accu/Y reg.
        BASIC_BE68       = $BE68      ; TI to string: convert TI to PETSCII string which starts with $0100 and ends with $00
        BASIC_BF11       = $BF11      ; Constant 0.5
        BASIC_BF16       = $BF16      ; Constant tables for integer PETSCII conversion
        BASIC_BF3A       = $BF3A      ; Constant tables to convert TI to TI$
        BASIC_BF71       = $BF71      ; BASIC function SQR
        BASIC_BF78       = $BF78      ; Power function FAC = ARG to the power of constant (accu/Y reg)
        BASIC_BF7B       = $BF7B      ; Power function FAC = ARG to the power of FAC
        BASIC_BFB4       = $BFB4      ; Makes FAC negative
        BASIC_BFBF       = $BFBF      ; Constant 1.44269504 = 1/LOG(2) (table of 8 constants to evaluate EXP - polynomal table)
        BASIC_BFC4       = $BFC4      ; Constant 07: 7 = Grade of polynome (followed by 8 coefficient constants)
        BASIC_BFC5       = $BFC5      ; Constant 2.149875 E-5
        BASIC_BFCA       = $BFCA      ; Constant 1.435231 E-4
        BASIC_BFCF       = $BFCF      ; Constant 1.342263 E-3
        BASIC_BFD4       = $BFD4      ; Constant 9.641017 E-3
        BASIC_BFD9       = $BFD9      ; Constant 5.550513 E-2
        BASIC_BFDE       = $BFDE      ; Constant 2.402263 E-4
        BASIC_BFE3       = $BFE3      ; Constant 6.931471 E-1
        BASIC_BFE8       = $BFE8      ; Constant 1.00
        BASIC_BFED       = $BFED      ; BASIC function EXP

