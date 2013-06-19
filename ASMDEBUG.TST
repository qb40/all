DECLARE FUNCTION tohex$ (a1&)
DECLARE FUNCTION removetab$ (k1$)
DECLARE FUNCTION replace$ (s$, r1$, r2$) 'string,string to change,string to replace
'Define a Procedure
DIM Proc.names(100) AS STRING
DIM Proc.instruction(100) AS STRING
DIM label(100) AS STRING
DIM labeladdr(100) AS STRING
procno% = 1
procsearch% = 0

'Clear screen
CLS

'Wait for user
'If key=enter then only enter to direct entry mode else
'move on to normal compile mode
k$ = INPUT$(1)
IF (k$ <> CHR$(13)) THEN
INPUT "File name"; sourcefile$
GOTO transfer
END IF

'+++++++++++++++++
'Direct Entry Mode
'+++++++++++++++++
'User will write down his ASM program here
INPUT "File Name"; sourcefile$
OPEN "O", #1, sourcefile$
PRINT
PRINT
COLOR 10
DO
LINE INPUT "Instruction>", a$
IF (a$ = "^z" OR a$ = "^Z") THEN EXIT DO
PRINT #1, a$
LOOP
CLOSE #1



'+++++++++++++++++++
'Normal Compile Mode
'+++++++++++++++++++
'Here the ASM program will be transferred to DEBUG for compilation
transfer:
PRINT
PRINT
COLOR 12
'=> Read file   = sourcefile$
'=> Result file = compiled$

'I input the compiled file here
INPUT "Result compiled file"; compiled$
'I am setting the initial size as zero
st1$ = "0"
PRINT
PRINT
'I show my styles here
COLOR 14
PRINT "Transferring Program to DEBUG.COM . . . "

'Create temporary real mnemonic program from source code
'Temporary file =  "zzzzzzzz.ini"

'Check if source file is present
fl1$ = sourcefile$
OPEN "B", #1, fl1$
IF (LOF(1) = 0) THEN
PRINT "Source File : "; fl1$; " : not found!"
CLOSE #1
KILL fl1$
k$ = INPUT$(1)
CLOSE #1
SYSTEM
END IF
CLOSE #1

'Open source file
OPEN "I", #1, fl1$
'Open temporary file
OPEN "O", #2, "zzzzzzzz.ini"
OPEN "O", #3, "zzzzzzzy.ini"

'My first task is to read all labels
labelno% = 0
DO UNTIL EOF(1)
LINE INPUT #1, ln$
'Check for label
IF (LEFT$(ln$, 2) = "::") THEN
labelno% = labelno% + 1
label(labelno%) = ln$ + " "
END IF
LOOP
CLOSE #1
OPEN "I", #1, fl1$


'Set required variables
procno% = 0
procstatus% = 0
prints% = 1

DO UNTIL EOF(1)
LINE INPUT #1, ln$
        'Check if the given one is a procedure(=>)
        IF (LEFT$(ln$, 2) = "=>") THEN
        procno% = procno% + 1
        Proc.names(procno%) = ln$
        procstatus% = 1
        PRINT #3, LTRIM$(RTRIM$(STR$(procno%)))
        prints% = 0
       
        'Check for a procedure comment
        ELSEIF (LEFT$(ln$, 1) = "#") THEN
        PRINT #3, ln$
        prints% = 0
        END IF

'Make ready for saving line
ln$ = removetab$(ln$)
ln1$ = ""

        'Remove comments
        FOR i% = 1 TO LEN(ln$)
        m$ = MID$(ln$, i%, 1)
        IF (m$ = ";") THEN EXIT FOR ELSE ln1$ = ln1$ + m$
        NEXT
ln$ = ln1$

        'Store 1st program line
        IF (prints% = 1) THEN
        IF (procstatus% = 1) THEN
        IF (LEFT$(ln$, 2) <> "::") THEN
        IF (LEN(ln$) <> 0) THEN
        Proc.instruction(procno%) = ln$
        procstatus% = 0
        END IF
        END IF
        END IF
        END IF

        'Check if a label has been used
        IF (prints% = 1) THEN
        FOR lv1% = 1 TO labelno%
        IF (INSTR(ln$, label(lv1%)) <> 0) THEN
        PRINT #2, "--" + label(lv1%)
        ln$ = replace$(ln$, label(lv1%), "0")
        EXIT FOR
        END IF
        NEXT
        END IF

'Save line
IF (prints% = 1 AND ln$ <> "") THEN PRINT #2, ln$
prints% = 1
LOOP
CLOSE #1, #2, #3

'Now the program contains the only invalid mnemonic : ::LABEL,--::LABEL
'It can only be removed by first transferring it to DEBUG.COM and
'then making the required changes.
'  ::LABEL indicates a label
'--::LABEL indicates use of a label

'Writing the transfer file (TRANSFER.INI)
OPEN "I", #1, "zzzzzzzz.ini"
OPEN "O", #2, "transfer.ini"

PRINT #2, "a 100"
DO UNTIL EOF(1)
LINE INPUT #1, ln$
PRINT #2, ln$
LOOP

PRINT #2, ""
PRINT #2, ""
PRINT #2, "r ip"
PRINT #2, "100"
PRINT #2, "r cx"
PRINT #2, "0"
PRINT #2, ""
PRINT #2, "n " + compiled$
PRINT #2, "w"
PRINT #2, ""
PRINT #2, "q"
CLOSE #1, #2

'Now transfer it
SHELL "debug <transfer.ini >check.ini"

'Now from CHECK.INI we can convert the temporary file to raw mnemonic
'and then we can save it in the temporary file
OPEN "I", #1, "check.ini"
OPEN "O", #2, "zzzzzzzz.ini"

'Remove all invalid ::LABEL directive
label2no% = 0
DO UNTIL EOF(1)
LINE INPUT #1, ln$
ln1$ = ""
'Check out wether it is an ERROR statement
IF (LEN(ln$) > 10) THEN
IF (MID$(ln$, 5, 1) = ":") THEN
'I have detected a statement
        k1$ = MID$(ln$, 6, 4)
        k2$ = RIGHT$(ln$, LEN(ln$) - 10)
        'Check if it is ::LABEL
        IF (LEFT$(k2$, 2) = "::") THEN
        'Yes, it is, so modify it
        label2no% = label2no% + 1
        labeladdr(label2no%) = RTRIM$(LTRIM$(HEX$(VAL("&H" + k1$))))
        ln1$ = ""
        ELSE
        'No, it is not, so simply save it
        ln1$ = k2$
        END IF
END IF
END IF
IF (ln1$ <> "") THEN PRINT #2, ln1$
LOOP
CLOSE #1, #2
OPEN "I", #1, "check.ini"
OPEN "O", #2, "zzzzzzzz.ini"


'Now I have to remove all --::LABEL directive
DO UNTIL EOF(1)
LINE INPUT #1, ln$
ln1$ = ""
'Check out wether it is an ERROR statement
IF (LEN(ln$) > 10) THEN
IF (MID$(ln$, 5, 1) = ":") THEN
'I have detected a statement
        k1$ = MID$(ln$, 6, 4)
        k2$ = RIGHT$(ln$, LEN(ln$) - 10)
        'Check if it is --::LABEL directive
        IF (LEFT$(k2$, 4) = "--::") THEN
        'Yes, it is, so modify it
        FOR lv1% = 1 TO labelno%
        IF (RIGHT$(k2$, LEN(k2$) - 2) = label(lv1%)) THEN
        lbl% = lv1%
        EXIT FOR
        END IF
        NEXT
        DO
        LINE INPUT #1, ln$
        IF (LEN(ln$) > 10) THEN k2$ = RIGHT$(ln$, LEN(ln$) - 10) ELSE k2$ = ""
        ln1$ = replace$(k2$, "0", labeladdr(lbl%))
        LOOP UNTIL (INSTR(ln$, "^") = 0 AND LEN(ln$) > 10)
        ELSE
        'No, so leave it as it is
        ln1$ = k2$
        END IF
END IF
END IF
IF (ln1$ <> "" AND LEFT$(ln1$, 2) <> "::") THEN PRINT #2, ln1$
LOOP
CLOSE #1, #2


'determine file size
OPEN "I", #1, "check.ini"
DO UNTIL EOF(1)
LINE INPUT #1, ln$
IF (LEN(ln$) > 5) THEN
IF (MID$(ln$, 5, 1) = ":") THEN
ln1$ = MID$(ln$, 6, 4)
END IF
END IF
LOOP
CLOSE #1
size& = VAL("&H" + LEFT$(ln1$, 2))
size& = size& * 256
size& = size& + VAL("&H" + RIGHT$(ln1$, 2))
size& = size& - &H100
size$ = HEX$(size&)

'write to TRANSFER.INI
'Writing the transfer file (TRANSFER.INI)
OPEN "I", #1, "zzzzzzzz.ini"
OPEN "O", #2, "transfer.ini"

PRINT #2, "a 100"
DO UNTIL EOF(1)
LINE INPUT #1, ln$
PRINT #2, ln$
LOOP

PRINT #2, ""
PRINT #2, ""
PRINT #2, "r ip"
PRINT #2, "100"
PRINT #2, "r cx"
PRINT #2, size$
PRINT #2, ""
PRINT #2, "n " + compiled$
PRINT #2, "w"
PRINT #2, ""
PRINT #2, "q"
CLOSE #1, #2

'Now transfer it
SHELL "debug <transfer.ini >check.ini"


'Now show offests of procedures
'procno% = 1
'procsearch% = 0
procread% = 1
OPEN "I", #1, "check.ini" 'Create PROC.INI file
OPEN "O", #2, "proc.ini"
OPEN "I", #3, "zzzzzzzy.ini"
LINE INPUT #3, sk$

DO UNTIL EOF(1)
LINE INPUT #1, kk$
IF (LEN(kk$) > 8) THEN
k1$ = RTRIM$(LTRIM$(RIGHT$(kk$, LEN(kk$) - 10)))
ELSE
k1$ = ""
GOTO nextproc
END IF
a1$ = removetab$(Proc.instruction(procread%))
IF (k1$ = a1$) THEN
PRINT #2, "Procedure number :"; procread%
PRINT #2, "===================================================="
PRINT #2, "|||||==="; Proc.names(procread%)
PRINT #2, "----------------------------------------------------"

nexts% = 0
DO UNTIL EOF(3)
LINE INPUT #3, sk$
IF (VAL(sk$) = procread% + 1) THEN EXIT DO
PRINT #2, "#"; sk$
LOOP

k1$ = MID$(kk$, 6, 4)
a1% = VAL("&H" + k1$) - &H100
PRINT #2, "Address start :", a1%; " offset."
PRINT #2, "===================================================="
PRINT #2, ""
PRINT #2, ""
PRINT #2, ""
PRINT #2, ""
procread% = procread% + 1
IF (procread% > procno%) THEN EXIT DO
END IF
nextproc:
LOOP
CLOSE #1, #3

PRINT #2, ""
PRINT #2, ""
PRINT #2, "Labels"
PRINT #2, "||||||"
PRINT #2, ""
FOR i% = 1 TO labelno%
PRINT #2, "Label :> "; label(i%)
PRINT #2, "Offset: "; VAL("&H" + labeladdr(i%)) - &H100
PRINT #2, ""
PRINT #2, ""
NEXT
CLOSE #2

KILL "transfer.ini"
KILL "zzzzzzzz.ini"
KILL "zzzzzzzy.ini"
SYSTEM

FUNCTION removetab$ (k1$)
b$ = ""
FOR i% = 1 TO LEN(k1$)
a$ = MID$(k1$, i%, 1)
IF (a$ = CHR$(9)) THEN b$ = b$ + " " ELSE b$ = b$ + a$
NEXT
b$ = LTRIM$(RTRIM$(b$))
removetab$ = b$
END FUNCTION

FUNCTION replace$ (s$, r1$, r2$) 'string,string to change,string to replace
l% = LEN(s$) - LEN(r1$) + 1   ' 6(12-7+1)
s% = LEN(r1$)
b$ = ""
i% = 1
DO UNTIL (i% > l%)
k$ = MID$(s$, i%, s%)
IF (k$ = r1$) THEN
b$ = b$ + r2$
i% = i% + s%
ELSE
b$ = b$ + LEFT$(k$, 1)
i% = i% + 1
END IF
LOOP
IF (i% < LEN(s$)) THEN z$ = RIGHT$(s$, LEN(s$) - i% - 1) ELSE z$ = ""
replace$ = b$ + z$
END FUNCTION

FUNCTION tohex$ (a1&)
b1& = a1&
st$ = SPACE$(8)
DEF SEG = VARSEG(st$)
mem1& = SADD(st$)
ps& = 7
DO UNTIL (b1& < 1)
z% = b1& MOD 16
b1& = b1& \ 16
IF (z% < 10) THEN z% = z% + 48 ELSE z% = z% + 55
POKE mem1& + ps&, z%
ps& = ps& - 1
LOOP
DEF SEG
st$ = LTRIM$(RTRIM$(st$))
tohex$ = st$
END FUNCTION

