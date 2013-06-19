DECLARE FUNCTION xms.unlock$ (es%, bx%)
DECLARE FUNCTION XMS.FREE$ (es%, bx%)
DECLARE FUNCTION xms.realloc$ (es%, bx%)
DECLARE FUNCTION XMS.MOVE$ (es%, bx%)
DECLARE SUB DEMO ()
DECLARE FUNCTION xms.dealloc$ (es%, bx%)
DECLARE FUNCTION XMS.ALLOC$ (es%, bx%)
DECLARE FUNCTION INT2STR$ (SWORD%)
DECLARE SUB XMS.ENTRIE (es%, bx%)
DECLARE FUNCTION XMM.DETECT% ()
DECLARE FUNCTION BIOSXMS.DETECT% ()
DECLARE FUNCTION xms.lock$ (es%, bx%)
DECLARE FUNCTION xsm.unlock$ (es%, bx%)
DEMO
END

FUNCTION BIOSXMS.DETECT%
'------------------------------------------------------
'THIS FUNCTION RETURNS XMS MEMORY BLOCKS OF 1KB
'FOR 286 AND 386 WITHOUT XMS MANAGER INSTALLED

'STACKPASSING :AX%

'IN:

'OUT          :AX%--NR OF 1 KB EXTENDED MEMBLOCKS

'ERRORRETURN  :AX%= 0 THEN FUNCTION NOT SUPPORTED OR XMS
'                     HANDLER INSTALLED
'-------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX

asm$ = asm$ + CHR$(&HB4) + CHR$(&H88)               'MOV AH,88
asm$ = asm$ + CHR$(&HCD) + CHR$(&H15)               'INT 15
asm$ = asm$ + CHR$(&H72) + CHR$(&H5)                'JC 5
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H58)                            'POP AX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)    'RETF 2
'________________________________________
codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL absolute(AX%, codeoff%)
'________________________________________
DEF SEG
BIOSXMS.DETECT% = AX%
END FUNCTION

SUB DATASEGMENT (LENGTH%)
'THIS SUB IS OFFERING THE MINIMAL NECESSARY REQUIREMENTS FOR SETTING
'UP A DATASEGMENT IN YOUR ASSEMBLER SUBS.
'-------------------------------------------------------------------------
'DATA
DIM DATAS%(LENGTH%)
DATASEG% = VARSEG(DATAS%(0)): DATAOFF% = VARPTR(DATAS%(0))
DATASEG$ = INT2STR$(DATASEG%): DATAOFF$ = INT2STR$(DATAOFF%)
'-----------------------------------
'CODE
asm$ = asm$ + CHR$(&H50)                 'PUSH AX
asm$ = asm$ + CHR$(&H1E)                 'PUSH DS
asm$ = asm$ + CHR$(&HB8) + DATASEG$      'MOV AX,DATASEG$
asm$ = asm$ + CHR$(&H8E) + CHR$(&HD8)    'MOV DS,AX
asm$ = asm$ + CHR$(&H1F)                 'POP DS
asm$ = asm$ + CHR$(&H58)                 'POP AX
'------------------------------------------------------------------------
END SUB

SUB DEMO

CLS
IF XMM.DETECT THEN PRINT "XMM DETECTED" ELSE PRINT "TOTAL XMS AVAILABLE FOR BIOS :"; HEX$(BIOSXMS.DETECT%); ""
XMS.ENTRIE es%, bx%
PRINT "DRIVER ENTRIE SEG                          : "; HEX$(es%)
PRINT "DRIVER ENTRIE OFF                          : "; HEX$(bx%)


'Let us initialize a few:
'------------------------
alloc$ = XMS.ALLOC$(es%, bx%): dealloc$ = xms.dealloc$(es%, bx%):
realloc$ = xms.realloc$(es%, bx%): move$ = XMS.MOVE$(es%, bx%):
lock$ = xms.lock$(es%, bx%): unlock$ = xms.unlock$(es%, bx%)

stringseg% = VARSEG(alloc$): DEF SEG = stringseg%: free$ = XMS.FREE$(es%, bx%)
'--------------------------------------------------------------------------
'free memory ?
'--------------
codeoff% = SADD(free$): CALL absolute(AX%, dx%, codeoff%)
PRINT "TOTAL XMS IN KB/ BYTES                     :"; dx%; "("; dx% * &H400&; ")"
PRINT "FREE  XMS IN KB/ BYTES                     :"; AX%; "("; AX% * &H400&; ")"

'Allocating XMS:
'----------------
codeoff% = SADD(alloc$): dx% = 64'64 KB
CALL absolute(AX%, dx%, codeoff%): DEF SEG
IF AX% THEN PRINT "ALLOCATED REQUESTED XMS BLOCKS WITH HANDLE :"; dx% ELSE PRINT "ERROR ALLOCATING XMS "; dx%
handle% = dx%    'save handle

'And reallocating XMS:
'----------------------
codeoff% = SADD(realloc$): bx% = 100'bigger then 64 kB
dx% = handle%: CALL absolute(bx%, dx%, codeoff%)
IF dx% > 1 THEN PRINT "ERROR REALLOCATING XMS BLOCK "; handle%; "WITH NR.:"; dx% ELSE PRINT "REALLOCATED XMS BLOCK WITH HANDLE          :"; handle%


'And deallocating
'-----------------
codeoff% = SADD(dealloc$): dx% = handle%: DEF SEG = stringseg%
CALL absolute(dx%, codeoff%)
IF dx% > 1 THEN PRINT "ERROR DEALLOCATING XMS BLOCK "; handle%; "WITH NR.:"; dx% ELSE PRINT "DEALLOCATED XMS BLOCK WITH HANDLE          :"; handle%


'End of bare demo 1


'NOW LETS GO FOR SOMETHING REAL. DEMO 1 WIll initialize twice a string
'of 22000 bytes copy them to xms( giving free again our near string in basic
'and returning them after each other.
CLS
'INITIALIZE SOME STRING:
'-----------------------
STATIC a$: a$ = STRING$(22000, "1"): Segm% = VARSEG(a$): OFFJE% = SADD(a$)
'Allocating XMS:
'----------------

codeoff% = SADD(alloc$): dx% = 64'64 KB
CALL absolute(AX%, dx%, codeoff%)
IF AX% THEN PRINT "ALLOCATED REQUESTED XMS BLOCKS WITH HANDLE :"; dx% ELSE PRINT "ERROR ALLOCATING XMS "; HEX$(dx% AND &HFF)
handle% = dx%    'save handle

'SET UP THE PARAMETERBLOCK:
DIM movparams%(7): segp% = VARSEG(movparams%(0)): offp% = VARPTR(movparams%(0))
movparams%(0) = 22000                'NUMBER OF BYTES TO MOVE
movparams%(1) = 0                    '(32 bits)
movparams%(2) = 0                    'HANDLE =0 MEANING A LOW.MEM
movparams%(3) = SADD(a$)             'OFFSET SOURCE
movparams%(4) = Segm%                'SEGMENT SOURCE
movparams%(5) = handle%              'XMS HANDLE
movparams%(6) = 0: movparams%(7) = 0    'DESTINY OFFSET:SEGMENT 0

'And let us move the 1 to the 0 to 22000 positions
'---------------
codeoff% = SADD(move$): DS% = segp%: SI% = offp%
CALL absolute(DS%, SI%, codeoff%)
IF DS% THEN PRINT "MOVE BLOCK TO XMS FAILED ON ERROR          : "; HEX$(DS% AND &HFF) ELSE PRINT "SUCSES MOVEING BLOCK TO XMS"

a$ = "": a$ = STRING$(22000, "R")
movparams%(0) = 22000                'NUMBER OF BYTES TO MOVE
movparams%(1) = 0                    '(32 bits)
movparams%(2) = 0                    'HANDLE =0 MEANING A LOW.MEM
movparams%(3) = SADD(a$)             'OFFSET SOURCE
movparams%(4) = Segm%                'SEGMENT SOURCE
movparams%(5) = handle%              'XMS HANDLE
movparams%(6) = 22000: movparams%(7) = 0 'DESTINY OFFSET:SEGMENT 0

'And let us move the R to the 22000 to 44000 positions
'--------------------------------------------------
codeoff% = SADD(move$): DS% = segp%: SI% = offp%
CALL absolute(DS%, SI%, codeoff%)
IF DS% THEN PRINT "MOVE BLOCK TO XMS FAILED ON ERROR          : "; HEX$(DS% AND &HFF) ELSE PRINT "SUCSES MOVEING BLOCK TO XMS"

PRINT
COLOR 0, 7: PRINT "Both strings copied to XMS now"
PRINT "Stringspace is free again im Basic": COLOR 7, 0:
PRINT : PRINT "Press a key to move on": PRINT
SLEEP:
'Let us fill a$ with some other rubbish
a$ = "": a$ = STRING$(22000, "E"):


'And move the original "1" back
'--------------------------------
movparams%(0) = 22000                'NUMBER OF BYTES TO MOVE
movparams%(1) = 0                    '(32 bits)=64 KB
movparams%(2) = handle%              'XNS HANDLE
movparams%(3) = 0                    'Not required for XMS
movparams%(4) = 0                    'SOURCE OFFSET:SEGMENT
movparams%(5) = 0                    'HANDLE= 0 meaning low.mem
movparams%(6) = SADD(a$):            'Required for low.mem:
movparams%(7) = Segm%                'DESTINY OFFSET:SEGMENT 0


'And let us move
'---------------
codeoff% = SADD(move$): DS% = segp%: SI% = offp%
CALL absolute(DS%, SI%, codeoff%)
IF DS% THEN PRINT "MOVE BLOCK FROM XMS FAILED ON ERROR          : "; HEX$(DS% AND &HFF) ELSE PRINT "SUCSES MOVEING BLOCK FROM XMS"

PRINT : COLOR 0, 7:
LOCATE 13, 1: PRINT "Getting back our first string from XMS"
COLOR 7, 0:
VIEW PRINT 15 TO 24
PRINT a$: SLEEP

'and move the original "1"back:
'--------------------------------
movparams%(0) = 22000                'NUMBER OF BYTES TO MOVE
movparams%(1) = 0                    '(32 bits)=64 KB
movparams%(2) = handle%              'XNS HANDLE
movparams%(3) = 22000                'Not required for XMS
movparams%(4) = 0                    'SOURCE OFFSET:SEGMENT
movparams%(5) = 0                    'HANDLE= 0 meaning low.mem
movparams%(6) = SADD(a$):            'Required for low.mem:
movparams%(7) = Segm%                'DESTINY OFFSET:SEGMENT 0


'And let us move
'---------------
codeoff% = SADD(move$): DS% = segp%: SI% = offp%
CALL absolute(DS%, SI%, codeoff%)
IF DS% THEN PRINT "MOVE BLOCK FROM XMS FAILED ON ERROR          : "; HEX$(DS% AND &HFF) ELSE PRINT "SUCSES MOVEING BLOCK FROM XMS"

VIEW PRINT:
COLOR 0, 7: LOCATE 13, 1: PRINT "Getting back our second string from XMS"
COLOR 7, 0
VIEW PRINT 15 TO 24



'And deallocating
'-----------------
codeoff% = SADD(dealloc$): dx% = handle%: DEF SEG = stringseg%
CALL absolute(dx%, codeoff%)
IF dx% > 1 THEN PRINT "ERROR DEALLOCATING XMS BLOCK "; handle%; "WITH NR.:"; dx% ELSE PRINT "DEALLOCATED XMS BLOCK WITH HANDLE          :"; handle%


'Did our moves had have sucses ?

PRINT a$: a$ = ""
'I think so, do you think so too ?

SLEEP: CLS

'----------------
'demo 2 locking:
'----------------

'free memory ?
'--------------
codeoff% = SADD(free$): CALL absolute(AX%, dx%, codeoff%)
PRINT "TOTAL XMS IN KB/ BYTES                     :"; dx%; "("; dx% * &H400&; ")"
PRINT "FREE  XMS IN KB/ BYTES                     :"; AX%; "("; AX% * &H400&; ")"

'Allocating XMS:
'----------------
codeoff% = SADD(alloc$): dx% = 64'64 KB
CALL absolute(AX%, dx%, codeoff%):
IF AX% THEN PRINT "ALLOCATED REQUESTED XMS BLOCKS WITH HANDLE :"; dx% ELSE PRINT "ERROR ALLOCATING XMS "; dx%
handle% = dx%    'save handle

'Locking XMS:
'----------------
codeoff% = SADD(lock$): dx% = handle%
CALL absolute(bx%, dx%, codeoff%):
IF dx% THEN dx$ = HEX$(dx%): bx$ = HEX$(bx%): PRINT "LOCKED XMS HANDLE" + SPACE$(26) + ":"; handle%; "to segment: "; STRING$(4 - LEN(dx$), "0"); dx$; STRING$(4 - LEN(bx$), "0"); bx$ ELSE PRINT "ERROR LOCKING XMS HANDLE       : "; HEX$(bx%)

'Unlocking
'----------
codeoff% = SADD(unlock$): dx% = handle%
CALL absolute(dx%, codeoff%):
IF dx% THEN PRINT "ERROR UNLOCKING XMS HANDLE "; HEX$(dx%) ELSE PRINT "UNLOCKED XMS HANDLE" + SPACE$(24) + ":"; handle%

'And deallocating
'-----------------
codeoff% = SADD(dealloc$): dx% = handle%:
CALL absolute(dx%, codeoff%)
IF dx% > 1 THEN PRINT "ERROR DEALLOCATING XMS BLOCK "; handle%; "WITH NR.:"; dx% ELSE PRINT "DEALLOCATED XMS BLOCK WITH HANDLE          :"; handle%

DEF SEG



END SUB

SUB helpjes
'-----------------------------------
'mov bx,[bp+06]
'mov dx,[bx]
'mov bx,[bx+08]
'mov bx,[bx]
'-----------------------------------

asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5F) + CHR$(&H8)
asm$ = asm$ + CHR$(&H8B) + CHR$(&H1F)
'________________________________________
codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL absolute(codeoff%)
'________________________________________
DEF SEG


END SUB

FUNCTION INT2STR$ (SWORD%)
'THIS FUNCTION IS TRANSLATING SWORD INTEGERS INTO A STRING. ITS ONLY USE
'IS WHEN YOU STILL USE ASM$ FOR ASSEMBLER FUNCTIONS( LIKE I DO). IN THAT
'CASE YOU CAN MAKE YOUR INTEGER VALUES USABLE ..
'THIS FUNCTION SIMPLY TRANSLATES THE HEXA BYTES
'INTO STRINGBYTES AS IS.
'----------------------------------------------------
DEF SEG = VARSEG(SWORD%)
PTR% = VARPTR(SWORD%)
INT2STR$ = CHR$(PEEK(PTR%)) + CHR$(PEEK(PTR% + 1))
DEF SEG

END FUNCTION

SUB POKESTRING (SEGJE%, OFFJE%, MAIN$)
'------------------------------------------------------
'THIS FUNCTION POKES A STRING (MIGHT BE ASCIIZ)INTO
'MEMORY AT A GIVEN LOCATION, MAKING IT POSSIBLE TO
'ACCESS STRINGS IN BYTE FORM
'------------------------------------------------------

DEF SEG = SEGJE%
FOR i% = 0 TO LEN(MAIN$) - 1
  POKE OFFJE% + i%, ASC(MID$(MAIN$, i% + 1, 1))
NEXT
DEF SEG

END SUB

SUB PROCSEGMENT (LENGTH%)
'------------------------------------------------------------------------
'THIS SUB IS GIVING THE MINIMAL REQUIREMENTS FOR AN PROCEDURE SEGMENT.
'IN GENERAL THIS PROC SEGMENT SHOULD BE PART OF YOUR MAIN PROGRAM.
'------------------------------------------------------------------------
'PROC
'-----
DIM PROCS%(LENGTH%)
PROCSEG% = VARSEG(PROCS%(0)): PROCOFF% = VARPTR(PROCS%(0))
PRNOFF% = PROCOFF% + LEN(NEWINT$): PROCOFF$ = INT2STR$(PROCOFF%)

'CODE
'-----
asm$ = asm$ + CHR$(&H9A) + NEWINTOFF$ + PROCSEG$  'CALL PROCSEG[NEWINTOFF$]

END SUB

FUNCTION XMM.DETECT%
'------------------------------------------------------------
'THIS FUNCTION RETURNS IF XMS MEMORY MANAGER INSTALLED

'STACKPASSING :AX%

'IN:

'OUT          :AX% =  &H0080 XMM INSTALLED

'ERRORRETURN  :AX% <> &H0080 XMM NOT INSTALLED( QBASIC:AX%=0)
'------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                           'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)              'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                           'PUSH AX
asm$ = asm$ + CHR$(&HB8) + CHR$(&H0) + CHR$(&H43)  'MOV AX,4300
asm$ = asm$ + CHR$(&HCD) + CHR$(&H2F)              'INT 2F
asm$ = asm$ + CHR$(&H3C) + CHR$(&H80)              'CMP AL,80
asm$ = asm$ + CHR$(&H74) + CHR$(&H2)               'JZ 2
asm$ = asm$ + CHR$(&H31) + CHR$(&HC0)              'XOR AX,AX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)  'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)               'MOV [BX],AX

asm$ = asm$ + CHR$(&H58)                           'POP AX
asm$ = asm$ + CHR$(&H5D)                           'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)   'RETF 2
'________________________________________
codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL absolute(AX%, codeoff%)
'________________________________________
DEF SEG
XMM.DETECT% = AX%
END FUNCTION

FUNCTION XMS.ALLOC$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION RETURNS IF FREE AND TOTAL XMS MEMORY

'STACKPASSING :AX%,DX%
'IN:          :ES[BX] DRIVER ENTRIE
'              DX%    NUMBER OF KB BLOCKS REQUESTED

'OUT          :AX% =  0 FAIL , AX%=1 SUCSES
'              DX% =  HANDLE /ERROR
'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)
asm$ = asm$ + CHR$(&H50)
asm$ = asm$ + CHR$(&H52)

asm$ = asm$ + CHR$(&HB4) + CHR$(&H9)                'MOV AH,9
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] 1 KB BLOCKS
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H2)                'JNZ 2
asm$ = asm$ + CHR$(&H89) + CHR$(&HDA)               'MOV DX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP AX
asm$ = asm$ + CHR$(&H58)                            'POP DX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4

XMS.ALLOC$ = asm$

END FUNCTION

FUNCTION xms.dealloc$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION FREES ALLOCATED XMS MEMORY BLOCKS

'STACKPASSING :DX%

'IN:          :ES[BX] DRIVER ENTRIE
'              DX%    HANDLE OF XMS BLOCK ALLOCATED

'OUT          :AX% =  0 FAIL , AX%=1 SUCSES
'              BL% =  ERROR
'              QBASIC : RETURNS DX%=1 SUCSES ELSE ERRORCODE
'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%):
'------------------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX
asm$ = asm$ + CHR$(&H52)                            'PUSH DX

asm$ = asm$ + CHR$(&HB4) + CHR$(&HA)                'MOV AH,A
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] HANDLE
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H2)                'JNZ 2
asm$ = asm$ + CHR$(&H89) + CHR$(&HD9)               'MOV AX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP dX
asm$ = asm$ + CHR$(&H58)                            'POP aX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)    'RETF 2

xms.dealloc$ = asm$
END FUNCTION

SUB XMS.DISPATCH (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION IS A DISPATCHER FOR ALL EXTENDED MEMORY FUNCTIONS
' (8-F)

'YOU SHOULD HAVE EXECUTED THE FUNCTIONS XMS.DETECT AND XMS.ENTRIE FIRST


'STACKPASSING :AX%,BX%,DSDX%

'IN/ OUT      :AX%   AX
'             :BX%   BX
'             :DSDX% DS OR DX DEPENDING ON THE REQUESTED FORMAT

'OUT(GENERAL) :AX% =  0 FAIL , AX%=1 SUCSES
'              DX% =  HANDLE /ERROR
'              BX% LOW BYTE= ERRORCODE

'LOOK FOR SPECIFICS FOR FUNCTIONS IN THE XMS.DOC SUB

'THIS FUNCTION CAN BE INITIALIZED!
'----------------------------------------------------------------
'INIT DRIVER ENTRIE
'-------------------
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)
asm$ = asm$ + CHR$(&H50)
asm$ = asm$ + CHR$(&H52)

asm$ = asm$ + CHR$(&HB4) + CHR$(&H9)                'MOV AH,9
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] 1 KB BLOCKS
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H2)                'JNZ 2
asm$ = asm$ + CHR$(&H89) + CHR$(&HDA)               'MOV DX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP AX
asm$ = asm$ + CHR$(&H58)                            'POP DX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4
'________________________________________
codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL absolute(AX%, dx%, codeoff%)
'________________________________________

END SUB

SUB XMS.ENTRIE (es%, bx%)
'--------------------------------------------------
'THIS FUNCTION DETECTS DRIVER ENTRIE,
'THE FREE XMS SIZE AND TOTAL XMS SIZE

'STACK PASSING : ES%,BX%

'IN            :

'OUT           : ES%= DRIVER ENTRIE SEGMENT
'                BX%= DRIVER ENTRIE OFFSET

'--------------------------------------------------
asm$ = asm$ + CHR$(&H55)                             'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)                'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                             'PUSH AX
asm$ = asm$ + CHR$(&H53)                             'PUSH BX
asm$ = asm$ + CHR$(&H6)                              'PUSH ES

asm$ = asm$ + CHR$(&HB8) + CHR$(&H10) + CHR$(&H43)   'MOV AX,4310
asm$ = asm$ + CHR$(&HCD) + CHR$(&H2F)                'INT 2F GET DRIVER ENTRIE
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)                'MOV AX,BX (NEED TO STORE BX)
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)    'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H8C) + CHR$(&H7)                 'MOV [BX],ES SEG DRIVER
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)    'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                 'MOV [BX],AX OFF DRIVER

asm$ = asm$ + CHR$(&H7)                              'POP ES
asm$ = asm$ + CHR$(&H5B)                             'POP BX
asm$ = asm$ + CHR$(&H58)                             'POP AX
asm$ = asm$ + CHR$(&H5D)                             'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)     'RETF 4
'________________________________________
codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL absolute(es%, bx%, codeoff%)

'________________________________________
DEF SEG

END SUB

FUNCTION XMS.FREE$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION RETURNS IF FREE AND TOTAL XMS MEMORY

'STACKPASSING :AX%,DX%

'IN:          :ES[BX] DRIVER ENTRIE

'OUT          :AX% =  FREE XMS
'              DX% =  TOTAL XMS

'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'----------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX
asm$ = asm$ + CHR$(&H53)                            'PUSH BX
asm$ = asm$ + CHR$(&H52)                            'PUSH DX

asm$ = asm$ + CHR$(&HB4) + CHR$(&H8)                'MOV AH,0800 QUERY FREE XMS
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP DX
asm$ = asm$ + CHR$(&H5B)                            'POP BX
asm$ = asm$ + CHR$(&H58)                            'POP AX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4



XMS.FREE$ = asm$
END FUNCTION

FUNCTION xms.lock$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION LOCKS XMS and return fysical adress

'STACKPASSING :BX%,DX%
'IN:          :ES[BX] DRIVER ENTRIE
'              DX%    NUMBER OF KB BLOCKS REQUESTED

'OUT          :AX%     =  0 FAIL , AX%=1 SUCSES
'              DX%:BX% =  LINEAR ADRESS/ERROR(in BL)
'QBASIC return 0 in DX% on error
'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                            'push bp
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'mov bp,sp
asm$ = asm$ + CHR$(&H50)                            'push ax
asm$ = asm$ + CHR$(&H52)                            'push dx

asm$ = asm$ + CHR$(&HB4) + CHR$(&HC)                'MOV AH,C
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] handle
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H4)                'JNZ 4
asm$ = asm$ + CHR$(&H30) + CHR$(&HFF)               'XOR BH,BH  'to zero on error
asm$ = asm$ + CHR$(&H89) + CHR$(&HDA)               'MOV DX,BX  'and put in DX
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)               'mov ax,bx  'need bx
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP AX
asm$ = asm$ + CHR$(&H58)                            'POP DX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4

xms.lock$ = asm$

END FUNCTION

FUNCTION XMS.MOVE$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION MOVES MEMORY BLOCKS TO XMS

'STACKPASSING :DS%,SI%

'IN:          :DS[SI] PTR TO MOVESTRUCTURE
'
                 'DW  : NR OF BYTES TO MOVE
                 'W   : HANDLE SOURCE: 0000 FOR LOW MEM
                 'DW  : OFFSET:SEGMENT OF SOURCE
                 'W   : HANDLE DESTINY: XMS HANDLE
                 'DW  : OFFSET:SEGMENT OF DESTINY 0000 FOR XMS

'              ES[BX] DRIVER ENTRIE

'OUT          :AX =  0 FAIL , AX%=1 SUCSES
'              BL =  ERROR
'QBASIC:       DS%=  0 SUCSES
'                    ELSE ERRORCODE

'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                            'Push AX
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H56)                            'Push SI
asm$ = asm$ + CHR$(&H1E)                            'Push DS


asm$ = asm$ + CHR$(&HB4) + CHR$(&HB)                'MOV AH,B
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H37)               'MOV SI,[BX] OFFSET
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H8E) + CHR$(&H1F)               'MOV DS,[BX] SEGMENT
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX 'SUCSES ?
asm$ = asm$ + CHR$(&H74) + CHR$(&H2)                'JZ 2    'NO ? JUMP
asm$ = asm$ + CHR$(&H31) + CHR$(&HDB)               'XOR BX,BX
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)               'MOV AX,BX
asm$ = asm$ + CHR$(&H1F)                            'POP DS  'have to restore before [bx] is adressed
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5E)                            'POP SI
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4

XMS.MOVE$ = asm$

END FUNCTION

FUNCTION xms.realloc$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION REALLOCATES AN ALREADY ALLOCATED XMS_HANDLE

'STACKPASSING :BX%,DX%
'IN:          :BX%    New request( KB)
'              DX%    XMS Handle

'OUT          :AX% =  0 FAIL , AX%=1 SUCSES
'              BL% =  ERROR

'QBSASIC      :DX% returns errorcode or 1
'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                            'push bp
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'mov bp,sp
asm$ = asm$ + CHR$(&H50)                            'push ax
asm$ = asm$ + CHR$(&H52)                            'push dx

asm$ = asm$ + CHR$(&HB4) + CHR$(&HF)                'MOV AH,F
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] XMS handle
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5F) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H1F)               'MOV BX,[BX]
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H2)                'JNZ 2
asm$ = asm$ + CHR$(&H89) + CHR$(&HD9)               'MOV AX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX


asm$ = asm$ + CHR$(&H5A)                            'POP dX
asm$ = asm$ + CHR$(&H58)                            'POP aX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4


xms.realloc$ = asm$

END FUNCTION

FUNCTION xms.unlock$ (es%, bx%)
'----------------------------------------------------------------
'THIS FUNCTION UNLOCKS XMS HANDLE

'STACKPASSING :DX%
'IN:          :ES[BX] DRIVER ENTRIE
'              DX%    HANDLE

'OUT          :AX     =  0 FAIL , AX%=1 SUCSES
'              BL     =  ERROR(in BL)
'QBASIC returns BX%=0 or error
'----------------------------------------------------------------
'INIT
'-----
es$ = INT2STR$(es%): bx$ = INT2STR$(bx%)
'------------------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                            'push bp
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'mov bp,sp
asm$ = asm$ + CHR$(&H50)                            'push ax
asm$ = asm$ + CHR$(&H52)                            'push dx

asm$ = asm$ + CHR$(&HB4) + CHR$(&HD)                'MOV AH,D
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H8B) + CHR$(&H17)               'MOV DX,[BX] handle
asm$ = asm$ + CHR$(&H9A) + bx$ + es$                'CALL DRIVER
asm$ = asm$ + CHR$(&H9) + CHR$(&HC0)                'OR AX,AX
asm$ = asm$ + CHR$(&H75) + CHR$(&H2)                'JNZ 2
asm$ = asm$ + CHR$(&H89) + CHR$(&HD9)               'MOV AX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP AX
asm$ = asm$ + CHR$(&H58)                            'POP DX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)    'RETF 2

xms.unlock$ = asm$

END FUNCTION

